-module(game_serv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/6]).

-include("include/softstate.hrl").

-define(CONNECTION_TIMEOUT, 40000).
-define(DIFFICULT_CHANGE_SECONDS, 30).
-define( COUNTDOWN_TO_START_SECONDS , 5).

-type game_state_type() :: init | running | waiting_players | waiting_players_reconect.


-record( game_user, {
	pid :: pid(),
	user_id = undefined,
	monitor = undefined,
	is_ready = false,
	is_connected = true
}).

-record(game_state, {

	game_difficult_change_timer = undefined,
	time_difficult_change_left = 0,

	configuration_url = undefined,
	configuration_version = undefined,

	user1 = #game_user{} :: #game_user{},
	user2 = #game_user{} :: #game_user{},

	state = init :: game_state_type(),

	game_logic_state = #game{} :: #game{}
}).





start_link( User_pid, User_id, User_pid2, User_id2, Conf_version, Conf_url  ) ->
    gen_server:start_link(?MODULE, [ User_pid, User_id, User_pid2, User_id2, Conf_version, Conf_url ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #game_state{ }}.










handle_cast([User_pid, User_id, User_pid2, User_id2, Conf_version, Conf_url], State = #game_state{ }) ->

	lager:info("new game with user_id ~p user ~p",[User_pid,User_pid2]),

	gen_server:cast(self() , game_created ),

	Connection_monitor1 = monitor(process, User_pid),
	Connection_monitor2 = monitor(process, User_pid2),

	{noreply, State#game_state{
				user1 = #game_user{ pid = User_pid, user_id = User_id, monitor = Connection_monitor1 },
				user2 = #game_user{ pid = User_pid2, user_id = User_id2, monitor = Connection_monitor2 },
				state = init,
				configuration_url = Conf_url,
				configuration_version = Conf_version
			}
	};








handle_cast( game_created, State = #game_state{ user1 = User1, user2 = User2, state = Game_State} ) 
				when Game_State == init ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	Seed = random:uniform(2147483646),

	Starting_game_logic_state = game_logic:create_new_game( User1#game_user.pid, User2#game_user.pid, Seed ),

	gen_server:cast( User1#game_user.pid , { enter_game, self(), User2#game_user.user_id, Seed } ),
	gen_server:cast( User2#game_user.pid , { enter_game, self(), User1#game_user.user_id, Seed } ),

	{ noreply, State#game_state{ game_logic_state = Starting_game_logic_state, state = waiting_players } };










handle_cast( { user_ready, User_pid} , State = #game_state{ state = Game_State, user2 = User2, user1 = User1 } ) 
				when Game_State == waiting_players ->

	case User2#game_user.pid == User_pid of
		true ->
			case User1#game_user.is_ready of
				true ->		gen_server:cast(self() , start_game );
				false ->	nothing_happens
			end,
			{ noreply, State#game_state{ user2 = User2#game_user{ is_ready = true} } };
		false ->
			case User2#game_user.is_ready of
				true ->		gen_server:cast(self() , start_game );
				false ->	nothing_happens
			end,
			{ noreply, State#game_state{ user1 = User1#game_user{ is_ready = true} } }
	end;












handle_cast( start_game, State = #game_state{ time_difficult_change_left = Time_left, user1 = User1, user2 = User2, state = Game_State} ) 
				when Game_State == waiting_players,
						User1#game_user.is_ready == true,
							User2#game_user.is_ready == true ->
	
	lager:info("game ~p is going to start",[self()]),

	StartTime = swiss:unix_timestamp() + ?COUNTDOWN_TO_START_SECONDS, 	%the game will start in <COUNTDOWN_TO_START_SECONDS> seconds

	gen_server:cast( User1#game_user.pid , {game_start , StartTime} ),
	gen_server:cast( User2#game_user.pid , {game_start , StartTime} ),

	Game_difficult_timer = case Time_left of 
		0 ->			erlang:send_after(timer:seconds(?DIFFICULT_CHANGE_SECONDS), self(), difficult_change);
		_ ->			erlang:send_after(Time_left, self(), difficult_change)
	end,

	{ noreply, State#game_state{  state = running,
									game_difficult_change_timer = Game_difficult_timer,
										user1 = User1#game_user{ is_ready = false}, 
											user2 = User2#game_user{ is_ready = false} } };









handle_cast( { update_piece, X, Y, Angle, User_pid } , State = #game_state{}  ) ->
	try 
		New_game_state = game_logic:handle_update_piece( User_pid, X, Y, Angle,  State#game_state.game_logic_state),
		{ noreply, State#game_state{ game_logic_state = New_game_state } }
	catch
		throw:invalid_move ->
			%% HACKER
			lager:info("user is a hacker"),
			gen_server:cast( self(), { user_lost_game, User_pid } ),
			{ noreply, State }
	end;




handle_cast( { place_piece, X, Y, Angle, User_pid } , State = #game_state{}  ) ->
	try 
		New_game_state = game_logic:handle_place_piece( User_pid, X, Y, Angle,  State#game_state.game_logic_state),
		{ noreply, State#game_state{ game_logic_state = New_game_state } }
	catch
		throw:out_of_bounds ->
			lager:info("user out of bounds"),
			gen_server:cast( self(), { user_lost_game, User_pid } ),
			{ noreply, State };
		throw:invalid_move ->
			%% HACKER
			lager:info("user is a hacker"),
			gen_server:cast( self(), { user_lost_game, User_pid } ),
			{ noreply, State }
	end;











handle_cast( { user_lost_game, Lost_user_pid } , State = #game_state{ user1 = User1, user2 = User2 } )  ->
	lager:info("game ~p is going to end",[self()]),

	Lost_msg = message_processor:create_lost_message(no_reason),
	Won_msg = message_processor:create_won_message(normal),

	case Lost_user_pid == User2#game_user.pid of
		true ->
			gen_server:cast( User2#game_user.pid, {send_message, Lost_msg}),
			gen_server:cast( User1#game_user.pid, {send_message, Won_msg});
		false ->
			gen_server:cast( User1#game_user.pid, {send_message, Lost_msg}),
			gen_server:cast( User2#game_user.pid, {send_message, Won_msg})
	end,

	erlang:cancel_timer(State#game_state.game_difficult_change_timer),

	{stop, normal, State#game_state{ state = init }};

%	{noreply, State#game_state{ state = waiting_players, 
%									game_difficult_change_timer = undefined,
%										user1 = User1#game_user{ is_ready = false }, 
%											user2 = User2#game_user{ is_ready = false } } };













handle_cast( { send_message_to_other, Msg, From_pid }, State = #game_state{ user1 = User1 , user2 = User2 }) 
				when User2#game_user.pid == From_pid ->
	gen_server:cast( User1#game_user.pid, {send_message, Msg }),
	{noreply, State};

handle_cast( { send_message_to_other, Msg, From_pid }, State = #game_state{ user1 = User1 , user2 = User2 }) 
				when User1#game_user.pid == From_pid ->
	gen_server:cast( User2#game_user.pid, {send_message, Msg }),
	{noreply, State};














handle_cast( {reconnecting, User_pid }, State = #game_state{ user1 = User1 , user2 = User2, 
																configuration_version = Conf_version,
																configuration_url = Conf_url,
																game_logic_state = #game{ user1_gamestate = User1_gamestate,
																							user2_gamestate = User2_gamestate,
																							difficult_level = _Current_difficult_level,
																							initial_seed = Initial_seed } }  )  ->
	lager:info("USER ~p RECONNECTED WITH BOARD -----",[User_pid]),

	case User_pid == User1#game_user.pid of
		true ->

			%gen_server:cast( User2#game_user.pid, {send_message, message_processor:create_user_reconected_message() }),
			Msg = message_processor:create_login_success( User1#game_user.user_id, Conf_url, Conf_version,
															User1_gamestate#user_gamestate.piece_generation_step,
																User1_gamestate#user_gamestate.current_piece_x,
																User1_gamestate#user_gamestate.current_piece_y,
																User1_gamestate#user_gamestate.current_piece_angle,
																	board:get_all_blocks( User1_gamestate#user_gamestate.board), 
																		User1_gamestate#user_gamestate.garbage_position_list,
																			User2_gamestate#user_gamestate.piece_generation_step, 
																				User2_gamestate#user_gamestate.current_piece,
																				 User2_gamestate#user_gamestate.current_piece_x,
																				 User2_gamestate#user_gamestate.current_piece_x,
																				 User2_gamestate#user_gamestate.current_piece_angle,
																					board:get_all_blocks( User2_gamestate#user_gamestate.board), 
																						User2_gamestate#user_gamestate.garbage_position_list,
																							Initial_seed, User2#game_user.user_id ),

			New_state = State#game_state{ user1 = User1#game_user{ is_connected = true } },

			board:print_board(User1_gamestate#user_gamestate.board);


		false ->
			%gen_server:cast( User1#game_user.pid, {send_message, message_processor:create_user_reconected_message() }),

			Msg = message_processor:create_login_success( User2#game_user.user_id, Conf_url, Conf_version,
															User2_gamestate#user_gamestate.piece_generation_step, 
																User2_gamestate#user_gamestate.current_piece_x,
																User2_gamestate#user_gamestate.current_piece_y,
																User2_gamestate#user_gamestate.current_piece_angle,
																	board:get_all_blocks( User2_gamestate#user_gamestate.board), 
																		User2_gamestate#user_gamestate.garbage_position_list,
																			User1_gamestate#user_gamestate.piece_generation_step, 
																				User1_gamestate#user_gamestate.current_piece_x,
																				User1_gamestate#user_gamestate.current_piece_y,
																				User1_gamestate#user_gamestate.current_piece_angle,
																					board:get_all_blocks( User1_gamestate#user_gamestate.board), 
																						User1_gamestate#user_gamestate.garbage_position_list,
																							Initial_seed, User1#game_user.user_id ),

			New_state = State#game_state{ user2 = User2#game_user{ is_connected = true } },

			board:print_board(User2_gamestate#user_gamestate.board)
	end,

	lager:info("user ~p reconected, i will send the game state ~p",[User_pid,Msg]),
	gen_server:cast( User_pid, {send_message, Msg }),
	gen_server:cast( User2#game_user.pid, {send_message, message_processor:create_game_restarts_message( User1#game_user.user_id ) }),
	gen_server:cast( User1#game_user.pid, {send_message, message_processor:create_game_restarts_message( User2#game_user.user_id ) }),

	{noreply, New_state#game_state{ state = waiting_players, 
									user1 = User1#game_user{ is_ready = false}, 
										user2 = User2#game_user{ is_ready = false}  } };















handle_cast({ user_disconected, User_pid , User_id } , State = #game_state{ game_difficult_change_timer = Game_difficult_timer, 
																				user1 = User1 , 
																					user2 = User2 } ) ->

	lager:info("user ~p disconected",[User_pid]),
	Msg = message_processor:create_user_disconects_message(User_id),

	State_with_user_disconected = case User_pid == User1#game_user.pid of 
		true ->
			gen_server:cast(User2#game_user.pid, {send_message, Msg }),
			State#game_state{ user1 = User1#game_user{ is_connected = false } };
		false ->
			gen_server:cast(User1#game_user.pid, {send_message, Msg }),
			State#game_state{ user2 = User2#game_user{ is_connected = false } }
	end,

	New_state = case Game_difficult_timer of
		undefined ->
			State_with_user_disconected#game_state{ game_difficult_change_timer = undefined, 
														state = waiting_players_reconect };
		_ ->
			Time_left = erlang:cancel_timer(Game_difficult_timer),
			State_with_user_disconected#game_state{ time_difficult_change_left = Time_left, 
														game_difficult_change_timer = undefined, 
															state = waiting_players_reconect }
	end,
	{noreply, New_state };









handle_cast(Msg, State ) ->
	lager:error("game_serv: unknown cast ~p received when state was ~p", [Msg, State]),
	{noreply, State}.












handle_info( difficult_change , State = #game_state{ state = Game_State } ) 
			when Game_State =/= running ->
	{ noreply, State#game_state{ } };

handle_info( difficult_change , State = #game_state{ state = Game_State, user1 = User1 , user2 = User2, game_logic_state = Game } ) 
			when Game_State == running ->

	New_level = Game#game.difficult_level + 1,

	Msg = message_processor:create_difficult_message(New_level),
	gen_server:cast( User1#game_user.pid , {send_message, Msg } ),
	gen_server:cast( User2#game_user.pid , {send_message, Msg } ),

	lager:info("game_serv: GAME DIFFICULT CHANGED TO ~p",[New_level]),

	New_game_difficult_timer = erlang:send_after(timer:seconds(?DIFFICULT_CHANGE_SECONDS), self(), difficult_change),

	{ noreply, State#game_state{ game_difficult_change_timer = New_game_difficult_timer,
									game_logic_state = Game#game{ difficult_level = New_level } } };







%%
%	called when the any user process stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #game_state{ user1 = User1 , user2 = User2 }) ->

	Won_msg = message_processor:create_won_message(disconect),

	case Reference == User1#game_user.monitor of
		true ->
			demonitor(User1#game_user.monitor),
			message_processor:process_user_disconect(User2#game_user.pid, self()),
			gen_server:cast( User1#game_user.pid, {send_message, Won_msg});
		false ->
			demonitor(User2#game_user.monitor),
			message_processor:process_user_disconect(User1#game_user.pid, self()),
			gen_server:cast( User1#game_user.pid, {send_message, Won_msg})
	end,

	lager:info("user ~p connection went down", [Pid]),
	{stop, normal, State#game_state{ state = init }};









handle_info(M,S) ->
	lager:error("unhandled info ~p", [M]),
	{noreply, S}.

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("game_serv: ~p terminate with reason: ~p", [self(),Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
