-module(game_serv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/9]).

-include("include/softstate.hrl").

-define(CONNECTION_TIMEOUT, 40000).
-define(DIFFICULT_CHANGE_SECONDS, 30).
-define(COUNTDOWN_TO_START_MSECONDS , 5000).

-define(DEBUG_BOARDSYNCH_REPORT_MRELAY, "email-smtp.us-east-1.amazonaws.com").
-define(DEBUG_BOARDSYNCH_REPORT_MUSER, "AKIAIQ7YY2TRPO3JFGRQ").
-define(DEBUG_BOARDSYNCH_REPORT_MPASS, "AiuWv21oHs/ZgRfCU3WHgYNAKcoCpq+oC3RgSxcTPXn6").
-define(DEBUG_BOARDSYNCH_REPORT_MSENDERADDR, "guilherme.andrade@miniclip.com").
-define(DEBUG_BOARDSYNCH_REPORT_MRECIPIENTS, ["guilherme.andrade@miniclip.com","g@gandrade.net","ana.oliveira@miniclip.com","miguel.barreiro@miniclip.com"]).


-type game_state_type() :: init | running | waiting_players | waiting_players_reconect.


-record( game_user, {
	pid :: pid(),
	user_id = undefined,
	monitor = undefined,
	is_ready = false,
	is_connected = true,
	powers_equipped = []
}).

-record(game_state, {
	tier = undefined,
	game_difficult_change_timer = undefined,
	time_difficult_change_left = 0,

	configuration_url = undefined,
	configuration_version = undefined,

	user1 = #game_user{} :: #game_user{},
	user2 = #game_user{} :: #game_user{},

	state = init :: game_state_type(),

	game_logic_state = #game{} :: #game{}
}).





start_link( Tier, User_pid, User_id, User1_powers, User_pid2, User_id2, User2_powers, Conf_version, Conf_url  ) ->
    gen_server:start_link(?MODULE, [ Tier, User_pid, User_id, User1_powers, User_pid2, User_id2, User2_powers, Conf_version, Conf_url ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #game_state{ }}.





debug_cmp_usergamestates( 
			GameState = #game_state {
				user1 = GameUser1, user2 = GameUser2
			},
			LocalPlayerState = #user_gamestate{
				board = LBoard, current_piece = LPiece, current_piece_angle = LPieceAngle, current_piece_x = LPieceX, current_piece_y=LPieceY,
				garbage_position_list = LGarbagePositionList, random_state = LRandomState,
				current_garbage_id = LGarbageId
			},
			RemotePlayerState = #user_gamestate{
				board = RBoard, current_piece = RPiece, current_piece_angle = RPieceAngle, current_piece_x = RPieceX, current_piece_y=RPieceY,
				garbage_position_list = RGarbagePositionList, random_state = RRandomState	
			},
			{RemotePlayerPieceBlock1Type, RemotePlayerPieceBlock2Type},
			RGarbageId,
			UserIndex
) ->
%	lager:debug( "Received Game State comparison request;\n\tLRandomState: ~p\nRRandomState: ~p\n\nLocal: ~p\n\nRemote: ~p\n\nRemote piece block types: ~p,~p\n", [LRandomState,RRandomState,LocalPlayerState,RemotePlayerState,RemotePlayerPieceBlock1Type,RemotePlayerPieceBlock2Type] ),

	lager:debug( "---------------------------------------------------------------" ),
	lager:debug( "\nLocal:" ),
	LBoardTxt = board:lager_print_board( LBoard ),
	lager:debug( "\nRemote:" ),
	RBoardTxt = board:lager_print_board( RBoard ),

	Ret = case board:are_boards_equal( LBoard, RBoard ) of
		true ->
			lager:debug( "\e[32mComparison success.\e[m" ),
			ok;
		false ->
			lager:debug( "\e[1m\e[31mComparison FAILED.\e[m" ),
			MailSenderName = "MiniOrbs @ " ++ swiss:get_localhostname(),
			VsString = binary_to_list(GameUser1#game_user.user_id) ++ " VS " ++ binary_to_list(GameUser2#game_user.user_id),
			PlayerIndexStr = "User #" ++ integer_to_list( UserIndex ),
			AppVersionStr = swiss:get_appversion_str(),
			MailSubject = "[Board Mismatch] " ++ VsString ++ " - " ++ PlayerIndexStr,
		
			MailBody = httpd_util:rfc1123_date() ++ "\n\n" 
				++ PlayerIndexStr ++ "\n"
				++ "Game: " ++ VsString ++ "\n" 
				++ "-------------------------------------------------\n" 
				++ "Version: " ++ AppVersionStr ++ "\n"
				++ "-------------------------------------------------\n\n" 
				++ "Server\n garb. id: " ++ integer_to_list(LGarbageId) ++ "\n board:\n" ++ LBoardTxt ++ "\n\n"
				++ "Client\n garb. id: " ++ integer_to_list(RGarbageId) ++ "\n board:\n" ++ RBoardTxt ++ "\n",
		
			swiss:send_email(
				{?DEBUG_BOARDSYNCH_REPORT_MRELAY, ?DEBUG_BOARDSYNCH_REPORT_MUSER, ?DEBUG_BOARDSYNCH_REPORT_MPASS},
				MailSenderName,
				?DEBUG_BOARDSYNCH_REPORT_MSENDERADDR, ?DEBUG_BOARDSYNCH_REPORT_MRECIPIENTS,
				MailSubject, MailBody
			),		
			error
	end,

	lager:debug( "---------------------------------------------------------------" ),
	Ret.






handle_cast([Tier, User_pid, User_id, User1_powers, User_pid2, User_id2, User2_powers, Conf_version, Conf_url], State = #game_state{ }) ->

	lager:debug("new game with user_id ~p user ~p",[User_pid,User_pid2]),

	gen_server:cast(self() , game_created ),

	Connection_monitor1 = monitor(process, User_pid),
	Connection_monitor2 = monitor(process, User_pid2),

	{noreply, State#game_state{
				tier = Tier,
				user1 = #game_user{ pid = User_pid, user_id = User_id, monitor = Connection_monitor1, powers_equipped = User1_powers },
				user2 = #game_user{ pid = User_pid2, user_id = User_id2, monitor = Connection_monitor2, powers_equipped = User2_powers },
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
	lager:debug("enter game was sent!"),

	Starting_game_logic_state = game_logic:create_new_game( User1#game_user.pid, User1#game_user.powers_equipped, 
																User2#game_user.pid, User2#game_user.powers_equipped, Seed ),

	gen_server:cast( User1#game_user.pid , { enter_game, User1#game_user.powers_equipped, self(), User2#game_user.user_id, Seed } ),
	gen_server:cast( User2#game_user.pid , { enter_game, User2#game_user.powers_equipped, self(), User1#game_user.user_id, Seed } ),

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
	
	lager:debug("game ~p is going to start",[self()]),

	StartTime = swiss:unix_timestamp_ms() + ?COUNTDOWN_TO_START_MSECONDS, 	%the game will start in <COUNTDOWN_TO_START_SECONDS> mseconds

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
			lager:debug("user is a hacker"),
			gen_server:cast( self(), { user_lost_game, User_pid } ),
			{ noreply, State }
	end;




handle_cast( { place_piece, X, Y, Angle, User_pid, Client_garbage_id } , State = #game_state{}  ) ->
	try 
		New_game_state = game_logic:handle_place_piece( User_pid, X, Y, Angle, State#game_state.game_logic_state, Client_garbage_id),
		{ noreply, State#game_state{ game_logic_state = New_game_state } }
	catch
		throw:out_of_bounds ->
			lager:debug("user out of bounds"),
			gen_server:cast( self(), { user_lost_game, User_pid } ),
			{ noreply, State };
		throw:invalid_move ->
			%% HACKER
			lager:debug("user is a hacker"),
			gen_server:cast( self(), { user_lost_game, User_pid } ),
			{ noreply, State }
	end;



handle_cast({ use_power, Power, User_pid }, State = #game_state{ user1 = User1, user2 = User2 }) ->
	Msg = message_processor:create_use_power_message( Power),
	case User_pid == User2#game_user.pid of
		true ->
			gen_server:cast( User1#game_user.pid, {send_message, Msg});
		false ->
			gen_server:cast( User2#game_user.pid, {send_message, Msg})
	end,
	New_game_state = game_logic:handle_power_use( User_pid, Power, State#game_state.game_logic_state ),
	{ noreply, State#game_state{ game_logic_state = New_game_state } };
	



handle_cast( { user_lost_game, Lost_user_pid } , State = #game_state{ tier = Tier, user1 = User1, user2 = User2 } )  ->
	lager:debug("game ~p is going to end",[self()]),

	case Lost_user_pid == User2#game_user.pid of
		true ->
			gen_server:cast( User2#game_user.pid, {game_lost, User2#game_user.powers_equipped, Tier, no_reason}),
			gen_server:cast( User1#game_user.pid, {game_win, User1#game_user.powers_equipped, Tier, normal});
		false ->
			gen_server:cast( User1#game_user.pid, {game_lost, User1#game_user.powers_equipped, Tier, no_reason}),
			gen_server:cast( User2#game_user.pid, {game_win, User2#game_user.powers_equipped, Tier, normal})
	end,

	erlang:cancel_timer(State#game_state.game_difficult_change_timer),

	rematch_queue_serv:enter(User1#game_user.pid, User1#game_user.user_id,
								User2#game_user.pid, User2#game_user.user_id, Tier),

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

	lager:debug("user ~p reconected, i will send the game state ~p",[User_pid,Msg]),
	gen_server:cast( User_pid, {send_message, Msg }),
	StartTime = swiss:unix_timestamp_ms() + ?COUNTDOWN_TO_START_MSECONDS,

	gen_server:cast( User2#game_user.pid, {send_message, message_processor:create_game_restarts_message( User1#game_user.user_id, StartTime ) }),
	gen_server:cast( User1#game_user.pid, {send_message, message_processor:create_game_restarts_message( User2#game_user.user_id, StartTime ) }),

	{noreply, New_state#game_state{ state = waiting_players, 
									user1 = User1#game_user{ is_ready = false}, 
										user2 = User2#game_user{ is_ready = false}  } };















handle_cast({ user_disconected, User_pid , User_id } , State = #game_state{ game_difficult_change_timer = Game_difficult_timer, 
																				user1 = User1 , 
																					user2 = User2 } ) ->

	lager:debug("user ~p disconected",[User_pid]),
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






handle_cast({ debug_confirm_board_synch, UserPid, {_RemoteOpponentUGStateElems, RemotePlayerUGStateElems}}, State = #game_state{ tier=Tier, user1=User1, user2=User2, game_logic_state=GameLogicState }) ->
	{LocalPlayerState,User,OpponentUser,UserIndex} = case {UserPid=:=User1#game_user.pid, UserPid=:=User2#game_user.pid} of
		{true,false} ->
			{GameLogicState#game.user1_gamestate,User1,User2,1};
		{false,true} ->
			{GameLogicState#game.user2_gamestate,User2,User1,2}
	end,

	{RemotePlayerState, RemotePlayerPieceBlockTypes, RemotePlayerLastGarbageId} = RemotePlayerUGStateElems,
	case debug_cmp_usergamestates( State, LocalPlayerState, RemotePlayerState, RemotePlayerPieceBlockTypes, RemotePlayerLastGarbageId, UserIndex ) of
		ok ->
			{noreply, State};
		error ->
			gen_server:cast( User#game_user.pid,         {game_lost, User#game_user.powers_equipped, Tier, board_mismatch}),
			gen_server:cast( OpponentUser#game_user.pid, {game_win,  OpponentUser#game_user.powers_equipped, Tier, disconect}),
			{stop, normal, State}
	end;
	
			





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

	lager:debug("game_serv: GAME DIFFICULT CHANGED TO ~p",[New_level]),

	New_game_difficult_timer = erlang:send_after(timer:seconds(?DIFFICULT_CHANGE_SECONDS), self(), difficult_change),

	{ noreply, State#game_state{ game_difficult_change_timer = New_game_difficult_timer,
									game_logic_state = Game#game{ difficult_level = New_level } } };










%%
%	called when the any user process stops
%%
handle_info({'DOWN', Reference, process, _Pid, _Reason}, State = #game_state{ user1 = User1 , user2 = User2 }) ->

	Won_msg = message_processor:create_won_message(disconect),

	case Reference == User1#game_user.monitor of
		true ->
			demonitor(User1#game_user.monitor),
			message_processor:process_user_disconect(User1#game_user.pid, self()),
			gen_server:cast( User2#game_user.pid, {send_message, Won_msg});
		false ->
			demonitor(User2#game_user.monitor),
			message_processor:process_user_disconect(User2#game_user.pid, self()),
			gen_server:cast( User1#game_user.pid, {send_message, Won_msg})
	end,
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











