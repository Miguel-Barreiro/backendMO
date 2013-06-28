-module(game_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-define(DIFFICULT_CHANGE_SECONDS, 30).

-include("include/softstate.hrl").

-define( COUNTDOWN_TO_START_SECONDS , 5).

-type game_state_type() :: running | waiting_players.


-record( game_user, {
	game_state = undefined,
	pid :: pid(),
	user_id = undefined,
	monitor = undefined,
	victories = 0,
	is_ready = true,
	is_connected = true
	}).

-record(game_state, {
	difficult_level = 0,
	starting_seed,

	user1 = #game_user{} :: #game_user{},
	user2 = #game_user{} :: #game_user{},

	state = waiting_players :: game_state_type()
}).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/4]).

start_link( User_pid, User_id, User_pid2, User_id2  ) ->
    gen_server:start_link(?MODULE, [ User_pid, User_id, User_pid2, User_id2 ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #game_state{ }}.


handle_cast([User_pid, User_id, User_pid2, User_id2], State = #game_state{ }) ->

	lager:info("new game with user_id ~p user ~p",[User_pid,User_pid2]),

	gen_server:cast( User_pid , {register_game_process,self()}),
	Connection_monitor1 = monitor(process, User_pid),
	
	gen_server:cast( User_pid2 , {register_game_process,self()}),
	Connection_monitor2 = monitor(process, User_pid2),

	gen_server:cast(self() , start_game ),

	{noreply, State#game_state{
				user1 = #game_user{ pid = User_pid, monitor = Connection_monitor1, user_id = User_id },
				user2 = #game_user{ pid = User_pid2, monitor = Connection_monitor2, user_id = User_id2 },
				state = waiting_players
			}
	};



handle_cast( start_game, State = #game_state{ user1 = User1, user2 = User2, state = Game_State} ) 
				when Game_State == waiting_players,
						User1#game_user.is_ready == true,
							User2#game_user.is_ready == true ->
	
	StartTime = swiss:unix_timestamp() + ?COUNTDOWN_TO_START_SECONDS, 	%the game will start in <COUNTDOWN_TO_START_SECONDS> seconds
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	Seed = random:uniform(2147483646),

	gen_server:cast( User1#game_user.pid , {game_start , User2#game_user.user_id , StartTime, Seed } ),
	gen_server:cast( User2#game_user.pid , {game_start , User1#game_user.user_id , StartTime, Seed } ),

	erlang:send_after(timer:seconds(?DIFFICULT_CHANGE_SECONDS), self(), difficult_change),

	{ noreply, State#game_state{ difficult_level = 0,
									starting_seed = Seed, 
										state = running, 
											user1 = User1#game_user{ is_ready = false}, 
												user2 = User2#game_user{ is_ready = false} } };



%handle_cast( { user_ready_rematch, User_pid} , State = #game_state{ state = Game_State, user2 = User2, user1 = User1 } ) 
%				when User1#game_user.pid == User_pid, Game_State == running, User1#game_user.is_connected == false ->
%	{ noreply, State };

%handle_cast( { user_ready_rematch, User_pid} , State = #game_state{ state = Game_State, user2 = User2, user1 = User1 } ) 
%				when User2#game_user.pid == User_pid, Game_State == running, User2#game_user.is_connected == false ->
%	{ noreply, State };


handle_cast( { user_ready_rematch, User_pid} , State = #game_state{ state = Game_State, user2 = User2, user1 = User1 } ) 
				when User1#game_user.pid == User_pid, Game_State == waiting_players ->
	case User2#game_user.is_ready of
		true ->
			lager:info("game ~p is going to start (rematch)",[self()]),
			gen_server:cast(self() , start_game );
		false ->
			nothing_happens
	end,
	{ noreply, State#game_state{ user1 = User1#game_user{is_ready = true} } };



handle_cast( { user_ready_rematch, User_pid} , State = #game_state{ state = Game_State, user1 = User1, user2 = User2 } ) 
				when User2#game_user.pid == User_pid, Game_State == waiting_players ->
	case User1#game_user.is_ready of
		true ->
			lager:info("game ~p is going to start (rematch)",[self()]),
			gen_server:cast(self() , start_game );
		false ->
			nothing_happens
	end,
	{ noreply, State#game_state{ user2 = User2#game_user{is_ready = true} } };




handle_cast( { user_lost_game, Lost_user_pid } , State = #game_state{ user1 = User1, user2 = User2 } ) 
				when Lost_user_pid == User1#game_user.pid ->
	lager:info("game ~p is going to end",[self()]),
	gen_server:cast(User2#game_user.pid, {send_won_message , no_reason}),
	gen_server:cast(User1#game_user.pid, {send_lost_message , no_reason}),
	{noreply, State#game_state{ state = waiting_players, 
									user1 = User1#game_user{ is_ready = false}, 
										user2 = User2#game_user{ is_ready = false} } };

handle_cast( { user_lost_game, Lost_user_pid } , State = #game_state{ user1 = User1, user2 = User2 } ) 
				when Lost_user_pid == User2#game_user.pid ->
	lager:info("game ~p is going to end",[self()]),
	gen_server:cast(User2#game_user.pid, {send_lost_message , no_reason}),
	gen_server:cast(User1#game_user.pid, {send_won_message , no_reason}),		
	{noreply, State#game_state{ state = waiting_players, 
									user1 = User1#game_user{ is_ready = false}, 
										user2 = User2#game_user{ is_ready = false} } };






handle_cast( { send_message_to_other, Msg, From_pid }, State = #game_state{ user1 = User1 , user2 = User2 }) 
				when User2#game_user.pid == From_pid ->
	gen_server:cast( User1#game_user.pid, {send_message, Msg }),
	{noreply, State};

handle_cast( { send_message_to_other, Msg, From_pid }, State = #game_state{ user1 = User1 , user2 = User2 }) 
				when User1#game_user.pid == From_pid ->
	gen_server:cast( User2#game_user.pid, {send_message, Msg }),
	{noreply, State};





handle_cast({ save_game_state, Game_state, User_pid } , State = #game_state{ user1 = User1 } ) 
				when User_pid == User1#game_user.pid ->
	{noreply, State#game_state{ user1 = User1#game_user{ game_state = Game_state } } };

handle_cast({ save_game_state, Game_state, User_pid } , State = #game_state{ user2 = User2 } ) 
				when User_pid == User2#game_user.pid ->
	{noreply, State#game_state{ user2 = User2#game_user{ game_state = Game_state } } };






handle_cast( {reconnecting, User_pid }, State = #game_state{ user1 = User1 , user2 = User2, starting_seed = Seed }  ) 
				when User_pid == User1#game_user.pid ->
	Msg = message_processor:create_game_state_message( User1#game_user.game_state, User2#game_user.game_state, Seed, User2#game_user.user_id),
	lager:info("user ~p reconected, i will send the game state ~p",[User_pid,Msg]),
	gen_server:cast(User_pid, {send_message, Msg }),
	{noreply, State};

handle_cast( {reconnecting, User_pid }, State = #game_state{ user1 = User1 , user2 = User2, starting_seed = Seed } ) 
				when User_pid == User2#game_user.pid ->
	Msg = message_processor:create_game_state_message( User2#game_user.game_state, User1#game_user.game_state, Seed, User1#game_user.user_id),
	lager:info("user ~p reconected, i will send the game state ~p",[User_pid,Msg]),
	gen_server:cast(User_pid, {send_message, Msg }),
	{noreply, State};





handle_cast({ user_disconected, User_pid , User_id } , State = #game_state{ user1 = User1 , user2 = User2 } )
				when User_pid == User1#game_user.pid ->
	lager:info("user ~p disconected",[User_pid]),
	Msg = message_processor:create_user_disconects_message(User_id),
	gen_server:cast(User2#game_user.pid, {send_message, Msg }),
	{noreply, State#game_state{ user1 = User1#game_user{ is_connected = false } }};

handle_cast({ user_disconected, User_pid , User_id } , State = #game_state{ user1 = User1 , user2 = User2 } )
				when User_pid == User2#game_user.pid ->
	lager:info("user ~p disconected",[User_pid]),
	Msg = message_processor:create_user_disconects_message(User_id),
	gen_server:cast(User1#game_user.pid, {send_message, Msg }),
	{noreply, State#game_state{ user2 = User2#game_user{ is_connected = false } }};







handle_cast(Msg, State ) ->
	lager:error("game_serv: unknown cast ~p received when state was ~p", [Msg, State]),
	{noreply, State}.






handle_info( difficult_change , State = #game_state{ state = Game_State } ) 
			when Game_State =/= running ->
	{ noreply, State#game_state{ } };



handle_info( difficult_change , State = #game_state{ state = Game_State, difficult_level = Level, user1 = User1 , user2 = User2 } ) 
			when Game_State == running ->

	New_level = Level + 1,

	gen_server:cast( User1#game_user.pid , {game_difficult_change , New_level } ),
	gen_server:cast( User2#game_user.pid , {game_difficult_change , New_level } ),

	lager:info("game_serv: GAME DIFFICULT CHANGED TO ~p",[New_level]),
	erlang:send_after(timer:seconds(?DIFFICULT_CHANGE_SECONDS), self(), difficult_change),
	{ noreply, State#game_state{ difficult_level = New_level } };







%%
%	called when the user1 connection stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #game_state{ user1 = User1 , user2 = User2 })
			 when Reference == User1#game_user.monitor ->

	lager:info("user ~p connection went down", [Pid]),

	demonitor(User1#game_user.monitor),
	message_processor:process_user_disconect(Pid, User2#game_user.pid, self()),
	
	{stop, normal, State};

%%
%	called when the user2 connection stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #game_state{ user1 = User1 , user2 = User2 })
			 when Reference == User2#game_user.monitor ->

	lager:info("user ~p connection went down", [Pid]),

	demonitor(User2#game_user.monitor),
	message_processor:process_user_disconect(Pid, User1#game_user.pid, self()),

	{stop, normal, State};




%%
%	called when the user disconect timeouts
%%
handle_info(connection_timeout, State = #game_state{}) ->
    lager:debug("connection timeout", []),
    {stop, normal, State};








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
