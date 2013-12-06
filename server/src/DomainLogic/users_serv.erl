-module(users_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).
-include("include/softstate.hrl").

-type user_states() :: init | in_rematch | in_queue | playing_game.

-record(user_process_state, {
		user_id,
		game_state = init :: user_states(), 
		connection_pid = undefined :: pid(),
		connection_monitor,
		game_pid = undefined :: pid(),
		rematch_pid = undefined :: pid(),
		
		game_monitor,
		rematch_monitor,
		disconect_timer,
	
		logic_user = #logic_user{} :: #logic_user{}
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/4]).


start_link(ConnectionPid , UserId, ClientTime, User ) ->
	gen_server:start_link(?MODULE, [ConnectionPid , UserId, ClientTime, User ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #user_process_state{}}.



handle_cast([ ConnectionPid , UserId, _ClientTime, User = #mc_user{} ], State = #user_process_state{ }) ->

	lager:debug("new user process with user_id ~p and connection ~p",[UserId,ConnectionPid]),

	swiss:subscribe(configuration),

	gen_server:cast( ConnectionPid , {register_user_process, self() }),
	ConnectionMonitor = monitor(process, ConnectionPid),

	Msg =  message_processor:create_login_success( UserId, 
													configurations_serv:get_current_url(), 
													configurations_serv:get_current_version(),
													User#mc_user.wallet),
	gen_server:cast( ConnectionPid , { reply, Msg }),

	LogicUser = user_logic:login( User , swiss:unix_timestamp() ),

	{noreply, State#user_process_state{
				connection_monitor = ConnectionMonitor,
				user_id = UserId,
				connection_pid = ConnectionPid,
				disconect_timer = undefined,
				logic_user = LogicUser
			}
	};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GAME LOGIC DEPENDANT

handle_cast( {send_message, Msg }, State = #user_process_state{ connection_pid = ConnectionPid }) 
				when ConnectionPid == undefined ->
	lager:error("users_serv: tried sending msg ~p but connection is down",[Msg]),
	{noreply, State};

handle_cast( {send_message, Msg }, State = #user_process_state{ connection_pid = ConnectionPid })->
	%lager:debug("users_serv: sending msg ~p to ~p",[Msg,ConnectionPid]),
	case is_process_alive(ConnectionPid) of
		true ->
			gen_server:cast( ConnectionPid, {reply, Msg});
		false ->
			lager:error("users_serv: tried sending msg ~p but connection is down",[Msg])
	end,	
	{noreply, State};







handle_cast( { time_sync, ClientTimestamp }, State = #user_process_state{ connection_pid = ConnectionPid })->
	CurrentTime = swiss:unix_timestamp_ms(),
	Msg = message_processor:create_time_sync_message( ClientTimestamp, CurrentTime),
	gen_server:cast( ConnectionPid, {reply, Msg } ),
	{noreply, State};






handle_cast( { send_message_to_other, _Msg }, State = #user_process_state{ game_pid = GamePid }) 
				when GamePid == undefined ->
	{noreply, State};

handle_cast( { send_message_to_other, Msg }, State = #user_process_state{ game_pid = GamePid }) ->
	gen_server:cast( GamePid, { send_message_to_other, Msg, self() } ),
	{noreply, State};





handle_cast( { place_piece , X, Y, Angle, ClientGarbageId } , State = #user_process_state{ game_pid = GamePid } )  when GamePid =/= undefined ->
	gen_server:cast( GamePid, { place_piece, X, Y, Angle, self(), ClientGarbageId } ),
	{noreply, State};




handle_cast( { update_piece, X, Y, Angle }, State = #user_process_state{ game_pid = GamePid } ) when GamePid =/= undefined ->
	gen_server:cast( GamePid, { update_piece, X, Y, Angle, self() } ),
	{noreply, State};



handle_cast( { use_power, Type }, State = #user_process_state{ game_pid = GamePid } )  when GamePid =/= undefined ->
	gen_server:cast( GamePid, { use_power, Type, self() } ),
	{noreply, State};




handle_cast( {game_start , StartTime }, State = #user_process_state{ connection_pid = ConnectionPid }) ->

	Msg = message_processor:create_start_message( StartTime ),
	gen_server:cast( ConnectionPid, {reply, Msg}),

	lager:debug("sending start game to ~p",[self()]),
	{noreply, State};







handle_cast( { enter_queue, _Tier, Powers }, State = #user_process_state{ connection_pid = ConnectionPid,
																			game_pid = GamePid, 
																				game_state = UserState, 
																					user_id = UserId,
																						logic_user = LogicUser }) 
				when UserState == init, GamePid == undefined ->
	
	case user_logic:can_enter_game( LogicUser, Powers ) of
		false ->
			lager:debug("users_serv: not enough coins to place in queue with powers ~p",[Powers]),

			Msg = message_processor:create_insufficient_lifes_message(),
			gen_server:cast( ConnectionPid, {reply, Msg}),
			{noreply, State};
		true ->
			lager:debug("users_serv: ready to place in queue with powers ~p",[Powers]),
			Elo = 10,
			Tier = beginner,
			queue_serv:enter( self(), UserId, Elo, Tier, Powers ),
			{noreply, State#user_process_state{ game_state = in_queue }}
	end;







handle_cast( { enter_game, Powers, GameProcessPid, OpponnentName, Seed } , State = #user_process_state{ connection_pid = ConnectionPid, 
																										game_state = UserState,
																										logic_user = LogicUser }) 
				when UserState == in_queue orelse UserState == in_rematch ->

	lager:info("enter game received when he is in ~p",[UserState]),

	case user_logic:handle_game_start( LogicUser, Powers ) of

		{error , not_enough_lifes} ->
			{stop, not_enough_lifes, State};

		{ok, NewLogicUser} ->
			Msg = message_processor:create_match_created_message( OpponnentName, Seed ),
			gen_server:cast( ConnectionPid, {reply, Msg}),

			lager:debug("i am now monitoring game ~p",[GameProcessPid]),
			NewState = State#user_process_state{ 	
					game_monitor =  monitor( process, GameProcessPid ) , 
					game_pid = GameProcessPid, 
					game_state = playing_game,
					rematch_pid = undefined,
					rematch_monitor = undefined,
					logic_user = NewLogicUser 
			},
			{noreply, NewState}
	end;




handle_cast( { enter_game , _GameProcessPid, _OpponnentName, _Seed } , State ) ->
	lager:debug("enter game was received when state was ~p",[State]),
	{noreply, State};








handle_cast( { ready, _QueueDetails }, State = #user_process_state{ game_pid = GamePid,  game_state = UserState }) 
				when UserState == playing_game, GamePid =/= undefined ->
	gen_server:cast( GamePid , { user_ready, self()}),
	{noreply, State};


handle_cast( { ready, _QueueDetails }, State = #user_process_state{} ) ->
	lager:debug("ready message sent at wrong time (state = ~p)",[State]),
	{noreply, State};









handle_cast( {game_lost, Powers, _Tier, Reason}, State = #user_process_state{ connection_pid = ConnectionPid, logic_user = LogicUser} ) ->

	Msg = message_processor:create_lost_message(Reason),
	gen_server:cast( ConnectionPid, {reply, Msg}),

	case user_logic:handle_game_lost( LogicUser, Powers ) of
		{error , not_enough_lifes} ->
			{noreply, State};

		{ok, NewLogicUser} ->
			{noreply, State#user_process_state{ logic_user = NewLogicUser }}
	end;


handle_cast( {game_win, Powers, Tier, Reason}, State = #user_process_state{ connection_pid = ConnectionPid, logic_user = LogicUser} ) ->

	Msg = message_processor:create_won_message(Reason),
	gen_server:cast( ConnectionPid, {reply, Msg}),

	case user_logic:handle_game_win( LogicUser, Powers, Tier ) of
		{error , not_enough_lifes} ->
			{noreply, State};

		{ok, NewLogicUser} ->
			{noreply, State#user_process_state{ logic_user = NewLogicUser }}
	end;






handle_cast({ buy_product, ProductId, Amount } , State = #user_process_state{ user_id = UserId, connection_pid = ConnectionPid } ) ->
	Msg = case user_store:update_wallet_balance( UserId, ProductId, Amount ) of
		{ok, NewAmount } ->						message_processor:create_success_buy_product_response_message(ProductId, NewAmount );
		{error, {insufficient, _Ins_Amount}} ->		message_processor:create_fail_buy_product_response_message()
	end,
	gen_server:cast( ConnectionPid, {reply, Msg}),
	{noreply, State};




handle_cast(message_rematch , State = #user_process_state{ game_state = in_rematch, rematch_pid = RPid } ) ->
	rematches_serv:set_user_rematch( RPid, self() ),
	{noreply, State};


handle_cast(message_no_rematch , State = #user_process_state{ game_state = in_rematch, rematch_pid = RPid } ) ->
	rematches_serv:remove_user( RPid, self() ),
	{noreply, State};


handle_cast({set_rematch_state,RQPid} , State = #user_process_state{} ) ->
	{noreply, State#user_process_state{ 
			game_state = in_rematch, 
			rematch_pid = RQPid, rematch_monitor = monitor(process, RQPid) 
	}};


handle_cast(remove_from_rematch , State = #user_process_state{ game_state = in_rematch, rematch_monitor = RematchMonitor } ) ->
	demonitor( RematchMonitor ),
	{noreply, State#user_process_state{ game_state = init, rematch_pid = undefined, rematch_monitor = undefined } };


handle_cast(remove_from_queue , State = #user_process_state{ connection_pid = ConnectionPid } ) ->
	Msg = message_processor:create_insufficient_lifes_message(),
	gen_server:cast( ConnectionPid, {reply, Msg}),
	{noreply, State#user_process_state{ game_state = init } };


handle_cast({debug_confirm_board_synch, RemoteStateElems}, State = #user_process_state{ game_pid=GamePid }) ->
	gen_server:cast( GamePid, {debug_confirm_board_synch,self(),RemoteStateElems} ),
	{noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast( { reconnecting, NewConnectionPid}, State ) ->
	{stop, normal, State};

handle_cast( { reconnecting, NewConnectionPid}, State = #user_process_state{ connection_pid = PreviousConnectionPid, 
																				connection_monitor = PreviousConnectionMonitor,
																				disconect_timer = DisconectTimer,
																				user_id = UserId,
																				game_pid = GameProcessPid,
																				logic_user = #logic_user{ user = User } } ) ->
	lager:debug("player '~p' reconnected" ,[self()]),

	%cancels the timeout for disconect
	case DisconectTimer of 
		undefined -> 	do_nothing;
		_ -> 			erlang:cancel_timer( DisconectTimer )
	end,
	
	case PreviousConnectionMonitor of
		undefined ->	do_nothing;
		_ ->			demonitor(PreviousConnectionMonitor),
						gen_server:cast( PreviousConnectionPid , { reply_with_disconnect, message_processor:create_disconect_message() } )
	end,

	gen_server:cast( NewConnectionPid , { register_user_process, self() } ),
	NewConnectionMonitor = monitor(process, NewConnectionPid),

	case GameProcessPid of
		undefined ->	Msg = message_processor:create_login_success( UserId, 
																		configurations_serv:get_current_url(), 
																			configurations_serv:get_current_version(), 
																				User#mc_user.wallet ),
						gen_server:cast( NewConnectionPid , { reply, Msg });

		_ ->			gen_server:cast( GameProcessPid , { reconnecting, self() } )

	end,
	{noreply, State#user_process_state{ connection_monitor = NewConnectionMonitor,
											connection_pid = NewConnectionPid,
												disconect_timer = undefined } };




handle_cast(accept, State ) ->
	{noreply, State}.



handle_info({configuration,{new_configuration, NewVersion, NewVersionUrl} }, State = #user_process_state{connection_monitor = ConnectionMonitor, 
																						game_pid = _GameProcessPid,
																						user_id = UserId}) ->
	lager:debug("User ~p is going to be disconected due to new configuration",[UserId]),
	
	Msg = message_processor:create_new_configuration_message( NewVersion, NewVersionUrl ),

	gen_server:cast(ConnectionMonitor,{reply, Msg}),

	{stop, normal, State};




%%
%	called when the user connection stops
%%
handle_info({'DOWN', Reference, process, _Pid, _Reason}, State = #user_process_state{connection_monitor = ConnectionMonitor, 
																						game_pid = GameProcessPid,
																						user_id = UserId}) 
				when Reference == ConnectionMonitor ->
	lager:debug("user connection went down", []),
	demonitor(ConnectionMonitor),
	TimerRef = erlang:send_after(?CONNECTION_TIMEOUT, self(), connection_timeout),

	case GameProcessPid of 
		undefined ->
			do_nothing;
		_alive ->
			gen_server:cast(GameProcessPid, { user_disconected, self() , UserId })
	end,

	{noreply, State#user_process_state{ disconect_timer = TimerRef,
											connection_monitor = undefined,  
												connection_pid = undefined,
													game_state = init } };

%%
%	called when the game stops
%%
handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{game_monitor = GameMonitor})
				when Reference == GameMonitor andalso State#user_process_state.game_state == in_rematch ->

	lager:debug("game stoped (~p) but i will continue", [Reason]),
	demonitor(GameMonitor),
	{noreply, State#user_process_state{ game_monitor = undefined, game_pid = undefined }};


handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{game_monitor = GameMonitor})
				when Reference == GameMonitor ->

	lager:debug("game stoped (~p) but i will continue", [Reason]),
	demonitor(GameMonitor),
	{noreply, State#user_process_state{ game_monitor = undefined, game_pid = undefined, game_state = init }};


handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{rematch_monitor = RematchMonitor})
				when Reference == RematchMonitor andalso State#user_process_state.game_state == in_rematch ->

	lager:debug("game stoped (~p) but i will continue", [Reason]),
	demonitor(RematchMonitor),
	{noreply, State#user_process_state{ rematch_monitor = undefined, rematch_pid = undefined }};



handle_info({ user_logic_msg, Msg}, State = #user_process_state{ logic_user = LogicUser }) ->
	lager:debug("generate life timeoutfor ~p", [self()]),
	{ok,NewLogicUser} = user_logic:handle_msg( Msg, LogicUser),

	{noreply, State#user_process_state{ logic_user = NewLogicUser } };

%%
%	called when the user disconect timeouts
%%
handle_info(connection_timeout, State = #user_process_state{ logic_user = LogicUser } ) ->
	lager:debug("connection timeout", []),
	user_logic:logout(LogicUser),
	{stop, normal, State};


handle_info(M,S) ->
	lager:error("unhandled info ~p", [M]),
	{noreply, S}.


handle_call( {can_enter_game, Powers }, _From, State = #user_process_state{ logic_user = LogicUser } ) ->
	{reply, user_logic:can_enter_game( LogicUser, Powers ), State};

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("users_serv: ~p terminate with reason: ~p", [self(),Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.












