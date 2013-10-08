-module(users_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-include("include/softstate.hrl").

-type user_states() :: init | in_rematch_queue | in_queue | playing_game.

-record(user_process_state, {
	session_start_time,
	user_id,
	game_state = init :: user_states(), 
	connection_pid = undefined :: pid(),
	connection_monitor,
	game_pid = undefined :: pid(),
	game_monitor,
	disconect_timer
}).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/3]).

start_link(Connection_pid , User_id, Client_time ) ->
	gen_server:start_link(?MODULE, [Connection_pid , User_id, Client_time ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #user_process_state{ session_start_time = swiss:unix_timestamp() }}.


handle_cast([ Connection_pid , User_id, _Client_time ], State = #user_process_state{ }) ->

	lager:debug("new user process with user_id ~p and connection ~p",[User_id,Connection_pid]),

	swiss:subscribe(configuration),

	gen_server:cast( Connection_pid , {register_user_process, self() }),
	Connection_monitor = monitor(process, Connection_pid),

	Msg =  message_processor:create_login_success( User_id, 
													configurations_serv:get_current_url(), 
													configurations_serv:get_current_version() ),
	gen_server:cast( Connection_pid , { reply, Msg }),

	{noreply, State#user_process_state{
				connection_monitor = Connection_monitor,
				user_id = User_id,
				connection_pid = Connection_pid,
				disconect_timer = undefined
			}
	};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GAME LOGIC DEPENDANT

handle_cast( {send_message, Msg }, State = #user_process_state{ connection_pid = Connection_pid }) 
				when Connection_pid == undefined ->
	lager:error("users_serv: tried sending msg ~p but connection is down",[Msg]),
	{noreply, State};

handle_cast( {send_message, Msg }, State = #user_process_state{ connection_pid = Connection_pid })->
	lager:debug("users_serv: sending msg ~p to ~p",[Msg,Connection_pid]),
	case is_process_alive(Connection_pid) of
		true ->
			gen_server:cast( Connection_pid, {reply, Msg});
		false ->
			lager:error("users_serv: tried sending msg ~p but connection is down",[Msg])
	end,	
	{noreply, State};







handle_cast( { time_sync, Client_timestamp }, State = #user_process_state{ connection_pid = Connection_pid })->
	Current_time = swiss:unix_timestamp_ms(),
	Msg = message_processor:create_time_sync_message( Client_timestamp, Current_time),
	gen_server:cast( Connection_pid, {reply, Msg } ),
	{noreply, State};






handle_cast( { send_message_to_other, _Msg }, State = #user_process_state{ game_pid = Game_pid }) 
				when Game_pid == undefined ->
	{noreply, State};

handle_cast( { send_message_to_other, Msg }, State = #user_process_state{ game_pid = Game_pid }) ->
	gen_server:cast( Game_pid, { send_message_to_other, Msg, self() } ),
	{noreply, State};





handle_cast( { place_piece , X, Y, Angle } , State = #user_process_state{ game_pid = Game_pid } ) ->
	gen_server:cast( Game_pid, { place_piece, X, Y, Angle, self() } ),
	{noreply, State};




handle_cast( { update_piece, X, Y, Angle }, State = #user_process_state{ game_pid = Game_pid } ) ->
	gen_server:cast( Game_pid, { update_piece, X, Y, Angle, self() } ),
	{noreply, State};






handle_cast( {game_start , StartTime }, 
					State = #user_process_state{ connection_pid = Connection_pid, 
														session_start_time = Session_start }) ->

	Msg = message_processor:create_start_message( StartTime ),
	gen_server:cast( Connection_pid, {reply, Msg}),

	lager:debug("sending start game to ~p",[self()]),

	{noreply, State};






handle_cast( { enter_queue, _Tier, Powers }, State = #user_process_state{ game_pid = Game_pid, game_state = User_state, user_id = User_id }) 
				when User_state == init, Game_pid == undefined ->
	lager:debug("users_serv: ready to place in queue with powers ~p",[Powers]),
	Elo = 10,
	Tier = beginner,
	queue_serv:enter( self(), User_id, Elo, Tier, Powers ),
	{noreply, State#user_process_state{ game_state = in_queue }};







handle_cast( { enter_game , Game_process_pid, Opponnent_name, Seed } , State = #user_process_state{ connection_pid = Connection_pid, game_state = User_state }) 
				when User_state == in_queue ->

	Msg = message_processor:create_match_found_message( Opponnent_name, Seed ),
	gen_server:cast( Connection_pid, {reply, Msg}),

	lager:debug("i am now monitoring game ~p",[Game_process_pid]),
	New_state = State#user_process_state{ 	game_monitor =  monitor(process, Game_process_pid ) , 
											game_pid = Game_process_pid, 
											game_state = playing_game },
	{noreply, New_state};







handle_cast( { ready, _Queue_details }, State = #user_process_state{ game_pid = Game_pid,  game_state = User_state }) 
				when User_state == playing_game, Game_pid =/= undefined ->
	gen_server:cast( Game_pid , { user_ready, self()}),
	{noreply, State};


handle_cast( { ready, _Queue_details }, State = #user_process_state{} ) ->
	lager:debug("ready message sent at wrong time "),
	{noreply, State};





handle_cast({ buy_product, Product_id, Amount } , State = #user_process_state{ user_id = User_id, connection_pid = Connection_pid } ) ->
	Msg = case user_store:update_wallet_balance( User_id, Product_id, Amount ) of
		{ok, New_amount } ->						message_processor:create_success_buy_product_response_message(Product_id, New_amount );
		{error, {insufficient, _Ins_Amount}} ->		message_processor:create_fail_buy_product_response_message()
	end,
	gen_server:cast( Connection_pid, {reply, Msg}),
	{noreply, State};




handle_cast(message_rematch , State = #user_process_state{ game_state = User_state } ) when User_state == in_rematch_queue ->
	rematch_queue_serv:set_user_rematch(self()),
	{noreply, State};


handle_cast(message_no_rematch , State = #user_process_state{ game_state = User_state } ) when User_state == in_rematch_queue ->
	rematch_queue_serv:remove_user(self()),
	{noreply, State};


handle_cast(set_rematch_queue_state , State = #user_process_state{} ) ->
	{noreply, State#user_process_state{ game_state = in_rematch_queue } };


handle_cast(remove_from_rematch_queue , State = #user_process_state{ connection_pid = Connection_pid } ) ->
	Msg = message_processor:create_rematch_timeout_message(),
	gen_server:cast( Connection_pid, {reply, Msg}),
	{noreply, State#user_process_state{ game_state = init } };


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



handle_cast( { reconnecting, New_connection_pid}, State = #user_process_state{ connection_pid = Previous_connection_pid, 
																				connection_monitor = Previous_connection_monitor,
																				disconect_timer = Disconect_timer,
																				user_id = User_id,
																				game_pid = Game_process_pid } ) ->
	lager:debug("player '~p' reconnected" ,[self()]),

	%cancels the timeout for disconect
	case Disconect_timer of 
		undefined -> 	do_nothing;
		_ -> 			erlang:cancel_timer( Disconect_timer )
	end,
	
	case Previous_connection_monitor of
		undefined ->	do_nothing;
		_ ->			demonitor(Previous_connection_monitor),
						gen_server:cast( Previous_connection_pid , { reply_with_disconnect, message_processor:create_disconect_message() } )
	end,

	gen_server:cast( New_connection_pid , { register_user_process, self() } ),
	New_connection_monitor = monitor(process, New_connection_pid),

	case Game_process_pid of
		undefined ->	Msg = message_processor:create_login_success( User_id, 
																		configurations_serv:get_current_url(), 
																			configurations_serv:get_current_version() ),
						gen_server:cast( New_connection_pid , { reply, Msg });

		_ ->			gen_server:cast( Game_process_pid , { reconnecting, self() } )

	end,
	{noreply, State#user_process_state{ connection_monitor = New_connection_monitor,
											connection_pid = New_connection_pid,
												disconect_timer = undefined } };




handle_cast(accept, State ) ->
	{noreply, State}.



handle_info({configuration,{new_configuration, New_version, New_version_url} }, State = #user_process_state{connection_monitor = Connection_monitor, 
																						game_pid = _Game_process_pid,
																						user_id = User_id}) ->
	lager:debug("User ~p is going to be disconected due to new configuration",[User_id]),
	
	Msg = message_processor:create_new_configuration_message( New_version, New_version_url ),

	gen_server:cast(Connection_monitor,{reply, Msg}),

	{stop, normal, State};




%%
%	called when the user connection stops
%%
handle_info({'DOWN', Reference, process, _Pid, _Reason}, State = #user_process_state{connection_monitor = Connection_monitor, 
																						game_pid = Game_process_pid,
																						user_id = User_id}) 
				when Reference == Connection_monitor ->
	lager:debug("user connection went down", []),
	demonitor(Connection_monitor),
	TimerRef = erlang:send_after(?CONNECTION_TIMEOUT, self(), connection_timeout),

	case Game_process_pid of 
		undefined ->
			do_nothing;
		_alive ->
			gen_server:cast(Game_process_pid, { user_disconected, self() , User_id })
	end,

	{noreply, State#user_process_state{ disconect_timer = TimerRef,
											connection_monitor = undefined,  
												connection_pid = undefined,
													game_state = init } };

%%
%	called when the game stops
%%

handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{game_monitor = Game_monitor})
				when Reference == Game_monitor andalso State#user_process_state.game_state == in_rematch_queue ->

	lager:debug("game stoped (~p) but i will continue", [Reason]),
	demonitor(Game_monitor),
	{noreply, State#user_process_state{ game_monitor = undefined, game_pid = undefined }};

handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{game_monitor = Game_monitor})
				when Reference == Game_monitor ->

	lager:debug("game stoped (~p) but i will continue", [Reason]),
	demonitor(Game_monitor),
	{noreply, State#user_process_state{ game_monitor = undefined, game_pid = undefined, game_state = init }};



%%
%	called when the user disconect timeouts
%%
handle_info(connection_timeout, State = #user_process_state{}) ->
	lager:debug("connection timeout", []),
	{stop, normal, State};

handle_info(M,S) ->
	lager:error("unhandled info ~p", [M]),
	{noreply, S}.



handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("users_serv: ~p terminate with reason: ~p", [self(),Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


