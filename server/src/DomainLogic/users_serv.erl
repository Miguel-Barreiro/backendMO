-module(users_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-include("include/softstate.hrl").

-type user_states() :: init | in_queue | playing_game.

-record(user_process_state, {
	session_start_time,
	client_start_time,
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


handle_cast([ Connection_pid , User_id, Client_time ], State = #user_process_state{ }) ->

	lager:info("new user process with user_id ~p and connection ~p",[User_id,Connection_pid]),

	gen_server:cast( Connection_pid , {register_user_process,self(), User_id }),
	Connection_monitor = monitor(process, Connection_pid),

	{noreply, State#user_process_state{
				client_start_time = Client_time,
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
	lager:info("users_serv: tried sending msg ~p but connection is down",[Msg]),
	{noreply, State};

handle_cast( {send_message, Msg }, State = #user_process_state{ connection_pid = Connection_pid })->
	lager:info("users_serv: sending msg ~p to ~p",[Msg,Connection_pid]),
	gen_server:cast( Connection_pid, {reply, Msg}),
	{noreply, State};




handle_cast( { send_message_to_other, _Msg }, State = #user_process_state{ game_pid = Game_pid }) 
				when Game_pid == undefined ->
	{noreply, State};

handle_cast( { send_message_to_other, Msg }, State = #user_process_state{ game_pid = Game_pid }) ->
	gen_server:cast( Game_pid, { send_message_to_other, Msg, self() } ),
	{noreply, State};




handle_cast( { save_game_state , Game_state } , State = #user_process_state{ game_pid = Game_pid } ) ->
	gen_server:cast( Game_pid, { save_game_state, Game_state, self() } ),
	{noreply, State};





handle_cast( { send_lost_message, _Lost_details }, State = #user_process_state{ game_pid = Game_pid }) 
				when Game_pid == undefined ->
	{noreply, State};

handle_cast( { send_lost_message, Lost_details }, State = #user_process_state{ connection_pid = Connection_pid, game_state = Game_state }) 
				when Game_state == playing_game ->
	gen_server:cast( Connection_pid, {send_lost_message,Lost_details} ),
	{noreply, State};





handle_cast( { send_won_message, _Won_details }, State = #user_process_state{ game_pid = Game_pid }) 
				when Game_pid == undefined ->
	{noreply, State};

handle_cast( { send_won_message, Won_details }, State = #user_process_state{ connection_pid = Connection_pid , game_state = Game_state }) 
				when Game_state == playing_game ->
	gen_server:cast( Connection_pid, {send_won_message,Won_details} ),
	{noreply, State};



handle_cast( {game_difficult_change , _New_level } ,  State = #user_process_state{ game_pid = Game_pid } )
				when Game_pid == undefined ->
	{noreply, State};

handle_cast( {game_difficult_change , New_level }, State = #user_process_state{ connection_pid = Connection_pid , game_state = Game_state }) 
				when Game_state == playing_game ->
	gen_server:cast( Connection_pid, {game_difficult_change, New_level} ),
	{noreply, State};




handle_cast( { ready, _Queue_details }, State = #user_process_state{ game_pid = Game_pid, game_state = User_state, user_id = User_id }) 
				when User_state == init, Game_pid == undefined ->
	lager:info("users_serv: ready to place in queue"),
	queue_serv:enter( self(), User_id ),
	{noreply, State#user_process_state{ game_state = in_queue }};

handle_cast( { ready, _Queue_details }, State = #user_process_state{ game_pid = Game_pid,  game_state = User_state }) 
				when User_state == playing_game, Game_pid =/= undefined ->
	lager:info("users_serv: ready rematch"),
	gen_server:cast( Game_pid , { user_ready_rematch, self()}),
	{noreply, State};

handle_cast( { ready, _Queue_details }, State = #user_process_state{ game_state = User_state }) 
				when User_state =/= init ->
	lager:error("users_serv: ready sent but user is not in correct ready state"),
	{noreply, State};






handle_cast( { lost_game, _Lost_details }, State = #user_process_state{ game_pid = Game_pid, game_state = User_state }) 
				when Game_pid =/= undefined, User_state == playing_game ->
	gen_server:cast(Game_pid , { user_lost_game, self() }),
	{noreply, State};

handle_cast( { lost_game, _Lost_details }, State = #user_process_state{ }) ->
	lager:error("users_serv: lost_game received but user is not in correct lost_game state"),
	{noreply, State};





handle_cast( {game_start , Opponnent_name , Start_date, Seed }, 
					State = #user_process_state{ connection_pid = Connection_pid, 
														session_start_time = Session_start,
														client_start_time = Client_time }) ->
	

	Client_start_time = Client_time + ( Start_date - Session_start),
	gen_server:cast( Connection_pid, {send_start_message , { Opponnent_name , Client_start_time, Seed }}),
	{noreply, State};






handle_cast( { register_game_process, Game_process_pid }, State = #user_process_state{ } ) ->
	lager:info("i am now monitoring game ~p",[Game_process_pid]),
	New_state = State#user_process_state{ 	game_monitor =  monitor(process, Game_process_pid ) , 
											game_pid = Game_process_pid, 
											game_state = playing_game },
	{noreply, New_state};




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



handle_cast( { reconnecting, New_connection_pid}, State = #user_process_state{ connection_pid = Previous_connection_pid, 
																				connection_monitor = Previous_connection_monitor,
																				disconect_timer = Disconect_timer,
																				user_id = User_id,
																				game_pid = Game_process_pid } ) ->

	lager:info("player '~p' reconnected" ,[self()]),

	%cancels the timeout for disconect
	case Disconect_timer of 
		undefined -> 	do_nothing;
		_ -> 			erlang:cancel_timer( Disconect_timer )
	end,
	
	case Previous_connection_monitor of
		undefined ->	do_nothing;
		_ ->			demonitor(Previous_connection_monitor),
						gen_server:cast( Previous_connection_pid , {reply_with_disconnect, message_processor:create_disconect_message() } )
	end,
	
	gen_server:cast( New_connection_pid , {register_user_process,self(), User_id } ),
	New_connection_monitor = monitor(process, New_connection_pid),

	case Game_process_pid of
		undefined ->
			do_nothing;
		_ ->
			gen_server:cast( Game_process_pid , {reconnecting, self() } )
	end,

	{noreply, State#user_process_state{ connection_monitor = New_connection_monitor,
											connection_pid = New_connection_pid,
												disconect_timer = undefined } };



handle_cast(accept, State ) ->
	{noreply, State}.


%%
%	called when the user connection stops
%%
handle_info({'DOWN', Reference, process, _Pid, _Reason}, State = #user_process_state{connection_monitor = Connection_monitor, 
																						game_pid = Game_process_pid,
																						user_id = User_id}) 
				when Reference == Connection_monitor ->
	lager:info("user connection went down", []),
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
												connection_pid = undefined } };
	%{stop, Reason, State};

%%
%	called when the game stops
%%
handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{game_monitor = Game_monitor})
				when Reference == Game_monitor ->
	lager:info("game stoped (~p) but i will continue", [Reason]),
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


%%
%		GETTERS
%%

handle_call( get_game_pid, _From, State = #user_process_state{ game_pid = Game_pid } ) ->
	{reply,{ ok , Game_pid},State};



handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("users_serv: ~p terminate with reason: ~p", [self(),Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


