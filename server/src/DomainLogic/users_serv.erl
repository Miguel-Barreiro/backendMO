-module(users_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-include("include/softstate.hrl").

-type user_states() :: init | lobby | in_queue | playing_game.
-type connection_states() :: connected | disconnected.

-record(user_process_state, {
	session_start_time,
	user_id,
	game_state = init :: user_states(), 
	connection_pid :: pid(),
	connection_monitor,
	game_pid :: pid(),
	game_monitor,
	connection_state = connected ::connection_states(),
	disconect_timer
}).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/2]).

start_link(Connection_pid , User_id ) ->
    gen_server:start_link(?MODULE, [Connection_pid , User_id ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #user_process_state{ session_start_time = swiss:unix_timestamp() }}.



handle_cast( {send_message, _Msg }, State = #user_process_state{ connection_pid = Connection_pid }) when Connection_pid == undefined ->
	{noreply, State};

handle_cast( {send_message, Msg }, State = #user_process_state{ connection_pid = Connection_pid })->
	gen_server:cast( Connection_pid, {reply, Msg}),
	{noreply, State};




handle_cast( { send_message_to_other, _Msg }, State = #user_process_state{ game_pid = Game_pid }) when Game_pid == undefined ->
	{noreply, State};

handle_cast( { send_message_to_other, Msg }, State = #user_process_state{ game_pid = Game_pid }) ->
	gen_server:cast( Game_pid, { send_message_to_other, Msg, self() } ),
	{noreply, State};





%handler_cast( { reconnecting , User_id, Connection_pid }, State = #user_process_state{ user_id = Current_user_id } ) 
%				when Current_user_id =/= User_id ->
%	lager:info("player '~p' tried to connect to the wrong proccess" ,[User_id]),
%	{noreply, State};
%
%handler_cast( { reconnecting , User_id, Connection_pid}, State = #user_process_state{ user_id = Current_user_id , connection_state = Connection_state } ) 
%				when Current_user_id == User_id, Connection_state == connected ->
%	lager:info("player '~p' tried to connect to a already connect user" ,[User_id]),
%	{noreply, State};
%
%handler_cast( { reconnecting , User_id, Connection_pid}, State = #user_process_state{ user_id = Current_user_id , connection_state = Connection_state } ) 
%				when Current_user_id == User_id, Connection_state == disconnected ->
%
%	lager:info("player '~p' reconnected" ,[User_id]),
%	gen_server:cast(self(), [ Connection_pid , User_id ]),
%
%	{noreply, State};






handle_cast([ Connection_pid , User_id ], State = #user_process_state{ }) ->

	lager:info("new user process with user_id ~p and connection ~p",[User_id,Connection_pid]),

	gen_server:cast( Connection_pid , {register_user_process,self()}),
	Connection_monitor = monitor(process, Connection_pid),

	%cancels the timeout for disconect
	case State#user_process_state.disconect_timer =/= undefined of
        true -> erlang:cancel_timer(State#user_process_state.disconect_timer);
        false -> ok
    end,

	queue_serv:enter( self() ),

	{noreply, State#user_process_state{ 
				connection_monitor = Connection_monitor,
				user_id = User_id, 
				connection_pid = Connection_pid,
				disconect_timer = undefined,
				connection_state = connected
			}
	};

handle_cast( { register_game_process, Game_process_pid }, State = #user_process_state{} ) ->
	New_state = State#user_process_state{ game_monitor =  monitor(process, Game_process_pid ) , game_pid = Game_process_pid },
	{noreply, New_state};


handle_cast(accept, State ) ->
	{noreply, State}.


%%
%	called when the user connection stops
%%
handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{connection_monitor = Connection_monitor}) when Reference == Connection_monitor ->
	lager:debug("user connection went down", []),
	demonitor(Connection_monitor , [flush]),
	%TimerRef = erlang:send_after(?CONNECTION_TIMEOUT, self(), connection_timeout),
	%{noreply, State#user_process_state{connection_state = disconnected, disconect_timer = TimerRef}};
	{stop, Reason, State};

%%
%	called when the game stops
%%
handle_info({'DOWN', Reference, process, _Pid, Reason}, State = #user_process_state{game_monitor = Game_monitor}) when Reference == Game_monitor ->
	lager:debug("game stoped but i will continue", []),
	demonitor(Game_monitor , [flush]),
	{noreply, State#user_process_state{ game_monitor = undefined, game_pid = undefined }};

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
	lager:error("users_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


