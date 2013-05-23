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
	connection_state = connected ::connection_states(),
	disconect_timer
}).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/2]).

start_link(Connection_pid , User_id ) ->
    gen_server:start_link(?MODULE, [Connection_pid , User_id ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #user_process_state{ session_start_time = swiss:unix_timestamp() }}.


handle_cast([ Connection_pid , User_id ], State = #user_process_state{ }) ->

	lager:info("new user process with user_id ~p and connection ~p",[User_id,Connection_pid]),

	gen_server:cast( Connection_pid , {register_user_process,self()}),
	Connection_monitor = monitor(process, Connection_pid),

	%cancels the timeout for disconect
	case State#user_process_state.disconect_timer =/= undefined of
        true -> erlang:cancel_timer(State#user_process_state.disconect_timer);
        false -> ok
    end,

	{noreply, State#user_process_state{ 
				connection_monitor = Connection_monitor,
				user_id = User_id, 
				connection_pid = Connection_pid,
				disconect_timer = undefined,
				connection_state = connected
			}
	};


handle_cast(accept, State ) ->
	{noreply, State}.


%%
%	called when the user connection stops
%%
handle_info({'DOWN', Reference, process, _Pid, _Reason}, State = #user_process_state{connection_monitor = Connection_monitor}) when Reference == Connection_monitor ->

	lager:debug("user connection went down", []),

	demonitor(Connection_monitor , [flush]),
	TimerRef = erlang:send_after(?CONNECTION_TIMEOUT, self(), connection_timeout),
    
	{noreply, State#user_process_state{connection_state = disconnected, disconect_timer = TimerRef}};

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


