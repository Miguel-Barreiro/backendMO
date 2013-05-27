-module(queue_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-record(queue_state, {
	queued_user_pid = undefined,
	user_monitor
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, #queue_state{ }}.


handle_cast( { add_user , User_pid }, State = #queue_state{ queued_user_pid = Queued_user }) when Queued_user == undefined ->

	User_monitor = monitor(process, User_pid),

	{noreply, State#queue_state{ queued_user_pid = User_pid, user_monitor = User_monitor }};

handle_cast( { add_user , User_pid }, State = #queue_state{ queued_user_pid = Queued_user }) when Queued_user =/= undefined ->
	game_sup:start_new_game_process( [ Queued_user, User_pid ] ),
	{noreply, State#queue_state{ queued_user_pid = undefined }};


handle_cast( Msg, State) ->
	lager:error("queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.

%%
%	called when the user connection stops
%%
handle_info({'DOWN', Reference, process, _Pid, _Reason}, State = #queue_state{user_monitor = Connection_monitor}) when Reference == Connection_monitor ->
	lager:debug("queue_serv: user connection went down", []),
	demonitor(Connection_monitor , [flush]),
	{noreply, State#queue_state{queued_user_pid = disconnected, user_monitor = disconnected}};


handle_info(Msg,State) ->
	lager:error("queue_serv: unhandled info ~p", [Msg]),
	{noreply, State}.

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("queue_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.