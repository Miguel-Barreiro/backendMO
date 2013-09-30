-module(queue_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-record(queue_state, {
	queued_user_pid = undefined :: pid(),
	queued_user_id,
	user_monitor
}).

-export([enter/2,leave/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, #queue_state{ }}.


enter(User_pid, User_id) ->
	gen_server:cast(whereis(?MODULE), {add_user, User_pid, User_id}).

leave(User_pid, User_id) ->
	gen_server:cast(whereis(?MODULE), {remove_user, User_pid, User_id}).






handle_cast( { add_user , User_pid, User_id }, State = #queue_state{ queued_user_pid = Queued_user }) 
				when Queued_user == undefined ->

	lager:info("queue_serv: added a new user (~p) to the queue",[User_pid]),
	User_monitor = monitor(process, User_pid),
	{noreply, State#queue_state{ queued_user_pid = User_pid, queued_user_id = User_id, user_monitor = User_monitor }};

handle_cast( { add_user , User_pid }, State = #queue_state{ queued_user_pid = Queued_user }) 
				when Queued_user =/= undefined, Queued_user == User_pid ->
	{noreply, State};

handle_cast( { add_user , User_pid, User_id }, State = #queue_state{ queued_user_pid = Queued_user, 
																		queued_user_id = Queued_user_id, 
																			user_monitor = User_monitor })
				 when Queued_user =/= undefined, Queued_user =/= User_pid ->
	
	case User_monitor of
		disconnected ->
			lager:info("queue_serv: added a new user (~p) to the queue",[User_pid]),
			New_user_monitor = monitor(process, User_pid),
			{noreply, State#queue_state{ queued_user_pid = User_pid, queued_user_id = User_id, user_monitor = New_user_monitor }};
		_ ->
			lager:info("queue_serv: added a new user (~p) and started a game",[User_pid]),
			demonitor(User_monitor),
			game_sup:start_new_game_process( [ Queued_user, Queued_user_id, User_pid, User_id, 
													configurations_serv:get_current_version(), configurations_serv:get_current_version()] ),
			{noreply, State#queue_state{ queued_user_pid = undefined , user_monitor = undefined}}
	end;










handle_cast( Msg, State) ->
	lager:error("queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.

%%
%	called when the user connection stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #queue_state{user_monitor = Connection_monitor}) when Reference == Connection_monitor ->
	lager:debug("queue_serv: user (~p) connection went down", [Pid]),
	demonitor(Connection_monitor),
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