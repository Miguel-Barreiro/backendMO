-module(game_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-include("include/softstate.hrl").


-record(game_state, {
	user1_pid :: pid(),
	user2_pid :: pid(),
	user1_monitor,
	user2_monitor
}).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/2]).

start_link( User_pid, User_pid2  ) ->
    gen_server:start_link(?MODULE, [ User_pid, User_pid2 ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #game_state{ }}.


handle_cast([User_pid, User_pid2], State = #game_state{ }) ->

	lager:info("new game with user_id ~p user ~p",[User_pid,User_pid2]),

	gen_server:cast( User_pid , {register_game_process,self()}),
	Connection_monitor1 = monitor(process, User_pid),
	
	gen_server:cast( User_pid2 , {register_game_process,self()}),
	Connection_monitor2 = monitor(process, User_pid2),

	{noreply, State#game_state{
				user1_pid = User_pid,
				user2_pid = User_pid2,
				user1_monitor = Connection_monitor1,
				user2_monitor = Connection_monitor2
			}
	};

handle_cast( { send_message_to_other, Msg, From_pid }, State = #game_state{ user1_pid = User1_pid, user2_pid = User2_pid })->
	case From_pid of
		User1_pid ->
			gen_server:cast(User2_pid, {send_message, Msg });
		User2_pid ->
			gen_server:cast(User1_pid, {send_message, Msg });
		_->
			ok
	end,
	{noreply, State};


handle_cast(accept, State ) ->
	{noreply, State}.


%%
%	called when the user1 connection stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #game_state{user1_monitor = Connection_monitor, user2_pid = User2_pid})
		 when Reference == Connection_monitor ->

	lager:debug("user ~p connection went down", [Pid]),

	demonitor(Connection_monitor , [flush]),
	message_processor:process_user_disconect(Pid, User2_pid, self()),
	
	{stop, normal, State};

%%
%	called when the user1 connection stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #game_state{user2_monitor = Connection_monitor, user1_pid = User1_pid }) 
		when Reference == Connection_monitor ->

	lager:debug("user ~p connection went down", [Pid]),

	demonitor(Connection_monitor , [flush]),
	message_processor:process_user_disconect(Pid, User1_pid, self()),

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
	lager:error("users_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
