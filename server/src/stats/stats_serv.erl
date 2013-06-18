-module(stats_serv).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([get_connections_number/0,add_connection/0,remove_connection/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

-record(stats_state, {
	number_connections = 0 :: non_neg_integer()
}).


get_connections_number()->
	gen_server:call(whereis(?MODULE), get_connections_number).

add_connection()->
	gen_server:call(whereis(?MODULE), add_connection).

remove_connection()->
	gen_server:call(whereis(?MODULE), remove_connection).


%------------------------- GEN SERVER ------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:info("stats_serv: comecei"),
	{ok, #stats_state{ }}.


handle_cast( Msg, State) ->
	lager:error("stats_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.


handle_info( get_connections_number, State = #stats_state{ number_connections = Connections_Number}) ->
	{reply, {ok, Connections_Number },State};

handle_info( add_connection, State = #stats_state{ number_connections = Connections_Number}) ->
	{noreply, State#stats_state{ number_connections = Connections_Number + 1 }};

handle_info( remove_connection, State = #stats_state{ number_connections = Connections_Number}) ->
	{noreply, State#stats_state{ number_connections = Connections_Number - 1 }};


handle_info(Msg,State) ->
	lager:error("stats_serv: unhandled info ~p", [Msg]),
	{noreply, State}.

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("stats_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.