-module(configurations_serv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).


-define(CONFIGURATION_POLLING_INTERVAL,240000).

-record(configurations_state, {
	latest_version = undefined
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->	
	gen_server:send_after(?CONFIGURATION_POLLING_INTERVAL, self(), poll_configuration),
	{ok, #configurations_state{}}.


handle_cast( Msg, State) ->
	lager:error("configurations_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.



download(Url) ->
	{ok, {{_Version, 200, _Reason}, _Headers, Body}} = httpc:request(Url),
	Body.

handle_info( poll_configuration , State = #configurations_state{} ) ->

	Latest_version = download("http://s3-us-west-2.amazonaws.com/miniorbs-temp/latest.txt"),
	lager:info("latest version is ~p",[Latest_version]),

	gen_server:send_after(?CONFIGURATION_POLLING_INTERVAL, self(), poll_configuration),

	{ noreply, State };

handle_info(Msg,State) ->
	lager:error("configurations_serv: unhandled info ~p", [Msg]),
	{noreply, State}.

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("configurations_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.