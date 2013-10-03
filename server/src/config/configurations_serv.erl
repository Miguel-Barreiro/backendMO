-module(configurations_serv).
-behaviour(gen_server).

-export([get_value/1, get_current_version/0, get_current_url/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).

-define(CONFIGURATION_VERSION_URL,"http://s3-us-west-2.amazonaws.com/miniorbs-temp/latest.txt").
-define(CONFIGURATION_BUCKET_URL,"http://s3-us-west-2.amazonaws.com/miniorbs-temp/").

%-define(CONFIGURATION_POLLING_INTERVAL,240000).
-define(CONFIGURATION_POLLING_INTERVAL,40000).

-record(configurations_state, {
	latest_version = undefined,
	url = undefined,
	values = gb_trees:empty() :: gb_tree()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->	
	gen_server:cast(self(), start),
	{ok, #configurations_state{}}.



get_current_version() ->
	gen_server:call(whereis(?MODULE), get_version).

get_current_url() ->
	gen_server:call(whereis(?MODULE), get_url).

get_value( Name ) ->
	gen_server:call(whereis(?MODULE), {get_value, Name}).




handle_cast( start, State) ->
	erlang:send(self(), poll_configuration),
	{noreply, State};

handle_cast( Msg, State) ->
	lager:error("configurations_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.



handle_info( poll_configuration , State = #configurations_state{} ) ->

	Lines = string:tokens( download(?CONFIGURATION_VERSION_URL), "\n"),
	[ Latest_version | _ ] = Lines,

	%lager:debug("latest version is ~p",[Latest_version]),

	New_state = case Latest_version == State#configurations_state.latest_version of 
		true ->			State;
		false ->		swiss:notify( configuration, { new_configuration, 
														Latest_version, 
															get_configuration_url_from_version(Latest_version) } ),
						State#configurations_state{  latest_version = Latest_version, 
														values = get_gb_tree_from_json( Latest_version ),
															url = ?CONFIGURATION_BUCKET_URL ++ Latest_version }
	end,
	erlang:send_after(?CONFIGURATION_POLLING_INTERVAL, self(), poll_configuration),
	{ noreply, New_state };



handle_info(Msg,State) ->
	lager:error("configurations_serv: unhandled info ~p", [Msg]),
	{noreply, State}.



handle_call({get_value, Name}, _From, State ) ->
	{reply, gb_trees:get( Name, State#configurations_state.values) , State};

handle_call( get_version, _From, State ) ->
	{reply, State#configurations_state.latest_version , State};

handle_call( get_url, _From, State ) ->
	{reply, State#configurations_state.url , State};



handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("configurations_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



get_configuration_url_from_version( Version ) ->
	?CONFIGURATION_BUCKET_URL ++ Version ++ "/config_values.json".


get_gb_tree_from_json( Version ) ->
	Json = download( get_configuration_url_from_version(Version) ),
	{Proplist} = ejson:decode(Json),
	{All_values} = proplists:get_value(<<"values">>,Proplist),
	Fun = fun( { Key, Value} , Tree ) ->  
		lager:debug("configuration value ~p -> ~p",[ binary_to_list(Key),Value]),
		gb_trees:insert(Key, Value, Tree) 
	end,
	lists:foldl( Fun, gb_trees:empty(), All_values ).



download(Url) ->
	%lager:debug("downloading ~p",[Url]),
	{ok, {{_Version, 200, _Reason}, _Headers, Body}} = httpc:request(Url),
	Body.
