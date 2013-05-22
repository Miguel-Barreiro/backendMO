-module(server).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	Supervisor = server_sup:start_link(),

	httpc:set_options([{max_sessions, 2000}, {pipeline_timeout, 100}, {max_pipeline_length, 5}]),

    server_db:start(),
 	sockserv_sup:launch_empty_listeners(),

    Supervisor.

stop(_State) ->
    ok.