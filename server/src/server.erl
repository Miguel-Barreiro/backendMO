-module(server).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	Supervisor = server_sup:start_link(),

	random:seed(),

    server_db:start(),
 	sockserv_sup:launch_empty_listeners(),

    Supervisor.

stop(_State) ->
    ok.