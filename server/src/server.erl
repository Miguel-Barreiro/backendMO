-module(server).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Res = sockserv_sup:start_link(),
    server_db:start(),
    sockserv_sup:launch_empty_listeners(),
    Res.

stop(_State) ->
    ok.