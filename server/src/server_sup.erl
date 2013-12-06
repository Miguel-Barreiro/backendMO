-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	lager:set_loglevel(lager_console_backend, debug),
	{ok, { {one_for_one, 5, 10}, [
		?CHILD(sockserv_sup, supervisor),
		?CHILD(users_sup, supervisor),
		?CHILD(queue_sup, supervisor),
		?CHILD(game_sup, supervisor),
		?CHILD(stats_sup, supervisor),
		?CHILD(configurations_sup, supervisor),
		?CHILD(rematches_sup,supervisor)
	]} }.

