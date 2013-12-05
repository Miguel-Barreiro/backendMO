-module(rematch_queue_sup).
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
%	{ok, { {one_for_one, 5, 10}, [ ?CHILD(rematch_queue_serv, worker) ] } }.
	{ok, {{simple_one_for_one, 60, 3600}, [
		{rematches, {rematch_queue_serv, start_link, []}, temporary, 1000, worker, [rematch_queue_serv]}
	]}}.

