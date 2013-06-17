-module(stats_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1 , start_new_user_process/1]).

-include("include/softstate.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 60, 3600}, [
    	{users, {stats_serv, start_link, []}, permanent, 1000, worker, [stats_serv]}
    ]}}.