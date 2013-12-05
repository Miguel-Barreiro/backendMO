-module(users_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1 , start_new_user_process/1]).

-include("include/softstate.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 60, 3600}, [
		{users, {users_serv, start_link, []}, temporary, 1000, worker, [users_serv]}
	]}}.

%%
%	@param UserDataList = [ ConnectionPid , UserId ]
%%
start_new_user_process( UserDataList )->
	supervisor:start_child(users_sup, UserDataList).
