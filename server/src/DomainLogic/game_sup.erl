-module(game_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1 , start_new_game_process/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 60, 3600}, [
    	{games, {game_serv, start_link, []}, temporary, 1000, worker, [game_serv]}
    ]}}.

start_new_game_process( Game_data_list )->
	lager:debug("game_sup: start new game with ~p",Game_data_list),
	supervisor:start_child(game_sup, Game_data_list ).