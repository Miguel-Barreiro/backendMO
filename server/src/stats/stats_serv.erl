-module(stats_serv).

-behaviour(gen_server).
-define(SERVER, ?MODULE).


-record(stats_state, {
	number_connections = 0 :: Integer
}).