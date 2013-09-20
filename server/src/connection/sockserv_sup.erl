-module(sockserv_sup).

-behaviour(supervisor).

-include("include/request_macros.hrl").

-export([start_link/0, start_socket/1, launch_empty_listeners/0]).
%-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	Ip ="0.0.0.0",
	%Ip ="127.0.0.1",
	%Port = 9000,
	%SSLPort = 9001,

	{ok, Port} = application:get_env(server, port),
	{ok, SSLPort} = application:get_env(server, ssl_port),

	CertFile = "../../etc/server_cert.pem",
	KeyFile = "../../etc/server_key.pem",

	httpc:set_options([{max_sessions, 2000}, {pipeline_timeout, 100}, {max_pipeline_length, 5}]),


    %{ok, Ip} = application:get_env(poolserver, poolserver_ip),
    {ok, IpInet} = inet:getaddr(Ip, inet),

   % Port = case application:get_env(poolserver, poolserver_port) of undefined -> 80; {ok, Any1} -> Any1 end,
   % SSLPort = case application:get_env(poolserver, poolserver_ssl_port) of undefined -> 443; {ok, Any2} -> Any2 end,
   % CertFile = case application:get_env(poolserver, ssl_cert) of undefined -> "etc/server_cert.pem"; {ok, Any3} -> Any3 end,
   % KeyFile = case application:get_env(poolserver, ssl_key) of undefined -> "etc/server_key.pem"; {ok, Any4} -> Any4 end,

    lager:info("setting up server at ~p:~p, SSL at port ~p", [IpInet, Port, SSLPort]),

	TcpOptions = [
		{ifaddr, IpInet},
		{active,false},
		{backlog, 100},
		binary,
		{reuseaddr, true}
	],

    SSLTcpOptions = TcpOptions ++ [
        {certfile, CertFile}, 
        {keyfile, KeyFile},
        {reuse_sessions, true},
        {ssl_imp, new}
    ],

	{ok, ListenSocket} = gen_tcp:listen(Port, TcpOptions),
	{ok, SSLListenSocket} = ssl:listen(SSLPort, SSLTcpOptions),

	{ok, {{simple_one_for_one, 60, 3600},
			[{socket,
				{sockserv_serv, start_link, [ListenSocket, SSLListenSocket]},
				temporary, 1000, worker, [sockserv_serv]}
			]}}.

start_socket(Type) ->
    supervisor:start_child(?MODULE, [Type]).

launch_empty_listeners() ->
    [start_socket(tcp) || _ <- lists:seq(1,10)],
    [start_socket(ssl) || _ <- lists:seq(1,20)],
    ok.
