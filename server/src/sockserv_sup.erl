-module(sockserv_sup).

-behaviour(supervisor).

-include("include/macros.hrl").

-export([start_link/0, start_socket/1, empty_listeners/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	Ip ="127.0.0.1",
    %{ok, Ip} = application:get_env(poolserver, poolserver_ip),
    {ok, IpInet} = inet:getaddr(Ip, inet), 

    Port = case application:get_env(poolserver, poolserver_port) of undefined -> 80; {ok, Any1} -> Any1 end,
    SSLPort = case application:get_env(poolserver, poolserver_ssl_port) of undefined -> 443; {ok, Any2} -> Any2 end,
    CertFile = case application:get_env(poolserver, ssl_cert) of undefined -> "etc/server_cert.pem"; {ok, Any3} -> Any3 end,
    KeyFile = case application:get_env(poolserver, ssl_key) of undefined -> "etc/server_key.pem"; {ok, Any4} -> Any4 end,
    lager:info("setting up pool server at ~p:~p, SSL at port ~p", [IpInet, Port, SSLPort]),

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

empty_listeners() ->
    [start_socket(tcp) || _ <- lists:seq(1,100)],
    [start_socket(ssl) || _ <- lists:seq(1,20)],
    ok.
