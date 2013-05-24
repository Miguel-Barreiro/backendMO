-module(sockserv_serv).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("include/softstate.hrl").
-include("include/request_macros.hrl").

-define(MAX_PACKET_SIZE, 4000).
-define(COMPRESS_TRESHOLD, 80).
-define(DEFAULT_INACTIVITY_TIMEOUT, 600).
-define(DEFAULT_PING_TIMEOUT, 65).
-define(DEFAULT_AUTH_TIMEOUT,3600).

-define(MAX_CONNECTIONS, 400).


-define(DISCONNECT_MESSAGE,"{ \"" ++ ?MESSAGE_ACTION_KEY ++ "\" : \"disconect\" , \"reason\" : \"game crashed\" }").


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(connection_state, {
	socket :: integer(),
	sslsocket :: integer(), 
	type :: 'tcp' | 'ssl',
	connstate = 'undefined' :: 'undefined' | 'handshake' | 'unauthorized' | 'authorized', 
	user_process_pid :: pid(), 
	user_monitor,
	last_packet_time,
	last_ping_time,
	peer_ip
}).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,code_change/3, terminate/2]).

start_link(Socket, SSLSocket, Type) ->
	gen_server:start_link(?MODULE, [Socket, SSLSocket, Type], []).

init([Socket, SSLSocket, Type]) ->
	gen_server:cast(self(), accept),
	
	{ok, #connection_state{
		socket = Socket, 
		sslsocket = SSLSocket,
		type = Type,
		last_ping_time = 0,
		last_packet_time = 0
	}}.


handle_cast({reply, Reply}, State = #connection_state{socket = Socket, type = Type}) ->
    %Packet = add_envelope(Reply),
	Packet = Reply,
	case Type of
		tcp -> gen_tcp:send(Socket, Packet);
		ssl -> ssl:send(Socket, Packet)
	end,
	{noreply, State};

handle_cast({reply_with_disconnect, Reply}, State = #connection_state{socket = Socket, type = Type})  ->
	%Packet = add_envelope(Reply),
	Packet = Reply,
	case Type of
		tcp -> gen_tcp:send(Socket, Packet);
		ssl -> ssl:send(Socket, Packet)
	end,
	message_processor:handle_disconect(),
    {stop, normal, State};


handle_cast( { register_user_process, User_process_pid }, State = #connection_state{} ) ->
	New_state = State#connection_state{ user_monitor =  monitor(process, User_process_pid ) , user_process_pid = User_process_pid },
	{noreply, New_state};


handle_cast(accept, State = #connection_state{socket = ListenSocket, sslsocket = SSLListenSocket, type = Type}) ->
    
    AcceptRet = case Type of
        ssl -> ssl:transport_accept(SSLListenSocket);
        tcp -> gen_tcp:accept(ListenSocket)
    end,

    case AcceptRet of
        {ok, AcceptSocket} ->

			lager:info("accepted another connection"),

			MaxConnections = ?MAX_CONNECTIONS,

            % Launch another acceptor so AWS LB wont kill us
			sockserv_sup:start_socket(Type),
			
			%for now :P
			Connections  = 0,

			case Connections > MaxConnections of
				true ->
					lager:notice("number of connections ~p exeeds limit of ~p", [Connections, MaxConnections]),
					timer:sleep(5000),
					{stop, normal, State};
				false ->
					Opts = [{packet,4}, {packet_size, 16384}, {active, once}, {nodelay, true}],
					%Opts = [{packet_size, 16384}, {active, once}, {nodelay, true}],
					{ok, Name} = case Type of
					    tcp ->
					        ok = inet:setopts(AcceptSocket, Opts),
					        inet:peername(AcceptSocket);
					    ssl ->
					        ok = ssl:ssl_accept(AcceptSocket),
					        ok = ssl:setopts(AcceptSocket, Opts),
					        ssl:peername(AcceptSocket)
					end,

					lager:debug("accepted ~p connection from: ~p", [Type, Name]),

					message_processor:handle_connect(),

					AuthTimeout = ?DEFAULT_AUTH_TIMEOUT,

					%erlang:send_after(timer:seconds(60), self(), check_inactivity_timeout),
					%erlang:send_after(timer:seconds(?DEFAULT_PING_TIMEOUT), self(), check_ping_timeout),
					%erlang:send_after(timer:seconds(AuthTimeout), self(), check_auth_state),

					{noreply, State#connection_state{peer_ip = Name, socket = AcceptSocket, connstate = handshake}}
			end;
		{error, Reason} ->
            lager:error("accept error ~p", [Reason]),
            handle_cast(accept, State)
    end.



%%
%	called when the user process stops
%%
handle_info({'DOWN', Reference, process, _Pid, _Reason}, State = #connection_state{user_monitor = Game_monitor}) when (Reference == Game_monitor) ->
	
	lager:debug("game either crashed or user was stoped so we disconect and send a disconect message", []),
	demonitor(Game_monitor , [flush]),

    gen_server:cast(self(), {reply_with_disconnect, ?DISCONNECT_MESSAGE}),
    {noreply, State};


handle_info({tcp, _Port, Msg}, State = #connection_state{socket=Socket}) ->
	inet:setopts(Socket, [{active, once}]),
	NewState = process(Msg, State),
	{noreply, NewState};

handle_info({tcp_closed, _Port}, State) ->
	lager:debug("connection closed", []),
	message_processor:handle_disconect(),
	{stop, normal, State};

handle_info({tcp_error, _Port, Error}, State) ->
	lager:error("tcp error ~p", [Error]),
	message_processor:handle_disconect(),
	{stop, normal, State};

handle_info({ssl, _Port, Msg}, State = #connection_state{socket=Socket}) ->
	ssl:setopts(Socket, [{active, once}]),
	NewState = process(Msg, State),
	{noreply, NewState};

handle_info({ssl_error, _Port, Error}, State) ->
	lager:error("SSL error ~p", [Error]),
	message_processor:handle_disconect(),
	{stop, normal, State};

handle_info({ssl_closed, _Port}, State) ->
	lager:debug("SSL connection closed", []),
	message_processor:handle_disconect(),
	{stop, normal, State};

handle_info(check_auth_state, State = #connection_state{connstate = Connstate}) when Connstate == unauthorized ->
	lager:info("connection not authorized in time. terminating"),
	message_processor:handle_disconect(),
	{stop, normal, State};

handle_info(check_auth_state, State) ->
    {noreply, State};

handle_info(check_inactivity_timeout, State = #connection_state{last_packet_time = LastPacketTime}) ->
    Timeout = ?DEFAULT_INACTIVITY_TIMEOUT,

    Ret = case swiss:unix_timestamp() - LastPacketTime > Timeout of
        true ->
            lager:debug("dropping connection due to inactivity"),
            message_processor:handle_disconect(),
            {stop, normal, State};
        false ->
            erlang:send_after(timer:seconds(60), self(), check_inactivity_timeout),
            {noreply, State}
    end,
    Ret;

handle_info(check_ping_timeout, State = #connection_state{last_ping_time = LastPingTime, last_packet_time = LastPacketTime}) ->
    LastActivity = max(LastPingTime, LastPacketTime),
    Ret = case swiss:unix_timestamp() - LastActivity > ?DEFAULT_PING_TIMEOUT of
        true ->
            lager:debug("dropping connection due to ping timeout"),
            message_processor:handle_disconect(),
            {stop, normal, State};
        false ->
            erlang:send_after(timer:seconds(?DEFAULT_PING_TIMEOUT), self(), check_ping_timeout),
            {noreply, State}
    end,
    Ret;

handle_info(M,S) ->
	lager:error("unhandled info ~p", [M]),
	{noreply, S}.

handle_call(_E, _From, State) ->
    {noreply, State}.

terminate(normal, #connection_state{connstate = CS}) when CS =/= undefined ->
    ok;

terminate(normal, _State) ->
    ok;

terminate(Reason, #connection_state{connstate = CS}) when CS =/= undefined ->
	lager:error("terminate reason: ~p", [Reason]),
    ok;

terminate(Reason, _State) ->
	lager:error("terminate reason: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.




process(Msg, State = #connection_state{ user_process_pid = User_process_pid} ) when User_process_pid == undefined ->
	case message_processor:process_pre_login_message(Msg) of
		{reply, Reply} -> gen_server:cast(self(), {reply, Reply} );
		{reply_with_disconnect, Last_reply} -> gen_server:cast(self(), {reply_with_disconnect, Last_reply} );
		_ -> nothing
	end,
	State;

process(Msg, State = #connection_state{} ) ->
	case message_processor:process(Msg, State#connection_state.user_process_pid) of
		{reply, Reply} -> gen_server:cast(self(), {reply, Reply} );
		{reply_with_disconnect, Last_reply} -> gen_server:cast(self(), {reply_with_disconnect, Last_reply} );
		_ -> nothing
	end,
	State.












