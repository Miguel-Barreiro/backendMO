-module(sockserv_serv).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("include/pool_pb.hrl").
-include("include/softstate.hrl").
-include("include/macros.hrl").

-define(MAX_PACKET_SIZE, 4000).
-define(COMPRESS_TRESHOLD, 80).
-define(DEFAULT_INACTIVITY_TIMEOUT, 600).
-define(DEFAULT_PING_TIMEOUT, 65).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(state, {
    socket :: integer(), 
    sslsocket :: integer(), 
    type :: 'tcp' | 'ssl', 
    connstate = 'undefined' :: 'undefined' | 'handshake' | 'unauthorized' | 'authorized', 
    userpid :: pid(), 
    usermonitor,
    last_packet_time,
    last_ping_time,
    peer_ip
}).

-export([start_link/3, add_envelope/1, open_envelope/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

start_link(Socket, SSLSocket, Type) ->
    gen_server:start_link(?MODULE, [Socket, SSLSocket, Type], []).

init([Socket, SSLSocket, Type]) ->
    gen_server:cast(self(), accept),
    {ok, #state{
        socket = Socket, 
        sslsocket = SSLSocket, 
        type = Type,
        last_ping_time = 0,
        last_packet_time = 0
    }}.

add_envelope(Req) ->
    Encoded = pool_pb:encode_req(Req),

    case erlang:iolist_size(Encoded) > ?COMPRESS_TRESHOLD of
        false ->
            pool_pb:encode_envelope(#envelope{
                type = uncompressed_req_t,
                uncompressed = Req
            });
        true ->
            Zipped = zlib:compress(Encoded),

            pool_pb:encode_envelope(#envelope{
                type = compressed_req_t,
                compressed = Zipped,
                uncompressed_size = erlang:iolist_size(Encoded)
            })
    end.

open_envelope(Packet) ->
    case pool_pb:decode_envelope(Packet) of
        #envelope{type = Type, uncompressed = Req} when Type == uncompressed_req_t -> Req;
        #envelope{type = Type, compressed = Zip} when Type == compressed_req_t ->
            pool_pb:decode_req(zlib:uncompress(Zip))
    end.

handle_cast({reply, Reply}, State = #state{socket = Socket, type = Type}) ->
    Packet = add_envelope(Reply),
    
    case Type of
        tcp -> gen_tcp:send(Socket, Packet);
        ssl -> ssl:send(Socket, Packet)
    end,
    {noreply, State};

handle_cast({reply_with_disconnect, Reply}, State = #state{socket = Socket, type = Type})  ->
    Packet = add_envelope(Reply),
    
    case Type of
        tcp -> gen_tcp:send(Socket, Packet);
        ssl -> ssl:send(Socket, Packet)
    end,
    {stop, normal, State};

handle_cast(accept, S = #state{socket = ListenSocket, sslsocket = SSLListenSocket, type = Type}) ->
    
    AcceptRet = case Type of
        ssl -> ssl:transport_accept(SSLListenSocket);
        tcp -> gen_tcp:accept(ListenSocket)
    end,

    case AcceptRet of
        {ok, AcceptSocket} ->

            Connections = stats:get(connections) + 1,
            {ok, MaxConnections} = config:get(max_connections),

            % Launch another acceptor so AWS LB wont kill us
            sockserv_sup:start_socket(Type),

            case Connections > MaxConnections of
                true -> 
                    lager:notice("number of connections ~p exeeds limit of ~p", [Connections, MaxConnections]),
                    timer:sleep(5000),
                    {stop, normal, S};
                false ->
                    Opts = [{packet,4}, {packet_size, 16384}, {active, once}, {nodelay, true}],
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

                    {ok, AuthTimeout} = config:get(connection_authorization_timeout),
                    {A1,A2,A3} = now(),
                    random:seed(A1, A2, A3),

                    erlang:send_after(timer:seconds(60), self(), check_inactivity_timeout),
                    erlang:send_after(timer:seconds(?DEFAULT_PING_TIMEOUT), self(), check_ping_timeout),
                    erlang:send_after(timer:seconds(AuthTimeout), self(), check_auth_state),
                    stats:send({?STATS_CONNECTIONS_TAG, {inc, 1}}),
                    stats:send({?STATS_ACCEPTS_TAG, {inc, 1}}),
                    {noreply, S#state{peer_ip = Name, socket = AcceptSocket, connstate = handshake}}
            end;
        {error, Reason} ->
            lager:error("accept error ~p", [Reason]),
            handle_cast(accept, S)
    end.

handle_info({tcp, _Port, Msg}, State = #state{socket=Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    NewState = process(Msg, State),
    {noreply, NewState};

handle_info({tcp_closed, _Port}, State) ->
    lager:debug("connection closed", []),
    {stop, normal, State};

handle_info({tcp_error, _Port, Error}, State) ->
    lager:error("tcp error ~p", [Error]),
    {stop, normal, State};

handle_info({ssl, _Port, Msg}, State = #state{socket=Socket}) ->
    ssl:setopts(Socket, [{active, once}]),
    NewState = process(Msg, State),
    {noreply, NewState};

handle_info({ssl_error, _Port, Error}, State) ->
    lager:error("SSL error ~p", [Error]),
    {stop, normal, State};

handle_info({ssl_closed, _Port}, State) ->
    lager:debug("SSL connection closed", []),
    {stop, normal, State};

handle_info(check_auth_state, State = #state{connstate = Connstate}) when Connstate == unauthorized ->
    lager:info("connection not authorized in time. terminating"),
    stats:send({?STATS_AUTH_TIMEOUTS_TAG, {inc, 1}}),
    {stop, normal, State};

handle_info(check_auth_state, State) ->
    {noreply, State};

handle_info(check_inactivity_timeout, State = #state{last_packet_time = LastPacketTime}) ->
    {ok, Timeout} = config:get(sockserver_inactivity_timeout, ?DEFAULT_INACTIVITY_TIMEOUT),
    Ret = case swiss:unix_timestamp() - LastPacketTime > Timeout of
        true ->
            lager:debug("dropping connection due to inactivity"),
            stats:send({?STATS_INACTIVITY_TIMEOUTS_TAG, {inc, 1}}),
            {stop, normal, State};
        false ->
            erlang:send_after(timer:seconds(60), self(), check_inactivity_timeout),
            {noreply, State}
    end,
    Ret;

handle_info(check_ping_timeout, State = #state{last_ping_time = LastPingTime, last_packet_time = LastPacketTime}) ->
    LastActivity = max(LastPingTime, LastPacketTime),
    Ret = case swiss:unix_timestamp() - LastActivity > ?DEFAULT_PING_TIMEOUT of
        true ->
            lager:debug("dropping connection due to ping timeout"), 
            stats:send({?STATS_PING_TIMEOUTS_TAG, {inc, 1}}),
            {stop, normal, State};
        false ->
            erlang:send_after(timer:seconds(?DEFAULT_PING_TIMEOUT), self(), check_ping_timeout),
            {noreply, State}
    end,
    Ret;

handle_info({'DOWN', Reference, process, Pid, Reason}, State = #state{usermonitor = UserMonitor}) when Reference == UserMonitor ->
    lager:debug("user is ~p DOWN, ~p", [Pid, Reason]),

    Disco = #req{
        type = disconnect_t,
        disconnect_field = #disconnect {
            reason = game_crashed,
            retry_sensible = true
        } 
    },
    gen_server:cast(self(), {reply_with_disconnect, Disco}),
    {noreply, State};

handle_info(M,S) ->
    lager:error("unhandled info ~p", [M]),
    {noreply, S}.

handle_call(_E, _From, State) ->
    {noreply, State}.

terminate(normal, #state{connstate = CS}) when CS =/= undefined ->
    stats:send({?STATS_CONNECTIONS_TAG, {dec, 1}}),
    stats:send({?STATS_DISCONNECTS_TAG, {inc, 1}}),
    ok;

terminate(normal, _State) ->
    ok;

terminate(Reason, #state{connstate = CS}) when CS =/= undefined ->
    lager:error("terminate reason: ~p", [Reason]),
    stats:send({?STATS_CONNECTIONS_TAG, {dec, 1}}),
    stats:send({?STATS_DISCONNECTS_TAG, {inc, 1}}),
    ok;

terminate(Reason, _State) ->
    lager:error("terminate reason: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ========== Shortcut ==========
process_packet(Packet = #req{type = Type}, State = #state{connstate = Connstate, userpid = UserPid}) 
    when (Connstate == authorized) andalso (
        (Type == aim_events_t) orelse 
        (Type == shot_taken_t) orelse
        (Type == animation_done_t) orelse
        (Type == balls_racked_t) orelse
        (Type == entergamequeue_t) orelse
        (Type == leavegamequeue_t)
    ) ->
    gen_server:cast(UserPid, {client_packet, Packet}),
    State;
%% ==============================

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate}) 
    when (Type == create_session_t) and (Connstate == handshake) ->

    {ok, VersionTable} = config:get(min_client_version),

    case lists:keyfind(Req#req.create_session_field#create_session.platform, 1, VersionTable) of
        {_Platform, MinVersion} when Req#req.create_session_field#create_session.client_version < MinVersion ->
            lager:info("incompatible_client ~p < ~p on plaform ~p", 
                [Req#req.create_session_field#create_session.client_version, MinVersion, Req#req.create_session_field#create_session.platform]),
            Disco = #req{
                type = disconnect_t,
                disconnect_field = #disconnect {
                    reason = incompatible_client,
                    retry_sensible = false
                } 
            },
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State;
        _Other ->
            process_packet(Req, State#state{ connstate = unauthorized })
    end;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate, peer_ip = PeerIP}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == create_miniclip_user) ->

    lager:info("process_packet(create_session_t:create_miniclip_user) device ~p", 
        [Req#req.create_session_field#create_session.device_name]),
    PushToken = Req#req.create_session_field#create_session.push_notification_token,
    Platform = Req#req.create_session_field#create_session.platform,
    DeviceDescription = swiss:parse_platform_string(Req#req.create_session_field#create_session.device_name),

    {IP, _Port} = PeerIP,
    CountryCode = swiss:ip_to_country_code(IP),

    NewState = case miniclip_api:create_user_profile(
        Req#req.create_session_field#create_session.create_data#user_creation_info.email, 
        Req#req.create_session_field#create_session.create_data#user_creation_info.nickname, 
        Req#req.create_session_field#create_session.create_data#user_creation_info.password, 
        Req#req.create_session_field#create_session.create_data#user_creation_info.birth_day, 
        Req#req.create_session_field#create_session.create_data#user_creation_info.birth_month, 
        Req#req.create_session_field#create_session.create_data#user_creation_info.birth_year, 
        swiss:ip_to_binary(IP), list_to_binary(CountryCode)) of

        {ok, #user_profile_rec{ profile_id = UserId }} -> 
            {ok, ProfileData} = miniclip_api:login_using_mc_credentials(
                Req#req.create_session_field#create_session.create_data#user_creation_info.email,
                Req#req.create_session_field#create_session.create_data#user_creation_info.password,
                ?MAX_AWARDS),

            %% TODO: get it from ProfileData
            {ok, Token} = miniclip_api:userid_to_token(UserId),

            Uids = [{openuid, Req#req.create_session_field#create_session.openuid}, 
                {sha1_mac, Req#req.create_session_field#create_session.sha1_mac}],

            {ok, UserPid} = connect_user(UserId, ProfileData, self(), PushToken, Token, undefined, Platform, Uids, DeviceDescription),
            UserMonitor = monitor(process, UserPid),
            State#state{connstate = authorized, userpid = UserPid, usermonitor = UserMonitor};
        {error, Error} ->
            Ret = case lists:keyfind(code, 1, Error) of
                {code, ?INVALID_DATE} -> invalid_date;
                {code, ?DUPLICATE_USERNAME} -> duplicate_email;
                {code, ?DUPLICATE_NICKNAME} -> duplicate_nickname;
                {code, ?INVALID_NICKNAME} -> invalid_nickname;
                {code, ?INVALID_SIGNATURE} -> invalid_nickname
            end,

            Disco = #req{
                type = disconnect_t,
                disconnect_field = #disconnect {
                    reason = Ret,
                    retry_sensible = false
                } 
            },
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State
    end,
    NewState;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == miniclip_username_password) ->

    lager:info("process_packet(create_session_t:miniclip_username_password) device ~p", 
        [Req#req.create_session_field#create_session.device_name]),
    PushToken = Req#req.create_session_field#create_session.push_notification_token,
    Platform = Req#req.create_session_field#create_session.platform,
    DeviceDescription = swiss:parse_platform_string(Req#req.create_session_field#create_session.device_name),

    NewState = case miniclip_api:login_using_mc_credentials(
        Req#req.create_session_field#create_session.username, 
        Req#req.create_session_field#create_session.password,
        ?MAX_AWARDS) of

        {ok, ProfileData} ->
            %% TODO: get it from ProfileData
            {ok, Token} = miniclip_api:userid_to_token(ProfileData#profile_data_rec.user_profile#user_profile_rec.profile_id),

            Uids = [{openuid, Req#req.create_session_field#create_session.openuid}, 
                {sha1_mac, Req#req.create_session_field#create_session.sha1_mac}],
            {ok, UserPid} = connect_user(ProfileData#profile_data_rec.user_profile#user_profile_rec.profile_id, ProfileData, 
                self(), PushToken, Token, undefined, Platform, Uids, DeviceDescription),
            UserMonitor = monitor(process, UserPid),
            State#state{connstate = authorized, userpid = UserPid, usermonitor = UserMonitor};
        {error, _Err} ->
            Disco = #req{
                type = disconnect_t,
                disconnect_field = #disconnect {
                    reason = incorrect_login,
                    retry_sensible = false
                } 
            },
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State
    end,
    NewState;


process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == miniclip) ->

    lager:info("process_packet(create_session_t:miniclip) device ~p", 
        [Req#req.create_session_field#create_session.device_name]),
    Token = Req#req.create_session_field#create_session.authentication_token,
    PushToken = Req#req.create_session_field#create_session.push_notification_token,
    Platform = Req#req.create_session_field#create_session.platform,
    DeviceDescription = swiss:parse_platform_string(Req#req.create_session_field#create_session.device_name),

    NewState = case miniclip_api:login_using_mc_token(Token, ?MAX_AWARDS) of
        {ok, ProfileData} ->
            Uids = [{openuid, Req#req.create_session_field#create_session.openuid}, 
                {sha1_mac, Req#req.create_session_field#create_session.sha1_mac}],

            {ok, UserPid} = connect_user(ProfileData#profile_data_rec.user_profile#user_profile_rec.profile_id, 
                ProfileData, self(), PushToken, Token, undefined, Platform, Uids, DeviceDescription),
            UserMonitor = monitor(process, UserPid),
            State#state{connstate = authorized, userpid = UserPid, usermonitor = UserMonitor};
        _Error ->
            Disco = #req{
                type = disconnect_t,
                disconnect_field = #disconnect {
                    reason = incorrect_login,
                    retry_sensible = false
                } 
            },
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State
    end,
    NewState;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == miniclip_web_token) ->

    lager:info("process_packet(create_session_t:miniclip_web_token) device ~p", 
        [Req#req.create_session_field#create_session.device_name]),
    Token = Req#req.create_session_field#create_session.authentication_token,
    PushToken = Req#req.create_session_field#create_session.push_notification_token,
    Platform = Req#req.create_session_field#create_session.platform,
    DeviceDescription = swiss:parse_platform_string(Req#req.create_session_field#create_session.device_name),

    NewState = case miniclip_api:login_using_mc_web_token(Token, ?MAX_AWARDS) of
        {ok, ProfileData} ->
            Uids = [{openuid, Req#req.create_session_field#create_session.openuid}, 
                {sha1_mac, Req#req.create_session_field#create_session.sha1_mac}],
            {ok, UserPid} = connect_user(ProfileData#profile_data_rec.user_profile#user_profile_rec.profile_id, 
                ProfileData, self(), PushToken, Token, undefined, Platform, Uids, DeviceDescription),
            UserMonitor = monitor(process, UserPid),
            State#state{connstate = authorized, userpid = UserPid, usermonitor = UserMonitor};
        _Error ->
            Disco = #req{
                type = disconnect_t,
                disconnect_field = #disconnect {
                    reason = incorrect_login,
                    retry_sensible = false
                } 
            },
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State
    end,
    NewState;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == facebook) ->

    lager:info("process_packet(create_session_t:facebook) device ~p", 
        [Req#req.create_session_field#create_session.device_name]),
    Token = Req#req.create_session_field#create_session.authentication_token,
    PushToken = Req#req.create_session_field#create_session.push_notification_token,
    Platform = Req#req.create_session_field#create_session.platform,
    DeviceDescription = swiss:parse_platform_string(Req#req.create_session_field#create_session.device_name),

    Disco = #req{
        type = disconnect_t,
        disconnect_field = #disconnect {
            reason = incorrect_login,
            retry_sensible = false
        } 
    },
    NewState = case miniclip_api:login_using_facebook_token(Token, ?MAX_AWARDS) of
        {ok, ProfileData} ->
            case facebook:validate_token(Token) of
                {ok, FacebookId, _Name} ->
                    Uids = [{openuid, Req#req.create_session_field#create_session.openuid}, 
                        {sha1_mac, Req#req.create_session_field#create_session.sha1_mac}],

                    {ok, UserPid} = connect_user(ProfileData#profile_data_rec.user_profile#user_profile_rec.profile_id, 
                        ProfileData, self(), PushToken, Token, FacebookId, Platform, Uids, DeviceDescription),
                    UserMonitor = monitor(process, UserPid),
                    State#state{connstate = authorized, userpid = UserPid, usermonitor = UserMonitor};
                _FBErr ->
                    gen_server:cast(self(), {reply_with_disconnect, Disco}),
                    State
            end;
        _Error ->
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State
    end,
    NewState;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate, peer_ip = PeerIP}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == guest) and 
        ((Req#req.create_session_field#create_session.authentication_token == undefined) orelse 
            (Req#req.create_session_field#create_session.authentication_token == <<"">>)) ->

    PushToken = Req#req.create_session_field#create_session.push_notification_token,
    Platform = Req#req.create_session_field#create_session.platform,
    DeviceDescription = swiss:parse_platform_string(Req#req.create_session_field#create_session.device_name),

    {IP, _Port} = PeerIP,
    CountryCode = swiss:ip_to_country_code(IP),

    lager:info("process_packet(create_session_t:new guest) device ~p", 
        [Req#req.create_session_field#create_session.device_name]),
    {ok, UserId, AccessToken} = miniclip_api:create_guest_user(list_to_binary(CountryCode)),

    NewState = case miniclip_api:login_using_mc_token(AccessToken, ?MAX_AWARDS) of
        {ok, ProfileData} ->
            Uids = [{openuid, Req#req.create_session_field#create_session.openuid}, 
                {sha1_mac, Req#req.create_session_field#create_session.sha1_mac}],
            {ok, UserPid} = connect_user(ProfileData#profile_data_rec.user_profile#user_profile_rec.profile_id, 
                ProfileData, self(), PushToken, AccessToken, undefined, Platform, Uids, DeviceDescription),
            UserMonitor = monitor(process, UserPid),
            State#state{connstate = authorized, userpid = UserPid, usermonitor = UserMonitor};
        _Error ->
            lager:notice("login with newly created token failed: ~p ~p", [UserId, AccessToken]),
            Disco = #req{
                type = disconnect_t,
                disconnect_field = #disconnect {
                    reason = incorrect_login,
                    retry_sensible = false
                } 
            },
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State
    end,
    NewState;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == miniclip_password_reminder) ->

    lager:debug("process_packet(password reminder:~p)", [Req#req.create_session_field#create_session.username]),

    Retval = case miniclip_api:recover_password(Req#req.create_session_field#create_session.username) of
        ok -> pw_reminder_sent;
        {error, _Error} -> pw_reminder_invalid_user
    end, 

    Disco = #req{
        type = disconnect_t,
        disconnect_field = #disconnect {
            reason = Retval,
            retry_sensible = false
        } 
    },
    gen_server:cast(self(), {reply_with_disconnect, Disco}),
    State;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate}) 
    when (Type == create_session_t) and (Connstate == unauthorized) and 
        (Req#req.create_session_field#create_session.realm == guest) ->

    lager:debug("process_packet(create_session_t:guest)", []),
    Token = Req#req.create_session_field#create_session.authentication_token,
    PushToken = Req#req.create_session_field#create_session.push_notification_token,
    Platform = Req#req.create_session_field#create_session.platform,
    DeviceDescription = swiss:parse_platform_string(Req#req.create_session_field#create_session.device_name),

    NewState = case miniclip_api:login_using_mc_token(Token, ?MAX_AWARDS) of
        {ok, ProfileData} ->
            Uids = [{openuid, Req#req.create_session_field#create_session.openuid}, 
                {sha1_mac, Req#req.create_session_field#create_session.sha1_mac}],
            {ok, UserPid} = connect_user(ProfileData#profile_data_rec.user_profile#user_profile_rec.profile_id, 
                ProfileData, self(), PushToken, Token, undefined, Platform, Uids, DeviceDescription),
            UserMonitor = monitor(process, UserPid),
            State#state{connstate = authorized, userpid = UserPid, usermonitor = UserMonitor};
        _Error ->
            Disco = #req{
                type = disconnect_t,
                disconnect_field = #disconnect {
                    reason = incorrect_login,
                    retry_sensible = false
                } 
            },
            gen_server:cast(self(), {reply_with_disconnect, Disco}),
            State
    end,
    NewState;

process_packet(Req = #req{type = Type}, State = #state{connstate = Connstate, peer_ip = PeerIP, userpid = UserPid})
    when (Type == upgrade_guest_request_t) and (Connstate == authorized) ->

    {IP, _Port} = PeerIP,
    CountryCode = swiss:ip_to_country_code(IP),

    gen_server:cast(UserPid, {client_packet, Req, swiss:ip_to_binary(IP), list_to_binary(CountryCode)}),
    State;

process_packet(Req = #req{type = Type}, State) when (Type == ping_t) ->
    Pang = #req {
        type = pang_t,
        pang_field = #pang {
            id = Req#req.ping_field#ping.id
        }
    },
    case Req#req.ping_field#ping.roundtrip of
        undefined -> ok;
        RoundTrip -> stats:send({histogram, ?STATS_PING_ROUNDTRIP_TAG, RoundTrip})
    end,
    gen_server:cast(self(), {reply, Pang}),
    State#state{ last_ping_time = swiss:unix_timestamp() };

process_packet(Packet, State = #state{connstate = Connstate, userpid = UserPid}) when Connstate == authorized ->
    gen_server:cast(UserPid, {client_packet, Packet}),
    State;

process_packet(Packet, State) -> 
    lager:debug("client sent packet ~p when not authorized", [Packet]),
    Disco = #req{
        type = disconnect_t,
        disconnect_field = #disconnect {
            reason = not_authorized,
            retry_sensible = false
        } 
    },
    gen_server:cast(self(), {reply_with_disconnect, Disco}),
    State.

process(Packet, State = #state{}) when is_binary(Packet) ->
    NewState = process_packet(open_envelope(Packet), State),
    NewState#state{ last_packet_time = swiss:unix_timestamp(), last_ping_time =  swiss:unix_timestamp() }.

connect_user(UserId, ProfileData, ConnectionPid, PushToken, AuthToken, FacebookId, Platform, Uids, DeviceDescription) when is_integer(UserId) ->
    connect_user(list_to_binary(integer_to_list(UserId)), ProfileData, ConnectionPid, PushToken, AuthToken, FacebookId, Platform, Uids, DeviceDescription);
connect_user(UserId, ProfileData, ConnectionPid, PushToken, AuthToken, FacebookId, Platform, Uids, DeviceDescription) ->

    Now = swiss:unix_timestamp(),
    NewUserRecord = #user {
        userid = UserId,
        state = creating,
        created_time = Now
    },

    Fun = fun () ->
        case mnesia:read({user, UserId}) of
            [] ->
                ok = mnesia:write(NewUserRecord),
                new_context;
            [#user{ state = State } = UserRecord] when State == created ->
                {existing_context, UserRecord#user.pid};
            [#user{ state = State, created_time = CreatedTime }] when (State == creating) andalso (CreatedTime > (Now - 30)) ->
                retry;
            [#user{ state = State }] when (State == creating) ->
                ok = mnesia:write(NewUserRecord),
                new_context
        end
    end,

    case mnesia:sync_transaction(Fun) of
        {atomic, new_context} ->
            lager:debug("connecting new user ~p", [UserId]),
            supervisor:start_child(users_sup, [UserId, ProfileData, ConnectionPid, PushToken, AuthToken, FacebookId, Platform, Uids, DeviceDescription]);
        {atomic, retry} ->
            timer:sleep(1000),
            connect_user(UserId, ProfileData, ConnectionPid, PushToken, AuthToken, FacebookId, Platform, Uids, DeviceDescription);
        {atomic, {existing_context, OldPid}} ->
            lager:debug("connecting existing user ~p", [UserId]),
            case catch gen_server:call(OldPid, {reconnect, ConnectionPid, PushToken, Platform, Uids, DeviceDescription}, infinity) of
                ok -> {ok, OldPid};
                Error -> 
                    lager:error("reconnect to user instance failed, removing user from mnesia: ~p (~p)", [UserId, Error]),
                    {atomic, ok} = mnesia:transaction(fun () -> mnesia:delete({user, UserId}) end),
                    connect_user(UserId, ProfileData, ConnectionPid, PushToken, AuthToken, FacebookId, Platform, Uids, DeviceDescription)
            end;
        {aborted, Err} ->
            lager:error("mnesia error ~p", [Err]),
            timer:sleep(10000),
            {error, Err}
    end.






