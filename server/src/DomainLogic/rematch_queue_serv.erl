-module(rematch_queue_serv).

-behaviour(gen_server).

-define(REMATCH_TIMEOUT , 30000).
-define(REMATCH_TIMETODIE, 15000).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/1]).
-export([ enter/5, set_user_powers/3, set_user_rematch/2, remove_user/2]).


-type rematch_state() :: ask_rematch | pick_powers.

-record( rematch_lobby_user, {
	user_pid = undefined :: pid(),
	user_id = undefined,
	user_monitor = undefined,
	is_ready = false,
	powers = undefined
}).

-record( rematch_lobby, {
	user_list = [] :: [{ binary(), #rematch_lobby_user{} } ],
	state = ask_rematch :: rematch_state(),
	rematch_timeout,
	tier = undefined,
	lobby_key
}).





enter(User1Pid,User1Id,User2Pid,User2Id,Tier) ->
	{ok, _Pid} = supervisor:start_child( rematch_queue_sup, [{enter_user_pair,User1Pid,User1Id,User2Pid,User2Id,Tier}] ),
	ok.


remove_user(RematchPid, UserPid) ->
	gen_server:cast(RematchPid, { remove_user, UserPid } ),
	ok.


set_user_rematch(RematchPid, UserPid) ->
	gen_server:cast(RematchPid, { set_user_rematch, UserPid } ),
	ok.

set_user_powers(RematchPid, UserPid, Powers) ->
	gen_server:cast(RematchPid, { set_user_powers, UserPid, Powers } ),
	ok.






start_link(Args) ->
	gen_server:start_link( ?MODULE, [Args], [] ).


init([InitMsg]) ->
	gen_server:cast( self(), InitMsg ),
	{ok, initializing}.




handle_cast( { remove_user, UserPid }, State ) ->
	lager:debug("no rematch received by ~p",[UserPid]),
	handle_remove_user( UserPid , State);





handle_cast( { set_user_powers, UserPid, Powers }, Lobby = #rematch_lobby{} ) ->
	User = proplists:get_value(UserPid, Lobby#rematch_lobby.user_list),
	NewUserList = [ { UserPid, User#rematch_lobby_user{ powers = Powers } }  |  proplists:delete(UserPid, Lobby#rematch_lobby.user_list) ],

	case lists:all(fun( { _ , CurrentUser}) -> CurrentUser#rematch_lobby_user.powers =/= undefined end, NewUserList) of
		true ->
			[ { User1Pid, User1 } , {User2Pid , User2}] = NewUserList,
			game_sup:start_new_game_process( [ Lobby#rematch_lobby.tier, User1Pid, User1#rematch_lobby_user.user_id,
													User2Pid, User2#rematch_lobby_user.user_id,
														configurations_serv:get_current_version(), configurations_serv:get_current_version()] ),
			{noreply, rematched};
		false ->
			NewLobby = Lobby#rematch_lobby{ user_list = NewUserList, state = pick_powers },
			{noreply, NewLobby}
	end;






handle_cast( { set_user_rematch, UserPid },  Lobby = #rematch_lobby{} ) ->
	User = proplists:get_value(UserPid, Lobby#rematch_lobby.user_list),

	ListWithoutUser = proplists:delete(UserPid, Lobby#rematch_lobby.user_list),
	NewUserList = [ { UserPid, User#rematch_lobby_user{ is_ready = true } }  |   ListWithoutUser],

	case lists:all(fun( { _ , CurrentUser}) -> CurrentUser#rematch_lobby_user.is_ready end, NewUserList) of
		true ->
			lager:debug("--PICK POWERS MODE--",[]),

%			erlang:cancel_timer( Lobby#rematch_lobby.rematch_timeout),

			%NewLobby = Lobby#rematch_lobby{ user_list = NewUserList, state = pick_powers },
			%{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:insert( LobbyKey, NewLobby ,gb_trees:delete(LobbyKey, Lobbies)) } };
			[ { User1Pid, User1 } , {User2Pid , User2}] = NewUserList,
			game_sup:start_new_game_process( [ Lobby#rematch_lobby.tier, User1Pid, User1#rematch_lobby_user.user_id, [],
													User2Pid, User2#rematch_lobby_user.user_id, [],
														configurations_serv:get_current_version(), 
														configurations_serv:get_current_url()] ),

			Fun = fun({ _CurrentUserPid, CurrentUser }) ->
				demonitor( CurrentUser#rematch_lobby_user.user_monitor)
			end,
			lists:foreach( Fun, Lobby#rematch_lobby.user_list ),
			{noreply, rematched};

		false ->
			Msg = message_processor:create_rematch_message(),
			lists:foreach( fun({ _ , CurrentUser }) ->  
								gen_server:cast( CurrentUser#rematch_lobby_user.user_pid, {send_message, Msg }) 
							end, ListWithoutUser ),
			NewLobby = Lobby#rematch_lobby{ user_list = NewUserList },
			{noreply, NewLobby}
	end;




handle_cast( { enter_user_pair, User1Pid, User1Id, User2Pid, User2Id, Tier }, 
					initializing ) ->
	
	gen_server:cast( User1Pid, {set_rematch_queue_state,self()} ),
	gen_server:cast( User2Pid, {set_rematch_queue_state,self()} ),
	lager:info("rematch queue enter ~p,~p",[User2Pid,User1Pid]),

	User1 = #rematch_lobby_user{ user_pid = User1Pid,
								user_id = User1Id,
								user_monitor = monitor(process, User1Pid) },
	User2 = #rematch_lobby_user{ user_pid = User2Pid,
								user_id = User2Id,
								user_monitor = monitor(process, User2Pid) },

	Lobby = #rematch_lobby{ user_list = [ {User1Pid , User1}, {User2Pid , User2} ],
			tier = Tier,
			rematch_timeout = erlang:send_after(?REMATCH_TIMEOUT, self(), { rematch_timeout, User1Pid, User2Pid }) 
	},

	{noreply, Lobby};


handle_cast( Msg, State) ->
	lager:error("rematch_queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.









%%
%	called when the user connection stops
%%

handle_info({'DOWN', _Reference, process, Pid, _Reason}, State = #rematch_lobby{} ) ->
	lager:debug("rematch_queue_serv: user (~p) connection went down", [Pid]),
	handle_remove_user( Pid , State );


handle_info({ rematch_timeout, User1Pid, User2Pid }, rematched ) ->
	{stop, rematched};


handle_info({ rematch_timeout, User1Pid, User2Pid }, Lobby = #rematch_lobby{} ) ->
	lager:debug("rematch_queue_serv: rematch_timeout for (~p,~p) reached", [User2Pid, User1Pid]),

	gen_server:cast( User2Pid, remove_from_rematch_queue ),
	gen_server:cast( User1Pid, remove_from_rematch_queue ),

	Msg = message_processor:create_rematch_timeout_message(),
	gen_server:cast( User2Pid,{send_message, Msg}),
	gen_server:cast( User1Pid,{send_message, Msg}),

	Fun = fun( { _CurrentUserPid, CurrentUser } ) ->
		demonitor( CurrentUser#rematch_lobby_user.user_monitor)
	end,
	lists:foreach( Fun, Lobby#rematch_lobby.user_list ),
	timer:sleep( ?REMATCH_TIMETODIE ),
	{stop, Lobby};




handle_info(Msg,State) ->
	lager:error("rematch_queue_serv: unhandled info ~p", [Msg]),
	{noreply, State}.

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("rematch_queue_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


handle_remove_user(UserPid, Lobby = #rematch_lobby{}) ->
	ListWithoutUser = proplists:delete(UserPid, Lobby#rematch_lobby.user_list),

	erlang:cancel_timer(Lobby#rematch_lobby.rematch_timeout),

	Msg = message_processor:create_no_rematch_message(),
	lists:foreach( fun({ _ , CurrentUser }) ->  gen_server:cast( CurrentUser#rematch_lobby_user.user_pid, {send_message, Msg }) end, ListWithoutUser ),

	Fun = fun({ CurrentUserPid, CurrentUser }) ->
		gen_server:cast( CurrentUserPid, remove_from_rematch_queue ),
		demonitor( CurrentUser#rematch_lobby_user.user_monitor)
	end,
	lists:foreach( Fun, Lobby#rematch_lobby.user_list ),
	{noreply, Lobby}.

