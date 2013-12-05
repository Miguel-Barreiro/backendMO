-module(rematch_queue_serv).

-behaviour(gen_server).

-define(REMATCH_TIMEOUT , 30000).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).
-export([ enter/5, set_user_powers/2, set_user_rematch/1, remove_user/1]).


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

-record(rematch_queue_state, {
	rematch_lobbies = gb_trees:empty() :: gb_tree(),
	lobby_key_by_user_id = gb_trees:empty() :: gb_tree()
}).




enter(User1Pid,User1Id,User2Pid,User2Id,Tier) ->

	gen_server:cast( User1Pid, set_rematch_queue_state ),
	gen_server:cast( User2Pid, set_rematch_queue_state ),

	gen_server:cast(whereis(?MODULE), { enter_user_pair, User1Pid, User1Id, User2Pid, User2Id, Tier } ),
	ok.


remove_user(UserPid) ->
	gen_server:cast(whereis(?MODULE), { remove_user, UserPid } ),
	ok.


set_user_rematch(UserPid) ->
	gen_server:cast(whereis(?MODULE), { set_user_rematch, UserPid } ),
	ok.

set_user_powers(UserPid, Powers) ->
	gen_server:cast(whereis(?MODULE), { set_user_powers, UserPid, Powers } ),
	ok.






start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
	{ok, #rematch_queue_state{ } }.




handle_cast( { remove_user, UserPid }, State ) ->
	lager:debug("no rematch received by ~p",[UserPid]),
	handle_remove_user( UserPid , State);





handle_cast( { set_user_powers, UserPid, Powers },
					State = #rematch_queue_state{ rematch_lobbies = Lobbies, lobby_key_by_user_id = KeysByUser} ) ->

	LobbyKey = gb_trees:get( UserPid, KeysByUser),
	Lobby = gb_trees:get( LobbyKey, Lobbies),
	User = proplists:get_value(UserPid, Lobby#rematch_lobby.user_list),
	NewUserList = [ { UserPid, User#rematch_lobby_user{ powers = Powers } }  |  proplists:delete(UserPid, Lobby#rematch_lobby.user_list) ],

	case lists:all(fun( { _ , CurrentUser}) -> CurrentUser#rematch_lobby_user.powers =/= undefined end, NewUserList) of
		true ->
			[ { User1Pid, User1 } , {User2Pid , User2}] = NewUserList,
			game_sup:start_new_game_process( [ Lobby#rematch_lobby.tier, User1Pid, User1#rematch_lobby_user.user_id,
													User2Pid, User2#rematch_lobby_user.user_id,
														configurations_serv:get_current_version(), configurations_serv:get_current_version()] ),
			NewUserTree = gb_trees:delete( User2Pid, gb_trees:delete( User1Pid, State#rematch_queue_state.lobby_key_by_user_id)),
			{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:delete(LobbyKey, Lobbies) , lobby_key_by_user_id = NewUserTree } };
		false ->
			NewLobby = Lobby#rematch_lobby{ user_list = NewUserList, state = pick_powers },
			{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:insert( LobbyKey, NewLobby, gb_trees:delete(LobbyKey, Lobbies))}}
	end;










handle_cast( { set_user_rematch, UserPid },  State = #rematch_queue_state{ rematch_lobbies = Lobbies, lobby_key_by_user_id = KeysByUser} ) ->

	LobbyKey = gb_trees:get( UserPid, KeysByUser),
	Lobby = gb_trees:get( LobbyKey, Lobbies),
	User = proplists:get_value(UserPid, Lobby#rematch_lobby.user_list),

	ListWithoutUser = proplists:delete(UserPid, Lobby#rematch_lobby.user_list),
	NewUserList = [ { UserPid, User#rematch_lobby_user{ is_ready = true } }  |   ListWithoutUser],

	case lists:all(fun( { _ , CurrentUser}) -> CurrentUser#rematch_lobby_user.is_ready end, NewUserList) of
		true ->
			lager:debug("--PICK POWERS MODE--",[]),

			erlang:cancel_timer( Lobby#rematch_lobby.rematch_timeout),

			%NewLobby = Lobby#rematch_lobby{ user_list = NewUserList, state = pick_powers },
			%{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:insert( LobbyKey, NewLobby ,gb_trees:delete(LobbyKey, Lobbies)) } };
			[ { User1Pid, User1 } , {User2Pid , User2}] = NewUserList,
			game_sup:start_new_game_process( [ Lobby#rematch_lobby.tier, User1Pid, User1#rematch_lobby_user.user_id, [],
													User2Pid, User2#rematch_lobby_user.user_id, [],
														configurations_serv:get_current_version(), 
														configurations_serv:get_current_url()] ),

			Fun = fun( { CurrentUserPid, CurrentUser } , NewTree ) ->
				demonitor( CurrentUser#rematch_lobby_user.user_monitor),
				gb_trees:delete( CurrentUserPid , NewTree)
			end,
			NewUserTree = lists:foldl( Fun, State#rematch_queue_state.lobby_key_by_user_id , Lobby#rematch_lobby.user_list),
			{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:delete(LobbyKey, Lobbies) , lobby_key_by_user_id = NewUserTree } };

		false ->
			Msg = message_processor:create_rematch_message(),
			lists:foreach( fun({ _ , CurrentUser }) ->  
								gen_server:cast( CurrentUser#rematch_lobby_user.user_pid, {send_message, Msg }) 
							end, ListWithoutUser ),

			NewLobby = Lobby#rematch_lobby{ user_list = NewUserList },
			{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:insert( LobbyKey, NewLobby ,gb_trees:delete(LobbyKey, Lobbies)) } }
	end;




handle_cast( { enter_user_pair, User1Pid, User1Id, User2Pid, User2Id, Tier }, 
					State = #rematch_queue_state{ rematch_lobbies = Lobbies, lobby_key_by_user_id = KeysByUser} ) ->
	
	lager:info("rematch queue enter ~p,~p",[User2Pid,User1Pid]),

	LobbyKey = {User2Pid, User1Pid},

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

	{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:insert( LobbyKey, Lobby, Lobbies),
											lobby_key_by_user_id = gb_trees:insert( User1Pid, LobbyKey, 
																	gb_trees:insert( User2Pid, LobbyKey, KeysByUser))} };


handle_cast( Msg, State) ->
	lager:error("rematch_queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.









%%
%	called when the user connection stops
%%

handle_info({'DOWN', _Reference, process, Pid, _Reason}, State = #rematch_queue_state{} ) ->
	lager:debug("rematch_queue_serv: user (~p) connection went down", [Pid]),
	handle_remove_user( Pid , State);


handle_info({ rematch_timeout, User1Pid, User2Pid }, State = #rematch_queue_state{ rematch_lobbies = Lobbies } ) ->
	lager:debug("rematch_queue_serv: rematch_timeout for (~p,~p) reached", [User2Pid, User1Pid]),

	gen_server:cast( User2Pid, remove_from_rematch_queue ),
	gen_server:cast( User1Pid, remove_from_rematch_queue ),

	Msg = message_processor:create_rematch_timeout_message(),
	gen_server:cast( User2Pid,{send_message, Msg}),
	gen_server:cast( User1Pid,{send_message, Msg}),

	LobbyKey = {User2Pid, User1Pid},
	Lobby = gb_trees:get( LobbyKey, Lobbies),

	Fun = fun( { CurrentUserPid, CurrentUser } , NewTree ) ->
		demonitor( CurrentUser#rematch_lobby_user.user_monitor),
		gb_trees:delete( CurrentUserPid , NewTree)
	end,
	NewUserTree = lists:foldl( Fun, State#rematch_queue_state.lobby_key_by_user_id , Lobby#rematch_lobby.user_list),
	{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:delete(LobbyKey, Lobbies) , lobby_key_by_user_id = NewUserTree } };




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


handle_remove_user( UserPid , State = #rematch_queue_state{ rematch_lobbies = Lobbies, lobby_key_by_user_id = KeysByUser}) ->

	case (catch gb_trees:get( UserPid, KeysByUser)) of
		
		{'EXIT', _ } ->
			{noreply, State};

		LobbyKey ->
			Lobby = gb_trees:get( LobbyKey, Lobbies),
			ListWithoutUser = proplists:delete(UserPid, Lobby#rematch_lobby.user_list),

			erlang:cancel_timer( Lobby#rematch_lobby.rematch_timeout),

			Msg = message_processor:create_no_rematch_message(),
			lists:foreach( fun({ _ , CurrentUser }) ->  gen_server:cast( CurrentUser#rematch_lobby_user.user_pid, {send_message, Msg }) end, ListWithoutUser ),

			Fun = fun( { CurrentUserPid, CurrentUser } , NewTree ) ->
				gen_server:cast( CurrentUserPid, remove_from_rematch_queue ),
				demonitor( CurrentUser#rematch_lobby_user.user_monitor),
				gb_trees:delete( CurrentUserPid , NewTree)
			end,
			NewUserTree = lists:foldl( Fun, State#rematch_queue_state.lobby_key_by_user_id , Lobby#rematch_lobby.user_list),
			{noreply, State#rematch_queue_state{ rematch_lobbies = gb_trees:delete(LobbyKey, Lobbies) , lobby_key_by_user_id = NewUserTree } }
	end.

