-module(rematch_queue_serv).

-behaviour(gen_server).

-define(REMATCH_TIMEOUT , 30000).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).
-export([ enter/4, set_user_powers/2, set_user_rematch/1, remove_user/1]).


-type rematch_state() :: ask_rematch | pick_powers.

-record( rematch_loby_user, {
	user_pid = undefined :: pid(),
	user_id = undefined,
	user_monitor = undefined,
	is_ready = false,
	powers = undefined
}).

-record( rematch_loby, {
	user_list = [] :: [{ binary(), #rematch_loby_user{} } ],
	state = ask_rematch :: rematch_state(),
	rematch_timeout,
	lobby_key
}).

-record(rematch_queue_state, {
	rematch_lobies = gb_sets:new():: gb_sets:gb_sets(),
	loby_key_by_user_id = gb_sets:new():: gb_sets:gb_sets()
}).




enter(User1_pid,User1_id,User2_pid,User2_id) ->
	gen_server:cast(whereis(?MODULE), { enter_user_pair, User1_pid, User1_id, User2_pid, User2_id } ),
	ok.


remove_user(User_pid) ->
	gen_server:cast(whereis(?MODULE), { remove_user, User_pid } ),
	ok.


set_user_rematch(User_pid) ->
	gen_server:cast(whereis(?MODULE), { set_user_rematch, User_pid } ),
	ok.

set_user_powers(User_pid, Powers) ->
	gen_server:cast(whereis(?MODULE), { set_user_powers, User_pid, Powers } ),
	ok.






start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
	{ok, #rematch_queue_state{ } }.




handle_cast( { set_user_powers, User_pid, Powers }, 
					State = #rematch_queue_state{ rematch_lobies = Lobies, loby_key_by_user_id = Keys_by_user} ) ->

	Lobby_key = gb_trees:get( User_pid, Keys_by_user),
	Lobby = gb_trees:get( Lobby_key, Lobies),
	User = proplists:get_value(User_pid, Lobby#rematch_loby.user_list),

	New_user_list = [ { User_pid, User#rematch_loby_user{ is_ready = true } }  |  proplists:delete(User_pid, Lobby#rematch_loby.user_list) ],
	New_lobby = Lobby#rematch_loby{ user_list = New_user_list },

	
	{noreply, State#rematch_queue_state{ rematch_lobies = gb_trees:insert( Lobby_key, New_lobby ,gb_trees:delete(Lobby_key, Lobies)) } };






handle_cast( { set_user_rematch, User_pid }, 
					State = #rematch_queue_state{ rematch_lobies = Lobies, loby_key_by_user_id = Keys_by_user} ) ->
	Lobby_key = gb_trees:get( User_pid, Keys_by_user),
	Lobby = gb_trees:get( Lobby_key, Lobies),
	User = proplists:get_value(User_pid, Lobby#rematch_loby.user_list),

	List_without_user = proplists:delete(User_pid, Lobby#rematch_loby.user_list),

	Msg = message_processor:create_rematch_message(),
	lists:foreach( fun({ _ , User }) ->  gen_server:cast( User#rematch_loby_user.user_pid, {send_message, Msg }) end, List_without_user ),

	New_user_list = [ { User_pid, User#rematch_loby_user{ is_ready = true } }  |   List_without_user],
	New_lobby = case lists:all(fun( { _ , User}) -> User#rematch_loby_user.is_ready end, New_user_list) of
		true ->
			Lobby#rematch_loby{ user_list = New_user_list, state = pick_powers };
		false ->
			Lobby#rematch_loby{ user_list = New_user_list }
	end,
	{noreply, State#rematch_queue_state{ rematch_lobies = gb_trees:insert( Lobby_key, New_lobby ,gb_trees:delete(Lobby_key, Lobies)) } };




handle_cast( { enter_user_pair, User1_pid, User1_id, User2_pid, User2_id }, 
					State = #rematch_queue_state{ rematch_lobies = Lobies, loby_key_by_user_id = Keys_by_user} ) ->
	
	Lobby_key = {User2_pid, User1_pid},

	User1 = #rematch_loby_user{ user_pid = User1_pid,
								user_id = User1_id,
								user_monitor = monitor(process, User1_pid) },
	User2 = #rematch_loby_user{ user_pid = User2_pid,
								user_id = User2_id,
								user_monitor = monitor(process, User2_pid) },

	Loby = #rematch_loby{ user_list = [ {User1_pid , User1}, {User2_pid , User2} ],
							rematch_timeout = erlang:send_after(?REMATCH_TIMEOUT, self(), { rematch_timeout, User1_pid, User2_pid }) },

	{noreply, State#rematch_queue_state{ rematch_lobies = gb_trees:insert( Lobby_key, Loby, Lobies),
											loby_key_by_user_id = gb_sets:insert( User1_pid, Lobby_key, 
																	gb_sets:insert( User2_pid, Lobby_key, Keys_by_user))} };


handle_cast( Msg, State) ->
	lager:error("rematch_queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.









%%
%	called when the user connection stops
%%

handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #rematch_queue_state{} ) ->
	lager:debug("rematch_queue_serv: user (~p) connection went down", [Pid]),
	{noreply, State };


handle_info({ rematch_timeout, User1_pid, User2_pid }, State = #rematch_queue_state{} ) ->
	lager:debug("rematch_queue_serv: rematch_timeout for (~p,~p) reached", [User1_pid, User2_pid]),
	{noreply, State };


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











set_pick_powers_state( Loby = #rematch_loby{} ) ->
	erlang:cancel_timer( Loby#rematch_loby.rematch_timeout ),
	Loby#rematch_loby{ rematch_timeout = undefined, state = pick_powers }.

