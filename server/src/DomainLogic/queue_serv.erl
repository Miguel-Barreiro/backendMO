-module(queue_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).



-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).
-export([enter/4,leave/2, test/0, calculate_match_desire_ratio/1]).


-type league_name() :: beginner | skilled | expert | master.

-record( queue_user, { 
	user_pid = undefined :: pid(),
	user_id,
	elo = 0,
	user_monitor
}).

-record( queue_league, {
	tier_level :: league_name(),
	user_list = gb_sets:new():: gb_sets:gb_sets()
}).

-record(queue_state, {
	league_list = [],
	league_by_pid = []
}).






start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
	State = #queue_state{ league_list = [ { beginner, #queue_league{ tier_level = beginner }},
												{ skilled, #queue_league{ tier_level = skilled }},
													{ expert, #queue_league{ tier_level = expert }},
														{ master, #queue_league{ tier_level = master }} ] },
	{ok, State }.






enter(User_pid, User_id, Elo, League ) ->
	gen_server:cast(whereis(?MODULE), {add_user, User_pid, User_id, Elo, League}).

leave(User_pid, User_id) ->
	gen_server:cast(whereis(?MODULE), {remove_user, User_pid, User_id}).


test() ->
	gen_server:cast(whereis(?MODULE), match_make_test ).











remove_user_by_pid( User_pid, State = #queue_state{})->

	League_name = proplists:get_value( User_pid ,State#queue_state.league_by_pid),
	League = proplists:get_value(League_name, State#queue_state.league_list),
	Predicate = fun( User ) ->
		User#queue_user.user_pid == User_pid
	end,
	New_list = gb_sets:filter(Predicate, League#queue_league.user_list ),
	State#queue_state{ league_by_pid = proplists:delete(User_pid, State#queue_state.league_by_pid),
							league_list = [ { League , League#queue_league{ user_list = New_list}} | proplists:delete(League, State#queue_state.league_list)] }.








handle_cast( match_make_test , State = #queue_state{} ) ->
	Fun = fun( X, Sum ) ->

		{ League_name , League } = X,
		%{ League_by_pid , Leagues } = Sum,

		Fun = fun( User ) ->
			User#queue_user.user_id
		end,
		New_list = lists:map(Fun, gb_sets:to_list( League#queue_league.user_list )),  
		%New_list = League#queue_league.user_list,
		{ Matches , Non_matched_user_list }	= match_make:match_users( New_list , fun calculate_match_desire_ratio/1 , 4 ),
		
		lager:info("for ~p matched ~p and couldnt match ~p",[League_name, Matches , Non_matched_user_list ]),

		ok
	end,

	Matrix = lists:foldl(Fun, {[],[]}, State#queue_state.league_list),


	%{ League_by_pid , Leagues } = lists:foldl(Fun, {[],[]}, State#queue_state.league_list),

	{noreply, State};




handle_cast( match_make , State = #queue_state{} ) ->

	Fun = fun( X, Sum ) ->

		{ League_name , League } = X,
		{ League_by_pid , Leagues } = Sum,

		Sorted_users = lists:keysort( #queue_user.elo , gb_sets:to_list(League#queue_league.user_list)),
		Unmatched_user_list = match_users( Sorted_users ),

		Unmatched_user_leagues_by_pid = [{ User#queue_user.user_pid , League_name} || User <- Unmatched_user_list ],
		New_league = #queue_league{ user_list = gb_sets:from_list(Unmatched_user_list), tier_level = League_name },

		lager:info("new league for ~p is ~p",[League_name,New_league]),

		New_leagues = [{League , New_league} | Leagues],
		New_league_by_pid = lists:append( Unmatched_user_leagues_by_pid , League_by_pid ),

		New_accumulator = { New_league_by_pid , New_leagues },
		New_accumulator
	end,

	{ League_by_pid , Leagues } = lists:foldl(Fun, {[],[]}, State#queue_state.league_list),

	lager:info("new state is going to be, League_by_pid ~p",[ League_by_pid ]),

	{noreply, State};







handle_cast( { add_user , User_pid, User_id, Elo, League_name }, State = #queue_state{ } ) ->	

	League = proplists:get_value(League_name, State#queue_state.league_list),
	%User_monitor = monitor(process, User_pid),
	User_monitor = new_monitor,
	Queue_user = #queue_user{ elo = Elo, user_pid = User_pid, user_id = User_id, user_monitor = User_monitor },
	New_user_list = gb_sets:add_element(Queue_user , League#queue_league.user_list ),
	New_state = State#queue_state{ league_by_pid = [ User_pid | State#queue_state.league_by_pid ],
									league_list = [ { League_name , League#queue_league{ user_list = New_user_list}} | proplists:delete(League_name, State#queue_state.league_list)] },
	{noreply, New_state};







handle_cast( Msg, State) ->
	lager:error("queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.

%%
%	called when the user connection stops
%%

handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #queue_state{} ) ->
	lager:debug("queue_serv: user (~p) connection went down", [Pid]),

	demonitor( Reference ),
	New_state = remove_user_by_pid( Pid, State),

	{noreply, New_state };




handle_info(Msg,State) ->
	lager:error("queue_serv: unhandled info ~p", [Msg]),
	{noreply, State}.

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("queue_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.




match_users( [ User1 | [ User2 | Rest_users] ] ) -> 
	%game_sup:start_new_game_process( [User1#queue_user.user_pid, User1#queue_user.user_id, User2#queue_user.user_pid, User2#queue_user.user_id] ),
	lager:info("MATCH ( ~p vs ~p ) ", [User1#queue_user.user_id , User2#queue_user.user_id] ),
	match_users( Rest_users );

match_users( Last_user_list ) -> 
	Last_user_list.








calculate_match_desire_ratio( User_list ) ->
	lists:sum(User_list).
%	Fun = fun( User , Accumulator ) ->
%		[ User#queue_user.user_id | Accumulator]
%	end,
%	lists:foldl( Fun, [], User_list).













