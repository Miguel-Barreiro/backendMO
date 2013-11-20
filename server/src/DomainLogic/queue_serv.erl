-module(queue_serv).
-behaviour(gen_server).

-define(MATCH_MAKE_INTERVAL, 500).


-define(GAME_SIZE, 2).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).
-export([enter/5, leave/2, calculate_match_desire_ratio/1]).


-type league_name() :: beginner | skilled | expert | master.

-record( queue_user, { 
	user_pid = undefined :: pid(),
	user_id,
	elo = 0,
	user_monitor,
	powers
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


	erlang:send_after(?MATCH_MAKE_INTERVAL, self(), match_make),

	{ok, State }.






enter(User_pid, User_id, Elo, League, Powers ) ->
	gen_server:cast(whereis(?MODULE), {add_user, User_pid, User_id, Elo, League, Powers}).

leave(User_pid, User_id) ->
	gen_server:cast(whereis(?MODULE), {remove_user, User_pid, User_id}).





remove_user_by_pid( User_pid, State = #queue_state{})->
	case proplists:get_value( User_pid ,State#queue_state.league_by_pid) of
		undefined ->
			State;
		League_name ->
			League = proplists:get_value(League_name, State#queue_state.league_list),
			Predicate = fun( User ) ->
				User#queue_user.user_pid == User_pid
			end,
			New_list = gb_sets:filter(Predicate, League#queue_league.user_list ),
			State#queue_state{ league_by_pid = proplists:delete(User_pid, State#queue_state.league_by_pid),
									league_list = [ { League , League#queue_league{ user_list = New_list}} | proplists:delete(League, State#queue_state.league_list)] }
	end.





handle_cast( { add_user , User_pid, User_id, Elo, League_name, Powers }, State = #queue_state{ } ) ->	

	League = proplists:get_value(League_name, State#queue_state.league_list),
	lager:debug("League ~p",[League]),
	User_monitor = monitor(process, User_pid),
	Queue_user = #queue_user{ elo = Elo, user_pid = User_pid, user_id = User_id, user_monitor = User_monitor, powers = Powers },
	New_user_list = gb_sets:add_element(Queue_user , League#queue_league.user_list ),
	New_league_list = [ { League_name , League#queue_league{ user_list = New_user_list}} | proplists:delete(League_name, State#queue_state.league_list)],
	New_state = State#queue_state{ league_by_pid = [ User_pid | State#queue_state.league_by_pid ],
									league_list = New_league_list },
	{noreply, New_state};







handle_cast( Msg, State) ->
	lager:error("queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.






handle_info( match_make , State = #queue_state{} ) ->

	Fun = fun( X, Sum ) ->

		{ League_name , League } = X,
		{ League_by_pid , Leagues } = Sum,

		New_list = gb_sets:to_list(League#queue_league.user_list),
		{ Matches , Unmatched_user_list } = match_make:match_users( New_list , fun calculate_match_desire_ratio/1 , ?GAME_SIZE ),

		Match_users = fun( [ User1, User2 ], Users_couldnt_match )->
			lager:info("new game with ~p and ~p",[User1#queue_user.user_id, User2#queue_user.user_id]),

			%case { gen_server:call(User1#queue_user.user_pid, {can_enter_game, User1#queue_user.powers} ), 
			%		gen_server:call(User2#queue_user.user_pid, {can_enter_game, User2#queue_user.powers} ) } of

			%	{true , true } ->
					demonitor( User1#queue_user.user_monitor ),
					demonitor( User2#queue_user.user_monitor ),

					game_sup:start_new_game_process( [User1#queue_user.user_pid, User1#queue_user.user_id, User1#queue_user.powers,
														User2#queue_user.user_pid, User2#queue_user.user_id, User2#queue_user.powers,
															configurations_serv:get_current_version(), 
															configurations_serv:get_current_url()] ),
					Users_couldnt_match

			%	{ true , false} ->
			%		gen_server:cast( User1#queue_user.user_pid, remove_from_queue),
			%		[ User2 | Users_couldnt_match];
				
			%	{ false , true} ->
			%		gen_server:cast( User2#queue_user.user_pid, remove_from_queue),

			%		[ User1 | Users_couldnt_match];	

			%	_other ->
			%		gen_server:cast( User2#queue_user.user_pid, remove_from_queue),
			%		gen_server:cast( User1#queue_user.user_pid, remove_from_queue),

			%		Users_couldnt_match
			%end
		end,
		
		{ Users_couldnt_match } = lists:foldl(Match_users, { [] }, Matches),

		Users_still_in_queue = lists:append( Users_couldnt_match , Unmatched_user_list),

		Unmatched_user_leagues_by_pid = [{ User#queue_user.user_pid , League_name} || User <- Users_still_in_queue ],
		New_league = #queue_league{ user_list = gb_sets:from_list(Users_still_in_queue), tier_level = League_name },

		New_leagues = [{League_name , New_league} | Leagues],
		New_league_by_pid = lists:append( Unmatched_user_leagues_by_pid , League_by_pid ),

		{ New_league_by_pid , New_leagues }
	end,

	{ League_by_pid , Leagues } = lists:foldl(Fun, {[],[]}, State#queue_state.league_list),

	erlang:send_after(?MATCH_MAKE_INTERVAL, self(), match_make),

	{noreply, State#queue_state{ league_list = Leagues, league_by_pid = League_by_pid }};




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









calculate_match_desire_ratio( User_list ) ->
%	lists:sum(User_list).
	Get_max = fun( User , Accumulator ) ->
		case User#queue_user.elo > Accumulator of 
			true-> User#queue_user.elo;
			false-> Accumulator
		end
	end,

	Get_min = fun( User , Accumulator ) ->
		case User#queue_user.elo < Accumulator of 
			true-> User#queue_user.elo;
			false-> Accumulator
		end
	end,

	Max_elo = lists:foldl( Get_max, 0, User_list),
	Min_elo = lists:foldl( Get_min, 1000, User_list),

	Dif_elo = Max_elo - Min_elo,

	1000 - Dif_elo.













