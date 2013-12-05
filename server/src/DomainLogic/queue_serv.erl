-module(queue_serv).
-behaviour(gen_server).

-include("include/softstate.hrl").

-define(MATCH_MAKE_INTERVAL, 500).

-define(GAME_SIZE, 2).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).
-export([enter/5, leave/2, calculate_match_desire_ratio/1]).



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






enter(UserPid, UserId, Elo, League, Powers ) ->
	gen_server:cast(whereis(?MODULE), {add_user, UserPid, UserId, Elo, League, Powers}).

leave(UserPid, UserId) ->
	gen_server:cast(whereis(?MODULE), {remove_user, UserPid, UserId}).





remove_user_by_pid( UserPid, State = #queue_state{})->
	case proplists:get_value( UserPid ,State#queue_state.league_by_pid) of
		undefined ->
			State;
		LeagueName ->
			League = proplists:get_value(LeagueName, State#queue_state.league_list),
			Predicate = fun( User ) ->
				User#queue_user.user_pid == UserPid
			end,
			NewList = gb_sets:filter(Predicate, League#queue_league.user_list ),
			State#queue_state{ league_by_pid = proplists:delete(UserPid, State#queue_state.league_by_pid),
									league_list = [ { League , League#queue_league{ user_list = NewList}} | proplists:delete(League, State#queue_state.league_list)] }
	end.





handle_cast( { add_user , UserPid, UserId, Elo, LeagueName, Powers }, State = #queue_state{ } ) ->	

	League = proplists:get_value(LeagueName, State#queue_state.league_list),
	lager:debug("League ~p",[League]),
	UserMonitor = monitor(process, UserPid),
	QueueUser = #queue_user{ elo = Elo, user_pid = UserPid, user_id = UserId, user_monitor = UserMonitor, powers = Powers },
	NewUserList = gb_sets:add_element(QueueUser , League#queue_league.user_list ),
	NewLeagueList = [ { LeagueName , League#queue_league{ user_list = NewUserList}} | proplists:delete(LeagueName, State#queue_state.league_list)],
	NewState = State#queue_state{ league_by_pid = [ UserPid | State#queue_state.league_by_pid ],
									league_list = NewLeagueList },
	{noreply, NewState};







handle_cast( Msg, State) ->
	lager:error("queue_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.






handle_info( match_make , State = #queue_state{} ) ->

	Fun = fun( X, Sum ) ->

		{ LeagueName , League } = X,
		{ LeagueByPid , Leagues } = Sum,

		NewList = gb_sets:to_list(League#queue_league.user_list),
		{ Matches , UnmatchedUserList } = match_make:match_users( NewList , fun calculate_match_desire_ratio/1 , ?GAME_SIZE ),

		MatchUsers = fun( [ User1, User2 ], UsersCouldntMatch )->
			lager:info("new game with ~p and ~p",[User1#queue_user.user_id, User2#queue_user.user_id]),

			%case { gen_server:call(User1#queue_user.user_pid, {can_enter_game, User1#queue_user.powers} ), 
			%		gen_server:call(User2#queue_user.user_pid, {can_enter_game, User2#queue_user.powers} ) } of

			%	{true , true } ->
					demonitor( User1#queue_user.user_monitor ),
					demonitor( User2#queue_user.user_monitor ),

					game_sup:start_new_game_process( [LeagueName, User1#queue_user.user_pid, User1#queue_user.user_id, User1#queue_user.powers,
														User2#queue_user.user_pid, User2#queue_user.user_id, User2#queue_user.powers,
															configurations_serv:get_current_version(), 
															configurations_serv:get_current_url()] ),
					UsersCouldntMatch

			%	{ true , false} ->
			%		gen_server:cast( User1#queue_user.user_pid, remove_from_queue),
			%		[ User2 | UsersCouldntMatch];
				
			%	{ false , true} ->
			%		gen_server:cast( User2#queue_user.user_pid, remove_from_queue),

			%		[ User1 | UsersCouldntMatch];	

			%	_other ->
			%		gen_server:cast( User2#queue_user.user_pid, remove_from_queue),
			%		gen_server:cast( User1#queue_user.user_pid, remove_from_queue),

			%		UsersCouldntMatch
			%end
		end,
		
		{ UsersCouldntMatch } = lists:foldl(MatchUsers, { [] }, Matches),

		UsersStillInQueue = lists:append( UsersCouldntMatch , UnmatchedUserList),

		UnmatchedUserLeaguesByPid = [{ User#queue_user.user_pid , LeagueName} || User <- UsersStillInQueue ],
		NewLeague = #queue_league{ user_list = gb_sets:from_list(UsersStillInQueue), tier_level = LeagueName },

		NewLeagues = [{LeagueName , NewLeague} | Leagues],
		NewLeagueByPid = lists:append( UnmatchedUserLeaguesByPid , LeagueByPid ),

		{ NewLeagueByPid , NewLeagues }
	end,

	{ LeagueByPid , Leagues } = lists:foldl(Fun, {[],[]}, State#queue_state.league_list),

	erlang:send_after(?MATCH_MAKE_INTERVAL, self(), match_make),

	{noreply, State#queue_state{ league_list = Leagues, league_by_pid = LeagueByPid }};




%%
%	called when the user connection stops
%%

handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #queue_state{} ) ->
	lager:debug("queue_serv: user (~p) connection went down", [Pid]),

	demonitor( Reference ),
	NewState = remove_user_by_pid( Pid, State),

	{noreply, NewState };




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









calculate_match_desire_ratio( UserList ) ->
%	lists:sum(UserList).
	GetMax = fun( User , Accumulator ) ->
		case User#queue_user.elo > Accumulator of 
			true-> User#queue_user.elo;
			false-> Accumulator
		end
	end,

	GetMin = fun( User , Accumulator ) ->
		case User#queue_user.elo < Accumulator of 
			true-> User#queue_user.elo;
			false-> Accumulator
		end
	end,

	MaxElo = lists:foldl( GetMax, 0, UserList),
	MinElo = lists:foldl( GetMin, 1000, UserList),

	DifElo = MaxElo - MinElo,

	1000 - DifElo.













