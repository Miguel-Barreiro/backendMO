-module(match_make).

-export([match_users/3]).





match_users( UserList , Match_Rate_Function , GameSize ) ->
	Matrix = calculate_desirable_matrix( UserList, GameSize, Match_Rate_Function ),

	%lager:info("MATRIX is ~p",[Matrix]),

	Matches = match_users_from_matrix( Matrix ),

	Fun = fun( MatchedUserList, Accumulator ) ->
		Pred = fun( User ) ->
			not lists:member(User, MatchedUserList )
		end,
		lists:filter(Pred, Accumulator)
	end,

	NonMatchedUserList = lists:foldl(Fun, UserList, Matches),

	{ Matches , NonMatchedUserList }.







match_users_from_matrix( UserMatrix ) ->
	SortedByValue = lists:reverse(lists:keysort( 2, UserMatrix)),
	match_users_from_sorted_matrix(SortedByValue).

match_users_from_sorted_matrix( [] ) ->	
	[];

match_users_from_sorted_matrix( [ BestValue | RestSortedMatrix] ) ->
	{ UsersToMatch , Value} = BestValue,
	UsersMatched = match_users_from_sorted_matrix( remove_match_from_matrix( UsersToMatch, RestSortedMatrix ) ),
	[ UsersToMatch | UsersMatched].










remove_match_from_matrix( UsersMatched, SortedMatrix ) ->
	Fun = fun( { UserList , _value } ) ->
		FunHasUser = fun( User ) -> lists:member( User, UsersMatched) end,
		not lists:any( FunHasUser, UserList)
	end,
	lists:filter(Fun, SortedMatrix).










calculate_desirable_matrix( UserList, GameSize, Match_Rate_Function ) ->
	calculate_desirable_matrix_for_users(  [], 0, UserList, GameSize, Match_Rate_Function ).







calculate_desirable_matrix_for_users( _UserList, _UserListSize , [], _GameSize, _Match_Rate_Function ) ->
	[];

calculate_desirable_matrix_for_users( UserList, UserListSize , AllUsersList, GameSize, Match_Rate_Function ) 
					when UserListSize == ( GameSize - 1)->

	Fun = fun( User , Accumulator ) ->
		MatchedUsers = [ User | UserList],
		[ { MatchedUsers , Match_Rate_Function( MatchedUsers ) } | Accumulator ]
	end,
	lists:foldl( Fun, [], AllUsersList);

calculate_desirable_matrix_for_users( UserList, UserListSize , [ FirstAllUsersList | RestAllUsersList ], GameSize, Match_Rate_Function ) ->

	Matrix = calculate_desirable_matrix_for_users( [ FirstAllUsersList | UserList], UserListSize + 1, RestAllUsersList, GameSize, Match_Rate_Function ),
	Matrix2 = calculate_desirable_matrix_for_users( UserList, UserListSize , RestAllUsersList, GameSize, Match_Rate_Function ),
	lists:append( Matrix, Matrix2 ).
