-module(match_make).

-export([match_users/3]).





match_users( User_list , Match_Rate_Function , Game_size ) ->
	Matrix = calculate_desirable_matrix( User_list, Game_size, Match_Rate_Function ),

	%lager:info("MATRIX is ~p",[Matrix]),

	Matches = match_users_from_matrix( Matrix ),

	Fun = fun( Matched_user_list, Accumulator ) ->
		Pred = fun( User ) ->
			not lists:member(User, Matched_user_list )
		end,
		lists:filter(Pred, Accumulator)
	end,

	Non_matched_user_list = lists:foldl(Fun, User_list, Matches),

	{ Matches , Non_matched_user_list }.







match_users_from_matrix( User_matrix ) ->
	Sorted_by_value = lists:reverse(lists:keysort( 2, User_matrix)),
	match_users_from_sorted_matrix(Sorted_by_value).

match_users_from_sorted_matrix( [] ) ->	
	[];

match_users_from_sorted_matrix( [ Best_value | Rest_sorted_matrix] ) ->
	{ Users_to_match , Value} = Best_value,
	Users_matched = match_users_from_sorted_matrix( remove_match_from_matrix( Users_to_match, Rest_sorted_matrix ) ),
	[ Users_to_match | Users_matched].










remove_match_from_matrix( Users_matched, Sorted_matrix ) ->
	Fun = fun( { User_list , _value } ) ->
		Fun_has_user = fun( User ) -> lists:member( User, Users_matched) end,
		not lists:any( Fun_has_user, User_list)
	end,
	lists:filter(Fun, Sorted_matrix).










calculate_desirable_matrix( User_list, Game_size, Match_Rate_Function ) ->
	
	lager:info("user list ~p",[User_list]),

	calculate_desirable_matrix_for_users(  [], 0, User_list, Game_size, Match_Rate_Function ).







calculate_desirable_matrix_for_users( _User_list, _User_list_size , [], _Game_size, _Match_Rate_Function ) ->
	[];

calculate_desirable_matrix_for_users( User_list, User_list_size , All_users_list, Game_size, Match_Rate_Function ) 
					when User_list_size == ( Game_size - 1)->

	Fun = fun( User , Accumulator ) ->
		Matched_users = [ User | User_list],
		[ { Matched_users , Match_Rate_Function( Matched_users ) } | Accumulator ]
	end,
	lists:foldl( Fun, [], All_users_list);

calculate_desirable_matrix_for_users( User_list, User_list_size , [ First_all_users_list | Rest_all_users_list ], Game_size, Match_Rate_Function ) ->

	Matrix = calculate_desirable_matrix_for_users( [ First_all_users_list | User_list], User_list_size + 1, Rest_all_users_list, Game_size, Match_Rate_Function ),
	Matrix2 = calculate_desirable_matrix_for_users( User_list, User_list_size , Rest_all_users_list, Game_size, Match_Rate_Function ),
	lists:append( Matrix, Matrix2 ).
