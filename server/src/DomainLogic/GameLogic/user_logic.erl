-module(user_logic).

-include("include/softstate.hrl").

-export([ login/2, logout/1, handle_msg/2, can_enter_game/1, handle_game_start/1 ]).


-define(LIFE_GENERATION_TIMEOUT,10000).
-define(MAX_LIFES,6).
-define(LIFE_GAME_COST,1).


-define(LIFES_KEY,<<"lifes">>).
-define(LAST_LOGIN_KEY,<<"time_since_last_login">>).



-spec login( User_id::binary(), integer() ) -> #logic_user{}.
login( User = #mc_user{}, Session_start_time ) ->
	
	{ New_user, New_timer_ref } = generate_missing_lifes( User, undefined, Session_start_time ),

	#logic_user{ user = New_user, 
					lifes_generate_timer = New_timer_ref,
					session_start_time = Session_start_time }.




-spec logout( Logic_user :: #logic_user{ } ) -> ok.
logout( #logic_user{ user = User, lifes_generate_timer = Timer_ref } ) ->
	
	Time_left = case Timer_ref of
		undefined ->		?LIFE_GENERATION_TIMEOUT;
		_other ->			erlang:cancel_timer( Timer_ref )
	end,
	%we pretend to logout before we actually do so the lifes generation takes into account the time we already waited
	Logout_timestamp = swiss:unix_timestamp() - ( ?LIFE_GENERATION_TIMEOUT - Time_left),
	user_store:save_profile_fields( User#mc_user.user_id , [{ ?LAST_LOGIN_KEY, Logout_timestamp, put}] ),
	ok.




-spec can_enter_game( Logic_user :: #logic_user{ } ) -> false | true.
can_enter_game( #logic_user{ user = User } ) ->
	Current_lifes = proplists:get_value(?LIFES_KEY, User#mc_user.properties),
	Current_lifes >= ?LIFE_GAME_COST.




-spec handle_game_start( Logic_user :: #logic_user{ } ) -> { ok ,#logic_user{}} | {error, not_enough_lifes }.
handle_game_start( Logic_user = #logic_user{ lifes_generate_timer = Timer_ref, user = User } ) ->
	case remove_lifes_from_user( 1, User, Timer_ref) of
		{ New_user ,New_timer_ref} ->
			{ok , Logic_user#logic_user{ lifes_generate_timer = New_timer_ref, user = New_user }};
		_other ->
			{error, not_enough_lifes}
	end.





-spec handle_msg( generate_life , Logic_user :: #logic_user{ } ) -> { ok ,#logic_user{}}.
handle_msg( generate_life, Logic_user = #logic_user{ lifes_generate_timer = Timer_ref, user = User } ) ->
	{ New_user ,New_timer_ref} = add_lifes_to_user( 1, User, Timer_ref),
	{ok, Logic_user#logic_user{ lifes_generate_timer = New_timer_ref, user = New_user }}.














add_lifes_to_user( Amount, User = #mc_user{} , Timer_ref) ->
	Current_lifes = case proplists:get_value(?LIFES_KEY, User#mc_user.properties) of
		undefined ->	6;
		Other -> 		Other
	end,

	lager:info("Current_lifes ~p  Amount ~p  MAX_LIFES ~p",[Current_lifes,Amount,?MAX_LIFES]),
	{New_properties, New_timer_ref } = 
	case ((Current_lifes + Amount) >= ?MAX_LIFES) of
		true when Timer_ref == undefined->
			user_store:save_profile_fields( User#mc_user.user_id, [{ ?LIFES_KEY, ?MAX_LIFES, put}] ),
			{[ { ?LIFES_KEY , ?MAX_LIFES} | proplists:delete( ?LIFES_KEY, User#mc_user.properties ) ], undefined };	
		true when Timer_ref =/= undefined->
			user_store:save_profile_fields( User#mc_user.user_id, [{ ?LIFES_KEY, ?MAX_LIFES, put}] ),
			erlang:cancel_timer( Timer_ref ),
			{[ { ?LIFES_KEY , ?MAX_LIFES} | proplists:delete( ?LIFES_KEY, User#mc_user.properties ) ], undefined };
		false ->
			user_store:save_profile_fields( User#mc_user.user_id, [{ ?LIFES_KEY, Amount, add}] ),
			{[ { ?LIFES_KEY , Current_lifes + Amount} | proplists:delete( ?LIFES_KEY, User#mc_user.properties ) ], Timer_ref}
	end,
	{User#mc_user{ properties = New_properties } , New_timer_ref}.






remove_lifes_from_user( Amount, User = #mc_user{}, Timer_ref ) ->

	New_timer_ref = case Timer_ref of
		undefined ->	erlang:send_after(?LIFE_GENERATION_TIMEOUT, self(), { user_logic_msg, generate_life});
		_other ->		Timer_ref
	end,

	Current_lifes = case proplists:get_value(?LIFES_KEY, User#mc_user.properties) of
		undefined ->	6;
		Other -> 		Other
	end,
	case Current_lifes >= Amount of
		true ->
			user_store:save_profile_fields( User#mc_user.user_id, [{ ?LIFES_KEY, -Amount, add}] ),
			New_properties = [ { ?LIFES_KEY , Current_lifes - Amount} | proplists:delete( ?LIFES_KEY, User#mc_user.properties ) ],
			{ User#mc_user{ properties = New_properties }, New_timer_ref };
		false ->
			{ error , not_enough_lifes}
	end.
	







generate_missing_lifes( User = #mc_user{}, Timer_ref, Session_start_time ) ->

	case proplists:get_value(?LAST_LOGIN_KEY, User#mc_user.properties) of
		
		undefined ->	
			{User, Timer_ref};

		Time_since_last_login ->
			Delta_time = Session_start_time - Time_since_last_login,
			Lifes_to_generate = Delta_time div ?LIFE_GENERATION_TIMEOUT,

			add_lifes_to_user( Lifes_to_generate, User, Timer_ref )
	end.


