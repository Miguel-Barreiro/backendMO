-module(user_logic).

-include("include/softstate.hrl").

-export([ init/1, login/2, logout/1, handle_msg/2 ]).

-export([can_enter_game/2,handle_game_start/2,handle_game_lost/2,handle_game_win/2]).


-define(LIFE_GENERATION_TIMEOUT, 120).
-define(MAX_LIFES, 6).
-define(LIFE_GAME_COST, 1).
-define(COIN_LIFE_RATIO, 1).


-define(LIFES_KEY, <<"lifes">>).
-define(COINS_KEY, <<"coins">>).
-define(LAST_LOGIN_KEY, <<"time_since_last_login">>).



-spec init( User_id::#mc_user{} ) -> #logic_user{}.
init( User = #mc_user{} ) ->
	
	{ New_user, _ } = add_lifes_to_user( ?MAX_LIFES, User, undefined),
	New_user.



-spec login( User::#mc_user{}, integer() ) -> #logic_user{}.
login( User = #mc_user{}, Session_start_time ) ->
	
	{ New_user, New_timer_ref } = generate_missing_lifes( User, undefined, Session_start_time ),

	#logic_user{ user = New_user, 
					lifes_generate_timer = New_timer_ref,
					session_start_time = Session_start_time }.




-spec logout( Logic_user :: #logic_user{ } ) -> ok.
logout( #logic_user{ user = User, lifes_generate_timer = Timer_ref } ) ->
	
	Time_left = case Timer_ref of
		undefined ->		?LIFE_GENERATION_TIMEOUT;
		_other ->			erlang:cancel_timer( Timer_ref ) div 1000
	end,
	%we pretend to logout before we actually do so the lifes generation takes into account the time we already waited
	Logout_timestamp = swiss:unix_timestamp() - ( ?LIFE_GENERATION_TIMEOUT - Time_left ),
	user_store:save_profile_fields( User#mc_user.user_id , [{ ?LAST_LOGIN_KEY, Logout_timestamp, put}] ),
	ok.




-spec can_enter_game( Logic_user :: #logic_user{ }, Powers :: [string()] ) -> false | true.
can_enter_game( #logic_user{ user = User }, Powers ) ->
	true.
	%Current_lifes = proplists:get_value(?LIFES_KEY, User#mc_user.wallet),
	% FIXME: no effect is temporary for development convenience.
	%Current_lifes >= ?LIFE_GAME_COST.




-spec handle_game_start( Logic_user :: #logic_user{ }, Powers :: [string()] ) -> { ok ,#logic_user{}} | {error, not_enough_lifes }.
handle_game_start( Logic_user = #logic_user{ lifes_generate_timer = _Timer_ref, user = _User }, _Powers ) ->
	{ ok , Logic_user}.



-spec handle_game_lost( Logic_user :: #logic_user{ }, Powers :: [string()] ) -> { ok ,#logic_user{}} | {error, not_enough_lifes }.
handle_game_lost( Logic_user = #logic_user{ lifes_generate_timer = Timer_ref, user = User }, Powers ) ->
	remove_lifes_from_user( ?LIFE_GAME_COST, User, Timer_ref ),
	{ ok , Logic_user}.


-spec handle_game_win( Logic_user :: #logic_user{ }, Powers :: [string()] ) -> { ok ,#logic_user{}} | {error, not_enough_lifes }.
handle_game_win( Logic_user = #logic_user{ lifes_generate_timer = Timer_ref, user = User }, Powers ) ->
	remove_lifes_from_user( ?LIFE_GAME_COST, User, Timer_ref ),
	{ ok , Logic_user}.



-spec handle_msg( generate_life , Logic_user :: #logic_user{ } ) -> { ok ,#logic_user{}}.
handle_msg( generate_life, Logic_user = #logic_user{ lifes_generate_timer = Timer_ref, user = User } ) ->
	{ New_user, New_timer_ref} = add_lifes_to_user( 1, User, Timer_ref),
	{ok, Logic_user#logic_user{ lifes_generate_timer = New_timer_ref, user = New_user }}.














add_lifes_to_user( Amount, User = #mc_user{} , Timer_ref) ->
	Current_lifes = case proplists:get_value(?LIFES_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,

	lager:debug("adding lifes: Current_lifes ~p  Amount ~p  MAX_LIFES ~p",[Current_lifes,Amount,?MAX_LIFES]),
	{New_wallet, New_timer_ref } = 
	case ((Current_lifes + Amount) >= ?MAX_LIFES) of
		true when Timer_ref == undefined->
			{ok, New_amount} = user_store:update_wallet_balance(User#mc_user.user_id, ?LIFES_KEY, ?MAX_LIFES - Current_lifes),

			{[ { ?LIFES_KEY , New_amount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ], undefined };	
		true when Timer_ref =/= undefined->
			{ok, New_amount} = user_store:update_wallet_balance(User#mc_user.user_id, ?LIFES_KEY, ?MAX_LIFES - Current_lifes),
			erlang:cancel_timer( Timer_ref ),
			{[ { ?LIFES_KEY , New_amount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ], undefined };
		false ->
			{ok, New_amount} = user_store:update_wallet_balance(User#mc_user.user_id, ?LIFES_KEY, Amount),
			{[ { ?LIFES_KEY , New_amount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ], Timer_ref}
	end,
	{User#mc_user{ wallet = New_wallet } , New_timer_ref}.




remove_lifes_from_user( Amount, User = #mc_user{}, Timer_ref ) ->

	New_timer_ref = case Timer_ref of
		undefined ->	erlang:send_after(?LIFE_GENERATION_TIMEOUT * 1000, self(), { user_logic_msg, generate_life});
		_other ->		Timer_ref
	end,

	Current_lifes = case proplists:get_value(?LIFES_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,
	lager:debug("removing lifes: Current_lifes ~p  Amount ~p  MAX_LIFES ~p",[Current_lifes,Amount,?MAX_LIFES]),

	case user_store:update_wallet_balance( User#mc_user.user_id, ?LIFES_KEY, -Amount) of
		{ok, New_amount} ->
			New_properties = [ { ?LIFES_KEY , New_amount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ],
			{ User#mc_user{ properties = New_properties }, New_timer_ref };
		{error, {insufficient, _ }} ->
			{ error , not_enough_lifes}
	end.
	


generate_missing_lifes( User = #mc_user{}, Timer_ref, Session_start_time ) ->

	case proplists:get_value(?LAST_LOGIN_KEY, User#mc_user.properties) of
		
		undefined ->
			{User, Timer_ref};

		Time_since_last_login ->

			lager:debug("generate_missing_lifes: Session_start_time ~p  , Time_since_last_login ~p",
							[Session_start_time,Time_since_last_login]),

			Delta_time = (Session_start_time - Time_since_last_login),
			case Delta_time div ?LIFE_GENERATION_TIMEOUT of
				0 ->					do_nothing,{User, Timer_ref};
				Lifes_to_generate when Lifes_to_generate < 0 ->
										do_nothing,{User, Timer_ref};
				Lifes_to_generate ->	add_lifes_to_user( Lifes_to_generate, User, Timer_ref )
			end
	end.







add_coins_to_user( Amount, User = #mc_user{} ) when Amount > 0 ->
	Current_coins = case proplists:get_value(?COINS_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,

	lager:debug("adding coins: Current_coins ~p  Amount ~p",[Current_coins,Amount]),
	{ok, NewBalance} = user_store:update_wallet_balance( User#mc_user.user_id, ?COINS_KEY, Amount ),
	NewWallet = [ {?COINS_KEY, NewBalance} | proplists:delete( ?COINS_KEY, User#mc_user.wallet ) ],
	User#mc_user{ wallet=NewWallet }.


remove_coins_from_user( Amount, User = #mc_user{} ) when Amount > 0 ->
	Current_coins = case proplists:get_value(?COINS_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,

	lager:debug("removing coins: Current_coins ~p  Amount ~p",[Current_coins,Amount]),
	case Amount > Current_coins of
		true ->
			{error, not_enough_coins};
		false ->
			{ok, NewBalance} = user_store:update_wallet_balance( User#mc_user.user_id, ?COINS_KEY, -Amount ),
			NewWallet = [ {?COINS_KEY, NewBalance} | proplists:delete( ?COINS_KEY, User#mc_user.wallet ) ],
			User#mc_user{ wallet=NewWallet }
	end.
			






max_coin_life_conversion( User ) ->
	max_coin_life_conversion( User, ?COIN_LIFE_RATIO ).

max_coin_life_conversion( User = #mc_user{}, CoinLifeRatio ) when is_integer(CoinLifeRatio) andalso CoinLifeRatio>0 
->
	CurrentCoins = case proplists:get_value(?COINS_KEY, User#mc_user.wallet) of
		undefined ->	0;
		OtherCVal ->	OtherCVal
	end,
	CurrentLives = case proplists:get_value(?LIFES_KEY, User#mc_user.wallet) of
		undefined ->	0;
		OtherLVal -> 	OtherLVal
	end,

	C_MaxSpendableCoins = CurrentCoins,
	C_MaxWinnableLives = C_MaxSpendableCoins * ?COIN_LIFE_RATIO,
	L_MaxWinnableLives = ?MAX_LIFES - CurrentLives,
	L_MaxSpendableCoins = trunc(L_MaxWinnableLives / ?COIN_LIFE_RATIO),

	case C_MaxSpendableCoins > L_MaxSpendableCoins of
		true ->
			{L_MaxSpendableCoins, L_MaxWinnableLives};
		false ->
			{C_MaxSpendableCoins, C_MaxWinnableLives}
	end.

	
	











