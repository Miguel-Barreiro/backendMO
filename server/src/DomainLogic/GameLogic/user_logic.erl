-module(user_logic).

-include("include/softstate.hrl").

-export([ init/1, login/2, logout/1, handle_msg/2 ]).

-export([can_enter_game/2,handle_game_start/2,handle_game_lost/2,handle_game_win/3]).
-export([xp2level/1]).


-define(LIFE_GENERATION_TIMEOUT, 120).
-define(MAX_LIFES, 6).
-define(LIFE_GAME_COST, 1).
-define(COIN_LIFE_RATIO, 1).

-define(LIFES_KEY, <<"lifes">>).
-define(LAST_LOGIN_KEY, <<"time_since_last_login">>).

-define(COINS_KEY, <<"coins">>).
-define(TIER_2_COINREWARD_DICT, [
		{1,20},
		{2,40},
		{3,75}
]).

-define(XP_KEY, <<"xp">>).
-define(TIER_2_XPREWARD_DICT, [
		{1,100},
		{2,200},
		{3,350}
]).
-define(LIFE_2_XP_DICT, [
		{0,0},
		{100,1},
		{200,2},
		{350,3},
		{450,4},
		{600,5},
		{750,6},
		{900,7},
		{1050,8},
		{1200,9},
		{1400,10},
		{1600,11},
		{1800,12},
		{2000,13}		
]).



-spec init( UserId::#mc_user{} ) -> #logic_user{}.
init( User = #mc_user{} ) ->
	
	{ NewUser, _ } = add_lifes_to_user( ?MAX_LIFES, User, undefined),
	NewUser.



-spec login( User::#mc_user{}, integer() ) -> #logic_user{}.
login( User = #mc_user{}, SessionStartTime ) ->
	
	{ NewUser, NewTimerRef } = generate_missing_lifes( User, undefined, SessionStartTime ),

	#logic_user{ user = NewUser, 
					lifes_generate_timer = NewTimerRef,
					session_start_time = SessionStartTime }.




-spec logout( LogicUser :: #logic_user{ } ) -> ok.
logout( #logic_user{ user = User, lifes_generate_timer = TimerRef } ) ->
	
	TimeLeft = case TimerRef of
		undefined ->		?LIFE_GENERATION_TIMEOUT;
		_other ->			erlang:cancel_timer( TimerRef ) div 1000
	end,
	%we pretend to logout before we actually do so the lifes generation takes into account the time we already waited
	LogoutTimestamp = swiss:unix_timestamp() - ( ?LIFE_GENERATION_TIMEOUT - TimeLeft ),
	user_store:save_profile_fields( User#mc_user.user_id , [{ ?LAST_LOGIN_KEY, LogoutTimestamp, put}] ),
	ok.




-spec can_enter_game( LogicUser :: #logic_user{ }, Powers :: [string()] ) -> false | true.
can_enter_game( #logic_user{ user = User }, Powers ) ->
	true.
	%CurrentLifes = proplists:get_value(?LIFES_KEY, User#mc_user.wallet),
	% FIXME: no effect is temporary for development convenience.
	%CurrentLifes >= ?LIFE_GAME_COST.




-spec handle_game_start( LogicUser :: #logic_user{ }, Powers :: [string()] ) -> { ok ,#logic_user{}} | {error, not_enough_lifes }.
handle_game_start( LogicUser = #logic_user{ lifes_generate_timer = _TimerRef, user = _User }, _Powers ) ->
	{ ok , LogicUser}.



-spec handle_game_lost( LogicUser :: #logic_user{ }, Powers :: [string()] ) -> { ok ,#logic_user{}} | {error, not_enough_lifes }.
handle_game_lost( LogicUser = #logic_user{ lifes_generate_timer = TimerRef, user = User }, Powers ) ->
	case remove_lifes_from_user( ?LIFE_GAME_COST, User, TimerRef ) of
		{error, ReasonL} ->
			{error, ReasonL};
		{NewUser, NewTimerRef} ->
			{ ok , LogicUser#logic_user{ lifes_generate_timer = NewTimerRef, user = NewUser }}
	end.


-spec handle_game_win( LogicUser :: #logic_user{ }, Powers :: [string()], Tier :: league_name() ) -> { ok ,#logic_user{}} | {error, not_enough_lifes }.
handle_game_win( LogicUser = #logic_user{ lifes_generate_timer = TimerRef, user = User }, Powers, Tier ) ->
	case remove_lifes_from_user( ?LIFE_GAME_COST, User, TimerRef ) of
		{error, ReasonL} ->
			{error, ReasonL};
		{NewUser1, NewTimerRef} ->
			{NewUser2,PrevXp,NewXp} = add_xp_to_user( tier2xp_reward(Tier), NewUser1 ),
			PrevLevel = xp2level(PrevXp),
			NewLevel = xp2level(NewXp),
			NewUser3 = add_coins_to_user( tier2coins_reward(Tier), NewUser2 ),
			{ ok , LogicUser#logic_user{ lifes_generate_timer = NewTimerRef, user = NewUser3 }}
	end.



-spec handle_msg( generate_life , LogicUser :: #logic_user{ } ) -> { ok ,#logic_user{}}.
handle_msg( generate_life, LogicUser = #logic_user{ lifes_generate_timer = TimerRef, user = User } ) ->
	{ NewUser, NewTimerRef} = add_lifes_to_user( 1, User, TimerRef),
	{ok, LogicUser#logic_user{ lifes_generate_timer = NewTimerRef, user = NewUser }}.














add_lifes_to_user( Amount, User = #mc_user{} , TimerRef) ->
	CurrentLifes = case proplists:get_value(?LIFES_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,

	lager:debug("adding lifes: CurrentLifes ~p  Amount ~p  MAX_LIFES ~p",[CurrentLifes,Amount,?MAX_LIFES]),
	{NewWallet, NewTimerRef } = 
	case ((CurrentLifes + Amount) >= ?MAX_LIFES) of
		true when TimerRef == undefined->
			{ok, NewAmount} = user_store:update_wallet_balance(User#mc_user.user_id, ?LIFES_KEY, ?MAX_LIFES - CurrentLifes),

			{[ { ?LIFES_KEY , NewAmount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ], undefined };	
		true when TimerRef =/= undefined->
			{ok, NewAmount} = user_store:update_wallet_balance(User#mc_user.user_id, ?LIFES_KEY, ?MAX_LIFES - CurrentLifes),
			erlang:cancel_timer( TimerRef ),
			{[ { ?LIFES_KEY , NewAmount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ], undefined };
		false ->
			{ok, NewAmount} = user_store:update_wallet_balance(User#mc_user.user_id, ?LIFES_KEY, Amount),
			{[ { ?LIFES_KEY , NewAmount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ], TimerRef}
	end,
	{User#mc_user{ wallet = NewWallet } , NewTimerRef}.




remove_lifes_from_user( Amount, User = #mc_user{}, TimerRef ) ->

	NewTimerRef = case TimerRef of
		undefined ->	erlang:send_after(?LIFE_GENERATION_TIMEOUT * 1000, self(), { user_logic_msg, generate_life});
		_other ->		TimerRef
	end,

	CurrentLifes = case proplists:get_value(?LIFES_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,
	lager:debug("removing lifes: CurrentLifes ~p  Amount ~p  MAX_LIFES ~p",[CurrentLifes,Amount,?MAX_LIFES]),

	case user_store:update_wallet_balance( User#mc_user.user_id, ?LIFES_KEY, -Amount) of
		{ok, NewAmount} ->
			NewWallet = [ { ?LIFES_KEY , NewAmount} | proplists:delete( ?LIFES_KEY, User#mc_user.wallet ) ],
			{ User#mc_user{ wallet = NewWallet }, NewTimerRef };
		{error, {insufficient, _ }} ->
			{ error , not_enough_lifes}
	end.
	


generate_missing_lifes( User = #mc_user{}, TimerRef, SessionStartTime ) ->

	case proplists:get_value(?LAST_LOGIN_KEY, User#mc_user.properties) of
		
		undefined ->
			{User, TimerRef};

		TimeSinceLastLogin ->

			lager:debug("generate_missing_lifes: SessionStartTime ~p  , TimeSinceLastLogin ~p",
							[SessionStartTime,TimeSinceLastLogin]),

			DeltaTime = (SessionStartTime - TimeSinceLastLogin),
			case DeltaTime div ?LIFE_GENERATION_TIMEOUT of
				0 ->					do_nothing,{User, TimerRef};
				LifesToGenerate when LifesToGenerate < 0 ->
										do_nothing,{User, TimerRef};
				LifesToGenerate ->	add_lifes_to_user( LifesToGenerate, User, TimerRef )
			end
	end.







add_coins_to_user( Amount, User = #mc_user{} ) when Amount > 0 ->
	CurrentCoins = case proplists:get_value(?COINS_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,

	lager:debug("adding coins: CurrentCoins ~p  Amount ~p",[CurrentCoins,Amount]),
	{ok, NewBalance} = user_store:update_wallet_balance( User#mc_user.user_id, ?COINS_KEY, Amount ),
	NewWallet = [ {?COINS_KEY, NewBalance} | proplists:delete( ?COINS_KEY, User#mc_user.wallet ) ],
	User#mc_user{ wallet=NewWallet }.


remove_coins_from_user( Amount, User = #mc_user{} ) when Amount > 0 ->
	CurrentCoins = case proplists:get_value(?COINS_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,

	lager:debug("removing coins: CurrentCoins ~p  Amount ~p",[CurrentCoins,Amount]),
	case Amount > CurrentCoins of
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

	




xp2level_gbtree_ceilkey( Tree, Target ) ->
	xp2level_gbtree_ceilkey( Tree, gb_trees:iterator(Tree), Target, -1 ).

xp2level_gbtree_ceilkey( Tree, Iterator, Target, PrevV ) ->
	case gb_trees:next(Iterator) of
		{K, V, NextIterator} ->
			case Target < K of
				true ->
					PrevV;
				false ->
					xp2level_gbtree_ceilkey( Tree, NextIterator, Target, V )
			end;
		none ->
			PrevV
	end.
	

xp2level( Xp ) ->
	xp2level( Xp, gb_trees:from_orddict(?LIFE_2_XP_DICT) ).


xp2level( Xp, Life2XpGbTree ) ->
	xp2level_gbtree_ceilkey( Life2XpGbTree, Xp ).


add_xp_to_user( Amount, User = #mc_user{} ) when Amount > 0 ->
	CurrentXp = case proplists:get_value(?XP_KEY, User#mc_user.wallet) of
		undefined ->	0;
		Other -> 		Other
	end,

	lager:debug("adding xp: CurrentXp ~p  Amount ~p",[CurrentXp,Amount]),
	{ok, NewXp} = user_store:update_wallet_balance( User#mc_user.user_id, ?XP_KEY, Amount ),
	NewWallet = [ {?XP_KEY, NewXp} | proplists:delete( ?XP_KEY, User#mc_user.wallet ) ],
	{User#mc_user{ wallet=NewWallet }, CurrentXp, (CurrentXp+1)}.




tier2xp_reward( beginner ) ->
	tier2xp_reward( 1 );

tier2xp_reward( Tier ) when is_integer(Tier) ->
	tier2xp_reward( Tier, gb_trees:from_orddict(?TIER_2_XPREWARD_DICT) ).

tier2xp_reward( Tier, Tier2XpRewardGbTree ) ->
	{value, Value} = gb_trees:lookup( Tier, Tier2XpRewardGbTree ),
	Value.



tier2coins_reward( beginner ) ->
	tier2coins_reward( 1 );

tier2coins_reward( Tier ) when is_integer(Tier) ->
	tier2coins_reward( Tier, gb_trees:from_orddict(?TIER_2_COINREWARD_DICT) ).

tier2coins_reward( Tier, Tier2CoinsRewardGbTree ) ->
	{value, Value} = gb_trees:lookup( Tier, Tier2CoinsRewardGbTree ),
	Value.












