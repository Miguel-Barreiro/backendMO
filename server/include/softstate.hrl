-record(broker_order, {
	userid, 
	opponent,
	tier, 
	userpid,
	cueid,
	node,
	created,
	is_rematch = false
}).

-record(inapps_definition_rec, {
	type :: 'coins' | 'product',
	product_id :: 'undefined' | integer(),
	quantity :: integer()
}).

-record(serpent_game_item_rec, {
	item_id :: integer(),
	coin_price :: 'undefined' | integer(),
	mc_price :: 'undefined' | integer()
}).

-record(game_item_rec, {
	item_id :: integer(),
	quantity :: integer()
}).

-record(progression_rec, {
	level :: integer(),
	relative_xp :: integer(),
	awards :: [] | [#inapps_definition_rec{}]
}).

-record(cost_and_winning_rec, {
	tier :: integer(),
	entry_cost :: integer(),
	match_win_coins :: integer(),
	match_win_xp :: integer(),
	match_lose_xp :: integer(),
	turn_timeout :: integer(),
	turn_timer_decrease :: integer(),
	turn_timer_minimum :: integer(),
	turn_timer_foul_modifier :: integer()
}).

-record(game_profile_rec, {
	game_profile_id :: 'undefined' | integer(),
	profile_id :: 'undefined' | integer(),
	games_played = 0 :: integer(),
	games_won = 0 :: integer(),
	games_lost = 0 :: integer(),
	games_tied = 0 :: integer(),
	tournament_games_played = 0 :: integer(),
	tournament_games_won = 0 :: integer(),
	tournament_games_lost = 0 :: integer(),
	tournament_games_tied = 0 :: integer(),
	tournaments_joined = 0 :: integer(),
	tournaments_won = 0 :: integer(),
	challenge_games_lost = 0 :: integer(),
	challenge_games_won = 0 :: integer(),
	challenge_games_tied = 0 :: integer(),
	challenge_games_played = 0 :: integer(),
	longest_win_streak = 0 :: integer(),
	longest_win_streak_timestamp = 0 :: integer(),
	seconds_played = 0 :: integer(),
	seconds_played_tournaments = 0 :: integer(),
	seconds_played_challenges = 0 :: integer(),
	current_win_streak = 0 :: integer(),
	daily_bonus_timestamp = 0 :: integer(),
	consecutive_daily_bonus_counter = 0 :: integer(),
	bonus_timestamp = 0 :: integer(),
	level = 1 :: integer(),
	progression = 0 :: integer(),
	total_winnings = 0 :: integer(),
	total_tournament_winnings = 0 :: integer()
}).

-record(user_profile_rec, {
	profile_id :: 'undefined' | integer(),
	display_name :: binary(),
	miniclip_id :: binary(),
	country_code :: binary(),
	is_guest :: boolean(),
	gag = 0 :: integer(),
	ban = 0 :: integer(),
	warn = 0 :: integer()
}).

-record(online_profile_rec, {
	token :: binary(),
	push_notification_token :: binary(),
	lastlogin :: integer(),
	last_opponent_id :: integer(),
	last_streak = 0 :: integer(),
	last_game_end = 0 :: integer(),
	last_game_starter :: integer()
}).

-record(award_rec, {
	award_id :: integer(),
	date_awarded :: integer()
}).

-record(profile_data_rec, {
	user_profile :: #user_profile_rec{},
	game_profile :: #game_profile_rec{},
	online_profile :: #online_profile_rec{},
	keys_and_values :: [] | [{binary(), integer(), boolean()}],
	awards :: [] | [#award_rec{}]
}).

-record(user, {
	userid :: binary(),
	state :: 'creating' | 'created',
	created_time :: integer(), 
	pid :: pid(),
	profile_data :: #profile_data_rec{}
}).

-record(device_description, {
	manufacturer :: binary(),
	platform :: binary(),
	os_version :: binary()
}).

% DB records
-record(friend_rec, {
	user_id :: integer(),
	facebook_id :: integer(),
	display_name :: binary(),
	level :: integer(),
	total_winnings :: integer() 
}).

-record(facebook_friend_rec, {
	facebook_id :: integer(),
	display_name :: binary()
}).

-record(find_fb_user_rec, {
	user_id :: integer()
}).

-record(get_user_tokens_rec, {
	token :: binary(),
	delivery_platform :: integer()
}).

-record(start_match_rec, {
	game_id :: integer()
}).

-record(login_user_rec, {
	is_new_user :: integer(),
	needs_update :: integer(),
	bonus_timestamp :: integer()
}).

-record(user_search_rec, {
	user_id :: integer(),
	display_name :: binary(),
	miniclip_id :: binary(),
	level :: integer(),
	total_winnings :: integer()
}).

-define(MAX_AWARDS, 100).
-define(TRANSACTION_LOCK_TIME, 5).

% DB errors
-define(TRANSACTION_EXISTS, <<"99004">>).
-define(TRANSACTION_NOT_FOUND, <<"99005">>).
-define(TRANSACTION_PENDING, <<"99006">>).
-define(USER_NEEDS_FB_UPDATE, <<"99007">>).

% Platform
-define(APPLE_IOS_PLATFORM, 0).
-define(GOOGLE_ANDROID_PLATFORM, 1).
-define(WEB_PLATFORM, 2).

% Errors
-define(INVALID_USER, <<"2">>).
-define(INVALID_DATE, <<"7">>).
-define(DUPLICATE_USERNAME, <<"1000">>).
-define(DUPLICATE_NICKNAME, <<"1004">>).
-define(INVALID_NICKNAME, <<"1005">>).
-define(INVALID_SIGNATURE, <<"10008">>).

-define(END_MATCH_REASON_NORMAL, 0).
-define(END_MATCH_REASON_CRASH, 1).
