%% @type position_record() = #position_record{
%%   pos_x() = float(),
%%   pos_y() = float()
%% }.
-record(position,{
  pos_x :: float(),
  pos_y :: float()}).

%% @type aim_event__aim_event_type() = cue_rotated | power_changed | english_changed | ball_dragged.
-type aim_event__aim_event_type() :: cue_rotated | power_changed | english_changed | ball_dragged.

%% @type aim_event_record() = #aim_event_record{
%%   type() = aim_event__aim_event_type(),
%%   angle() = 'undefined' | float(),
%%   power() = 'undefined' | float(),
%%   new_english() = 'undefined' | #position{},
%%   ball_index() = 'undefined' | integer(),
%%   new_ball_position() = 'undefined' | #position{}
%% }.
-record(aim_event,{
  type :: aim_event__aim_event_type(),
  angle :: 'undefined' | float(),
  power :: 'undefined' | float(),
  new_english :: 'undefined' | #position{},
  ball_index :: 'undefined' | integer(),
  new_ball_position :: 'undefined' | #position{}}).

%% @type aim_events_record() = #aim_events_record{
%%   events() = [#aim_event{}]
%% }.
-record(aim_events,{
  events = [] :: [#aim_event{}]}).

%% @type ball_state__ball_state_enum() = on_table | in_ball_runner | off_table.
-type ball_state__ball_state_enum() :: on_table | in_ball_runner | off_table.

%% @type ball_state_record() = #ball_state_record{
%%   ball_number() = integer(),
%%   ball_state() = ball_state__ball_state_enum(),
%%   ball_runner_index() = 'undefined' | integer(),
%%   ball_position() = 'undefined' | #position{}
%% }.
-record(ball_state,{
  ball_number :: integer(),
  ball_state :: ball_state__ball_state_enum(),
  ball_runner_index :: 'undefined' | integer(),
  ball_position :: 'undefined' | #position{}}).

%% @type table_state__state() = unbroken | open | closed.
-type table_state__state() :: unbroken | open | closed.

%% @type table_state_record() = #table_state_record{
%%   rule_state() = table_state__state(),
%%   stripes_player() = binary(),
%%   balls() = [#ball_state{}]
%% }.
-record(table_state,{
  rule_state :: table_state__state(),
  stripes_player :: binary(),
  balls = [] :: [#ball_state{}]}).

%% @type shot_outcome__outcome_type() = give_turn | game_over | turn_timeout.
-type shot_outcome__outcome_type() :: give_turn | game_over | turn_timeout.

%% @type shot_outcome_record() = #shot_outcome_record{
%%   outcome() = shot_outcome__outcome_type(),
%%   next_player() = 'undefined' | binary(),
%%   winner() = 'undefined' | binary(),
%%   next_turn_time() = 'undefined' | integer(),
%%   shot_id() = integer(),
%%   table() = 'undefined' | #table_state{}
%% }.
-record(shot_outcome,{
  outcome :: shot_outcome__outcome_type(),
  next_player :: 'undefined' | binary(),
  winner :: 'undefined' | binary(),
  next_turn_time :: 'undefined' | integer(),
  shot_id :: integer(),
  table :: 'undefined' | #table_state{}}).

%% @type balls_racked_record() = #balls_racked_record{
%%   table() = #table_state{}
%% }.
-record(balls_racked,{
  table :: #table_state{}}).

%% @type shot_data_record() = #shot_data_record{
%%   player() = binary(),
%%   angle() = float(),
%%   english() = #position{},
%%   power() = float(),
%%   table() = #table_state{},
%%   max_power() = float(),
%%   spin_factor() = float(),
%%   powerup_used() = [integer()],
%%   shot_id() = integer()
%% }.
-record(shot_data,{
  player :: binary(),
  angle :: float(),
  english :: #position{},
  power :: float(),
  table :: #table_state{},
  max_power :: float(),
  spin_factor :: float(),
  powerup_used = [] :: [integer()],
  shot_id :: integer()}).

%% @type game_event_record() = #game_event_record{
%%   event_id() = integer(),
%%   parameters() = [binary()]
%% }.
-record(game_event,{
  event_id :: integer(),
  parameters = [] :: [binary()]}).

%% @type animation_done_record() = #animation_done_record{
%%   outcome() = #shot_outcome{},
%%   table() = #table_state{},
%%   events() = [#game_event{}],
%%   legal_balls_potted() = 'undefined' | integer(),
%%   foul() = boolean(),
%%   shot_id() = integer()
%% }.
-record(animation_done,{
  outcome :: #shot_outcome{},
  table :: #table_state{},
  events = [] :: [#game_event{}],
  legal_balls_potted :: 'undefined' | integer(),
  foul :: boolean(),
  shot_id :: integer()}).

%% @type referee_request_record() = #referee_request_record{
%%   shot_data() = #shot_data{},
%%   opponent() = binary()
%% }.
-record(referee_request,{
  shot_data :: #shot_data{},
  opponent :: binary()}).

%% @type referee_rack_request_record() = #referee_rack_request_record{
%%   random_seed() = integer()
%% }.
-record(referee_rack_request,{
  random_seed :: integer()}).

%% @type aim_event_queue_record() = #aim_event_queue_record{
%%   events() = [#aim_event{}]
%% }.
-record(aim_event_queue,{
  events = [] :: [#aim_event{}]}).

%% @type user_creation_info_record() = #user_creation_info_record{
%%   email() = binary(),
%%   nickname() = binary(),
%%   password() = binary(),
%%   birth_year() = integer(),
%%   birth_month() = integer(),
%%   birth_day() = integer()
%% }.
-record(user_creation_info,{
  email :: binary(),
  nickname :: binary(),
  password :: binary(),
  birth_year :: integer(),
  birth_month :: integer(),
  birth_day :: integer()}).

%% @type create_session__authentication_realm() = guest | facebook | miniclip | miniclip_username_password | create_miniclip_user | miniclip_password_reminder | miniclip_web_token.
-type create_session__authentication_realm() :: guest | facebook | miniclip | miniclip_username_password | create_miniclip_user | miniclip_password_reminder | miniclip_web_token.

%% @type create_session_record() = #create_session_record{
%%   realm() = create_session__authentication_realm(),
%%   client_version() = non_neg_integer(),
%%   authentication_token() = 'undefined' | binary(),
%%   username() = 'undefined' | binary(),
%%   password() = 'undefined' | binary(),
%%   push_notification_token() = 'undefined' | binary(),
%%   create_data() = 'undefined' | #user_creation_info{},
%%   platform() = integer(),
%%   device_name() = 'undefined' | binary(),
%%   openuid() = 'undefined' | binary(),
%%   sha1_mac() = 'undefined' | binary()
%% }.
-record(create_session,{
  realm :: create_session__authentication_realm(),
  client_version :: non_neg_integer(),
  authentication_token :: 'undefined' | binary(),
  username :: 'undefined' | binary(),
  password :: 'undefined' | binary(),
  push_notification_token :: 'undefined' | binary(),
  create_data :: 'undefined' | #user_creation_info{},
  platform :: integer(),
  device_name :: 'undefined' | binary(),
  openuid :: 'undefined' | binary(),
  sha1_mac :: 'undefined' | binary()}).

%% @type match_data__game_state() = awaiting_players | starting | in_progress | finished.
-type match_data__game_state() :: awaiting_players | starting | in_progress | finished.

%% @type match_data_record() = #match_data_record{
%%   state() = match_data__game_state(),
%%   row() = integer(),
%%   column() = integer(),
%%   player_a() = 'undefined' | binary(),
%%   player_a_display_name() = 'undefined' | binary(),
%%   player_b() = 'undefined' | binary(),
%%   player_b_display_name() = 'undefined' | binary(),
%%   winner() = 'undefined' | binary(),
%%   event_player_a() = 'undefined' | #game_event{},
%%   event_player_b() = 'undefined' | #game_event{}
%% }.
-record(match_data,{
  state :: match_data__game_state(),
  row :: integer(),
  column :: integer(),
  player_a :: 'undefined' | binary(),
  player_a_display_name :: 'undefined' | binary(),
  player_b :: 'undefined' | binary(),
  player_b_display_name :: 'undefined' | binary(),
  winner :: 'undefined' | binary(),
  event_player_a :: 'undefined' | #game_event{},
  event_player_b :: 'undefined' | #game_event{}}).

%% @type tournament_state_record() = #tournament_state_record{
%%   size() = integer(),
%%   matches() = [#match_data{}]
%% }.
-record(tournament_state,{
  size :: integer(),
  matches = [] :: [#match_data{}]}).

%% @type game_queue_data__game_type() = normal_game | tournament_game.
-type game_queue_data__game_type() :: normal_game | tournament_game.

%% @type game_queue_data_record() = #game_queue_data_record{
%%   type() = game_queue_data__game_type(),
%%   tier() = integer(),
%%   opponent() = 'undefined' | binary(),
%%   opponent_name() = 'undefined' | binary(),
%%   opponent_rank() = 'undefined' | integer(),
%%   tournament() = 'undefined' | #tournament_state{}
%% }.
-record(game_queue_data,{
  type :: game_queue_data__game_type(),
  tier :: integer(),
  opponent :: 'undefined' | binary(),
  opponent_name :: 'undefined' | binary(),
  opponent_rank :: 'undefined' | integer(),
  tournament :: 'undefined' | #tournament_state{}}).

%% @type daily_spin_bonus_record() = #daily_spin_bonus_record{
%%   prize() = integer(),
%%   total_spins() = integer()
%% }.
-record(daily_spin_bonus,{
  prize :: integer(),
  total_spins :: integer()}).

%% @type free_coins_bonus_record() = #free_coins_bonus_record{
%%   elapsed_time() = integer(),
%%   total_time() = integer(),
%%   amount() = integer()
%% }.
-record(free_coins_bonus,{
  elapsed_time :: integer(),
  total_time :: integer(),
  amount :: integer()}).

%% @type game_metadata__game_type_t() = normal | tournament.
-type game_metadata__game_type_t() :: normal | tournament.

%% @type game_metadata__tournament_progress_t() = final | semi_finals | quarter_finals | eighth_finals.
-type game_metadata__tournament_progress_t() :: final | semi_finals | quarter_finals | eighth_finals.

%% @type game_metadata_record() = #game_metadata_record{
%%   type() = game_metadata__game_type_t(),
%%   tier() = integer(),
%%   pot() = integer(),
%%   tournament_progress() = 'undefined' | game_metadata__tournament_progress_t(),
%%   player_win_count() = 'undefined' | integer(),
%%   opponent_win_count() = 'undefined' | integer(),
%%   opponent_cue_id() = integer()
%% }.
-record(game_metadata,{
  type :: game_metadata__game_type_t(),
  tier :: integer(),
  pot :: integer(),
  tournament_progress :: 'undefined' | game_metadata__tournament_progress_t(),
  player_win_count :: 'undefined' | integer(),
  opponent_win_count :: 'undefined' | integer(),
  opponent_cue_id :: integer()}).

%% @type product_info_record() = #product_info_record{
%%   product_id() = integer(),
%%   quantity() = integer()
%% }.
-record(product_info,{
  product_id :: integer(),
  quantity :: integer()}).

%% @type tier_info_record() = #tier_info_record{
%%   tier_id() = integer(),
%%   tier_population() = integer()
%% }.
-record(tier_info,{
  tier_id :: integer(),
  tier_population :: integer()}).

%% @type achievement_list_record() = #achievement_list_record{
%%   achievement_id() = [integer()]
%% }.
-record(achievement_list,{
  achievement_id = [] :: [integer()]}).

%% @type product_list_record() = #product_list_record{
%%   product() = [#product_info{}]
%% }.
-record(product_list,{
  product = [] :: [#product_info{}]}).

%% @type tier_info_list_record() = #tier_info_list_record{
%%   tier() = [#tier_info{}]
%% }.
-record(tier_info_list,{
  tier = [] :: [#tier_info{}]}).

%% @type session_state__current_state() = not_started | waiting_game_starting | waiting_rack | waiting_player_shot | waiting_player_result | waiting_opponent_shot | waiting_shot_outcome | waiting_game_started.
-type session_state__current_state() :: not_started | waiting_game_starting | waiting_rack | waiting_player_shot | waiting_player_result | waiting_opponent_shot | waiting_shot_outcome | waiting_game_started.

%% @type session_state_record() = #session_state_record{
%%   latest_configuration() = integer(),
%%   state() = session_state__current_state(),
%%   user_id() = binary(),
%%   user_display_name() = binary(),
%%   user_cash() = integer(),
%%   user_xp() = integer(),
%%   user_level() = integer(),
%%   last_table_state() = 'undefined' | #animation_done{},
%%   last_shot_data() = 'undefined' | #shot_data{},
%%   turn_time_left() = 'undefined' | integer(),
%%   total_player_control_time() = 'undefined' | integer(),
%%   total_opponent_control_time() = 'undefined' | integer(),
%%   opponent() = 'undefined' | binary(),
%%   opponent_name() = 'undefined' | binary(),
%%   opponent_rank() = 'undefined' | integer(),
%%   wait_start() = 'undefined' | #game_queue_data{},
%%   authentication_token() = 'undefined' | binary(),
%%   daily_spin_bonus_data() = 'undefined' | #daily_spin_bonus{},
%%   game_data() = 'undefined' | #game_metadata{},
%%   free_coins_bonus_data() = #free_coins_bonus{},
%%   games_won() = integer(),
%%   tournaments_won() = integer(),
%%   total_legal_balls_potted() = integer(),
%%   win_streak() = integer(),
%%   current_game_time() = 'undefined' | integer(),
%%   achievements() = #achievement_list{},
%%   products() = #product_list{},
%%   tier_population() = #tier_info_list{},
%%   selected_cue() = integer(),
%%   web_upgrade() = boolean(),
%%   total_turn_time() = 'undefined' | integer(),
%%   miniclip_id() = binary(),
%%   random_seed() = 'undefined' | integer()
%% }.
-record(session_state,{
  latest_configuration :: integer(),
  state :: session_state__current_state(),
  user_id :: binary(),
  user_display_name :: binary(),
  user_cash :: integer(),
  user_xp :: integer(),
  user_level :: integer(),
  last_table_state :: 'undefined' | #animation_done{},
  last_shot_data :: 'undefined' | #shot_data{},
  turn_time_left :: 'undefined' | integer(),
  total_player_control_time :: 'undefined' | integer(),
  total_opponent_control_time :: 'undefined' | integer(),
  opponent :: 'undefined' | binary(),
  opponent_name :: 'undefined' | binary(),
  opponent_rank :: 'undefined' | integer(),
  wait_start :: 'undefined' | #game_queue_data{},
  authentication_token :: 'undefined' | binary(),
  daily_spin_bonus_data :: 'undefined' | #daily_spin_bonus{},
  game_data :: 'undefined' | #game_metadata{},
  free_coins_bonus_data :: #free_coins_bonus{},
  games_won :: integer(),
  tournaments_won :: integer(),
  total_legal_balls_potted :: integer(),
  win_streak :: integer(),
  current_game_time :: 'undefined' | integer(),
  achievements :: #achievement_list{},
  products :: #product_list{},
  tier_population :: #tier_info_list{},
  selected_cue :: integer(),
  web_upgrade :: boolean(),
  total_turn_time :: 'undefined' | integer(),
  miniclip_id :: binary(),
  random_seed :: 'undefined' | integer()}).

%% @type user_info_update__update_type_t() = cash | xp | level | product | ads_reward_cash.
-type user_info_update__update_type_t() :: cash | xp | level | product | ads_reward_cash.

%% @type user_info_update_record() = #user_info_update_record{
%%   update_type() = user_info_update__update_type_t(),
%%   delta() = integer(),
%%   final() = integer(),
%%   product_id() = 'undefined' | integer()
%% }.
-record(user_info_update,{
  update_type :: user_info_update__update_type_t(),
  delta :: integer(),
  final :: integer(),
  product_id :: 'undefined' | integer()}).

%% @type enter_gamequeue_record() = #enter_gamequeue_record{
%%   tier() = integer(),
%%   opponent() = 'undefined' | binary(),
%%   tournament_size() = 'undefined' | integer(),
%%   cue_id() = integer(),
%%   is_rematch() = boolean()
%% }.
-record(enter_gamequeue,{
  tier :: integer(),
  opponent :: 'undefined' | binary(),
  tournament_size :: 'undefined' | integer(),
  cue_id :: integer(),
  is_rematch :: boolean()}).

%% @type enter_gamequeue_result__result_type() = success | insufficent_funds | nonexisting_opponent.
-type enter_gamequeue_result__result_type() :: success | insufficent_funds | nonexisting_opponent.

%% @type enter_gamequeue_result_record() = #enter_gamequeue_result_record{
%%   enter_result() = enter_gamequeue_result__result_type(),
%%   update_data() = [#user_info_update{}]
%% }.
-record(enter_gamequeue_result,{
  enter_result :: enter_gamequeue_result__result_type(),
  update_data = [] :: [#user_info_update{}]}).

%% @type game_starting_record() = #game_starting_record{
%%   opponent() = binary(),
%%   opponent_name() = binary(),
%%   opponent_rank() = integer(),
%%   metadata() = #game_metadata{},
%%   random_seed() = integer()
%% }.
-record(game_starting,{
  opponent :: binary(),
  opponent_name :: binary(),
  opponent_rank :: integer(),
  metadata :: #game_metadata{},
  random_seed :: integer()}).

%% @type game_started_record() = #game_started_record{
%%   starting_player() = binary(),
%%   table() = 'undefined' | #table_state{}
%% }.
-record(game_started,{
  starting_player :: binary(),
  table :: 'undefined' | #table_state{}}).

%% @type game_ended__game_ended_type() = normal | opponent_disconnected | result_mismatch.
-type game_ended__game_ended_type() :: normal | opponent_disconnected | result_mismatch.

%% @type game_ended_record() = #game_ended_record{
%%   type() = game_ended__game_ended_type(),
%%   update_data() = [#user_info_update{}]
%% }.
-record(game_ended,{
  type :: game_ended__game_ended_type(),
  update_data = [] :: [#user_info_update{}]}).

%% @type disconnect__disconnect_reason() = network_error | relogin_elsewhere | incorrect_login | incompatible_client | game_crashed | maintenance | not_authorized | database_issue | duplicate_nickname | duplicate_email | invalid_password | invalid_date | invalid_nickname | pw_reminder_sent | pw_reminder_invalid_user.
-type disconnect__disconnect_reason() :: network_error | relogin_elsewhere | incorrect_login | incompatible_client | game_crashed | maintenance | not_authorized | database_issue | duplicate_nickname | duplicate_email | invalid_password | invalid_date | invalid_nickname | pw_reminder_sent | pw_reminder_invalid_user.

%% @type disconnect_record() = #disconnect_record{
%%   reason() = disconnect__disconnect_reason(),
%%   retry_sensible() = boolean()
%% }.
-record(disconnect,{
  reason :: disconnect__disconnect_reason(),
  retry_sensible :: boolean()}).

%% @type ping__ping_type() = shallow | deep.
-type ping__ping_type() :: shallow | deep.

%% @type ping_record() = #ping_record{
%%   id() = integer(),
%%   type() = ping__ping_type(),
%%   roundtrip() = 'undefined' | integer()
%% }.
-record(ping,{
  id :: integer(),
  type :: ping__ping_type(),
  roundtrip :: 'undefined' | integer()}).

%% @type pang_record() = #pang_record{
%%   id() = integer()
%% }.
-record(pang,{
  id :: integer()}).

%% @type user_data_type_record() = #user_data_type_record{
%%   user_id() = binary(),
%%   display_name() = binary(),
%%   rank() = integer(),
%%   games_played() = integer(),
%%   games_won() = integer(),
%%   tournaments_played() = integer(),
%%   tournaments_won() = integer(),
%%   country_code() = binary(),
%%   total_winnings() = integer(),
%%   tournament_winnings() = integer(),
%%   total_time_played() = integer(),
%%   total_legal_balls_potted() = integer(),
%%   win_streak() = integer()
%% }.
-record(user_data_type,{
  user_id :: binary(),
  display_name :: binary(),
  rank :: integer(),
  games_played :: integer(),
  games_won :: integer(),
  tournaments_played :: integer(),
  tournaments_won :: integer(),
  country_code :: binary(),
  total_winnings :: integer(),
  tournament_winnings :: integer(),
  total_time_played :: integer(),
  total_legal_balls_potted :: integer(),
  win_streak :: integer()}).

%% @type user_info_result__result_type() = success | failure.
-type user_info_result__result_type() :: success | failure.

%% @type user_info_result_record() = #user_info_result_record{
%%   request_result() = user_info_result__result_type(),
%%   user_data() = 'undefined' | #user_data_type{}
%% }.
-record(user_info_result,{
  request_result :: user_info_result__result_type(),
  user_data :: 'undefined' | #user_data_type{}}).

%% @type tuple_record() = #tuple_record{
%%   key() = binary(),
%%   value() = binary()
%% }.
-record(tuple,{
  key :: binary(),
  value :: binary()}).

%% @type configuration_changed_record() = #configuration_changed_record{
%%   new_configuration_version() = integer(),
%%   configuration_changes() = [#tuple{}]
%% }.
-record(configuration_changed,{
  new_configuration_version :: integer(),
  configuration_changes = [] :: [#tuple{}]}).

%% @type invite_record() = #invite_record{
%%   opponent() = binary(),
%%   tier() = integer()
%% }.
-record(invite,{
  opponent :: binary(),
  tier :: integer()}).

%% @type invite_declined_record() = #invite_declined_record{
%%   opponent() = binary(),
%%   opponent_name() = binary(),
%%   is_rematch() = boolean()
%% }.
-record(invite_declined,{
  opponent :: binary(),
  opponent_name :: binary(),
  is_rematch :: boolean()}).

%% @type match_request_record() = #match_request_record{
%%   opponent() = binary(),
%%   opponent_name() = binary(),
%%   tier() = integer(),
%%   is_rematch() = boolean()
%% }.
-record(match_request,{
  opponent :: binary(),
  opponent_name :: binary(),
  tier :: integer(),
  is_rematch :: boolean()}).

%% @type decline_match_request_record() = #decline_match_request_record{
%%   opponent() = binary(),
%%   is_rematch() = boolean()
%% }.
-record(decline_match_request,{
  opponent :: binary(),
  is_rematch :: boolean()}).

%% @type friend_info_record() = #friend_info_record{
%%   user_id() = binary(),
%%   facebook_id() = 'undefined' | binary(),
%%   display_name() = binary(),
%%   rank() = integer(),
%%   total_winnings() = integer()
%% }.
-record(friend_info,{
  user_id :: binary(),
  facebook_id :: 'undefined' | binary(),
  display_name :: binary(),
  rank :: integer(),
  total_winnings :: integer()}).

%% @type get_friend_response_record() = #get_friend_response_record{
%%   friends() = [#friend_info{}]
%% }.
-record(get_friend_response,{
  friends = [] :: [#friend_info{}]}).

%% @type chat_message_request_record() = #chat_message_request_record{
%%   user_id_to() = binary(),
%%   message_type() = integer()
%% }.
-record(chat_message_request,{
  user_id_to :: binary(),
  message_type :: integer()}).

%% @type chat_message_notification_record() = #chat_message_notification_record{
%%   user_id_from() = binary(),
%%   message_type() = integer()
%% }.
-record(chat_message_notification,{
  user_id_from :: binary(),
  message_type :: integer()}).

%% @type collect_free_coins_response_record() = #collect_free_coins_response_record{
%%   next_bonus() = #free_coins_bonus{},
%%   update() = 'undefined' | #user_info_update{}
%% }.
-record(collect_free_coins_response,{
  next_bonus :: #free_coins_bonus{},
  update :: 'undefined' | #user_info_update{}}).

%% @type transaction_validate_request_record() = #transaction_validate_request_record{
%%   transaction_id() = non_neg_integer(),
%%   receipt() = binary(),
%%   signature() = 'undefined' | binary()
%% }.
-record(transaction_validate_request,{
  transaction_id :: non_neg_integer(),
  receipt :: binary(),
  signature :: 'undefined' | binary()}).

%% @type transaction_validate_response__result_type() = success | invalid | transient.
-type transaction_validate_response__result_type() :: success | invalid | transient.

%% @type transaction_validate_response_record() = #transaction_validate_response_record{
%%   transaction_id() = non_neg_integer(),
%%   request_result() = transaction_validate_response__result_type(),
%%   update_data() = [#user_info_update{}]
%% }.
-record(transaction_validate_response,{
  transaction_id :: non_neg_integer(),
  request_result :: transaction_validate_response__result_type(),
  update_data = [] :: [#user_info_update{}]}).

%% @type purchase_product_request_record() = #purchase_product_request_record{
%%   product_id() = integer(),
%%   quantity() = integer()
%% }.
-record(purchase_product_request,{
  product_id :: integer(),
  quantity :: integer()}).

%% @type purchase_product_response__result_type() = success | insufficent_funds | invalid_product | transient_error.
-type purchase_product_response__result_type() :: success | insufficent_funds | invalid_product | transient_error.

%% @type purchase_product_response_record() = #purchase_product_response_record{
%%   request_result() = purchase_product_response__result_type(),
%%   product_id() = integer(),
%%   final_product_quantity() = 'undefined' | integer(),
%%   final_coins_quantity() = 'undefined' | integer()
%% }.
-record(purchase_product_response,{
  request_result :: purchase_product_response__result_type(),
  product_id :: integer(),
  final_product_quantity :: 'undefined' | integer(),
  final_coins_quantity :: 'undefined' | integer()}).

%% @type upgrade_guest_request_record() = #upgrade_guest_request_record{
%%   upgrade_info() = 'undefined' | #user_creation_info{},
%%   facebook_token() = 'undefined' | binary()
%% }.
-record(upgrade_guest_request,{
  upgrade_info :: 'undefined' | #user_creation_info{},
  facebook_token :: 'undefined' | binary()}).

%% @type upgrade_guest_response__result_type() = success | not_eligible | duplicate_nickname | duplicate_email | invalid_password | invalid_date | invalid_nickname | invalid_email.
-type upgrade_guest_response__result_type() :: success | not_eligible | duplicate_nickname | duplicate_email | invalid_password | invalid_date | invalid_nickname | invalid_email.

%% @type upgrade_guest_response_record() = #upgrade_guest_response_record{
%%   request_result() = upgrade_guest_response__result_type()
%% }.
-record(upgrade_guest_response,{
  request_result :: upgrade_guest_response__result_type()}).

%% @type user_result_record() = #user_result_record{
%%   user_id() = binary(),
%%   display_name() = binary(),
%%   miniclip_id() = binary(),
%%   level() = non_neg_integer(),
%%   winnings() = non_neg_integer()
%% }.
-record(user_result,{
  user_id :: binary(),
  display_name :: binary(),
  miniclip_id :: binary(),
  level :: non_neg_integer(),
  winnings :: non_neg_integer()}).

%% @type search_user_result_record() = #search_user_result_record{
%%   results() = [#user_result{}]
%% }.
-record(search_user_result,{
  results = [] :: [#user_result{}]}).

%% @type daily_spin_response__result_type() = success | not_eligible.
-type daily_spin_response__result_type() :: success | not_eligible.

%% @type daily_spin_response_record() = #daily_spin_response_record{
%%   spin_result() = daily_spin_response__result_type(),
%%   spins_left() = 'undefined' | non_neg_integer(),
%%   update() = 'undefined' | #user_info_update{}
%% }.
-record(daily_spin_response,{
  spin_result :: daily_spin_response__result_type(),
  spins_left :: 'undefined' | non_neg_integer(),
  update :: 'undefined' | #user_info_update{}}).

%% @type offline_game__offline_game_type() = hotseat | quickfire.
-type offline_game__offline_game_type() :: hotseat | quickfire.

%% @type offline_game_record() = #offline_game_record{
%%   type() = offline_game__offline_game_type(),
%%   amount() = non_neg_integer()
%% }.
-record(offline_game,{
  type :: offline_game__offline_game_type(),
  amount :: non_neg_integer()}).

%% @type offline_game_debit_record() = #offline_game_debit_record{
%%   games() = [#offline_game{}]
%% }.
-record(offline_game_debit,{
  games = [] :: [#offline_game{}]}).

%% @type req__message_type() = create_session_t | session_state_t | entergamequeue_t | leavegamequeue_t | game_starting_t | game_started_t | game_ended_t | disconnect_t | balls_racked_t | shot_taken_t | animate_shot_t | animation_done_t | aim_events_t | shot_outcome_t | ping_t | pang_t | quit_game_t | game_queue_data_t | user_info_request_t | user_info_result_t | configuration_changed_t | enter_gamequeue_result_t | tournament_event_t | invite_t | match_request_t | get_friend_request_t | get_friend_response_t | add_friend_t | chat_message_request_t | chat_message_notification_t | user_info_update_t | collect_free_coins_request_t | collect_free_coins_response_t | validate_transaction_request_t | validate_transaction_response_t | new_achievements_t | purchase_product_request_t | purchase_product_response_t | upgrade_guest_request_t | upgrade_guest_response_t | invite_declined_t | decline_match_request_t | nonce_request_t | nonce_response_t | set_selected_cue_t | search_user_t | search_user_result_t | daily_spin_request_t | daily_spin_response_t | offline_game_debit_t | referee_request_t | referee_response_t | referee_rack_request_t | referee_rack_response_t | delete_friend_t.
-type req__message_type() :: create_session_t | session_state_t | entergamequeue_t | leavegamequeue_t | game_starting_t | game_started_t | game_ended_t | disconnect_t | balls_racked_t | shot_taken_t | animate_shot_t | animation_done_t | aim_events_t | shot_outcome_t | ping_t | pang_t | quit_game_t | game_queue_data_t | user_info_request_t | user_info_result_t | configuration_changed_t | enter_gamequeue_result_t | tournament_event_t | invite_t | match_request_t | get_friend_request_t | get_friend_response_t | add_friend_t | chat_message_request_t | chat_message_notification_t | user_info_update_t | collect_free_coins_request_t | collect_free_coins_response_t | validate_transaction_request_t | validate_transaction_response_t | new_achievements_t | purchase_product_request_t | purchase_product_response_t | upgrade_guest_request_t | upgrade_guest_response_t | invite_declined_t | decline_match_request_t | nonce_request_t | nonce_response_t | set_selected_cue_t | search_user_t | search_user_result_t | daily_spin_request_t | daily_spin_response_t | offline_game_debit_t | referee_request_t | referee_response_t | referee_rack_request_t | referee_rack_response_t | delete_friend_t.

%% @type req_record() = #req_record{
%%   type() = req__message_type(),
%%   create_session_field() = 'undefined' | #create_session{},
%%   session_state_field() = 'undefined' | #session_state{},
%%   entergamequeue_field() = 'undefined' | #enter_gamequeue{},
%%   game_starting_field() = 'undefined' | #game_starting{},
%%   game_ended_field() = 'undefined' | #game_ended{},
%%   disconnect_field() = 'undefined' | #disconnect{},
%%   balls_racked_field() = 'undefined' | #balls_racked{},
%%   shot_taken_field() = 'undefined' | #shot_data{},
%%   animate_shot_field() = 'undefined' | #shot_data{},
%%   aim_events_field() = 'undefined' | #aim_events{},
%%   animation_done_field() = 'undefined' | #animation_done{},
%%   shot_outcome_field() = 'undefined' | #shot_outcome{},
%%   ping_field() = 'undefined' | #ping{},
%%   pang_field() = 'undefined' | #pang{},
%%   queue_data() = 'undefined' | #game_queue_data{},
%%   user_info_request_data() = 'undefined' | binary(),
%%   user_info_result_data() = 'undefined' | #user_info_result{},
%%   configuration_changed_data() = 'undefined' | #configuration_changed{},
%%   enter_gamequeue_result_data() = 'undefined' | #enter_gamequeue_result{},
%%   tournament_event_data() = 'undefined' | #match_data{},
%%   invite_data() = 'undefined' | #invite{},
%%   match_request_data() = 'undefined' | #match_request{},
%%   get_friend_response_data() = 'undefined' | #get_friend_response{},
%%   add_friend_data() = 'undefined' | binary(),
%%   chat_message_request_data() = 'undefined' | #chat_message_request{},
%%   chat_message_notification_data() = 'undefined' | #chat_message_notification{},
%%   user_info_update_data() = 'undefined' | #user_info_update{},
%%   collect_free_coins_response_data() = 'undefined' | #collect_free_coins_response{},
%%   transaction_validate_request_data() = 'undefined' | #transaction_validate_request{},
%%   transaction_validate_response_data() = 'undefined' | #transaction_validate_response{},
%%   new_achievements_data() = 'undefined' | #achievement_list{},
%%   purchase_product_request_data() = 'undefined' | #purchase_product_request{},
%%   purchase_product_response_data() = 'undefined' | #purchase_product_response{},
%%   upgrade_guest_request_data() = 'undefined' | #upgrade_guest_request{},
%%   upgrade_guest_response_data() = 'undefined' | #upgrade_guest_response{},
%%   invite_declined_data() = 'undefined' | #invite_declined{},
%%   decline_match_request_data() = 'undefined' | #decline_match_request{},
%%   nonce_response() = 'undefined' | non_neg_integer(),
%%   selected_cue() = 'undefined' | integer(),
%%   search_user_term() = 'undefined' | binary(),
%%   search_user_result_data() = 'undefined' | #search_user_result{},
%%   daily_spin_response_data() = 'undefined' | #daily_spin_response{},
%%   offline_game_debit_data() = 'undefined' | #offline_game_debit{},
%%   referee_request_data() = 'undefined' | #referee_request{},
%%   referee_response_data() = 'undefined' | #animation_done{},
%%   referee_rack_request_data() = 'undefined' | #referee_rack_request{},
%%   referee_rack_response_data() = 'undefined' | #balls_racked{},
%%   game_started_data() = 'undefined' | #game_started{},
%%   delete_friend_data() = 'undefined' | binary()
%% }.
-record(req,{
  type :: req__message_type(),
  create_session_field :: 'undefined' | #create_session{},
  session_state_field :: 'undefined' | #session_state{},
  entergamequeue_field :: 'undefined' | #enter_gamequeue{},
  game_starting_field :: 'undefined' | #game_starting{},
  game_ended_field :: 'undefined' | #game_ended{},
  disconnect_field :: 'undefined' | #disconnect{},
  balls_racked_field :: 'undefined' | #balls_racked{},
  shot_taken_field :: 'undefined' | #shot_data{},
  animate_shot_field :: 'undefined' | #shot_data{},
  aim_events_field :: 'undefined' | #aim_events{},
  animation_done_field :: 'undefined' | #animation_done{},
  shot_outcome_field :: 'undefined' | #shot_outcome{},
  ping_field :: 'undefined' | #ping{},
  pang_field :: 'undefined' | #pang{},
  queue_data :: 'undefined' | #game_queue_data{},
  user_info_request_data :: 'undefined' | binary(),
  user_info_result_data :: 'undefined' | #user_info_result{},
  configuration_changed_data :: 'undefined' | #configuration_changed{},
  enter_gamequeue_result_data :: 'undefined' | #enter_gamequeue_result{},
  tournament_event_data :: 'undefined' | #match_data{},
  invite_data :: 'undefined' | #invite{},
  match_request_data :: 'undefined' | #match_request{},
  get_friend_response_data :: 'undefined' | #get_friend_response{},
  add_friend_data :: 'undefined' | binary(),
  chat_message_request_data :: 'undefined' | #chat_message_request{},
  chat_message_notification_data :: 'undefined' | #chat_message_notification{},
  user_info_update_data :: 'undefined' | #user_info_update{},
  collect_free_coins_response_data :: 'undefined' | #collect_free_coins_response{},
  transaction_validate_request_data :: 'undefined' | #transaction_validate_request{},
  transaction_validate_response_data :: 'undefined' | #transaction_validate_response{},
  new_achievements_data :: 'undefined' | #achievement_list{},
  purchase_product_request_data :: 'undefined' | #purchase_product_request{},
  purchase_product_response_data :: 'undefined' | #purchase_product_response{},
  upgrade_guest_request_data :: 'undefined' | #upgrade_guest_request{},
  upgrade_guest_response_data :: 'undefined' | #upgrade_guest_response{},
  invite_declined_data :: 'undefined' | #invite_declined{},
  decline_match_request_data :: 'undefined' | #decline_match_request{},
  nonce_response :: 'undefined' | non_neg_integer(),
  selected_cue :: 'undefined' | integer(),
  search_user_term :: 'undefined' | binary(),
  search_user_result_data :: 'undefined' | #search_user_result{},
  daily_spin_response_data :: 'undefined' | #daily_spin_response{},
  offline_game_debit_data :: 'undefined' | #offline_game_debit{},
  referee_request_data :: 'undefined' | #referee_request{},
  referee_response_data :: 'undefined' | #animation_done{},
  referee_rack_request_data :: 'undefined' | #referee_rack_request{},
  referee_rack_response_data :: 'undefined' | #balls_racked{},
  game_started_data :: 'undefined' | #game_started{},
  delete_friend_data :: 'undefined' | binary()}).

%% @type envelope__content_type() = uncompressed_req_t | compressed_req_t.
-type envelope__content_type() :: uncompressed_req_t | compressed_req_t.

%% @type envelope_record() = #envelope_record{
%%   type() = envelope__content_type(),
%%   uncompressed() = 'undefined' | #req{},
%%   compressed() = 'undefined' | binary(),
%%   uncompressed_size() = 'undefined' | integer()
%% }.
-record(envelope,{
  type :: envelope__content_type(),
  uncompressed :: 'undefined' | #req{},
  compressed :: 'undefined' | binary(),
  uncompressed_size :: 'undefined' | integer()}).

