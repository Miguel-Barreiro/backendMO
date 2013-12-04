%% @type piece_rotation() = up | down | right | left.
-type piece_rotation() :: up | down | right | left.

%% @type color_block() = red | yellow | blue | green | purple | white.
-type color_block() :: red | yellow | blue | green | purple | white.

%% @type type_block() = garbage | garbage_hard | garbage_color | basic_block | bomb | chromatic_bomb | tornado | reinforcements | cloner | shapeshifter | paint | ghost.
-type type_block() :: garbage | garbage_hard | garbage_color | basic_block | bomb | chromatic_bomb | tornado | reinforcements | cloner | shapeshifter | paint | ghost.

%% @type user_item_record() = #user_item_record{
%%   name() = binary(),
%%   amount() = integer()
%% }.
-record(user_item,{
  name :: binary(),
  amount :: integer()}).

%% @type user_wallet_record() = #user_wallet_record{
%%   items() = [#user_item{}]
%% }.
-record(user_wallet,{
  items = [] :: [#user_item{}]}).

%% @type block_position_record() = #block_position_record{
%%   x() = integer(),
%%   y() = integer(),
%%   type() = type_block(),
%%   color() = 'undefined' | color_block(),
%%   exploding_times_left() = integer()
%% }.
-record(block_position,{
  x :: integer(),
  y :: integer(),
  type :: type_block(),
  color :: 'undefined' | color_block(),
  exploding_times_left :: integer()}).

%% @type garbage_position_record() = #garbage_position_record{
%%   type() = type_block(),
%%   x() = integer(),
%%   color() = 'undefined' | color_block(),
%%   hardness() = 'undefined' | integer()
%% }.
-record(garbage_position,{
  type :: type_block(),
  x :: integer(),
  color :: 'undefined' | color_block(),
  hardness :: 'undefined' | integer()}).

%% @type game_state_record() = #game_state_record{
%%   current_random() = integer(),
%%   current_piece_x() = integer(),
%%   current_piece_y() = integer(),
%%   current_piece_angle() = piece_rotation(),
%%   current_piece_block1_type() = type_block(),
%%   current_piece_block2_type() = type_block(),
%%   blocks() = [#block_position{}],
%%   garbage_message_list() = [#garbage_position{}]
%% }.
-record(game_state,{
  current_random :: integer(),
  current_piece_x :: integer(),
  current_piece_y :: integer(),
  current_piece_angle :: piece_rotation(),
  current_piece_block1_type :: type_block(),
  current_piece_block2_type :: type_block(),
  blocks = [] :: [#block_position{}],
  garbage_message_list = [] :: [#garbage_position{}]}).

%% @type message_game_state_record() = #message_game_state_record{
%%   opponent_state() = #game_state{},
%%   player_state() = #game_state{},
%%   starting_seed() = integer(),
%%   opponent_name() = binary()
%% }.
-record(message_game_state,{
  opponent_state :: #game_state{},
  player_state :: #game_state{},
  starting_seed :: integer(),
  opponent_name :: binary()}).

%% @type message_login_record() = #message_login_record{
%%   user_id() = 'undefined' | binary(),
%%   client_time() = integer()
%% }.
-record(message_login,{
  user_id :: 'undefined' | binary(),
  client_time :: integer()}).

%% @type messagelogin_success__previous_state() = lobby | playing_game.
-type messagelogin_success__previous_state() :: lobby | playing_game.

%% @type messagelogin_success_record() = #messagelogin_success_record{
%%   user_id() = binary(),
%%   previous_state() = messagelogin_success__previous_state(),
%%   configuration_url() = binary(),
%%   configuration_version() = binary(),
%%   wallet() = #user_wallet{},
%%   game_state() = 'undefined' | #message_game_state{}
%% }.
-record(messagelogin_success,{
  user_id :: binary(),
  previous_state :: messagelogin_success__previous_state(),
  configuration_url :: binary(),
  configuration_version :: binary(),
  wallet :: #user_wallet{},
  game_state :: 'undefined' | #message_game_state{}}).

%% @type message_game_start_record() = #message_game_start_record{
%%   start_timestamp() = integer()
%% }.
-record(message_game_start,{
  start_timestamp :: integer()}).

%% @type message_game_end_record() = #message_game_end_record{
%%   reason() = integer()
%% }.
-record(message_game_end,{
  reason :: integer()}).

%% @type message_update_piece_record() = #message_update_piece_record{
%%   x() = integer(),
%%   y() = integer(),
%%   state() = piece_rotation()
%% }.
-record(message_update_piece,{
  x :: integer(),
  y :: integer(),
  state :: piece_rotation()}).

%% @type message_place_piece_record() = #message_place_piece_record{
%%   x() = integer(),
%%   y() = integer(),
%%   state() = piece_rotation(),
%%   placed_garbage_id() = integer()
%% }.
-record(message_place_piece,{
  x :: integer(),
  y :: integer(),
  state :: piece_rotation(),
  placed_garbage_id :: integer()}).

%% @type message_opponent_place_piece_record() = #message_opponent_place_piece_record{
%%   x() = integer(),
%%   y() = integer(),
%%   state() = piece_rotation(),
%%   garbage() = [#garbage_position{}],
%%   garbage_id() = integer(),
%%   opponent_garbage_id() = integer()
%% }.
-record(message_opponent_place_piece,{
  x :: integer(),
  y :: integer(),
  state :: piece_rotation(),
  garbage = [] :: [#garbage_position{}],
  garbage_id :: integer(),
  opponent_garbage_id :: integer()}).

%% @type message_generated_garbage_record() = #message_generated_garbage_record{
%%   garbage() = [#garbage_position{}],
%%   garbage_id() = integer()
%% }.
-record(message_generated_garbage,{
  garbage = [] :: [#garbage_position{}],
  garbage_id :: integer()}).

%% @type message_difficult_change_record() = #message_difficult_change_record{
%%   level() = integer()
%% }.
-record(message_difficult_change,{
  level :: integer()}).

%% @type message_user_disconected_record() = #message_user_disconected_record{
%%   opponent() = binary()
%% }.
-record(message_user_disconected,{
  opponent :: binary()}).

%% @type message_restart_game_record() = #message_restart_game_record{
%%   opponent() = binary(),
%%   start_timestamp() = integer()
%% }.
-record(message_restart_game,{
  opponent :: binary(),
  start_timestamp :: integer()}).

%% @type message_generic_power__power_type() = frenzy | trash | redbutton.
-type message_generic_power__power_type() :: frenzy | trash | redbutton.

%% @type message_generic_power_record() = #message_generic_power_record{
%%   type() = message_generic_power__power_type(),
%%   power_data() = 'undefined' | binary()
%% }.
-record(message_generic_power,{
  type :: message_generic_power__power_type(),
  power_data :: 'undefined' | binary()}).

%% @type message_enter_queue_record() = #message_enter_queue_record{
%%   tier() = integer(),
%%   powers_equipped() = [binary()]
%% }.
-record(message_enter_queue,{
  tier :: integer(),
  powers_equipped = [] :: [binary()]}).

%% @type message_match_created_record() = #message_match_created_record{
%%   seed() = integer(),
%%   opponent_name() = binary(),
%%   start_level() = integer()
%% }.
-record(message_match_created,{
  seed :: integer(),
  opponent_name :: binary(),
  start_level :: integer()}).

%% @type message_new_configuration_record() = #message_new_configuration_record{
%%   new_version() = binary(),
%%   new_url() = binary()
%% }.
-record(message_new_configuration,{
  new_version :: binary(),
  new_url :: binary()}).

%% @type message_buy_product_record() = #message_buy_product_record{
%%   product_id() = binary()
%% }.
-record(message_buy_product,{
  product_id :: binary()}).

%% @type message_buy_product_response__response_type() = response_success | response_fail.
-type message_buy_product_response__response_type() :: response_success | response_fail.

%% @type message_buy_product_response_record() = #message_buy_product_response_record{
%%   type() = message_buy_product_response__response_type(),
%%   new_amount() = 'undefined' | #user_item{}
%% }.
-record(message_buy_product_response,{
  type :: message_buy_product_response__response_type(),
  new_amount :: 'undefined' | #user_item{}}).

%% @type message_time_sync_record() = #message_time_sync_record{
%%   client_timestamp() = 'undefined' | integer(),
%%   server_timestamp() = 'undefined' | integer()
%% }.
-record(message_time_sync,{
  client_timestamp :: 'undefined' | integer(),
  server_timestamp :: 'undefined' | integer()}).

%% @type message_debug_board_record() = #message_debug_board_record{
%%   opponent_state() = #game_state{},
%%   player_state() = #game_state{},
%%   player_last_garbage_id() = integer()
%% }.
-record(message_debug_board,{
  opponent_state :: #game_state{},
  player_state :: #game_state{},
  player_last_garbage_id :: integer()}).

%% @type request__request_type() = message_login_code | message_place_piece_code | message_update_piece_code | message_opponent_place_piece_code | message_game_end_code | message_game_start_code | message_ready_code | message_lost_game | message_login_sucess | message_disconect | message_difficult_change | message_get_game_state | message_user_disconected | message_game_restart | message_generic_power | message_enter_queue | message_match_created | message_generated_garbage_code | message_user_reconected | message_new_configuration_version | message_buy_product | message_buy_product_response | message_sync_time | message_rematch | message_no_rematch | message_rematch_timeout | message_not_enough_lifes | message_debug_board.
-type request__request_type() :: message_login_code | message_place_piece_code | message_update_piece_code | message_opponent_place_piece_code | message_game_end_code | message_game_start_code | message_ready_code | message_lost_game | message_login_sucess | message_disconect | message_difficult_change | message_get_game_state | message_user_disconected | message_game_restart | message_generic_power | message_enter_queue | message_match_created | message_generated_garbage_code | message_user_reconected | message_new_configuration_version | message_buy_product | message_buy_product_response | message_sync_time | message_rematch | message_no_rematch | message_rematch_timeout | message_not_enough_lifes | message_debug_board.

%% @type request_record() = #request_record{
%%   type() = request__request_type(),
%%   login_content() = 'undefined' | #message_login{},
%%   place_piece_content() = 'undefined' | #message_place_piece{},
%%   update_piece_content() = 'undefined' | #message_update_piece{},
%%   opponent_place_piece_content() = 'undefined' | #message_opponent_place_piece{},
%%   game_end_content() = 'undefined' | #message_game_end{},
%%   game_start_content() = 'undefined' | #message_game_start{},
%%   login_sucess_content() = 'undefined' | #messagelogin_success{},
%%   difficult_change_content() = 'undefined' | #message_difficult_change{},
%%   user_disconected_content() = 'undefined' | #message_user_disconected{},
%%   restart_game_content() = 'undefined' | #message_restart_game{},
%%   power_content() = 'undefined' | #message_generic_power{},
%%   enter_queue_content() = 'undefined' | #message_enter_queue{},
%%   match_created_content() = 'undefined' | #message_match_created{},
%%   generated_garbage_content() = 'undefined' | #message_generated_garbage{},
%%   new_configuration_content() = 'undefined' | #message_new_configuration{},
%%   buy_product_content() = 'undefined' | #message_buy_product{},
%%   buy_product_response_content() = 'undefined' | #message_buy_product_response{},
%%   message_sync_content() = 'undefined' | #message_time_sync{},
%%   debug_game_state_content() = 'undefined' | #message_debug_board{}
%% }.
-record(request,{
  type :: request__request_type(),
  login_content :: 'undefined' | #message_login{},
  place_piece_content :: 'undefined' | #message_place_piece{},
  update_piece_content :: 'undefined' | #message_update_piece{},
  opponent_place_piece_content :: 'undefined' | #message_opponent_place_piece{},
  game_end_content :: 'undefined' | #message_game_end{},
  game_start_content :: 'undefined' | #message_game_start{},
  login_sucess_content :: 'undefined' | #messagelogin_success{},
  difficult_change_content :: 'undefined' | #message_difficult_change{},
  user_disconected_content :: 'undefined' | #message_user_disconected{},
  restart_game_content :: 'undefined' | #message_restart_game{},
  power_content :: 'undefined' | #message_generic_power{},
  enter_queue_content :: 'undefined' | #message_enter_queue{},
  match_created_content :: 'undefined' | #message_match_created{},
  generated_garbage_content :: 'undefined' | #message_generated_garbage{},
  new_configuration_content :: 'undefined' | #message_new_configuration{},
  buy_product_content :: 'undefined' | #message_buy_product{},
  buy_product_response_content :: 'undefined' | #message_buy_product_response{},
  message_sync_content :: 'undefined' | #message_time_sync{},
  debug_game_state_content :: 'undefined' | #message_debug_board{}}).

