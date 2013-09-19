%% @type piece_rotation() = up | down | right | left.
-type piece_rotation() :: up | down | right | left.

%% @type block_color() = garbage | red | yellow | blue | green | purple | white.
-type block_color() :: garbage | red | yellow | blue | green | purple | white.

%% @type block_position_record() = #block_position_record{
%%   x() = integer(),
%%   y() = integer(),
%%   color() = integer()
%% }.
-record(block_position,{
  x :: integer(),
  y :: integer(),
  color :: integer()}).

%% @type garbage_position_record() = #garbage_position_record{
%%   x() = integer()
%% }.
-record(garbage_position,{
  x :: integer()}).

%% @type game_state_record() = #game_state_record{
%%   current_random() = integer(),
%%   current_piece_x() = integer(),
%%   current_piece_y() = integer(),
%%   current_piece_angle() = piece_rotation(),
%%   current_piece_color1() = block_color(),
%%   current_piece_color2() = block_color(),
%%   blocks() = [#block_position{}],
%%   garbage_message_list() = [#garbage_position{}]
%% }.
-record(game_state,{
  current_random :: integer(),
  current_piece_x :: integer(),
  current_piece_y :: integer(),
  current_piece_angle :: piece_rotation(),
  current_piece_color1 :: block_color(),
  current_piece_color2 :: block_color(),
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
%%   game_state() = 'undefined' | #message_game_state{}
%% }.
-record(messagelogin_success,{
  user_id :: binary(),
  previous_state :: messagelogin_success__previous_state(),
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
%%   state() = piece_rotation()
%% }.
-record(message_place_piece,{
  x :: integer(),
  y :: integer(),
  state :: piece_rotation()}).

%% @type message_opponent_place_piece_record() = #message_opponent_place_piece_record{
%%   x() = integer(),
%%   y() = integer(),
%%   state() = piece_rotation(),
%%   garbage() = [#garbage_position{}]
%% }.
-record(message_opponent_place_piece,{
  x :: integer(),
  y :: integer(),
  state :: piece_rotation(),
  garbage = [] :: [#garbage_position{}]}).

%% @type message_generated_garbage_record() = #message_generated_garbage_record{
%%   garbage() = [#garbage_position{}]
%% }.
-record(message_generated_garbage,{
  garbage = [] :: [#garbage_position{}]}).

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
%%   opponent() = binary()
%% }.
-record(message_restart_game,{
  opponent :: binary()}).

%% @type message_generic_power_record() = #message_generic_power_record{
%%   type() = integer(),
%%   power_data() = 'undefined' | binary()
%% }.
-record(message_generic_power,{
  type :: integer(),
  power_data :: 'undefined' | binary()}).

%% @type message_enter_queue_record() = #message_enter_queue_record{
%%   tier() = integer()
%% }.
-record(message_enter_queue,{
  tier :: integer()}).

%% @type message_match_found_record() = #message_match_found_record{
%%   seed() = integer(),
%%   opponent_name() = binary(),
%%   start_level() = integer()
%% }.
-record(message_match_found,{
  seed :: integer(),
  opponent_name :: binary(),
  start_level :: integer()}).

%% @type request__request_type() = message_login_code | message_place_piece_code | message_update_piece_code | message_opponent_place_piece_code | message_game_end_code | message_game_start_code | message_ready_code | message_lost_game | message_login_sucess | message_disconect | message_difficult_change | message_get_game_state | message_user_disconected | message_game_restart | message_generic_power | message_enter_queue | message_match_found | message_generated_garbage_code.
-type request__request_type() :: message_login_code | message_place_piece_code | message_update_piece_code | message_opponent_place_piece_code | message_game_end_code | message_game_start_code | message_ready_code | message_lost_game | message_login_sucess | message_disconect | message_difficult_change | message_get_game_state | message_user_disconected | message_game_restart | message_generic_power | message_enter_queue | message_match_found | message_generated_garbage_code.

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
%%   match_found_content() = 'undefined' | #message_match_found{},
%%   generated_garbage_content() = 'undefined' | #message_generated_garbage{}
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
  match_found_content :: 'undefined' | #message_match_found{},
  generated_garbage_content :: 'undefined' | #message_generated_garbage{}}).

