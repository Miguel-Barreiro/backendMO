%% @type message_login_record() = #message_login_record{
%%   user_id() = binary(),
%%   client_time() = integer()
%% }.
-record(message_login,{
  user_id :: binary(),
  client_time :: integer()}).

%% @type messagelogin_success_record() = #messagelogin_success_record{
%%   user_id() = binary()
%% }.
-record(messagelogin_success,{
  user_id :: binary()}).

%% @type message_game_start_record() = #message_game_start_record{
%%   seed() = integer(),
%%   opponent_name() = binary(),
%%   start_level() = integer(),
%%   start_timestamp() = integer()
%% }.
-record(message_game_start,{
  seed :: integer(),
  opponent_name :: binary(),
  start_level :: integer(),
  start_timestamp :: integer()}).

%% @type message_game_end_record() = #message_game_end_record{
%%   reason() = integer()
%% }.
-record(message_game_end,{
  reason :: integer()}).

%% @type message_update_piece_record() = #message_update_piece_record{
%%   x() = integer(),
%%   y() = integer(),
%%   state() = integer()
%% }.
-record(message_update_piece,{
  x :: integer(),
  y :: integer(),
  state :: integer()}).

%% @type message_place_piece_record() = #message_place_piece_record{
%%   x() = integer(),
%%   y() = integer(),
%%   state() = integer()
%% }.
-record(message_place_piece,{
  x :: integer(),
  y :: integer(),
  state :: integer()}).

%% @type message_place_garbage_record() = #message_place_garbage_record{
%%   garbage_position() = [integer()]
%% }.
-record(message_place_garbage,{
  garbage_position = [] :: [integer()]}).

%% @type message_player_ready_record() = #message_player_ready_record{
%%   unused() = 'undefined' | integer()
%% }.
-record(message_player_ready,{
  unused :: 'undefined' | integer()}).

%% @type message_player_lost_record() = #message_player_lost_record{
%%   unused() = 'undefined' | integer()
%% }.
-record(message_player_lost,{
  unused :: 'undefined' | integer()}).

%% @type message_disconect_record() = #message_disconect_record{
%%   unused() = 'undefined' | integer()
%% }.
-record(message_disconect,{
  unused :: 'undefined' | integer()}).

%% @type message_difficult_change_record() = #message_difficult_change_record{
%%   level() = integer()
%% }.
-record(message_difficult_change,{
  level :: integer()}).

%% @type request__request_type() = message_login_code | message_place_piece_code | message_update_piece_code | message_place_garbage_code | message_game_end_code | message_game_start_code | message_ready_code | message_lost_game | message_login_sucess | message_disconect | message_difficult_change.
-type request__request_type() :: message_login_code | message_place_piece_code | message_update_piece_code | message_place_garbage_code | message_game_end_code | message_game_start_code | message_ready_code | message_lost_game | message_login_sucess | message_disconect | message_difficult_change.

%% @type request_record() = #request_record{
%%   type() = request__request_type(),
%%   login_content() = 'undefined' | #message_login{},
%%   place_piece_content() = 'undefined' | #message_place_piece{},
%%   update_piece_content() = 'undefined' | #message_update_piece{},
%%   place_garbage_content() = 'undefined' | #message_place_garbage{},
%%   game_end_content() = 'undefined' | #message_game_end{},
%%   game_start_content() = 'undefined' | #message_game_start{},
%%   player_ready_content() = 'undefined' | #message_player_ready{},
%%   player_lost_content() = 'undefined' | #message_player_lost{},
%%   login_sucess_content() = 'undefined' | #messagelogin_success{},
%%   disconect_content() = 'undefined' | #message_disconect{},
%%   difficult_change_content() = 'undefined' | #message_difficult_change{}
%% }.
-record(request,{
  type :: request__request_type(),
  login_content :: 'undefined' | #message_login{},
  place_piece_content :: 'undefined' | #message_place_piece{},
  update_piece_content :: 'undefined' | #message_update_piece{},
  place_garbage_content :: 'undefined' | #message_place_garbage{},
  game_end_content :: 'undefined' | #message_game_end{},
  game_start_content :: 'undefined' | #message_game_start{},
  player_ready_content :: 'undefined' | #message_player_ready{},
  player_lost_content :: 'undefined' | #message_player_lost{},
  login_sucess_content :: 'undefined' | #messagelogin_success{},
  disconect_content :: 'undefined' | #message_disconect{},
  difficult_change_content :: 'undefined' | #message_difficult_change{}}).

