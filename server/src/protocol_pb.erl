-module(protocol_pb).
-include("protocol_pb.hrl").

-export([
  to_piece_rotation/1,from_piece_rotation/1,
  to_block_color/1,from_block_color/1,
  encode_block_position/1,decode_block_position/1,
  encode_message_garbage_list/1,decode_message_garbage_list/1,
  encode_game_state/1,decode_game_state/1,
  encode_message_game_state/1,decode_message_game_state/1,
  encode_message_login/1,decode_message_login/1,
  to_messagelogin_success__previous_state/1,from_messagelogin_success__previous_state/1,
  encode_messagelogin_success/1,decode_messagelogin_success/1,
  encode_message_game_start/1,decode_message_game_start/1,
  encode_message_game_end/1,decode_message_game_end/1,
  encode_message_update_piece/1,decode_message_update_piece/1,
  encode_message_place_piece/1,decode_message_place_piece/1,
  encode_message_opponent_place_piece/1,decode_message_opponent_place_piece/1,
  encode_message_generated_garbage/1,decode_message_generated_garbage/1,
  encode_message_difficult_change/1,decode_message_difficult_change/1,
  encode_message_user_disconected/1,decode_message_user_disconected/1,
  encode_message_restart_game/1,decode_message_restart_game/1,
  encode_message_generic_power/1,decode_message_generic_power/1,
  encode_message_enter_queue/1,decode_message_enter_queue/1,
  encode_message_match_found/1,decode_message_match_found/1,
  to_request__request_type/1,from_request__request_type/1,
  encode_request/1,decode_request/1]).

to_piece_rotation(1) -> up;
to_piece_rotation(2) -> down;
to_piece_rotation(3) -> right;
to_piece_rotation(4) -> left;
to_piece_rotation(undefined) -> undefined.

from_piece_rotation(up) -> 1;
from_piece_rotation(down) -> 2;
from_piece_rotation(right) -> 3;
from_piece_rotation(left) -> 4;
from_piece_rotation(undefined) -> undefined.

to_block_color(1) -> garbage;
to_block_color(2) -> red;
to_block_color(3) -> yellow;
to_block_color(4) -> blue;
to_block_color(5) -> green;
to_block_color(6) -> purple;
to_block_color(7) -> white;
to_block_color(undefined) -> undefined.

from_block_color(garbage) -> 1;
from_block_color(red) -> 2;
from_block_color(yellow) -> 3;
from_block_color(blue) -> 4;
from_block_color(green) -> 5;
from_block_color(purple) -> 6;
from_block_color(white) -> 7;
from_block_color(undefined) -> undefined.

decode_block_position(B) ->
  case decode_block_position_impl(B) of
    undefined -> #block_position{};
    Any -> Any
  end.

decode_block_position_impl(<<>>) -> undefined;
decode_block_position_impl(Binary) ->
  protocol_buffers:decode(Binary,#block_position{},
     fun(1,Val,Rec) -> Rec#block_position{x = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#block_position{y = protocol_buffers:cast(int32,Val)};
        (3,Val,Rec) -> Rec#block_position{color = protocol_buffers:cast(int32,Val)}
      end).

encode_block_position(undefined) -> undefined;
encode_block_position(R) when is_record(R,block_position) ->
  [
    protocol_buffers:encode(1,int32,R#block_position.x),
    protocol_buffers:encode(2,int32,R#block_position.y),
    protocol_buffers:encode(3,int32,R#block_position.color)
  ].

decode_message_garbage_list(B) ->
  case decode_message_garbage_list_impl(B) of
    undefined -> #message_garbage_list{};
    Any -> Any
  end.

decode_message_garbage_list_impl(<<>>) -> undefined;
decode_message_garbage_list_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_garbage_list{},
     fun        (1,{length_encoded,_}=Val,#message_garbage_list{garbage_position=F}=Rec) when is_list(F) -> Rec#message_garbage_list{garbage_position = Rec#message_garbage_list.garbage_position ++ protocol_buffers:cast(int32,Val)};
        (1,Val,#message_garbage_list{garbage_position=F}=Rec) when is_list(F) -> Rec#message_garbage_list{garbage_position = Rec#message_garbage_list.garbage_position ++ [protocol_buffers:cast(int32,Val)]}

      end).

encode_message_garbage_list(undefined) -> undefined;
encode_message_garbage_list(R) when is_record(R,message_garbage_list) ->
  [
    protocol_buffers:encode(1,int32,R#message_garbage_list.garbage_position)
  ].

decode_game_state(B) ->
  case decode_game_state_impl(B) of
    undefined -> #game_state{};
    Any -> Any
  end.

decode_game_state_impl(<<>>) -> undefined;
decode_game_state_impl(Binary) ->
  protocol_buffers:decode(Binary,#game_state{},
     fun(1,Val,Rec) -> Rec#game_state{current_random = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#game_state{current_piece_x = protocol_buffers:cast(int32,Val)};
        (3,Val,Rec) -> Rec#game_state{current_piece_y = protocol_buffers:cast(int32,Val)};
        (4,{varint,Enum},Rec) -> Rec#game_state{current_piece_angle=to_piece_rotation(Enum)};
        (5,{varint,Enum},Rec) -> Rec#game_state{current_piece_color1=to_block_color(Enum)};
        (6,{varint,Enum},Rec) -> Rec#game_state{current_piece_color2=to_block_color(Enum)};
        (7,{length_encoded,Bin},#game_state{blocks=F}=Rec) when is_list(F) -> Rec#game_state{blocks = Rec#game_state.blocks ++ [decode_block_position_impl(Bin)]}
;
        (8,{length_encoded,Bin},#game_state{garbage_message_list=F}=Rec) when is_list(F) -> Rec#game_state{garbage_message_list = Rec#game_state.garbage_message_list ++ [decode_message_garbage_list_impl(Bin)]}

      end).

encode_game_state(undefined) -> undefined;
encode_game_state(R) when is_record(R,game_state) ->
  [
    protocol_buffers:encode(1,int32,R#game_state.current_random),
    protocol_buffers:encode(2,int32,R#game_state.current_piece_x),
    protocol_buffers:encode(3,int32,R#game_state.current_piece_y),
    protocol_buffers:encode(4,int32,from_piece_rotation(R#game_state.current_piece_angle)),
    protocol_buffers:encode(5,int32,from_block_color(R#game_state.current_piece_color1)),
    protocol_buffers:encode(6,int32,from_block_color(R#game_state.current_piece_color2)),
    [ protocol_buffers:encode(7,length_encoded,encode_block_position(X)) || X <- R#game_state.blocks],
    [ protocol_buffers:encode(8,length_encoded,encode_message_garbage_list(X)) || X <- R#game_state.garbage_message_list]
  ].

decode_message_game_state(B) ->
  case decode_message_game_state_impl(B) of
    undefined -> #message_game_state{};
    Any -> Any
  end.

decode_message_game_state_impl(<<>>) -> undefined;
decode_message_game_state_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_game_state{},
     fun(1,{length_encoded,Bin},Rec) -> Rec#message_game_state{opponent_state = decode_game_state_impl(Bin)};
        (2,{length_encoded,Bin},Rec) -> Rec#message_game_state{player_state = decode_game_state_impl(Bin)};
        (3,Val,Rec) -> Rec#message_game_state{starting_seed = protocol_buffers:cast(int32,Val)};
        (4,Val,Rec) -> Rec#message_game_state{opponent_name = protocol_buffers:cast(string,Val)}
      end).

encode_message_game_state(undefined) -> undefined;
encode_message_game_state(R) when is_record(R,message_game_state) ->
  [
    protocol_buffers:encode(1,length_encoded,encode_game_state(R#message_game_state.opponent_state)),
    protocol_buffers:encode(2,length_encoded,encode_game_state(R#message_game_state.player_state)),
    protocol_buffers:encode(3,int32,R#message_game_state.starting_seed),
    protocol_buffers:encode(4,length_encoded,R#message_game_state.opponent_name)
  ].

decode_message_login(B) ->
  case decode_message_login_impl(B) of
    undefined -> #message_login{};
    Any -> Any
  end.

decode_message_login_impl(<<>>) -> undefined;
decode_message_login_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_login{},
     fun(1,Val,Rec) -> Rec#message_login{user_id = protocol_buffers:cast(string,Val)};
        (2,Val,Rec) -> Rec#message_login{client_time = protocol_buffers:cast(int32,Val)}
      end).

encode_message_login(undefined) -> undefined;
encode_message_login(R) when is_record(R,message_login) ->
  [
    protocol_buffers:encode(1,length_encoded,R#message_login.user_id),
    protocol_buffers:encode(2,int32,R#message_login.client_time)
  ].

to_messagelogin_success__previous_state(1) -> lobby;
to_messagelogin_success__previous_state(2) -> playing_game;
to_messagelogin_success__previous_state(undefined) -> undefined.

from_messagelogin_success__previous_state(lobby) -> 1;
from_messagelogin_success__previous_state(playing_game) -> 2;
from_messagelogin_success__previous_state(undefined) -> undefined.

decode_messagelogin_success(B) ->
  case decode_messagelogin_success_impl(B) of
    undefined -> #messagelogin_success{};
    Any -> Any
  end.

decode_messagelogin_success_impl(<<>>) -> undefined;
decode_messagelogin_success_impl(Binary) ->
  protocol_buffers:decode(Binary,#messagelogin_success{},
     fun(1,Val,Rec) -> Rec#messagelogin_success{user_id = protocol_buffers:cast(string,Val)};
        (2,{varint,Enum},Rec) -> Rec#messagelogin_success{previous_state=to_messagelogin_success__previous_state(Enum)};
        (3,{length_encoded,Bin},Rec) -> Rec#messagelogin_success{game_state = decode_message_game_state_impl(Bin)}
      end).

encode_messagelogin_success(undefined) -> undefined;
encode_messagelogin_success(R) when is_record(R,messagelogin_success) ->
  [
    protocol_buffers:encode(1,length_encoded,R#messagelogin_success.user_id),
    protocol_buffers:encode(2,int32,from_messagelogin_success__previous_state(R#messagelogin_success.previous_state)),
    protocol_buffers:encode(3,length_encoded,encode_message_game_state(R#messagelogin_success.game_state))
  ].

decode_message_game_start(B) ->
  case decode_message_game_start_impl(B) of
    undefined -> #message_game_start{};
    Any -> Any
  end.

decode_message_game_start_impl(<<>>) -> undefined;
decode_message_game_start_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_game_start{},
     fun(4,Val,Rec) -> Rec#message_game_start{start_timestamp = protocol_buffers:cast(int32,Val)}
      end).

encode_message_game_start(undefined) -> undefined;
encode_message_game_start(R) when is_record(R,message_game_start) ->
  [
    protocol_buffers:encode(4,int32,R#message_game_start.start_timestamp)
  ].

decode_message_game_end(B) ->
  case decode_message_game_end_impl(B) of
    undefined -> #message_game_end{};
    Any -> Any
  end.

decode_message_game_end_impl(<<>>) -> undefined;
decode_message_game_end_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_game_end{},
     fun(1,Val,Rec) -> Rec#message_game_end{reason = protocol_buffers:cast(int32,Val)}
      end).

encode_message_game_end(undefined) -> undefined;
encode_message_game_end(R) when is_record(R,message_game_end) ->
  [
    protocol_buffers:encode(1,int32,R#message_game_end.reason)
  ].

decode_message_update_piece(B) ->
  case decode_message_update_piece_impl(B) of
    undefined -> #message_update_piece{};
    Any -> Any
  end.

decode_message_update_piece_impl(<<>>) -> undefined;
decode_message_update_piece_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_update_piece{},
     fun(1,Val,Rec) -> Rec#message_update_piece{x = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#message_update_piece{y = protocol_buffers:cast(int32,Val)};
        (3,{varint,Enum},Rec) -> Rec#message_update_piece{state=to_piece_rotation(Enum)}
      end).

encode_message_update_piece(undefined) -> undefined;
encode_message_update_piece(R) when is_record(R,message_update_piece) ->
  [
    protocol_buffers:encode(1,int32,R#message_update_piece.x),
    protocol_buffers:encode(2,int32,R#message_update_piece.y),
    protocol_buffers:encode(3,int32,from_piece_rotation(R#message_update_piece.state))
  ].

decode_message_place_piece(B) ->
  case decode_message_place_piece_impl(B) of
    undefined -> #message_place_piece{};
    Any -> Any
  end.

decode_message_place_piece_impl(<<>>) -> undefined;
decode_message_place_piece_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_place_piece{},
     fun(1,Val,Rec) -> Rec#message_place_piece{x = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#message_place_piece{y = protocol_buffers:cast(int32,Val)};
        (3,{varint,Enum},Rec) -> Rec#message_place_piece{state=to_piece_rotation(Enum)}
      end).

encode_message_place_piece(undefined) -> undefined;
encode_message_place_piece(R) when is_record(R,message_place_piece) ->
  [
    protocol_buffers:encode(1,int32,R#message_place_piece.x),
    protocol_buffers:encode(2,int32,R#message_place_piece.y),
    protocol_buffers:encode(3,int32,from_piece_rotation(R#message_place_piece.state))
  ].

decode_message_opponent_place_piece(B) ->
  case decode_message_opponent_place_piece_impl(B) of
    undefined -> #message_opponent_place_piece{};
    Any -> Any
  end.

decode_message_opponent_place_piece_impl(<<>>) -> undefined;
decode_message_opponent_place_piece_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_opponent_place_piece{},
     fun(1,Val,Rec) -> Rec#message_opponent_place_piece{x = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#message_opponent_place_piece{y = protocol_buffers:cast(int32,Val)};
        (3,{varint,Enum},Rec) -> Rec#message_opponent_place_piece{state=to_piece_rotation(Enum)};
        (4,{length_encoded,Bin},Rec) -> Rec#message_opponent_place_piece{garbage = decode_message_garbage_list_impl(Bin)}
      end).

encode_message_opponent_place_piece(undefined) -> undefined;
encode_message_opponent_place_piece(R) when is_record(R,message_opponent_place_piece) ->
  [
    protocol_buffers:encode(1,int32,R#message_opponent_place_piece.x),
    protocol_buffers:encode(2,int32,R#message_opponent_place_piece.y),
    protocol_buffers:encode(3,int32,from_piece_rotation(R#message_opponent_place_piece.state)),
    protocol_buffers:encode(4,length_encoded,encode_message_garbage_list(R#message_opponent_place_piece.garbage))
  ].

decode_message_generated_garbage(B) ->
  case decode_message_generated_garbage_impl(B) of
    undefined -> #message_generated_garbage{};
    Any -> Any
  end.

decode_message_generated_garbage_impl(<<>>) -> undefined;
decode_message_generated_garbage_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_generated_garbage{},
     fun(4,{length_encoded,Bin},Rec) -> Rec#message_generated_garbage{garbage = decode_message_garbage_list_impl(Bin)}
      end).

encode_message_generated_garbage(undefined) -> undefined;
encode_message_generated_garbage(R) when is_record(R,message_generated_garbage) ->
  [
    protocol_buffers:encode(4,length_encoded,encode_message_garbage_list(R#message_generated_garbage.garbage))
  ].

decode_message_difficult_change(B) ->
  case decode_message_difficult_change_impl(B) of
    undefined -> #message_difficult_change{};
    Any -> Any
  end.

decode_message_difficult_change_impl(<<>>) -> undefined;
decode_message_difficult_change_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_difficult_change{},
     fun(1,Val,Rec) -> Rec#message_difficult_change{level = protocol_buffers:cast(int32,Val)}
      end).

encode_message_difficult_change(undefined) -> undefined;
encode_message_difficult_change(R) when is_record(R,message_difficult_change) ->
  [
    protocol_buffers:encode(1,int32,R#message_difficult_change.level)
  ].

decode_message_user_disconected(B) ->
  case decode_message_user_disconected_impl(B) of
    undefined -> #message_user_disconected{};
    Any -> Any
  end.

decode_message_user_disconected_impl(<<>>) -> undefined;
decode_message_user_disconected_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_user_disconected{},
     fun(1,Val,Rec) -> Rec#message_user_disconected{opponent = protocol_buffers:cast(string,Val)}
      end).

encode_message_user_disconected(undefined) -> undefined;
encode_message_user_disconected(R) when is_record(R,message_user_disconected) ->
  [
    protocol_buffers:encode(1,length_encoded,R#message_user_disconected.opponent)
  ].

decode_message_restart_game(B) ->
  case decode_message_restart_game_impl(B) of
    undefined -> #message_restart_game{};
    Any -> Any
  end.

decode_message_restart_game_impl(<<>>) -> undefined;
decode_message_restart_game_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_restart_game{},
     fun(1,Val,Rec) -> Rec#message_restart_game{opponent = protocol_buffers:cast(string,Val)}
      end).

encode_message_restart_game(undefined) -> undefined;
encode_message_restart_game(R) when is_record(R,message_restart_game) ->
  [
    protocol_buffers:encode(1,length_encoded,R#message_restart_game.opponent)
  ].

decode_message_generic_power(B) ->
  case decode_message_generic_power_impl(B) of
    undefined -> #message_generic_power{};
    Any -> Any
  end.

decode_message_generic_power_impl(<<>>) -> undefined;
decode_message_generic_power_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_generic_power{},
     fun(1,Val,Rec) -> Rec#message_generic_power{type = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#message_generic_power{power_data = protocol_buffers:cast(string,Val)}
      end).

encode_message_generic_power(undefined) -> undefined;
encode_message_generic_power(R) when is_record(R,message_generic_power) ->
  [
    protocol_buffers:encode(1,int32,R#message_generic_power.type),
    protocol_buffers:encode(2,length_encoded,R#message_generic_power.power_data)
  ].

decode_message_enter_queue(B) ->
  case decode_message_enter_queue_impl(B) of
    undefined -> #message_enter_queue{};
    Any -> Any
  end.

decode_message_enter_queue_impl(<<>>) -> undefined;
decode_message_enter_queue_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_enter_queue{},
     fun(1,Val,Rec) -> Rec#message_enter_queue{tier = protocol_buffers:cast(int32,Val)}
      end).

encode_message_enter_queue(undefined) -> undefined;
encode_message_enter_queue(R) when is_record(R,message_enter_queue) ->
  [
    protocol_buffers:encode(1,int32,R#message_enter_queue.tier)
  ].

decode_message_match_found(B) ->
  case decode_message_match_found_impl(B) of
    undefined -> #message_match_found{};
    Any -> Any
  end.

decode_message_match_found_impl(<<>>) -> undefined;
decode_message_match_found_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_match_found{},
     fun(1,Val,Rec) -> Rec#message_match_found{seed = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#message_match_found{opponent_name = protocol_buffers:cast(string,Val)};
        (3,Val,Rec) -> Rec#message_match_found{start_level = protocol_buffers:cast(int32,Val)}
      end).

encode_message_match_found(undefined) -> undefined;
encode_message_match_found(R) when is_record(R,message_match_found) ->
  [
    protocol_buffers:encode(1,int32,R#message_match_found.seed),
    protocol_buffers:encode(2,length_encoded,R#message_match_found.opponent_name),
    protocol_buffers:encode(3,int32,R#message_match_found.start_level)
  ].

to_request__request_type(1) -> message_login_code;
to_request__request_type(2) -> message_place_piece_code;
to_request__request_type(3) -> message_update_piece_code;
to_request__request_type(4) -> message_opponent_place_piece_code;
to_request__request_type(5) -> message_game_end_code;
to_request__request_type(6) -> message_game_start_code;
to_request__request_type(7) -> message_ready_code;
to_request__request_type(8) -> message_lost_game;
to_request__request_type(9) -> message_login_sucess;
to_request__request_type(10) -> message_disconect;
to_request__request_type(11) -> message_difficult_change;
to_request__request_type(12) -> message_get_game_state;
to_request__request_type(13) -> message_user_disconected;
to_request__request_type(14) -> message_game_restart;
to_request__request_type(15) -> message_generic_power;
to_request__request_type(16) -> message_enter_queue;
to_request__request_type(17) -> message_match_found;
to_request__request_type(18) -> message_generated_garbage_code;
to_request__request_type(undefined) -> undefined.

from_request__request_type(message_login_code) -> 1;
from_request__request_type(message_place_piece_code) -> 2;
from_request__request_type(message_update_piece_code) -> 3;
from_request__request_type(message_opponent_place_piece_code) -> 4;
from_request__request_type(message_game_end_code) -> 5;
from_request__request_type(message_game_start_code) -> 6;
from_request__request_type(message_ready_code) -> 7;
from_request__request_type(message_lost_game) -> 8;
from_request__request_type(message_login_sucess) -> 9;
from_request__request_type(message_disconect) -> 10;
from_request__request_type(message_difficult_change) -> 11;
from_request__request_type(message_get_game_state) -> 12;
from_request__request_type(message_user_disconected) -> 13;
from_request__request_type(message_game_restart) -> 14;
from_request__request_type(message_generic_power) -> 15;
from_request__request_type(message_enter_queue) -> 16;
from_request__request_type(message_match_found) -> 17;
from_request__request_type(message_generated_garbage_code) -> 18;
from_request__request_type(undefined) -> undefined.

decode_request(B) ->
  case decode_request_impl(B) of
    undefined -> #request{};
    Any -> Any
  end.

decode_request_impl(<<>>) -> undefined;
decode_request_impl(Binary) ->
  protocol_buffers:decode(Binary,#request{},
     fun(1,{varint,Enum},Rec) -> Rec#request{type=to_request__request_type(Enum)};
        (2,{length_encoded,Bin},Rec) -> Rec#request{login_content = decode_message_login_impl(Bin)};
        (3,{length_encoded,Bin},Rec) -> Rec#request{place_piece_content = decode_message_place_piece_impl(Bin)};
        (4,{length_encoded,Bin},Rec) -> Rec#request{update_piece_content = decode_message_update_piece_impl(Bin)};
        (5,{length_encoded,Bin},Rec) -> Rec#request{opponent_place_piece_content = decode_message_opponent_place_piece_impl(Bin)};
        (6,{length_encoded,Bin},Rec) -> Rec#request{game_end_content = decode_message_game_end_impl(Bin)};
        (7,{length_encoded,Bin},Rec) -> Rec#request{game_start_content = decode_message_game_start_impl(Bin)};
        (8,{length_encoded,Bin},Rec) -> Rec#request{login_sucess_content = decode_messagelogin_success_impl(Bin)};
        (9,{length_encoded,Bin},Rec) -> Rec#request{difficult_change_content = decode_message_difficult_change_impl(Bin)};
        (10,{length_encoded,Bin},Rec) -> Rec#request{user_disconected_content = decode_message_user_disconected_impl(Bin)};
        (11,{length_encoded,Bin},Rec) -> Rec#request{restart_game_content = decode_message_restart_game_impl(Bin)};
        (12,{length_encoded,Bin},Rec) -> Rec#request{power_content = decode_message_generic_power_impl(Bin)};
        (13,{length_encoded,Bin},Rec) -> Rec#request{enter_queue_content = decode_message_enter_queue_impl(Bin)};
        (14,{length_encoded,Bin},Rec) -> Rec#request{match_found_content = decode_message_match_found_impl(Bin)};
        (15,{length_encoded,Bin},Rec) -> Rec#request{generated_garbage_content = decode_message_generated_garbage_impl(Bin)}
      end).

encode_request(undefined) -> undefined;
encode_request(R) when is_record(R,request) ->
  [
    protocol_buffers:encode(1,int32,from_request__request_type(R#request.type)),
    protocol_buffers:encode(2,length_encoded,encode_message_login(R#request.login_content)),
    protocol_buffers:encode(3,length_encoded,encode_message_place_piece(R#request.place_piece_content)),
    protocol_buffers:encode(4,length_encoded,encode_message_update_piece(R#request.update_piece_content)),
    protocol_buffers:encode(5,length_encoded,encode_message_opponent_place_piece(R#request.opponent_place_piece_content)),
    protocol_buffers:encode(6,length_encoded,encode_message_game_end(R#request.game_end_content)),
    protocol_buffers:encode(7,length_encoded,encode_message_game_start(R#request.game_start_content)),
    protocol_buffers:encode(8,length_encoded,encode_messagelogin_success(R#request.login_sucess_content)),
    protocol_buffers:encode(9,length_encoded,encode_message_difficult_change(R#request.difficult_change_content)),
    protocol_buffers:encode(10,length_encoded,encode_message_user_disconected(R#request.user_disconected_content)),
    protocol_buffers:encode(11,length_encoded,encode_message_restart_game(R#request.restart_game_content)),
    protocol_buffers:encode(12,length_encoded,encode_message_generic_power(R#request.power_content)),
    protocol_buffers:encode(13,length_encoded,encode_message_enter_queue(R#request.enter_queue_content)),
    protocol_buffers:encode(14,length_encoded,encode_message_match_found(R#request.match_found_content)),
    protocol_buffers:encode(15,length_encoded,encode_message_generated_garbage(R#request.generated_garbage_content))
  ].

