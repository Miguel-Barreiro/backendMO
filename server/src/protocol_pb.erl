-module(protocol_pb).
-include("protocol_pb.hrl").

-export([
  encode_block_position/1,decode_block_position/1,
  encode_message_garbage_list/1,decode_message_garbage_list/1,
  encode_game_state/1,decode_game_state/1,
  encode_message_login/1,decode_message_login/1,
  encode_messagelogin_success/1,decode_messagelogin_success/1,
  encode_message_game_start/1,decode_message_game_start/1,
  encode_message_game_end/1,decode_message_game_end/1,
  encode_message_update_piece/1,decode_message_update_piece/1,
  encode_message_place_piece/1,decode_message_place_piece/1,
  encode_message_place_garbage/1,decode_message_place_garbage/1,
  encode_message_difficult_change/1,decode_message_difficult_change/1,
  encode_message_game_state/1,decode_message_game_state/1,
  encode_message_user_disconected/1,decode_message_user_disconected/1,
  to_request__request_type/1,from_request__request_type/1,
  encode_request/1,decode_request/1]).

decode_block_position(B) ->
  case decode_block_position_impl(B) of
    undefined -> #block_position{};
    Any -> Any
  end.

decode_block_position_impl(<<>>) -> undefined;
decode_block_position_impl(Binary) ->
  protocol_buffers:decode(Binary,#block_position{},
     fun(1,Val,Rec) -> Rec#block_position{position = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#block_position{color = protocol_buffers:cast(int32,Val)}
      end).

encode_block_position(undefined) -> undefined;
encode_block_position(R) when is_record(R,block_position) ->
  [
    protocol_buffers:encode(1,int32,R#block_position.position),
    protocol_buffers:encode(2,int32,R#block_position.color)
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
        (2,Val,Rec) -> Rec#game_state{current_piece_position = protocol_buffers:cast(int32,Val)};
        (3,Val,Rec) -> Rec#game_state{current_piece_state = protocol_buffers:cast(int32,Val)};
        (4,Val,Rec) -> Rec#game_state{current_piece_color = protocol_buffers:cast(int32,Val)};
        (5,{length_encoded,Bin},#game_state{blocks=F}=Rec) when is_list(F) -> Rec#game_state{blocks = Rec#game_state.blocks ++ [decode_block_position_impl(Bin)]}
;
        (6,{length_encoded,Bin},#game_state{garbage_message_list=F}=Rec) when is_list(F) -> Rec#game_state{garbage_message_list = Rec#game_state.garbage_message_list ++ [decode_message_garbage_list_impl(Bin)]}

      end).

encode_game_state(undefined) -> undefined;
encode_game_state(R) when is_record(R,game_state) ->
  [
    protocol_buffers:encode(1,int32,R#game_state.current_random),
    protocol_buffers:encode(2,int32,R#game_state.current_piece_position),
    protocol_buffers:encode(3,int32,R#game_state.current_piece_state),
    protocol_buffers:encode(4,int32,R#game_state.current_piece_color),
    [ protocol_buffers:encode(5,length_encoded,encode_block_position(X)) || X <- R#game_state.blocks],
    [ protocol_buffers:encode(6,length_encoded,encode_message_garbage_list(X)) || X <- R#game_state.garbage_message_list]
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

decode_messagelogin_success(B) ->
  case decode_messagelogin_success_impl(B) of
    undefined -> #messagelogin_success{};
    Any -> Any
  end.

decode_messagelogin_success_impl(<<>>) -> undefined;
decode_messagelogin_success_impl(Binary) ->
  protocol_buffers:decode(Binary,#messagelogin_success{},
     fun(1,Val,Rec) -> Rec#messagelogin_success{user_id = protocol_buffers:cast(string,Val)}
      end).

encode_messagelogin_success(undefined) -> undefined;
encode_messagelogin_success(R) when is_record(R,messagelogin_success) ->
  [
    protocol_buffers:encode(1,length_encoded,R#messagelogin_success.user_id)
  ].

decode_message_game_start(B) ->
  case decode_message_game_start_impl(B) of
    undefined -> #message_game_start{};
    Any -> Any
  end.

decode_message_game_start_impl(<<>>) -> undefined;
decode_message_game_start_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_game_start{},
     fun(1,Val,Rec) -> Rec#message_game_start{seed = protocol_buffers:cast(int32,Val)};
        (2,Val,Rec) -> Rec#message_game_start{opponent_name = protocol_buffers:cast(string,Val)};
        (3,Val,Rec) -> Rec#message_game_start{start_level = protocol_buffers:cast(int32,Val)};
        (4,Val,Rec) -> Rec#message_game_start{start_timestamp = protocol_buffers:cast(int32,Val)}
      end).

encode_message_game_start(undefined) -> undefined;
encode_message_game_start(R) when is_record(R,message_game_start) ->
  [
    protocol_buffers:encode(1,int32,R#message_game_start.seed),
    protocol_buffers:encode(2,length_encoded,R#message_game_start.opponent_name),
    protocol_buffers:encode(3,int32,R#message_game_start.start_level),
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
        (3,Val,Rec) -> Rec#message_update_piece{state = protocol_buffers:cast(int32,Val)}
      end).

encode_message_update_piece(undefined) -> undefined;
encode_message_update_piece(R) when is_record(R,message_update_piece) ->
  [
    protocol_buffers:encode(1,int32,R#message_update_piece.x),
    protocol_buffers:encode(2,int32,R#message_update_piece.y),
    protocol_buffers:encode(3,int32,R#message_update_piece.state)
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
        (3,Val,Rec) -> Rec#message_place_piece{state = protocol_buffers:cast(int32,Val)};
        (4,{length_encoded,Bin},Rec) -> Rec#message_place_piece{game_state = decode_game_state_impl(Bin)}
      end).

encode_message_place_piece(undefined) -> undefined;
encode_message_place_piece(R) when is_record(R,message_place_piece) ->
  [
    protocol_buffers:encode(1,int32,R#message_place_piece.x),
    protocol_buffers:encode(2,int32,R#message_place_piece.y),
    protocol_buffers:encode(3,int32,R#message_place_piece.state),
    protocol_buffers:encode(4,length_encoded,encode_game_state(R#message_place_piece.game_state))
  ].

decode_message_place_garbage(B) ->
  case decode_message_place_garbage_impl(B) of
    undefined -> #message_place_garbage{};
    Any -> Any
  end.

decode_message_place_garbage_impl(<<>>) -> undefined;
decode_message_place_garbage_impl(Binary) ->
  protocol_buffers:decode(Binary,#message_place_garbage{},
     fun(1,{length_encoded,Bin},Rec) -> Rec#message_place_garbage{garbage = decode_message_garbage_list_impl(Bin)}
      end).

encode_message_place_garbage(undefined) -> undefined;
encode_message_place_garbage(R) when is_record(R,message_place_garbage) ->
  [
    protocol_buffers:encode(1,length_encoded,encode_message_garbage_list(R#message_place_garbage.garbage))
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

to_request__request_type(1) -> message_login_code;
to_request__request_type(2) -> message_place_piece_code;
to_request__request_type(3) -> message_update_piece_code;
to_request__request_type(4) -> message_place_garbage_code;
to_request__request_type(5) -> message_game_end_code;
to_request__request_type(6) -> message_game_start_code;
to_request__request_type(7) -> message_ready_code;
to_request__request_type(8) -> message_lost_game;
to_request__request_type(9) -> message_login_sucess;
to_request__request_type(10) -> message_disconect;
to_request__request_type(11) -> message_difficult_change;
to_request__request_type(12) -> message_get_game_state;
to_request__request_type(13) -> message_game_state;
to_request__request_type(14) -> message_user_disconected;
to_request__request_type(undefined) -> undefined.

from_request__request_type(message_login_code) -> 1;
from_request__request_type(message_place_piece_code) -> 2;
from_request__request_type(message_update_piece_code) -> 3;
from_request__request_type(message_place_garbage_code) -> 4;
from_request__request_type(message_game_end_code) -> 5;
from_request__request_type(message_game_start_code) -> 6;
from_request__request_type(message_ready_code) -> 7;
from_request__request_type(message_lost_game) -> 8;
from_request__request_type(message_login_sucess) -> 9;
from_request__request_type(message_disconect) -> 10;
from_request__request_type(message_difficult_change) -> 11;
from_request__request_type(message_get_game_state) -> 12;
from_request__request_type(message_game_state) -> 13;
from_request__request_type(message_user_disconected) -> 14;
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
        (5,{length_encoded,Bin},Rec) -> Rec#request{place_garbage_content = decode_message_place_garbage_impl(Bin)};
        (6,{length_encoded,Bin},Rec) -> Rec#request{game_end_content = decode_message_game_end_impl(Bin)};
        (7,{length_encoded,Bin},Rec) -> Rec#request{game_start_content = decode_message_game_start_impl(Bin)};
        (8,{length_encoded,Bin},Rec) -> Rec#request{login_sucess_content = decode_messagelogin_success_impl(Bin)};
        (9,{length_encoded,Bin},Rec) -> Rec#request{difficult_change_content = decode_message_difficult_change_impl(Bin)};
        (10,{length_encoded,Bin},Rec) -> Rec#request{game_state_content = decode_message_game_state_impl(Bin)};
        (11,{length_encoded,Bin},Rec) -> Rec#request{user_disconected_content = decode_message_user_disconected_impl(Bin)}
      end).

encode_request(undefined) -> undefined;
encode_request(R) when is_record(R,request) ->
  [
    protocol_buffers:encode(1,int32,from_request__request_type(R#request.type)),
    protocol_buffers:encode(2,length_encoded,encode_message_login(R#request.login_content)),
    protocol_buffers:encode(3,length_encoded,encode_message_place_piece(R#request.place_piece_content)),
    protocol_buffers:encode(4,length_encoded,encode_message_update_piece(R#request.update_piece_content)),
    protocol_buffers:encode(5,length_encoded,encode_message_place_garbage(R#request.place_garbage_content)),
    protocol_buffers:encode(6,length_encoded,encode_message_game_end(R#request.game_end_content)),
    protocol_buffers:encode(7,length_encoded,encode_message_game_start(R#request.game_start_content)),
    protocol_buffers:encode(8,length_encoded,encode_messagelogin_success(R#request.login_sucess_content)),
    protocol_buffers:encode(9,length_encoded,encode_message_difficult_change(R#request.difficult_change_content)),
    protocol_buffers:encode(10,length_encoded,encode_message_game_state(R#request.game_state_content)),
    protocol_buffers:encode(11,length_encoded,encode_message_user_disconected(R#request.user_disconected_content))
  ].

