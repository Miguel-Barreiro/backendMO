-module(game_protocol_pb).
-include("game_protocol_pb.hrl").

-export([
  encode_envelope/1,decode_envelope/1]).

decode_envelope(B) ->
  case decode_envelope_impl(B) of
    undefined -> #envelope{};
    Any -> Any
  end.

decode_envelope_impl(<<>>) -> undefined;
decode_envelope_impl(Binary) ->
  protocol_buffers:decode(Binary,#envelope{},
     fun(4,Val,Rec) -> Rec#envelope{uncompressed_size = protocol_buffers:cast(int32,Val)}
      end).

encode_envelope(undefined) -> undefined;
encode_envelope(R) when is_record(R,envelope) ->
  [
    protocol_buffers:encode(4,int32,R#envelope.uncompressed_size)
  ].

