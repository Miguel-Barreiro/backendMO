-module(swiss).

-export([unix_timestamp/0, unix_timestamp_ms/0, subscribe/1, notify/2, ip_to_binary/1, string_join/2, string_replace/3, to_integer/1]).

-include("include/softstate.hrl").

unix_timestamp() ->
    {Msec, Sec, _} = now(),
    Msec * 1000000 + Sec.

unix_timestamp_ms() ->
    {A, B, C} = now(),
    (((A * 1000000) + B) * 1000) + C div 1000.

subscribe(EventType) ->
    gproc:reg({p, l, EventType}).

notify(EventType, Msg) ->
    gproc:send({p, l, EventType}, {EventType, Msg}).

ip_to_binary({A,B,C,D}) ->
    list_to_binary(io_lib:format("~p.~p.~p.~p", [A,B,C,D])).

string_replace(Needle, Haystack, Replace) ->
    case lists:splitwith(fun(C) -> C =/= Needle end, Haystack) of
        {L1, []} -> L1;
        {L1, [_]} -> L1 ++ Replace;
        {L1, [_ | L2]} -> L1 ++ Replace ++ L2
    end.

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(string_join1(Items, Sep, []))).

string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];
string_join1([Head | Tail], Sep, Acc) ->
    string_join1(Tail, Sep, [Sep, Head | Acc]).


to_integer(Value) when is_integer(Value) -> Value;
to_integer(Value) when is_binary(Value) -> list_to_integer(binary_to_list(Value)).
