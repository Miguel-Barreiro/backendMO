-module(swiss).

-export([unix_timestamp/0, unix_timestamp_ms/0, subscribe/1, notify/2, ip_to_binary/1, string_join/2, string_replace/3, to_integer/1, send_email/6, get_localhostname/0, get_appversion_str/0]).

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

to_list(Value) when is_list(Value) -> Value;
to_list(Value) when is_binary(Value) -> list_to_binary(Value).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> list_to_binary(Value).


send_email( {Relay,Username,Password}, SenderName, Sender, Recipients, Subject, Body ) when
		(is_list(SenderName) orelse is_binary(SenderName)) andalso (is_list(Sender) orelse is_binary(Sender))
			andalso is_list(Recipients) andalso (is_list(Subject) orelse is_binary(Subject))
			andalso (is_list(Body) orelse is_binary(Body))
->
	BinSenderName = to_binary(SenderName),
	BinSender = to_binary(Sender),
	BinJoinedRecipients = to_binary( string:join( lists:map( fun(R)->to_list(R)end, Recipients ), "," ) ), 
	BinSubject = to_binary(Subject),
	BinBody = to_binary(Body),
	FinalBody = <<
			<<"From: \"">>/binary, BinSenderName/binary, <<"\" <">>/binary, BinSender/binary, <<">\n">>/binary,
			<<"To: ">>/binary, BinJoinedRecipients/binary,  <<"\n">>/binary,
			<<"Subject: ">>/binary, BinSubject/binary, <<"\n">>/binary,
			<<"\n">>/binary,
			BinBody/binary
	>>,
	{ok, _Pid} = gen_smtp_client:send( {Sender, Recipients, FinalBody}, [{relay, Relay}, {username, Username}, {password, Password}] ),
	ok.


get_localhostname() ->
	{ok, LocalHostName} = inet:gethostname(),
	LocalHostName.

get_appversion_str() ->
	case {application:get_env(server,git_commit), application:get_env(server,git_branch)} of 
		{{ok,Commit},{ok,Branch}} ->
			"Git, " ++ Commit ++ " @ branch '" ++ Branch ++ "'";
		_ ->
			"UNKNOWN"
	end.


	


