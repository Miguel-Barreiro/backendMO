-module(message_processor).

-include("include/softstate.hrl").

-export([process/1]).


process(Msg)->
	lager:info("Message: ~p received", [Msg]),
	Decoded = ejson:decode(Msg),
	{[{<<"userId">>, Id}]} = Decoded,
	User = #user{ userid = Id, pid = self(), state = in_queue },
	lager:info("save resulted: ~p ", [server_db:push_user_data(User)]),
%	{reply, "received " ++ Msg}.
	{ ok , List} = server_db:get_unmatched_all_users(),
	Response = [turn_user_into_list( User_result ) || User_result <- List],
	{reply, ejson:encode(Response)}.



turn_user_into_list( User = #user{  userid = UserId } ) ->
	{[ {"userId" , UserId} ]}.

