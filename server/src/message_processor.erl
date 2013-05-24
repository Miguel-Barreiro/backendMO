-module(message_processor).

-include("include/softstate.hrl").
-include("include/request_macros.hrl").

-export([process/2 , process_pre_login_message/1, handle_disconect/0, handle_connect/0]).



process_pre_login_message(Msg) ->
	lager:info("Message: ~p received", [Msg]),
	Decoded = ejson:decode(Msg),
	{[{<<"userId">>, User_id} | _ ]} = Decoded,

	{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id ]),

%	case server_db:get_user_data( User_id ) of 
%		{ error, no_user } ->
%		{ ok, UserData } ->
%		{ error, Reason }
%	end,

	User = #user{ 	user_id = User_id, 
					user_process_pid = Child_pid, 
					user_connection_pid = self(), 
					state = in_queue },

	server_db:push_user_data(User),

	{ ok , List} = server_db:get_all_unmatched_users(),
%	Response = [turn_user_into_list( User_result ) || User_result <- List],
	Response = { [ { "result", 0 } ] },
	{reply, ejson:encode(Response)}.



process(Msg, User_process_pid) ->
	{reply, "[ \"result\" : 0 ]"}.


turn_user_into_list( _User = #user{  user_id = User_id } ) ->
	{[ {"userId" , User_id} ]}.


handle_connect() ->
	ok.

handle_disconect() ->
	lager:info("client disconect"),
	ok.

	