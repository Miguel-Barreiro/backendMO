-module(message_processor).

-include("include/softstate.hrl").
-include("include/request_macros.hrl").

-export([process/2 , process_pre_login_message/1, handle_disconect/0, handle_connect/0]).


-define(MESSAGE_LOGIN_CODE,<<"1">>).
-define(MESSAGE_PLACE_PIECE_CODE,<<"2">>).
-define(MESSAGE_UPDATE_PIECE_CODE,<<"3">>).
-define(MESSAGE_PLACE_GARBAGE_CODE,<<"4">>).
-define(MESSAGE_GAME_END_CODE,<<"5">>).
-define(MESSAGE_GAME_START_CODE,<<"6">>).
-define(MESSAGE_READY_CODE,<<"7">>).


-define(DISCONECT_RESPONSE,<<"you sir are out of order">>).

process_pre_login_message(Msg) ->
	lager:info("Message: ~p received", [Msg]),
	Decoded = ejson:decode(Msg),

	case lists:keysearch(<<"code">>, 1, Decoded) of
		{ Message_code, _Tuple} -> 
			message_processor:process_message( Message_code , no_user_process , Decoded, Msg );
		false ->
			Response = ejson:encode( {[ { <<"reason">> , ?DISCONECT_RESPONSE } ]} ),
			{reply_with_disconnect, Response }
	end.

process(Msg, User_process_pid) ->
	lager:info("Message: ~p received", [Msg]),
	Decoded = ejson:decode(Msg),

	case lists:keysearch(<<"code">>, 1, Decoded) of
		{ Message_code, _Tuple} ->
			message_processor:process_message( Message_code , User_process_pid, Decoded, Msg );
		false ->
			Response = ejson:encode( {[ { <<"reason">> , ?DISCONECT_RESPONSE } ]} ),
			{reply_with_disconnect, Response }
	end.



turn_user_into_list( _User = #user{  user_id = User_id } ) ->
	{[ {"userId" , User_id} ]}.


handle_connect() ->
	ok.

handle_disconect() ->
	lager:info("client disconect"),
	ok.

	



%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										MESSAGE PROCESSING
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
process_message( ?MESSAGE_LOGIN_CODE, User_process_pid, Message_decoded, _Message_encoded ) ->

	case lists:keysearch(<<"userId">>, 1, Message_decoded) of
		{ User_id, _Tuple} ->
			case server_db:get_user_data( User_id ) of 
				{ error, no_user } ->

					%% @WARNING: THIS SHOULD BE DONE INSIDE A TRANSATION SO if 2 people try to login at the same time with the same id 2 user processes dont get spawn

					{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id ]),
					User = #user{ 	user_id = User_id, 
									user_process_pid = Child_pid,
									user_connection_pid = self(), 
									state = in_queue },

					ok = server_db:push_user_data(User),
					{no_reply};

				{ ok, _User_data = #user{  user_process_pid = User_pid } } when User_pid == undefined ->
					{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id ]),
					{no_reply};

				{ ok, _User_data = #user{ user_process_pid = _User_pid } } ->
					{no_reply};

				{ error, Reason } ->
					lager:error("Couldnt get user. ~p",[Reason]),
					{no_reply}
			end;
		false ->
			Response = ejson:encode( {[ { <<"reason">> , ?DISCONECT_RESPONSE } ]} ),
			{reply_with_disconnect, Response }
	end;

process_message( ?MESSAGE_PLACE_PIECE_CODE, User_process_pid, Message_decoded, Message_encoded ) ->
	{no_reply};

process_message( Other_code, _User_process_pid, _Message_decoded, _Message_encoded ) ->
	Response = ejson:encode( {[ { <<"reason">> , ?DISCONECT_RESPONSE }, { <<"code_given">> , Other_code } ]} ),
	{reply_with_disconnect, Response }.












