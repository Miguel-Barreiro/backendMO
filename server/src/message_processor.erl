-module(message_processor).

-include("include/softstate.hrl").
-include("include/request_macros.hrl").

-include("include/protocol_pb.hrl").

-export([process/2 , process_pre_login_message/1, handle_disconect/0, handle_connect/0, process_message/4, process_user_disconect/3]).
-export([create_lost_message/1,create_won_message/1, create_start_message/1, create_login_success/1, create_difficult_message/1]).

-define(MESSAGE_LOGIN_CODE, 1).
-define(MESSAGE_READY_CODE,7).
-define(MESSAGE_LOST_CODE,8).
-define(MESSAGE_LOGIN_SUCESS,9).
-define(MESSAGE_GAME_END_CODE,5).

-define(MESSAGE_PLACE_PIECE_CODE,2).
-define(MESSAGE_GAME_START_CODE,6).
-define(MESSAGE_UPDATE_PIECE_CODE,3).
-define(MESSAGE_PLACE_GARBAGE_CODE,4).

-define(MESSAGE_DIFFICULT_CHANGE,11).

-define(DISCONECT_RESPONSE,<<"you sir are out of order">>).


-define( GAME_END_OPPONNENT_LOST , 1).
-define( GAME_END_OPPONNENT_WON , 2).
-define( GAME_END_OPPONNENT_DISCONECT , 3).

process_pre_login_message(Msg) ->
	Request = protocol_pb:decode_request(Msg),

	case Request#request.type of
		undefined -> {reply_with_disconnect, create_disconect_message() };
		_other -> message_processor:process_message( Request#request.type , no_user_process , Request, Msg )
	end.



process(Msg, User_process_pid) ->
	lager:debug("Message: ~p received", [Msg]),
	Request = protocol_pb:decode_request(Msg),

	case Request#request.type of
		undefined -> {reply_with_disconnect, create_disconect_message() };
		_other -> message_processor:process_message( Request#request.type , User_process_pid , Request, Msg )
	end.


process_user_disconect( _Disconected_pid, User_pid, _Game_pid) ->
	gen_server:cast( User_pid ,{send_won_message, disconect }),
	ok.

handle_connect() ->
	ok.

handle_disconect() ->
	lager:info("client disconect"),
	ok.



%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										MESSAGE creation
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

create_login_success( User_id ) ->
	Req = #request{ type = message_login_sucess,
					login_sucess_content = #messagelogin_success{ user_id = User_id }},
	protocol_pb:encode_request(Req).

create_start_message( { Opponnent_name , Start_date, Seed } ) ->
	Req = #request{ type = message_game_start_code,
					game_start_content = #message_game_start{  
						seed = Seed,
						opponent_name = Opponnent_name,
						start_level = 0,
						start_timestamp = Start_date
					}},
	protocol_pb:encode_request(Req).

create_lost_message(_Lost_details) ->
	Req = #request{ type = message_game_end_code,
					game_end_content = #message_game_end{ reason = ?GAME_END_OPPONNENT_WON } },
	protocol_pb:encode_request(Req).


create_won_message(disconect) ->
	Req = #request{ type = message_game_end_code,
					game_end_content = #message_game_end{  
						reason = ?GAME_END_OPPONNENT_DISCONECT
					}},
	protocol_pb:encode_request(Req);


create_won_message(_Won_details) ->
	Req = #request{ type = message_game_end_code,
					game_end_content = #message_game_end{  
						reason = ?GAME_END_OPPONNENT_LOST
					}},
	protocol_pb:encode_request(Req).

create_difficult_message( Level ) ->
	Req = #request{ type = message_difficult_change,
					difficult_change_content = #message_difficult_change{  
						level = Level
					}},
	protocol_pb:encode_request(Req).


create_disconect_message() ->
	Req = #request{ type = message_disconect, disconect_content = #message_disconect{} },
	protocol_pb:encode_request(Req).

%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										MESSAGE PROCESSING
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



process_message( message_login_code, 
					_User_process_pid, 
						_Message_decoded = #request{ login_content = #message_login{ client_time = Client_time , user_id = User_id} },
							_Message_encoded ) ->

	case {Client_time, User_id} of
		{ _ , undefined } ->
			{reply_with_disconnect, create_disconect_message() };
		{undefined, _ } -> 
			{reply_with_disconnect, create_disconect_message() };
		_other ->
			case server_db:get_user_data( User_id ) of 
				{ error, no_user } ->
					%% @WARNING: THIS SHOULD BE DONE INSIDE A TRANSATION SO if 2 people try to login at the same time with the same id 2 user processes dont get spawn
					{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id, Client_time ]),
					User = #user{ 	user_id = User_id,
									user_process_pid = Child_pid,
									user_connection_pid = self(), 
									state = in_queue },

					ok = server_db:push_user_data(User),
					{no_reply};

				{ ok, User_data = #user{  user_process_pid = User_pid } } when User_pid == undefined ->
					{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id, Client_time ]),

					User = User_data#user{ 	user_process_pid = Child_pid,
											user_connection_pid = self(), 
											state = in_queue },

					ok = server_db:push_user_data(User),

					{no_reply};

				{ ok, User_data = #user{ user_process_pid = User_pid } } ->
					case is_process_alive( User_pid ) of
						false->
							{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id, Client_time ]),
							User = User_data#user{ 	user_process_pid = Child_pid,
													user_connection_pid = self(), 
													state = in_queue };
						true->
							gen_server:cast( User_pid, { reconnecting , User_id, self()} ),
							User = User_data#user{  user_connection_pid = self(), 
													state = in_queue }
					end,
					ok = server_db:push_user_data(User),
					{no_reply};

				{ error, Reason } ->
					lager:error("Couldnt get user. ~p",[Reason]),
					{no_reply}
			end
	end;

process_message( message_ready_code, User_process_pid, _Message_decoded, _Message_encoded ) 
			when User_process_pid =/= no_user_process ->
	lager:info("user ~p is ready",[User_process_pid]),
	gen_server:cast( User_process_pid, { ready, no_details }),
	{no_reply};

process_message( message_lost_game, User_process_pid, _Message_decoded, _Message_encoded ) 
			when User_process_pid =/= no_user_process ->
	lager:info("user ~p said he lost",[User_process_pid]),
	gen_server:cast( User_process_pid, { lost_game, no_details }),
	{no_reply};

process_message( Client_message_code, User_process_pid, _Message_decoded, Message_encoded )
			when User_process_pid =/= no_user_process,
					Client_message_code == message_update_piece_code orelse
					Client_message_code == message_place_garbage_code orelse
					Client_message_code == message_place_piece_code ->
	gen_server:cast( User_process_pid, { send_message_to_other, Message_encoded }),
	{no_reply};


process_message( Other_code, _User_process_pid, _Message_decoded, _Message_encoded ) ->	
	lager:error("I ~p , received unkown message code ~p ",[self(),Other_code]),
	{reply_with_disconnect, create_disconect_message() }.
%	{reply, Response }.












