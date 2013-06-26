-module(message_processor).

-include("include/softstate.hrl").
-include("include/request_macros.hrl").

-include("include/protocol_pb.hrl").

-export([process/2 , process_pre_login_message/1, handle_disconect/0, handle_connect/0, process_message/4, process_user_disconect/3]).
-export([create_lost_message/1,create_won_message/1, create_start_message/1, create_login_success/1, create_difficult_message/1,create_disconect_message/0]).

-export([create_user_disconects_message/1, create_game_state_message/2]).

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
	Req = #request{ type = message_disconect },
	protocol_pb:encode_request(Req).


create_user_disconects_message( User_id ) ->
	Req = #request{ type = message_user_disconected, user_disconected_content = #message_user_disconected{ opponent = User_id } },
	protocol_pb:encode_request(Req).





create_game_state_message( Player_game_state, Opponent_game_state ) ->
	Player_message_state = 
	case Player_game_state of 
		undefined ->	 #game_state{};
		_ -> 			Player_game_state
	end,
	Opponent_message_state = 
	case Opponent_game_state of 
		undefined ->	#game_state{};
		_ ->			Opponent_game_state
	end,
	Req = #request{ type = message_game_state, 
						game_state_content = #message_game_state{ opponent_state = Player_message_state, 
																			player_state = Opponent_message_state } 
					},
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

			User_creation_function = fun() -> 
				{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id, Client_time ]),
				#user{ 	user_id = User_id, user_process_pid = Child_pid }
			end,

			Relogin_User_function = fun( #user{ user_process_pid = User_pid } ) ->
				case is_process_alive( User_pid ) of
					false ->
						{save, User_creation_function() };
					true ->
						gen_server:cast( User_pid, { reconnecting , self() } ),
						dont_save
				end
			end,

			case server_db:login_user(User_id, User_creation_function, Relogin_User_function) of
				ok ->
					{no_reply};
				{ error, Reason } ->
					lager:error( "login failed: ~p", [Reason] ),
					{reply_with_disconnect, create_disconect_message() }
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

process_message( message_place_piece_code, 
					User_process_pid, 
						#request{ place_piece_content = #message_place_piece{ game_state = Game_state } }, 
							Message_encoded ) 
			when User_process_pid =/= no_user_process ->

	gen_server:cast( User_process_pid, { save_game_state , Game_state } ),
	gen_server:cast( User_process_pid, { send_message_to_other, Message_encoded }),
	{no_reply};

process_message( place_garbage_content, 
					User_process_pid, 
						#request{ place_garbage_content = #message_place_garbage{ game_state = Game_state } }, 
							Message_encoded ) 
			when User_process_pid =/= no_user_process ->

	gen_server:cast( User_process_pid, { save_game_state , Game_state } ),
	gen_server:cast( User_process_pid, { send_message_to_other, Message_encoded }),
	{no_reply};


process_message( Client_message_code, User_process_pid, _Message_decoded, Message_encoded )
			when User_process_pid =/= no_user_process, Client_message_code == message_update_piece_code ->
	gen_server:cast( User_process_pid, { send_message_to_other, Message_encoded }),
	{no_reply};


process_message( Other_code, _User_process_pid, _Message_decoded, _Message_encoded ) ->	
	lager:error("I ~p , received unkown message code ~p ",[self(),Other_code]),
	{reply_with_disconnect, create_disconect_message() }.
%	{reply, Response }.












