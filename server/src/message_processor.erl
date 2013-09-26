-module(message_processor).

-include("include/softstate.hrl").
-include("include/request_macros.hrl").

-include("include/protocol_pb.hrl").

-export([process/2 , process_pre_login_message/1, handle_disconect/0, handle_connect/0, process_message/4, process_user_disconect/2]).
-export([create_lost_message/1,create_won_message/1, create_difficult_message/1,create_disconect_message/0]).

-export([create_login_success/1, create_login_success/15]).

-export([create_match_found_message/2, create_start_message/1]).
-export([create_user_disconects_message/1, create_game_restarts_message/1, create_user_reconected_message/0]).

-export([create_opponent_place_piece_message/5, create_generated_garbage_message/1 ]).
-export([create_update_piece_message/3]).

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
	Request = protocol_pb:decode_request(Msg),

	case Request#request.type of
		undefined -> {reply_with_disconnect, create_disconect_message() };
		_other -> message_processor:process_message( Request#request.type , User_process_pid , Request, Msg )
	end.


process_user_disconect(_User_pid, _Game_pid) ->
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
	lager:info("LOGIN SUCCESS WITHOUT STATE "),
	Req = #request{ type = message_login_sucess, login_sucess_content = #messagelogin_success{ user_id = User_id, previous_state = lobby }},
	protocol_pb:encode_request(Req).



create_login_success( User_id, 
						Player_current_random_step, Player_current_piece_x, Player_current_piece_y, 
						Player_current_piece_angle, Player_block_list, Player_garbage_list,
							Opponent_current_random_step, Opponent_current_piece_x, Opponent_current_piece_y, 
							Opponent_current_piece_angle, Opponent_block_list, Opponent_garbage_list,
								Starting_seed, 
									Oppponent_user_id) ->

	Fun = fun( Block = #block{}, Result_block_list ) -> 
		Color = case Block#block.type of
			color ->			Block#block.color;
			garbage ->			garbage
		end,
		New_block_position = #block_position{ x = Block#block.x, y = Block#block.y, color = Color },
		[ New_block_position | Result_block_list]
	end,

	Opponent_block_position_list = lists:foldl(Fun, [], Opponent_block_list),
	Player_block_position_list = lists:foldl(Fun, [], Player_block_list),

%	lager:info("Opponent_block_position_list ~p",[Opponent_block_position_list]),
%	lager:info("Player_block_position_list ~p",[Player_block_position_list]),

	Opponent_garbage_message_list =  lists:foldl( fun( X, Result) -> [ #garbage_position{ x = X } | Result] end , [], Player_garbage_list),
	Player_garbage_message_list = lists:foldl( fun( X, Result) -> [ #garbage_position{ x = X } | Result] end , [], Opponent_garbage_list),

%	lager:info("Player_garbage_message_list ~p",[Player_garbage_message_list]),
%	lager:info("Opponent_garbage_message_list ~p",[Opponent_garbage_message_list]),

	Opponent_game_state = #game_state{ current_random = Opponent_current_random_step,
										current_piece_x = Opponent_current_piece_x,
											current_piece_y = Opponent_current_piece_y,
												current_piece_angle = Opponent_current_piece_angle,
													blocks = Opponent_block_position_list,
														garbage_message_list = Opponent_garbage_message_list  },

	Player_game_state = #game_state{ current_random = Player_current_random_step, 
										current_piece_x = Player_current_piece_x,
											current_piece_y = Player_current_piece_y,
												current_piece_angle = Player_current_piece_angle,
													blocks = Player_block_position_list,
														garbage_message_list = Player_garbage_message_list },

	Message_game_state = #message_game_state{ opponent_state = Opponent_game_state, 
												player_state = Player_game_state, 
													starting_seed = Starting_seed, 
														opponent_name = Oppponent_user_id },

	Req = #request{ type = message_login_sucess,
					login_sucess_content = #messagelogin_success{ user_id = User_id, 
																	previous_state = playing_game, 
																		game_state = Message_game_state }},
	protocol_pb:encode_request(Req).



create_generated_garbage_message( Garbages_position_list) ->
	Req = #request{ type = message_generated_garbage_code,
					generated_garbage_content = #message_generated_garbage{ 
						garbage = lists:foldl( fun( X, Result) -> [ #garbage_position{ x = X } | Result] end , [], Garbages_position_list)
					}},
	protocol_pb:encode_request(Req).


create_opponent_place_piece_message( Garbages_position_list, _Piece = #piece{}, X, Y, Angle ) ->

	Garbage_list_part = lists:foldl( fun( X_pposition, Result) -> [ #garbage_position{ x = X_pposition } | Result] end , [], Garbages_position_list),
	Req = #request{ type = message_opponent_place_piece_code,
					opponent_place_piece_content = #message_opponent_place_piece{ 
						garbage = Garbage_list_part,
						x = X,
						y = Y,
						state = Angle
					}},
	protocol_pb:encode_request(Req).




create_match_found_message( Opponnent_name , Seed  ) ->
	Req = #request{ type = message_match_found,
					match_found_content = #message_match_found{  
						seed = Seed,
						opponent_name = Opponnent_name,
						start_level = 0
					}},
	protocol_pb:encode_request(Req).



create_start_message( Start_date ) ->
	Req = #request{ type = message_game_start_code,
					game_start_content = #message_game_start{  
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



create_game_restarts_message( User_id ) ->
	Req = #request{ type = message_game_restart, restart_game_content = #message_restart_game{ opponent = User_id } },
	protocol_pb:encode_request(Req).
	


create_user_disconects_message( User_id ) ->
	Req = #request{ type = message_user_disconected, user_disconected_content = #message_user_disconected{ opponent = User_id } },
	protocol_pb:encode_request(Req).



create_user_reconected_message() ->
	Req = #request{ type = message_user_reconected },
	protocol_pb:encode_request(Req).


create_update_piece_message( Angle, X, Y) ->
	Req = #request{ type = message_update_piece_code, 
						update_piece_content = #message_update_piece{ x = X, y = Y, state = Angle } },
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
			{ ok, New_guest_id } = persistent_db:create_user( "guest" ),
			login_guest_user( New_guest_id , Client_time );

		{undefined, _ } -> 
			{ reply_with_disconnect, create_disconect_message() };

		_other ->

			lager:info("User id no login e ~p",[User_id]),

			User_id_list = binary_to_list(User_id),
			case persistent_db:get_user_by_guest_id( User_id_list ) of
				{ error, _error } ->	{ ok, New_guest_id } = persistent_db:create_user( "Guest" ),										
										login_guest_user( New_guest_id , Client_time );
				{ok, _user } ->			login_guest_user( User_id_list , Client_time )
			end
	end;



process_message( message_ready_code, User_process_pid, _Message_decoded, _Message_encoded ) 
			when User_process_pid =/= no_user_process ->
	lager:info("user ~p is ready",[User_process_pid]),
	gen_server:cast( User_process_pid, { ready, no_details }),
	{no_reply};



process_message( message_enter_queue, User_process_pid, 
					#request{ enter_queue_content = #message_enter_queue{ tier = Tier } }, 
						_Message_encoded ) 
			when User_process_pid =/= no_user_process ->

	lager:info("user ~p enters the queue",[User_process_pid]),
	gen_server:cast( User_process_pid, { enter_queue, Tier }),
	{no_reply};



process_message( message_lost_game, User_process_pid, _Message_decoded, _Message_encoded ) 
			when User_process_pid =/= no_user_process ->
	lager:info("user ~p said he lost",[User_process_pid]),
	gen_server:cast( User_process_pid, { lost_game, no_details }),
	{no_reply};



process_message( message_place_piece_code, 
					User_process_pid, 
						#request{ place_piece_content = Message }, 
							_Message_encoded ) 
			when User_process_pid =/= no_user_process ->

	lager:info("place piece received to ~p",[User_process_pid]),
	gen_server:cast( User_process_pid, { place_piece, 
											Message#message_place_piece.x, 
												Message#message_place_piece.y, 
													Message#message_place_piece.state } ),
	{no_reply};



process_message( message_update_piece_code, User_process_pid, #request{ update_piece_content = Message }, Message_encoded )
			when User_process_pid =/= no_user_process ->

	gen_server:cast( User_process_pid, { update_piece, Message#message_update_piece.x, Message#message_update_piece.y, 
												Message#message_update_piece.state }),
	gen_server:cast( User_process_pid, { send_message_to_other, Message_encoded }),
	{no_reply};



process_message( message_generic_power, User_process_pid, _Message_decoded, Message_encoded )
			when User_process_pid =/= no_user_process ->
	lager:info("generic power received ~p",[self()]),
	gen_server:cast( User_process_pid, { send_message_to_other, Message_encoded }),
	{no_reply};



process_message( Other_code, User_process_pid, _Message_decoded, _Message_encoded ) ->	
	lager:error("I ~p , received unkown message code ~p when user is ~p",[self(),Other_code,User_process_pid]),
	{reply_with_disconnect, create_disconect_message() }.






%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										PRIVATE FUNCTIONS
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


login_guest_user( User_id , Client_time ) ->
	User_creation_function = fun() -> 
		lager:info("created a new user proccess in login"),
		{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id, Client_time ]),
		#user{ 	user_id = User_id, user_process_pid = Child_pid }
	end,

	Relogin_User_function = fun( #user{ user_process_pid = User_pid } ) ->
		case is_process_alive( User_pid ) of
			false ->
				{save, User_creation_function() };
			true ->
				lager:info("reconnected a user to an existing proccess"),
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
	end.






