-module(message_processor).

-include("include/softstate.hrl").
-include("include/protocol_pb.hrl").

-export([process/2 , process_pre_login_message/1, handle_disconect/0, handle_connect/0, process_message/4, process_user_disconect/2]).

-export([create_lost_message/1,create_won_message/1, create_difficult_message/1,create_disconect_message/0, create_use_power_message/1]).
-export([create_login_success/4, create_login_success/18]).
-export([create_match_created_message/2, create_start_message/1]).
-export([create_user_disconects_message/1, create_game_restarts_message/2, create_user_reconected_message/0]).
-export([create_opponent_place_piece_message/7, create_generated_garbage_message/2 ]).
-export([create_update_piece_message/3, create_new_configuration_message/2]).
-export([create_rematch_message/0, create_no_rematch_message/0, create_rematch_timeout_message/0]).
-export([create_insufficient_lifes_message/0 ]).

-export([create_fail_buy_product_response_message/0, create_success_buy_product_response_message/2]).

-export([create_time_sync_message/2, create_debug_board/12]).



-define(DISCONECT_RESPONSE,<<"you sir are out of order">>).


-define(GAME_END_OPPONNENT_LOST , 1).
-define(GAME_END_OPPONNENT_WON , 2).
-define(GAME_END_OPPONNENT_DISCONECT , 3).
-define(GAME_END_PLAYER_MISMATCH , 4).


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
	lager:debug("client disconect"),
	ok.



%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										MESSAGE creation
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

create_login_success( User_id, Configuration_url, Configuration_version, Wallet ) ->
	lager:debug("LOGIN SUCCESS WITHOUT STATE "),

	Message_wallet = #user_wallet{ items = lists:foldl( fun convert_wallet_to_protocol/2, [], Wallet ) },

	Req = #request{ type = message_login_sucess, 
						login_sucess_content = #messagelogin_success{ user_id = User_id, 
																		previous_state = lobby,
																		configuration_url = Configuration_url,
																		configuration_version = Configuration_version,
																		wallet = Message_wallet }},
	protocol_pb:encode_request(Req).



create_login_success( User_id, Configuration_url, Configuration_version, 
						Player_current_random_step, Player_current_piece_x, Player_current_piece_y, 
						Player_current_piece_angle, Player_block_list, Player_garbage_list,
							Opponent_current_random_step, Opponent_current_piece_x, Opponent_current_piece_y, 
							Opponent_current_piece_angle, Opponent_block_list, Opponent_garbage_list,
								Starting_seed, 
									Oppponent_user_id, Wallet ) ->

	lager:debug("active piece player us ~p  ~p,~p ",[Player_current_piece_angle,Player_current_piece_x,Player_current_piece_y]),
	lager:debug("active piece opponent is ~p  ~p,~p ",[Opponent_current_piece_angle,Opponent_current_piece_x,Opponent_current_piece_y]),

	Fun = fun( Block = #block{}, Result_block_list ) -> 
		New_block_position = #block_position{ 	x = Block#block.x, 
													y = Block#block.y, 
														color = get_protocol_color_from_block(Block), 
															type = get_protocol_type_from_block(Block),
																exploding_times_left = Block#block.hardness
											},
		[ New_block_position | Result_block_list]
	end,

	Opponent_block_position_list = lists:foldl(Fun, [], Opponent_block_list),
	Player_block_position_list = lists:foldl(Fun, [], Player_block_list),

%	lager:debug("Opponent_block_position_list ~p",[Opponent_block_position_list]),
%	lager:debug("Player_block_position_list ~p",[Player_block_position_list]),

	Opponent_garbage_message_list =  lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], Player_garbage_list),
	Player_garbage_message_list = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], Opponent_garbage_list),

%	lager:debug("Player_garbage_message_list ~p",[Player_garbage_message_list]),
%	lager:debug("Opponent_garbage_message_list ~p",[Opponent_garbage_message_list]),

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


	Message_wallet = #user_wallet{ items = lists:foldl( fun convert_wallet_to_protocol/2, [], Wallet ) },


	Req = #request{ type = message_login_sucess,
					login_sucess_content = #messagelogin_success{ user_id = User_id, 
																	previous_state = playing_game,
																		configuration_url = Configuration_url,
																			configuration_version = Configuration_version,
																				game_state = Message_game_state,
																					wallet = Message_wallet }},
	protocol_pb:encode_request(Req).



create_generated_garbage_message( Garbages_position_list, Garbage_id) ->
	
	lager:info("create_generated_garbage_message for ~p with garbage_id ~p",[Garbages_position_list,Garbage_id]),

	Req = #request{ type = message_generated_garbage_code,
					generated_garbage_content = #message_generated_garbage{ 
						garbage = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], lists:reverse(Garbages_position_list) ),
						garbage_id = Garbage_id
					}},
	protocol_pb:encode_request(Req).


create_opponent_place_piece_message( Garbages_position_list, _Piece = #piece{}, X, Y, Angle, Client_garbage_id, Generated_garbage_id ) ->

	lager:info("create_opponent_place_piece_message for ~p with garbage_id ~p ",[Garbages_position_list,Client_garbage_id]),

	Garbage_list_part = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], lists:reverse(Garbages_position_list) ),

	Req = #request{ type = message_opponent_place_piece_code,
					opponent_place_piece_content = #message_opponent_place_piece{ 
						garbage = Garbage_list_part,
						x = X,
						y = Y,
						state = Angle,
						garbage_id = Generated_garbage_id,
						opponent_garbage_id = Client_garbage_id
					}},
	protocol_pb:encode_request(Req).




create_match_created_message( Opponnent_name , Seed  ) ->
	Req = #request{ type = message_match_created,
					match_created_content = #message_match_created{  
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



create_lost_message(board_mismatch) ->
	Req = #request{
			type = message_game_end_code,
			game_end_content = #message_game_end{ reason = ?GAME_END_PLAYER_MISMATCH }
	},
	protocol_pb:encode_request( Req );


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



create_game_restarts_message( User_id , Start_date ) ->
	Req = #request{ type = message_game_restart, restart_game_content = #message_restart_game{ opponent = User_id, start_timestamp = Start_date } },
	protocol_pb:encode_request(Req).
	


create_user_disconects_message( User_id ) ->
	Req = #request{ type = message_user_disconected, user_disconected_content = #message_user_disconected{ opponent = User_id } },
	protocol_pb:encode_request(Req).



create_user_reconected_message() ->
	Req = #request{ type = message_user_reconected },
	protocol_pb:encode_request(Req).


create_new_configuration_message( New_version, New_version_url ) ->
	Req = #request{ type = message_new_configuration_version, 
						new_configuration_content = #message_new_configuration{  new_version = New_version, new_url = New_version_url } },
	protocol_pb:encode_request(Req).




create_fail_buy_product_response_message() ->
	Req = #request{ type = message_buy_product_response, 
						buy_product_response_content = #message_buy_product_response{ type = response_fail } },
	protocol_pb:encode_request(Req).

create_success_buy_product_response_message( Item, New_amount ) ->
	lager:debug("buy product response is ~p = ~p",[Item, New_amount]),
	Req = #request{ type = message_buy_product_response, 
						buy_product_response_content = #message_buy_product_response{ type = response_success , 
																						new_amount = #user_item{ name = Item, 
																													amount = New_amount } } },
	protocol_pb:encode_request(Req).




create_update_piece_message( Angle, X, Y) ->
	Req = #request{ type = message_update_piece_code, 
						update_piece_content = #message_update_piece{ x = X, y = Y, state = Angle } },
	protocol_pb:encode_request(Req).
	



create_time_sync_message( Client_time, Server_time ) ->
	Req = #request{ type = message_sync_time, 
						message_sync_content = #message_time_sync{ client_timestamp = Client_time, 
																	server_timestamp = Server_time } },
	protocol_pb:encode_request(Req).



create_rematch_message() ->
	Req = #request{ type = message_rematch },
	protocol_pb:encode_request(Req).
	

create_no_rematch_message() ->
	Req = #request{ type = message_no_rematch },
	protocol_pb:encode_request(Req).


create_rematch_timeout_message() ->
	Req = #request{ type = message_rematch_timeout },
	protocol_pb:encode_request(Req).


create_insufficient_lifes_message() ->
	Req = #request{ type = message_not_enough_lifes },
	protocol_pb:encode_request(Req).



create_use_power_message(Type) ->
	lager:info("message power sent ~p",[Type]),
	Req = #request{ type = message_generic_power, power_content = #message_generic_power{ type = Type } },
	protocol_pb:encode_request(Req).


create_debug_board(Player_current_random_step, Player_current_piece_x, Player_current_piece_y, 
						Player_current_piece_angle, Player_block_list, Player_garbage_list,
							Opponent_current_random_step, Opponent_current_piece_x, Opponent_current_piece_y, 
							Opponent_current_piece_angle, Opponent_block_list, Opponent_garbage_list )  ->




	Fun = fun( Block = #block{}, Result_block_list ) -> 
		New_block_position = #block_position{ 	x = Block#block.x, 
													y = Block#block.y, 
														color = get_protocol_color_from_block(Block), 
															type = get_protocol_type_from_block(Block),
																exploding_times_left = Block#block.hardness
											},
		[ New_block_position | Result_block_list]
	end,

	Opponent_block_position_list = lists:foldl(Fun, [], Opponent_block_list),
	Player_block_position_list = lists:foldl(Fun, [], Player_block_list),

%	lager:debug("Opponent_block_position_list ~p",[Opponent_block_position_list]),
%	lager:debug("Player_block_position_list ~p",[Player_block_position_list]),

	Opponent_garbage_message_list =  lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], Opponent_garbage_list),
	Player_garbage_message_list = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], Player_garbage_list),

%	lager:debug("Player_garbage_message_list ~p",[Player_garbage_message_list]),
%	lager:debug("Opponent_garbage_message_list ~p",[Opponent_garbage_message_list]),

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



	Req = #request{ type = message_debug_board, 
						debug_game_state_content =  #message_debug_board{

							opponent_state = Opponent_game_state,
							player_state = Player_game_state
						}
					},
	protocol_pb:encode_request(Req).






extract_elems_from_debug_boards( OpponentState, PlayerState ) ->
	[OpponentStateElems, PlayerStateElems] = lists:map(
			fun(PbGameState) ->
				{
						PbGameState#game_state.current_random,
						PbGameState#game_state.current_piece_x,
						PbGameState#game_state.current_piece_y,
						PbGameState#game_state.current_piece_angle,
						PbGameState#game_state.blocks, 
						PbGameState#game_state.garbage_message_list
				}
			end,
			[OpponentState, PlayerState]
	),
	{OpponentStateElems, PlayerStateElems}.



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
			{ ok, {New_guest_id , User } } = user_store:create_local_user( <<"Guest">> , 'infinity' ),
			User_initiated = user_logic:init( User ),
			login_guest_user( New_guest_id , Client_time, User_initiated );

		{undefined, _ } -> 
			{ reply_with_disconnect, create_disconect_message() };

		_other ->

			lager:info("User id no login e ~p",[User_id]),

			case user_store:login_local_user( User_id ) of
				{ error, _error } ->	{ ok, {New_guest_id , User } } = user_store:create_local_user( <<"Guest">> , 'infinity' ),
										User_initiated = user_logic:init( User ),									
										login_guest_user( New_guest_id , Client_time, User_initiated );
				{ok, User } ->			login_guest_user( User_id , Client_time, User )
			end
	end;



process_message( message_ready_code, User_process_pid, _Message_decoded, _Message_encoded ) 
			when User_process_pid =/= no_user_process ->
	lager:debug("user ~p is ready",[User_process_pid]),
	gen_server:cast( User_process_pid, { ready, no_details }),
	{no_reply};



process_message( message_enter_queue, User_process_pid, 
					#request{ enter_queue_content = #message_enter_queue{ tier = Tier, powers_equipped = Powers } }, 
						_Message_encoded )
			when User_process_pid =/= no_user_process ->

	lager:debug("user ~p enters the queue",[User_process_pid]),
	gen_server:cast( User_process_pid, { enter_queue, Tier, Powers }),
	{no_reply};



process_message( message_lost_game, User_process_pid, _Message_decoded, _Message_encoded ) 
			when User_process_pid =/= no_user_process ->
	lager:debug("user ~p said he lost",[User_process_pid]),
	gen_server:cast( User_process_pid, { lost_game, no_details }),
	{no_reply};



process_message( message_place_piece_code, 
					User_process_pid, 
						#request{ place_piece_content = Message }, 
							_Message_encoded ) 
			when User_process_pid =/= no_user_process ->

	lager:debug("place piece received to ~p with garbage id ~p",[User_process_pid,Message#message_place_piece.placed_garbage_id]),
	gen_server:cast( User_process_pid, { place_piece, 
											Message#message_place_piece.x, 
												Message#message_place_piece.y, 
													Message#message_place_piece.state,
														Message#message_place_piece.placed_garbage_id } ),
	{no_reply};



process_message( message_update_piece_code, User_process_pid, #request{ update_piece_content = Message }, Message_encoded )
			when User_process_pid =/= no_user_process ->

	gen_server:cast( User_process_pid, { update_piece, Message#message_update_piece.x, Message#message_update_piece.y, 
												Message#message_update_piece.state }),
	gen_server:cast( User_process_pid, { send_message_to_other, Message_encoded }),
	{no_reply};



process_message( message_generic_power, User_process_pid, #request{ power_content = Message }, Message_encoded )
			when User_process_pid =/= no_user_process ->
	lager:debug("~p generic power received ~p",[self(),Message#message_generic_power.type]),

	gen_server:cast( User_process_pid, { use_power, Message#message_generic_power.type }),
	{no_reply};



process_message( message_buy_product, User_process_pid, #request{ buy_product_content = Message }, _Message_encoded ) ->
	lager:debug("buy product ~p received ~p ",[Message#message_buy_product.product_id, self()]),
	gen_server:cast( User_process_pid, { buy_product, Message#message_buy_product.product_id, 1 }),
	{no_reply};




process_message( message_sync_time, User_process_pid, #request{ message_sync_content = Message }, _Message_encoded ) ->
	gen_server:cast( User_process_pid, { time_sync, Message#message_time_sync.client_timestamp }),
	{no_reply};



process_message( message_rematch, User_process_pid, #request{ message_sync_content = _Message }, _Message_encoded ) ->
	gen_server:cast( User_process_pid, message_rematch ),
	{no_reply};


process_message( message_no_rematch, User_process_pid, #request{ message_sync_content = _Message }, _Message_encoded ) ->
	gen_server:cast( User_process_pid, message_no_rematch),
	{no_reply};


process_message( message_debug_board, User_process_pid, #request{ debug_game_state_content=DebugGameStateContent }, _Message_encoded ) ->
	gen_server:cast(
		User_process_pid,
		extract_elems_from_debug_boards(
				DebugGameStateContent#message_debug_board.opponent_state,
				DebugGameStateContent#message_debug_board.player_state
		)
	),
	{no_reply};


process_message( Other_code, User_process_pid, _Message_decoded, _Message_encoded ) ->	
	lager:error("I ~p , received unkown message code ~p when user is ~p",[self(),Other_code,User_process_pid]),
	{reply_with_disconnect, create_disconect_message() }.





%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										PRIVATE FUNCTIONS
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


login_guest_user( User_id , Client_time, User ) ->
	User_creation_function = fun() ->
		lager:debug("created a new user proccess in login"),
		{ok, Child_pid } = users_sup:start_new_user_process([ self() , User_id, Client_time, User ]),
		#user{ 	user_id = User_id, user_process_pid = Child_pid }
	end,

	Relogin_User_function = fun( #user{ user_process_pid = User_pid } ) ->
		case is_process_alive( User_pid ) of
			false ->
				{save, User_creation_function() };
			true ->
				lager:debug("reconnected a user to an existing proccess"),
				gen_server:cast( User_pid, { reconnecting , self() } ),
				dont_save  
				%{save, User_creation_function() }
		end
	end,

	case server_db:login_user(User_id, User_creation_function, Relogin_User_function) of
		ok ->
			{no_reply};
		{ error, Reason } ->
			lager:error( "login failed: ~p", [Reason] ),
			{reply_with_disconnect, create_disconect_message() }
	end.


convert_wallet_to_protocol( { Item_name, Amount } , Rest_items ) ->
		[ #user_item{ name = Item_name , amount = Amount } | Rest_items].



convert_garbage_to_protocol_garbage( { Type , X } , Result ) ->
	Garbage_result = case Type of
		garbage ->						#garbage_position{ x = X, type = garbage, color = undefined};
		{garbage_color, Color } ->		#garbage_position{ x = X, type = garbage_color, color = Color};
		{garbage_hard, Hardness} ->		#garbage_position{ x = X, type = garbage_hard, color = undefined }
	end,
	[ Garbage_result | Result].



get_protocol_color_from_block( Block = #block{} ) ->
	Block#block.color.


get_protocol_type_from_block( #block{ type = Type} ) when Type == color ->
	basic_block;
get_protocol_type_from_block( #block{ type = Type} ) ->
	Type.



