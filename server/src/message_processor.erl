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



process(Msg, UserProcessPid) ->
	Request = protocol_pb:decode_request(Msg),
	lager:debug( "Received MSG:\n\t~p----------------------------------", [Request] ),

	case Request#request.type of
		undefined -> {reply_with_disconnect, create_disconect_message() };
		_other -> message_processor:process_message( Request#request.type , UserProcessPid , Request, Msg )
	end.


process_user_disconect(_UserPid, _GamePid) ->
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

create_login_success( UserId, ConfigurationUrl, ConfigurationVersion, Wallet ) ->
	lager:debug("LOGIN SUCCESS WITHOUT STATE "),

	MessageWallet = #user_wallet{ items = lists:foldl( fun convert_wallet_to_protocol/2, [], Wallet ) },

	Req = #request{ type = message_login_sucess, 
						login_sucess_content = #messagelogin_success{ user_id = UserId, 
																		previous_state = lobby,
																		configuration_url = ConfigurationUrl,
																		configuration_version = ConfigurationVersion,
																		wallet = MessageWallet }},
	protocol_pb:encode_request(Req).



create_login_success( UserId, ConfigurationUrl, ConfigurationVersion, 
						PlayerCurrentRandomStep, PlayerCurrentPiece_x, PlayerCurrentPiece_y, 
						PlayerCurrentPieceAngle, PlayerBlockList, PlayerGarbageList,
							OpponentCurrentRandomStep, OpponentCurrentPiece_x, OpponentCurrentPiece_y, 
							OpponentCurrentPieceAngle, OpponentBlockList, OpponentGarbageList,
								StartingSeed, 
									OppponentUserId, Wallet ) ->

	lager:debug("active piece player us ~p  ~p,~p ",[PlayerCurrentPieceAngle,PlayerCurrentPiece_x,PlayerCurrentPiece_y]),
	lager:debug("active piece opponent is ~p  ~p,~p ",[OpponentCurrentPieceAngle,OpponentCurrentPiece_x,OpponentCurrentPiece_y]),

	Fun = fun( Block = #block{}, ResultBlockList ) -> 
		NewBlockPosition = #block_position{ 	x = Block#block.x, 
													y = Block#block.y, 
														color = get_protocol_color_from_block(Block), 
															type = get_protocol_type_from_block(Block),
																exploding_times_left = Block#block.hardness
											},
		[ NewBlockPosition | ResultBlockList]
	end,

	OpponentBlockPositionList = lists:foldl(Fun, [], OpponentBlockList),
	PlayerBlockPositionList = lists:foldl(Fun, [], PlayerBlockList),

%	lager:debug("OpponentBlockPositionList ~p",[OpponentBlockPositionList]),
%	lager:debug("PlayerBlockPositionList ~p",[PlayerBlockPositionList]),

	OpponentGarbageMessageList =  lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], PlayerGarbageList),
	PlayerGarbageMessageList = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], OpponentGarbageList),

%	lager:debug("PlayerGarbageMessageList ~p",[PlayerGarbageMessageList]),
%	lager:debug("OpponentGarbageMessageList ~p",[OpponentGarbageMessageList]),

	OpponentGameState = #game_state{ current_random = OpponentCurrentRandomStep,
										current_piece_x = OpponentCurrentPiece_x,
											current_piece_y = OpponentCurrentPiece_y,
												current_piece_angle = OpponentCurrentPieceAngle,
													blocks = OpponentBlockPositionList,
														garbage_message_list = OpponentGarbageMessageList  },

	PlayerGameState = #game_state{ current_random = PlayerCurrentRandomStep, 
										current_piece_x = PlayerCurrentPiece_x,
											current_piece_y = PlayerCurrentPiece_y,
												current_piece_angle = PlayerCurrentPieceAngle,
													blocks = PlayerBlockPositionList,
														garbage_message_list = PlayerGarbageMessageList },

	MessageGameState = #message_game_state{ opponent_state = OpponentGameState, 
												player_state = PlayerGameState, 
													starting_seed = StartingSeed, 
														opponent_name = OppponentUserId },


	MessageWallet = #user_wallet{ items = lists:foldl( fun convert_wallet_to_protocol/2, [], Wallet ) },


	Req = #request{ type = message_login_sucess,
					login_sucess_content = #messagelogin_success{ user_id = UserId, 
																	previous_state = playing_game,
																		configuration_url = ConfigurationUrl,
																			configuration_version = ConfigurationVersion,
																				game_state = MessageGameState,
																					wallet = MessageWallet }},
	protocol_pb:encode_request(Req).



create_generated_garbage_message( GarbagePositionsList, GarbageId) ->
	
	lager:info("create_generated_garbage_message for ~p with garbage_id ~p",[GarbagePositionsList,GarbageId]),

	Req = #request{ type = message_generated_garbage_code,
					generated_garbage_content = #message_generated_garbage{ 
						garbage = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], lists:reverse(GarbagePositionsList) ),
						garbage_id = GarbageId
					}},
	protocol_pb:encode_request(Req).


create_opponent_place_piece_message( GarbagePositionsList, _Piece = #piece{}, X, Y, Angle, ClientGarbageId, GeneratedGarbageId ) ->

	lager:info("create_opponent_place_piece_message for ~p with garbage_id ~p ",[GarbagePositionsList,ClientGarbageId]),

	GarbageListPart = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], lists:reverse(GarbagePositionsList) ),

	Req = #request{ type = message_opponent_place_piece_code,
					opponent_place_piece_content = #message_opponent_place_piece{ 
						garbage = GarbageListPart,
						x = X,
						y = Y,
						state = Angle,
						garbage_id = GeneratedGarbageId,
						opponent_garbage_id = ClientGarbageId
					}},
	protocol_pb:encode_request(Req).




create_match_created_message( OpponentName , Seed  ) ->
	Req = #request{ type = message_match_created,
					match_created_content = #message_match_created{  
						seed = Seed,
						opponent_name = OpponentName,
						start_level = 0
					}},
	protocol_pb:encode_request(Req).



create_start_message( StartDate ) ->
	Req = #request{ type = message_game_start_code,
					game_start_content = #message_game_start{  
						start_timestamp = StartDate
					}},
	protocol_pb:encode_request(Req).



create_lost_message(board_mismatch) ->
	Req = #request{
			type = message_game_end_code,
			game_end_content = #message_game_end{ reason = ?GAME_END_PLAYER_MISMATCH }
	},
	protocol_pb:encode_request( Req );


create_lost_message(_LostDetails) ->
	Req = #request{ type = message_game_end_code,
					game_end_content = #message_game_end{ reason = ?GAME_END_OPPONNENT_WON } },
	protocol_pb:encode_request(Req).




create_won_message(disconect) ->
	Req = #request{ type = message_game_end_code,
					game_end_content = #message_game_end{  
						reason = ?GAME_END_OPPONNENT_DISCONECT
					}},
	protocol_pb:encode_request(Req);


create_won_message(_WonDetails) ->
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



create_game_restarts_message( UserId , StartDate ) ->
	Req = #request{ type = message_game_restart, restart_game_content = #message_restart_game{ opponent = UserId, start_timestamp = StartDate } },
	protocol_pb:encode_request(Req).
	


create_user_disconects_message( UserId ) ->
	Req = #request{ type = message_user_disconected, user_disconected_content = #message_user_disconected{ opponent = UserId } },
	protocol_pb:encode_request(Req).



create_user_reconected_message() ->
	Req = #request{ type = message_user_reconected },
	protocol_pb:encode_request(Req).


create_new_configuration_message( NewVersion, NewVersionUrl ) ->
	Req = #request{ type = message_new_configuration_version, 
						new_configuration_content = #message_new_configuration{  new_version = NewVersion, new_url = NewVersionUrl } },
	protocol_pb:encode_request(Req).




create_fail_buy_product_response_message() ->
	Req = #request{ type = message_buy_product_response, 
						buy_product_response_content = #message_buy_product_response{ type = response_fail } },
	protocol_pb:encode_request(Req).

create_success_buy_product_response_message( Item, NewAmount ) ->
	lager:debug("buy product response is ~p = ~p",[Item, NewAmount]),
	Req = #request{ type = message_buy_product_response, 
						buy_product_response_content = #message_buy_product_response{ type = response_success , 
																						new_amount = #user_item{ name = Item, 
																													amount = NewAmount } } },
	protocol_pb:encode_request(Req).




create_update_piece_message( Angle, X, Y) ->
	Req = #request{ type = message_update_piece_code, 
						update_piece_content = #message_update_piece{ x = X, y = Y, state = Angle } },
	protocol_pb:encode_request(Req).
	



create_time_sync_message( ClientTime, ServerTime ) ->
	Req = #request{ type = message_sync_time, 
						message_sync_content = #message_time_sync{ client_timestamp = ClientTime, 
																	server_timestamp = ServerTime } },
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


create_debug_board(PlayerCurrentRandomStep, PlayerCurrentPiece_x, PlayerCurrentPiece_y, 
						PlayerCurrentPieceAngle, PlayerBlockList, PlayerGarbageList,
							OpponentCurrentRandomStep, OpponentCurrentPiece_x, OpponentCurrentPiece_y, 
							OpponentCurrentPieceAngle, OpponentBlockList, OpponentGarbageList )  ->




	Fun = fun( Block = #block{}, ResultBlockList ) -> 
		NewBlockPosition = #block_position{ 	x = Block#block.x, 
													y = Block#block.y, 
														color = get_protocol_color_from_block(Block), 
															type = get_protocol_type_from_block(Block),
																exploding_times_left = Block#block.hardness
											},
		[ NewBlockPosition | ResultBlockList]
	end,

	OpponentBlockPositionList = lists:foldl(Fun, [], OpponentBlockList),
	PlayerBlockPositionList = lists:foldl(Fun, [], PlayerBlockList),

%	lager:debug("OpponentBlockPositionList ~p",[OpponentBlockPositionList]),
%	lager:debug("PlayerBlockPositionList ~p",[PlayerBlockPositionList]),

	OpponentGarbageMessageList =  lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], OpponentGarbageList),
	PlayerGarbageMessageList = lists:foldl( fun convert_garbage_to_protocol_garbage/2, [], PlayerGarbageList),

%	lager:debug("PlayerGarbageMessageList ~p",[PlayerGarbageMessageList]),
%	lager:debug("OpponentGarbageMessageList ~p",[OpponentGarbageMessageList]),

	OpponentGameState = #game_state{ current_random = OpponentCurrentRandomStep,
										current_piece_x = OpponentCurrentPiece_x,
											current_piece_y = OpponentCurrentPiece_y,
												current_piece_angle = OpponentCurrentPieceAngle,
													blocks = OpponentBlockPositionList,
														garbage_message_list = OpponentGarbageMessageList  },

	PlayerGameState = #game_state{ current_random = PlayerCurrentRandomStep, 
										current_piece_x = PlayerCurrentPiece_x,
											current_piece_y = PlayerCurrentPiece_y,
												current_piece_angle = PlayerCurrentPieceAngle,
													blocks = PlayerBlockPositionList,
														garbage_message_list = PlayerGarbageMessageList },



	Req = #request{ type = message_debug_board, 
						debug_game_state_content =  #message_debug_board{

							opponent_state = OpponentGameState,
							player_state = PlayerGameState
						}
					},
	protocol_pb:encode_request(Req).






convert_protocolbltype_to_gamebltype( garbage_color, 1 ) -> 
	color;

convert_protocolbltype_to_gamebltype( ProtocolBlType, _ ) ->
	convert_protocolbltype_to_gamebltype( ProtocolBlType ).

convert_protocolbltype_to_gamebltype( basic_block ) -> 
	color;

convert_protocolbltype_to_gamebltype( ProtocolBlType ) when is_atom(ProtocolBlType) -> 
	ProtocolBlType.

convert_protocolblcolor_to_gameblcolor( ProtocolBlColor ) when is_atom(ProtocolBlColor) -> 
	ProtocolBlColor.

convert_protocolpangle_to_gamepangle( ProtocolPAngle ) when is_atom(ProtocolPAngle) ->
	ProtocolPAngle.




convert_protocolgstate_to_gameboard( PBlocks ) ->
	lists:foldl(
		fun( PBlock, GBoard ) ->
				Type = convert_protocolbltype_to_gamebltype( 
						PBlock#block_position.type, PBlock#block_position.exploding_times_left
				),
				Color = convert_protocolblcolor_to_gameblcolor( 
						PBlock#block_position.color
				),
				Hardness = PBlock#block_position.exploding_times_left,
				X = PBlock#block_position.x,
				Y = PBlock#block_position.y,

				GBlock = #block{
						type=Type, color=Color, hardness=Hardness, x=X, y=Y
				},
				board:set_block( GBlock, X, Y, GBoard )
		end,
		board:new_empty( ?DEFAULT_BOARD_WIDTH, ?DEFAULT_BOARD_HEIGHT ),
		PBlocks
	).


convert_protocolgstate_to_usergstateelems( ProtocolGState, ProtocolLastGarbageId ) ->
	PrRandom = ProtocolGState#game_state.current_random,
	PrPieceX = ProtocolGState#game_state.current_piece_x,
	PrPieceY = ProtocolGState#game_state.current_piece_y,
	PrPieceAngle = ProtocolGState#game_state.current_piece_angle,
	PrPieceBlock1Type = ProtocolGState#game_state.current_piece_block1_type,
	PrPieceBlock2Type = ProtocolGState#game_state.current_piece_block2_type,
	PrBlocks = ProtocolGState#game_state.blocks,
%	PrGarbageMessageList = ProtocolGState#game_state.garbage_message_list,
	PrLastGarbageId = ProtocolLastGarbageId,


	UStateBoard = convert_protocolgstate_to_gameboard( PrBlocks ),
%	UStatePiece = ,
	UStatePieceX = PrPieceX,
	UStatePieceY = PrPieceY,
	UStatePieceAngle = convert_protocolpangle_to_gamepangle( PrPieceAngle ),
	
%	UStateGarbageId = ,
%	UStateGarbagePositionList = ,
	UStatePieceGenerationStep = PrRandom,

	
	GState = #user_gamestate{
		board = UStateBoard, 
		current_piece_angle = UStatePieceAngle,
		current_piece_x = UStatePieceX, current_piece_y = UStatePieceY,
		piece_generation_step = UStatePieceGenerationStep
	},
	{GState, {PrPieceBlock1Type,PrPieceBlock2Type}, ProtocolLastGarbageId}.



%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										MESSAGE PROCESSING
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


process_message( message_login_code, 
					_UserProcessPid, 
						_MessageDecoded = #request{ login_content = #message_login{ client_time = ClientTime , user_id = UserId} },
							_MessageEncoded ) ->

	case {ClientTime, UserId} of

		{ _ , undefined } ->
			{ ok, {NewGuestId , User } } = user_store:create_local_user( <<"Guest">> , 'infinity' ),
			UserInitiated = user_logic:init( User ),
			login_guest_user( NewGuestId , ClientTime, UserInitiated );

		{undefined, _ } -> 
			{ reply_with_disconnect, create_disconect_message() };

		_other ->

			lager:info("User id no login e ~p",[UserId]),

			case user_store:login_local_user( UserId ) of
				{ error, _error } ->	{ ok, {NewGuestId , User } } = user_store:create_local_user( <<"Guest">> , 'infinity' ),
										UserInitiated = user_logic:init( User ),									
										login_guest_user( NewGuestId , ClientTime, UserInitiated );
				{ok, User } ->			login_guest_user( UserId , ClientTime, User )
			end
	end;



process_message( message_ready_code, UserProcessPid, _MessageDecoded, _MessageEncoded ) 
			when UserProcessPid =/= no_user_process ->
	lager:debug("user ~p is ready",[UserProcessPid]),
	gen_server:cast( UserProcessPid, { ready, no_details }),
	{no_reply};



process_message( message_enter_queue, UserProcessPid, 
					#request{ enter_queue_content = #message_enter_queue{ tier = Tier, powers_equipped = Powers } }, 
						_MessageEncoded )
			when UserProcessPid =/= no_user_process ->

	lager:debug("user ~p enters the queue",[UserProcessPid]),
	gen_server:cast( UserProcessPid, { enter_queue, Tier, Powers }),
	{no_reply};



process_message( message_lost_game, UserProcessPid, _MessageDecoded, _MessageEncoded ) 
			when UserProcessPid =/= no_user_process ->
	lager:debug("user ~p said he lost",[UserProcessPid]),
	gen_server:cast( UserProcessPid, { lost_game, no_details }),
	{no_reply};



process_message( message_place_piece_code, 
					UserProcessPid, 
						#request{ place_piece_content = Message }, 
							_MessageEncoded ) 
			when UserProcessPid =/= no_user_process ->

	lager:debug("place piece received to ~p with garbage id ~p",[UserProcessPid,Message#message_place_piece.placed_garbage_id]),
	gen_server:cast( UserProcessPid, { place_piece, 
											Message#message_place_piece.x, 
												Message#message_place_piece.y, 
													Message#message_place_piece.state,
														Message#message_place_piece.placed_garbage_id } ),
	{no_reply};



process_message( message_update_piece_code, UserProcessPid, #request{ update_piece_content = Message }, MessageEncoded )
			when UserProcessPid =/= no_user_process ->

	gen_server:cast( UserProcessPid, { update_piece, Message#message_update_piece.x, Message#message_update_piece.y, 
												Message#message_update_piece.state }),
	gen_server:cast( UserProcessPid, { send_message_to_other, MessageEncoded }),
	{no_reply};



process_message( message_generic_power, UserProcessPid, #request{ power_content = Message }, MessageEncoded )
			when UserProcessPid =/= no_user_process ->
	lager:debug("~p generic power received ~p",[self(),Message#message_generic_power.type]),

	gen_server:cast( UserProcessPid, { use_power, Message#message_generic_power.type }),
	{no_reply};



process_message( message_buy_product, UserProcessPid, #request{ buy_product_content = Message }, _MessageEncoded ) ->
	lager:debug("buy product ~p received ~p ",[Message#message_buy_product.product_id, self()]),
	gen_server:cast( UserProcessPid, { buy_product, Message#message_buy_product.product_id, 1 }),
	{no_reply};




process_message( message_sync_time, UserProcessPid, #request{ message_sync_content = Message }, _MessageEncoded ) ->
	gen_server:cast( UserProcessPid, { time_sync, Message#message_time_sync.client_timestamp }),
	{no_reply};



process_message( message_rematch, UserProcessPid, #request{ message_sync_content = _Message }, _MessageEncoded ) ->
	gen_server:cast( UserProcessPid, message_rematch ),
	{no_reply};


process_message( message_no_rematch, UserProcessPid, #request{ message_sync_content = _Message }, _MessageEncoded ) ->
	gen_server:cast( UserProcessPid, message_no_rematch),
	{no_reply};


process_message( message_debug_board, UserProcessPid, #request{ debug_game_state_content=DebugGameStateContent }, _MessageEncoded ) ->
	Fun = fun() ->
		gen_server:cast(
			UserProcessPid,
			{
				debug_confirm_board_synch,
				{
	%				convert_protocolgstate_to_usergstateelems(
	%					DebugGameStateContent#message_debug_board.opponent_state
	%				),
					undefined,
	
					convert_protocolgstate_to_usergstateelems(
						DebugGameStateContent#message_debug_board.player_state,
						DebugGameStateContent#message_debug_board.player_last_garbage_id
					)
				}
			}
		)
	end,
	catch Fun(),
	{no_reply};


process_message( OtherCode, UserProcessPid, _MessageDecoded, _MessageEncoded ) ->	
	lager:error("I ~p , received unkown message code ~p when user is ~p",[self(),OtherCode,UserProcessPid]),
	{reply_with_disconnect, create_disconect_message() }.





%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										PRIVATE FUNCTIONS
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


login_guest_user( UserId , ClientTime, User ) ->
	UserCreationFunction = fun() ->
		lager:debug("created a new user proccess in login"),
		{ok, ChildPid } = users_sup:start_new_user_process([ self() , UserId, ClientTime, User ]),
		#user{ 	user_id = UserId, user_process_pid = ChildPid }
	end,

	ReloginUserFunction = fun( #user{ user_process_pid = UserPid } ) ->
		case is_process_alive( UserPid ) of
			false ->
				{save, UserCreationFunction() };
			true ->
				lager:debug("reconnected a user to an existing proccess"),
				gen_server:cast( UserPid, { reconnecting , self() } ),
				dont_save  
				%{save, UserCreationFunction() }
		end
	end,

	case server_db:login_user(UserId, UserCreationFunction, ReloginUserFunction) of
		ok ->
			{no_reply};
		{ error, Reason } ->
			lager:error( "login failed: ~p", [Reason] ),
			{reply_with_disconnect, create_disconect_message() }
	end.


convert_wallet_to_protocol( { ItemName, Amount } , RestItems ) ->
		[ #user_item{ name = ItemName , amount = Amount } | RestItems].



convert_garbage_to_protocol_garbage( { Type , X } , Result ) ->
	GarbageResult = case Type of
		garbage ->						#garbage_position{ x = X, type = garbage, color = undefined};
		{garbage_color, Color } ->		#garbage_position{ x = X, type = garbage_color, color = Color};
		{garbage_hard, Hardness} ->		#garbage_position{ x = X, type = garbage_hard, color = undefined, hardness = Hardness }
	end,
	[ GarbageResult | Result].



get_protocol_color_from_block( Block = #block{} ) ->
	Block#block.color.


get_protocol_type_from_block( #block{ type = Type} ) when Type == color ->
	basic_block;
get_protocol_type_from_block( #block{ type = Type} ) ->
	Type.



