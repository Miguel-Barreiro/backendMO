-module(game_logic).

-include("include/softstate.hrl").

-export([ handle_place_piece/6, handle_update_piece/5, create_new_game/5, handle_power_use/3 ]).

-export([ activate_ability_blocks/2, apply_gravity_combo_loop/2]).

-define(BOARD_WIDTH , ?DEFAULT_BOARD_WIDTH).
-define(BOARD_HEIGHT , ?DEFAULT_BOARD_HEIGHT).
-define(STARTING_PIECE_X , 3).


%-------------- PUBLIC -------------------------


create_new_game( User1Pid, User1Powers, User2Pid, User2Powers, InitialSeed  ) ->

	{ NewRandomState, Piece } = calculate_next_piece( InitialSeed ),

	User1Gamestate = #user_gamestate{ user_pid = User1Pid,
										board = board:new_empty( ?BOARD_WIDTH, ?BOARD_HEIGHT ),
											current_piece = Piece,
												random_state = NewRandomState },

	User2Gamestate = #user_gamestate{ user_pid = User2Pid,
										board = board:new_empty( ?BOARD_WIDTH, ?BOARD_HEIGHT ),
											current_piece = Piece,
												random_state = NewRandomState },

	GameRules = game_rules:get_current_rules(<<"Normal">>),

	User1FinalGamestate = equip_powers(User1Gamestate,User1Powers),
	User2FinalGamestate = equip_powers(User2Gamestate,User2Powers),

	#game{ user1_gamestate = User1FinalGamestate, 
			user2_gamestate = User2FinalGamestate,
				initial_seed = InitialSeed, 
					game_rules = GameRules }.





equip_powers( UserGamestate = #user_gamestate{}, Powers )->
	UserGamestate#user_gamestate{ board = equip_powers(UserGamestate#user_gamestate.board, Powers) };
equip_powers( Board = #board{}, [] )->
	Board;
equip_powers( Board = #board{}, [ Power | Rest ] )->
	NewBoard = case Power of
		<<"frenzypower">> ->			Board#board{ active_powers_equiped = [ frenzy | Board#board.active_powers_equiped] };
		<<"trashpower">> ->				Board#board{ active_powers_equiped = [ trash | Board#board.active_powers_equiped] };
		<<"redbuttonpower">> ->			Board#board{ active_powers_equiped = [ redbutton | Board#board.active_powers_equiped] };
		<<"overloadpower">> ->			Board#board{ is_overload_active = true };
		<<"killingblowpower">> ->		Board#board{ killing_blow_active = true };
		<<"barrierpower">> ->			Board#board{ barrier_active = true }
	end,
	equip_powers( NewBoard, Rest ).





handle_power_use( UserPid, Power, Game = #game{} ) when UserPid == (Game#game.user1_gamestate)#user_gamestate.user_pid -> 

	io:format("\n-------------------------------- \n",[]),
	io:format("\n USER 1 ~p USED POWER \n",[UserPid]),

	OpponentPid = (Game#game.user2_gamestate)#user_gamestate.user_pid,
	{NewGamestate, NewOpponentGamestate} = handle_power_use( Power, UserPid, OpponentPid, 
																	Game#game.user1_gamestate, Game#game.user2_gamestate, 
																		Game#game.game_rules),
	Game#game{ user1_gamestate = NewGamestate, user2_gamestate = NewOpponentGamestate };


handle_power_use( UserPid, Power, Game = #game{} ) when UserPid == (Game#game.user2_gamestate)#user_gamestate.user_pid -> 
	io:format("\n-------------------------------- \n",[]),
	io:format("\n USER 1 ~p USED POWER \n",[UserPid]),

	OpponentPid = (Game#game.user1_gamestate)#user_gamestate.user_pid,
	{NewGamestate, NewOpponentGamestate} = handle_power_use( Power, UserPid, OpponentPid, 
																	Game#game.user2_gamestate, Game#game.user1_gamestate, 
																		Game#game.game_rules),
	Game#game{ user1_gamestate = NewGamestate, user2_gamestate = NewOpponentGamestate }.


handle_power_use( Power, _UserPid, _OpponentPid, UserGamestate = #user_gamestate{}, OpponentGamestate = #user_gamestate{}, GameRules = #game_logic_rules{} ) ->
	{UserBoard, OpponentBoard} = powers_logic:handle_use_power( Power, UserGamestate#user_gamestate.board, OpponentGamestate#user_gamestate.board, GameRules),
	{ UserGamestate#user_gamestate{ board = UserBoard }, OpponentGamestate#user_gamestate{ board = OpponentBoard } }.







activate_ability_blocks( Board = #board{}, GameRules = #game_logic_rules{} ) ->

	Blocks = board:get_all_blocks(Board),

	FilterFun = fun( Block = #block{} ) ->
		case Block#block.type of
			bomb -> 				true;
			chromatic_bomb -> 		true;
			paint -> 				true;
			tornado -> 				true;
			reinforcements -> 		true;
			ghost ->				true;
			_other -> 				false
		end
	end,
	AbilityBlocks = lists:filter( FilterFun, Blocks ),
	NewBoard = activate_blocks( order_activated_block_list( AbilityBlocks ) , Board),
	BoardAfterGhosts = trigger_ghosts(NewBoard, GameRules),
	BoardWithReinforcements = release_reinforcements( BoardAfterGhosts ),
	BoardAfterAbilityChain = reset_board( BoardWithReinforcements ),
	{ New_Combos , NewBoard} = apply_gravity_combo_loop( BoardAfterAbilityChain, GameRules ).





%throws out_of_bounds (in case the user has lost)
%throws invalid_move (in case of an invalid move)
handle_place_piece( UserPid, X, Y, Angle,  Game = #game{}, ClientGarbageId ) when UserPid == (Game#game.user1_gamestate)#user_gamestate.user_pid->
	
	io:format("\n-------------------------------- \n",[]),
	io:format("\n USER 1 ~p PLACE A PIECE \n",[UserPid]),

	OpponentPid = (Game#game.user2_gamestate)#user_gamestate.user_pid,
	{NewGamestate, NewOpponentGamestate} = handle_place_piece( UserPid, 
																	OpponentPid, ClientGarbageId,
																		(Game#game.user1_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user1_gamestate, 
																					Game#game.user2_gamestate,
																						Game#game.game_rules ),


	board:print_board( NewGamestate#user_gamestate.board ),
	io:format("\n-------------------------------- \n",[]),

	Msg = message_processor:create_debug_board(NewGamestate#user_gamestate.random_state, 
													0, 0, up,
													board:get_all_blocks( NewGamestate#user_gamestate.board), 
													[],
														NewOpponentGamestate#user_gamestate.random_state, 
														0, 0, up, 
														board:get_all_blocks( NewOpponentGamestate#user_gamestate.board), [] ),
	gen_server:cast( UserPid , { send_message, Msg} ),

	Game#game{ user1_gamestate = NewGamestate, user2_gamestate = NewOpponentGamestate };



handle_place_piece( UserPid, X, Y, Angle, Game = #game{}, ClientGarbageId ) when UserPid == (Game#game.user2_gamestate)#user_gamestate.user_pid->

	io:format("\n--------------------------------\n",[]),
	io:format("\n USER 2 (~p) PLACE A PIECE \n",[UserPid]),

	OpponentPid = (Game#game.user1_gamestate)#user_gamestate.user_pid,
	{NewGamestate, NewOpponentGamestate} = handle_place_piece( UserPid, 
																	OpponentPid, ClientGarbageId,
																		(Game#game.user2_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user2_gamestate, 
																					Game#game.user1_gamestate,
																						Game#game.game_rules ),

	board:print_board( NewGamestate#user_gamestate.board ),
	io:format("\n--------------------------------\n",[]),

	Msg = message_processor:create_debug_board(NewGamestate#user_gamestate.random_state,
													0, 0, up,
													board:get_all_blocks( NewGamestate#user_gamestate.board), 
													[],
														NewOpponentGamestate#user_gamestate.random_state, 
														0, 0, up, 
														board:get_all_blocks( NewOpponentGamestate#user_gamestate.board), [] ),
	gen_server:cast( UserPid , { send_message, Msg} ),

	Game#game{ user2_gamestate = NewGamestate, user1_gamestate = NewOpponentGamestate }.




handle_place_piece( UserPid, OpponentPid, ClientGarbageId,
						Piece = #piece{}, X, Y, 
							Angle, BeginGamestate = #user_gamestate{}, BeginOpponentGamestate = #user_gamestate{}, 
								GameRules = #game_logic_rules{} ) ->

	io:format("\nplaced the piece [~p ~p] with angle ~p in ~p,~p with client_garbage_id ~p\n",
					[board:get_block_representation(Piece#piece.block1),board:get_block_representation(Piece#piece.block2),Angle,X,Y,ClientGarbageId]),

	case Piece == BeginGamestate#user_gamestate.current_piece of	
		false ->
			lager:debug("invalid piece place: wrong piece",[]),
			throw( invalid_move );
		true ->

			BoardAfterPlacePiece = place_piece( Piece, X, Y, Angle, BeginGamestate#user_gamestate.board),
			{ GeneratedGarbagePositionList, 
				GeneratedGarbageId, 
					ResultGamestate, 
						ResultOpponentGamestate} = execute_turn( BeginGamestate#user_gamestate{ 	board = BoardAfterPlacePiece#board{ reinforcements = [] } }, 
																										BeginOpponentGamestate, 
																											GameRules, 
																												ClientGarbageId ),

			send_turn_messages(UserPid, OpponentPid, GeneratedGarbagePositionList, Piece, Angle, X, Y, ClientGarbageId, GeneratedGarbageId),

			{ ResultGamestate, ResultOpponentGamestate }
	end.





execute_turn( PlayerGamestate = #user_gamestate{ board = Board}, 
				OpponentGamestate = # user_gamestate{ board = Opponent_Board}, 
					GameRules = #game_logic_rules{}, ClientGarbageId) ->

	{ NormalCombos , ResultLoopBoard } = apply_gravity_combo_loop( Board, GameRules ),

	{ RedButtonCombos, BoardAfterRedButton } = powers_logic:trigger_red_button(ResultLoopBoard, GameRules),
	CombosTotal = lists:append(RedButtonCombos, NormalCombos),			

	{GamestateAfterPiece, NextPiece} = calculate_next_piece( PlayerGamestate#user_gamestate{ board = BoardAfterRedButton}, CombosTotal, GameRules ),

	{GarbageToReleaseList, GamestateAfterGarbageRelease} = get_garbage_to_release(ClientGarbageId,GamestateAfterPiece),
	BoardAfterReleaseGarbage = release_garbage_list( GamestateAfterGarbageRelease#user_gamestate.board, GarbageToReleaseList ),

	GeneratedGarbagePositionList = calculate_garbage_from_combos( CombosTotal, BoardAfterReleaseGarbage, Opponent_Board, GameRules ),
	{NewOpponentGamestate,GeneratedGarbageId} = add_garbage_to_stack( GeneratedGarbagePositionList, OpponentGamestate ),

	{ ResultPlayer_Board, ResultOpponentBoard } = powers_logic:handle_turn_passed(BoardAfterReleaseGarbage, 
																						NewOpponentGamestate#user_gamestate.board,
																							GameRules),
	ResultGamestate = GamestateAfterGarbageRelease#user_gamestate{
								board = ResultPlayer_Board#board{ triggered_abilities = [] },
								current_piece = NextPiece,
								current_piece_angle = down,
								current_piece_x = ?STARTING_PIECE_X,
								current_piece_y = ResultPlayer_Board#board.height - 1,
								piece_generation_step = GamestateAfterPiece#user_gamestate.piece_generation_step + 1
						},

	ResultOpponentGamestate = NewOpponentGamestate#user_gamestate{ board = ResultOpponentBoard},

	{ GeneratedGarbagePositionList, GeneratedGarbageId, ResultGamestate, ResultOpponentGamestate}.









send_turn_messages(UserPid, OpponentPid, GeneratedGarbagePositionList, Piece, Angle, X, Y, ClientGarbageId, GeneratedGarbageId) ->
	case length(GeneratedGarbagePositionList) of
		0 ->
			do_nothing;
		_other ->
			MsgGenerated = message_processor:create_generated_garbage_message( GeneratedGarbagePositionList, 
																				GeneratedGarbageId ),
			gen_server:cast( UserPid , { send_message, MsgGenerated } )
	end,

	Msg = message_processor:create_opponent_place_piece_message( GeneratedGarbagePositionList, Piece, 
																	X, Y, Angle, 
																		ClientGarbageId,
																		GeneratedGarbageId ),
	gen_server:cast( OpponentPid , { send_message, Msg } ).



get_garbage_to_release( ClientGarbageId, PlayerState = #user_gamestate{} ) ->
	
	lager:debug("current garbage stack is ~p\n",[PlayerState#user_gamestate.garbage_position_list]),

	Fun = fun( { GarbageId , GarbageList}, {ResultList, UnreleaseGarbage} ) ->
		case GarbageId =< ClientGarbageId of
			true ->			{ lists:append( GarbageList , ResultList ) , UnreleaseGarbage};
			false ->		{ ResultList, [ { GarbageId , GarbageList} | UnreleaseGarbage ] }
		end
	end,
	{ResultList, UnreleaseGarbage} = lists:foldl( Fun, {[],[]}, PlayerState#user_gamestate.garbage_position_list ),

	lager:info("\n garbage to release is ~p and unreleased now is ~p\n",[ResultList,UnreleaseGarbage]),

	{ResultList,PlayerState#user_gamestate{ garbage_position_list = UnreleaseGarbage } }.



add_garbage_to_stack( GarbageList, PlayerState = #user_gamestate{} ) ->
	NewOpponentGarbageList = [ {PlayerState#user_gamestate.current_garbage_id, GarbageList} | PlayerState#user_gamestate.garbage_position_list ],
	{
		PlayerState#user_gamestate{ garbage_position_list = NewOpponentGarbageList, 
										current_garbage_id = PlayerState#user_gamestate.current_garbage_id + 1 },

		PlayerState#user_gamestate.current_garbage_id 
	}.





%throws out_of_bounds (in case the user has lost)
%throws invalid_move (in case of an invalid move)
handle_update_piece( UserPid, X, Y, Angle,  Game = #game{}  ) when UserPid == (Game#game.user1_gamestate)#user_gamestate.user_pid->
	OpponentPid = (Game#game.user2_gamestate)#user_gamestate.user_pid,
	{NewGamestate, NewOpponentGamestate} = handle_update_piece( UserPid, 
																	OpponentPid,
																		(Game#game.user1_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user1_gamestate, 
																					Game#game.user2_gamestate ),

	Game#game{ user1_gamestate = NewGamestate, user2_gamestate = NewOpponentGamestate };

handle_update_piece( UserPid, X, Y, Angle, Game = #game{} ) when UserPid == (Game#game.user2_gamestate)#user_gamestate.user_pid->

	OpponentPid = (Game#game.user1_gamestate)#user_gamestate.user_pid,
	{NewGamestate, NewOpponentGamestate} = handle_update_piece( UserPid, 
																	OpponentPid,
																		(Game#game.user2_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user2_gamestate, 
																					Game#game.user1_gamestate ),

	Game#game{ user2_gamestate = NewGamestate, user1_gamestate = NewOpponentGamestate }.





handle_update_piece( _UserPid, OpponentPid, Piece = #piece{}, X, Y, Angle, Gamestate = #user_gamestate{}, OpponentGamestate = #user_gamestate{} ) ->

	case Piece == Gamestate#user_gamestate.current_piece of	
		false ->
			lager:debug("invalid piece update: wrong piece",[]),
			throw( invalid_move );
		true ->
			gen_server:cast( OpponentPid , { send_message, message_processor:create_update_piece_message( Angle, X, Y) } ),
			NewGamestate = Gamestate#user_gamestate{  current_piece_angle = Angle,
															current_piece_x = X,
																current_piece_y = Y },
			{ NewGamestate, OpponentGamestate }
	end.







%-------------- PRIVATE -------------------------


apply_gravity_combo_loop( Board = #board{} , GameRules = #game_logic_rules{} ) ->
	BoardAfterGravity = simulate_gravity( Board ),
	Combos = calculate_combos( BoardAfterGravity, GameRules ),
	case Combos of
		[] ->
			{ [], BoardAfterGravity };
		_other ->
			BoardAfterPopCombos = pop_combos( BoardAfterGravity, Combos ),
			BoardAfterGhosts = trigger_ghosts(BoardAfterPopCombos, GameRules),
			BoardWithReinforcements = release_reinforcements( BoardAfterGhosts ),
			BoardAfterAbilityChain = reset_board( BoardWithReinforcements ),

			{ New_Combos , NewBoard} = apply_gravity_combo_loop( BoardAfterAbilityChain, GameRules ),
			{ [ Combos | New_Combos ] , NewBoard}
	end.



reset_board( Board = #board{} ) ->
	Board#board{ painted = [], reinforcements = [], ghosts_to_trigger = 0 }.



release_reinforcements( Board = #board{} ) ->

	XList = lists:seq(0, Board#board.width - 1 ),

	FunForeachReinforcement = fun( Color, { Y, Result_Board }) ->
		
		Fun = fun( X , InnerResult_Board) ->
			case board:get_block( X, Y, InnerResult_Board) of
				empty ->
					board:set_block( #block{ type = color, color = Color }, X, Y, InnerResult_Board);
				_block ->
					lager:info("reinforcements caused the out of bounds"),
					throw(out_of_bounds)
			end
		end,
		{ Y - 1, lists:foldl( Fun, Result_Board, XList )}
	end,

	{_,FinalBoard} = lists:foldl( FunForeachReinforcement, { Board#board.height -1, Board},  Board#board.reinforcements ),
	FinalBoard.



place_piece( Piece = #piece{}, X, Y, up, Board = #board{} ) ->
	Real_y = board:get_column_height( X, Board),
	case Real_y == Y - 1 of 
		true ->
			 board:set_block( Piece#piece.block1, X, Y, 
			 					board:set_block( Piece#piece.block2, X , Y - 1, Board ) );
		false ->
			lager:error("piece2 was supposed to be in ~p,~p but was in ~p,~p",[X,Real_y,X,Y - 1]),
			board:print_board(Board),
			throw( invalid_move )
	end;


place_piece( Piece = #piece{}, X, Y, down, Board = #board{} ) ->
	Real_y = board:get_column_height( X, Board),
	case Y == Real_y of
		true ->
			board:set_block( Piece#piece.block1, X , Y,
		 						board:set_block( Piece#piece.block2, X , Y + 1, Board ) );
		false ->
			lager:error("piece1 was supposed to be in ~p,~p but was in ~p,~p",[X,Real_y,X,Y]),
			board:print_board(Board),
			throw( invalid_move )
	end;


place_piece( Piece = #piece{}, X, Y, left, Board = #board{} ) ->
	Real_y = board:get_column_height( X, Board),
	RealY2 = board:get_column_height( X + 1, Board),
	case Y == Real_y orelse Y == RealY2 of
		true ->
			board:set_block( Piece#piece.block1, X , Real_y,
			 					board:set_block( Piece#piece.block2, X + 1, RealY2, Board ) );
		false ->
			lager:error("piece1 was supposed to be in ~p,~p but was in ~p,~p",[X,Real_y,X,Y]),
			lager:error("piece2 was supposed to be in ~p,~p but was in ~p,~p",[X + 1 ,RealY2,X + 1,Y]),
			board:print_board(Board),
			throw( invalid_move )
	end;


place_piece( Piece = #piece{}, X, Y, right, Board = #board{} ) ->
	Real_y = board:get_column_height( X, Board),
	RealY2 = board:get_column_height( X - 1, Board),
	case Y == Real_y orelse Y == RealY2 of
		true ->
			board:set_block( Piece#piece.block1, X , Real_y,
			 					board:set_block( Piece#piece.block2, X - 1, RealY2, Board ) );
		false ->
			lager:error("piece1 was supposed to be in ~p,~p but was in ~p,~p",[X,Real_y,X,Y]),
			lager:error("piece2 was supposed to be in ~p,~p but was in ~p,~p",[X -1 ,RealY2,X -1,Y]),
			board:print_board(Board),
			throw( invalid_move )
	end.








calculate_combos( Board = #board{}, GameRules = #game_logic_rules{} )->
	Blocks = board:get_all_blocks(Board),

	Fun = fun( Block = #block{} , Combos ) ->
		%lets ignore blocks already in combos (except shapeshifter)
		case lists:any( fun( Combo ) -> sets:is_element( Block, Combo ) andalso Block#block.type =/= shapeshifter end , Combos) of
			true ->
				Combos;
			false when Block#block.type == shapeshifter ->
				Combos;
			false ->
				{NewCombo, _ } = calculate_combo_for_piece( Block, Block#block.x, Block#block.y, Board),
				case sets:size(NewCombo) of
					0 ->		Combos;
					_other ->	[NewCombo | Combos]
				end
		end
	end,
	All_Combos = lists:foldl(Fun, [], Blocks),

	PredMinimumComboSize = fun( Combo ) ->
		sets:size( Combo ) >= GameRules#game_logic_rules.min_combo_size
	end,
	lists:filter(PredMinimumComboSize, All_Combos).










pop_combos( Board = #board{}, ComboList ) ->

	ComboBlocks = lists:foldl( fun( Combo, AllComboBlocks )->
									lists:append( AllComboBlocks, sets:to_list(Combo) )
								 end, [], ComboList),


	BoardWithoutCombos = lists:foldl( fun( Block = #block{}, BoardWithoutCombos ) ->
											board:remove_block( Block#block.x, Block#block.y, BoardWithoutCombos)
										end, Board, ComboBlocks),

	BlocksByProximity = lists:foldl( fun( Combo, List )->
										lists:append( List, get_afected_by_combo_proximity(Combo , BoardWithoutCombos))
										end, [], ComboList), 
	
	BoardAfterRemoveCombosProximity = activate_blocks_by_combo_proximity( BlocksByProximity, BoardWithoutCombos),
	activate_combo_blocks( order_activated_block_list(ComboBlocks), BoardAfterRemoveCombosProximity).





get_afected_by_combo_proximity( Combo , Board = #board{}) ->
	Fun = fun ( Block, Set) ->
		add_affected_blocks_around_to_set( Block#block.x, Block#block.y, Board, Combo, Set )
	end,
	sets:to_list( lists:foldl(Fun , sets:new(), sets:to_list(Combo) ) ).

add_affected_blocks_around_to_set( X, Y, Board = #board{}, Combo, Set) ->
	add_affected_block_around_to_set( X + 1, Y, Board, Combo,
		add_affected_block_around_to_set( X, Y + 1, Board, Combo,
			add_affected_block_around_to_set( X - 1, Y, Board, Combo,
				add_affected_block_around_to_set( X, Y - 1, Board, Combo, Set )))).

add_affected_block_around_to_set( X, Y, Board = #board{}, Combo, Set ) ->
	
	case board:get_block( X, Y, Board ) of
		empty ->	
			Set;
		Block when	Block#block.type == garbage_hard orelse 
						Block#block.type == garbage_color orelse 
							Block#block.type == garbage orelse 
								Block#block.type == cloner ->
			case sets:is_element( Block, Combo ) of
				true ->
					Set;
				false ->
					sets:add_element(Block, Set)
			end;
		_other ->
			Set
	end.
	




get_activated_by_blocks_abilities( BlockList , Board = #board{} ) ->
	Fun = fun( Block, List ) ->
		lists:append( get_activated_by_block_abilities( Block, Board ) , List)
	end,
	lists:foldl( Fun, [], BlockList ).




get_color_order( Color )->
	case Color of
		red ->			1;
		yellow ->		2;
		blue ->			3;
		green ->		4;
		purple ->		5;
		white ->		6;
		undefined ->	7
	end.



order_activated_block_list( BlockList ) ->
	Fun = fun( Block = #block{} , Block2 = #block{} ) ->
		get_color_order(Block#block.color) > get_color_order(Block2#block.color)
	end,
	lists:sort( Fun , BlockList ).




activate_blocks_by_combo_proximity( [], Board = #board{}) ->
	Board;
activate_blocks_by_combo_proximity( [ Block | Rest], Board = #board{}) ->

	case board:get_block( Block#block.x, Block#block.y, Board) of
		
		ClonerBlock when ClonerBlock#block.type == cloner ->
			NewBoard = pop_cloner( ClonerBlock, Board),
			activate_blocks_by_combo_proximity( Rest, NewBoard);

		GarbageBlock when GarbageBlock#block.type == garbage_hard, GarbageBlock#block.hardness > 1 ->
			NewBoard = board:set_block( GarbageBlock#block{ hardness = GarbageBlock#block.hardness - 1 }, 
								GarbageBlock#block.x, GarbageBlock#block.y, board:remove_block( GarbageBlock#block.x, GarbageBlock#block.y , Board)),
			activate_blocks_by_combo_proximity( Rest, NewBoard);

		GarbageBlock when GarbageBlock#block.type == garbage_hard ->
			NewBoard = board:remove_block( GarbageBlock#block.x, GarbageBlock#block.y , Board),
			activate_blocks_by_combo_proximity( Rest, NewBoard);


		GarbageBlock when GarbageBlock#block.type == garbage_color ->
			NewBoard = board:set_block( #block{ type = color , color = GarbageBlock#block.color }, 
								GarbageBlock#block.x, GarbageBlock#block.y, 
									board:remove_block( GarbageBlock#block.x, GarbageBlock#block.y, Board) ),
			activate_blocks_by_combo_proximity( Rest, NewBoard);

		GarbageBlock when GarbageBlock#block.type == garbage ->
			NewBoard = board:remove_block( GarbageBlock#block.x, GarbageBlock#block.y, Board),
			activate_blocks_by_combo_proximity( Rest, NewBoard);

		_other ->
			Board
	end.







activate_combo_blocks( [], Board = #board{} ) ->
	Board;

activate_combo_blocks( BlockList, Board = #board{} ) ->

	ActivateFun = fun( Block, Result_Board = #board{} ) ->
		activate_block( Block, Result_Board )
	end,

	BoardAfterActivate = lists:foldl( ActivateFun , Board, BlockList ),

	NewActivatedList = order_activated_block_list( get_activated_by_blocks_abilities(BlockList , BoardAfterActivate) ),
	activate_blocks( NewActivatedList, BoardAfterActivate).


activate_blocks( [], Board = #board{} ) ->
	Board;
activate_blocks( BlockList, Board = #board{} ) ->
	
	RemoveFun =  fun( Block, Result_Board = #board{} ) ->
		remove_block( Block, Result_Board )
	end,

	ActivateFun = fun( Block, Result_Board = #board{} ) ->
		activate_block( Block, Result_Board )
	end,

	BoardAfterRemoved = lists:foldl( RemoveFun , Board, BlockList ),
	BoardAfterActivate = lists:foldl( ActivateFun , BoardAfterRemoved, BlockList ),

	NewActivatedList = order_activated_block_list( get_activated_by_blocks_abilities(BlockList , BoardAfterActivate) ),
	activate_blocks( NewActivatedList, BoardAfterActivate).







remove_block(PaintBlock = #block{}, Board = #board{} ) when PaintBlock#block.type == paint ->
	Board;
remove_block( Block = #block{}, Board = #board{} )->
	%io:format("\n removing ~p",[Block]),
	board:remove_block( Block#block.x, Block#block.y, Board ).


activate_block( Block = #block{}, Board = #board{} ) when Block#block.type == tornado ->
	io:format("pop~p\n",[Block]),
	Board#board{ triggered_abilities = add_ability_block( Block, Board#board.triggered_abilities) };

activate_block( Block = #block{}, Board = #board{} ) when Block#block.type == chromatic_bomb ->
	io:format("pop~p\n",[Block]),
	Board#board{ triggered_abilities = add_ability_block( Block, Board#board.triggered_abilities) };

activate_block( Block = #block{}, Board = #board{} ) when Block#block.type == bomb ->
	io:format("pop~p\n",[Block]),
	Board#board{ triggered_abilities = add_ability_block( Block, Board#board.triggered_abilities) };

activate_block( Block = #block{}, Board = #board{} ) when Block#block.type == paint ->
	io:format("pop~p\n",[Block]),
	pop_paint( Block, Board );

activate_block( Block = #block{}, Board = #board{} ) when Block#block.type == reinforcements ->
	io:format("pop~p\n",[Block]),
	pop_reinforcement( Block, Board );

activate_block( Block = #block{}, Board = #board{} ) when Block#block.type == ghost ->
	io:format("pop~p\n",[Block]),
	pop_ghost( Block, Board);

 activate_block( _ , Board = #board{} ) ->
	Board.




add_ability_block( Block, List ) ->
	case lists:any( fun( AnyBlock ) -> AnyBlock == Block end, List) of
		false ->	[ Block | List];
		true ->		List
	end.





get_activated_by_block_abilities( Block = #block{}, Board = #board{} ) ->
	X = Block#block.x,
	Y = Block#block.y,

	case Block#block.type of
		bomb ->
			add_block_to_list( X + 1, Y, Board,
				add_block_to_list( X, Y + 1, Board,
					add_block_to_list( X, Y - 1, Board,
						add_block_to_list( X - 1, Y, Board, [] ))));
		chromatic_bomb ->
			get_all_same_color( Block#block.color, Board );

		tornado ->
			List = get_tornado_activated_blocks( X, Y, Board ),
			List;
		_other ->
			[]
	end.


add_block_to_list( X, Y, Board = #board{}, List) ->
	case board:get_block( X, Y, Board) of
		empty ->			List;
		Block ->			[Block | List]
	end.







get_tornado_activated_blocks( X, Y, Board = #board{}) ->

	FirstRing = [ {1,0}, {-1,1}, {-1,-1}, {1,1}, {-1,0}, {1,-1}, {0,1}, {0,-1} ],
	SecondRing = [ {0,2}, {-2,-2}, {2,-2}, {2,2}, {0,-2}, {-2,2}, {2,0}, {-1,2}, {-1,-2}, {2,1}, {-2,0}, {2,-1}, {1,-2}, {-2,1}, {1,2}, {-2,-1} ],

	FirstRingPieces = get_tornado_pieces_at( 2, FirstRing, X, Y, Board ),
	SecondRingPieces = get_tornado_pieces_at( 2, SecondRing, X, Y, Board ),
	lists:append( FirstRingPieces, SecondRingPieces).


get_tornado_pieces_at( 0 , _, _, _, _ ) ->
	[];

get_tornado_pieces_at( _, [], _, _, _ ) ->
	[];

get_tornado_pieces_at( Amount, [ {Dx,Dy} | Rest], X, Y, Board = #board{} ) ->
	case Block = board:get_block( X + Dx , Y + Dy, Board) of
		empty ->
			get_tornado_pieces_at( Amount, Rest, X, Y, Board);
		Block when Block#block.type == garbage orelse 
					Block#block.type == garbage_hard orelse 
						Block#block.type == garbage_color ->
			get_tornado_pieces_at( Amount, Rest, X, Y, Board);
		Block ->
			[ Block | get_tornado_pieces_at( Amount - 1, Rest, X, Y, Board) ]
	end.





change_block_color( X, Y, NewColor, Board = #board{} ) ->
	case board:get_block( X, Y , Board) of
		empty ->
			Board;
		Block when Block#block.type == shapeshifter->
			Board;
		Block when Block#block.type == garbage_color->
			Board;
		Block ->
			case proplists:get_value( {X,Y}, Board#board.painted ) of
				undefined ->
					NewBoard = board:set_block( Block#block{ color = NewColor }, X, Y, board:remove_block( X, Y, Board)),
					NewBoard#board{ painted = [ {{X,Y}, NewColor} | NewBoard#board.painted ] };
				Color when Color == NewColor ->
					board:set_block( Block#block{ color = NewColor }, X, Y, board:remove_block( X, Y, Board));
				_color  ->
					board:set_block( Block#block{ type = shapeshifter, color = undefined }, X, Y, board:remove_block( X, Y, Board))
			end
	end.



pop_paint( Block, Board = #board{}) ->
	
	X = Block#block.x,
	Y = Block#block.y,

	NewColor = Block#block.color,
	BoardWithoutPaint = board:remove_block( X, Y , Board),
	
	ResultBoard = change_block_color( X + 1, Y, NewColor, 
						change_block_color( X, Y + 1, NewColor, 
							change_block_color( X - 1, Y, NewColor, 
								change_block_color( X, Y -1, NewColor, BoardWithoutPaint)))),
	ResultBoard#board{ triggered_abilities = add_ability_block( Block, Board#board.triggered_abilities) }.





pop_reinforcement( Block, Board = #board{}) ->
	NewColor = Block#block.color,
	ResultBoard = board:remove_block( Block#block.x, Block#block.y , Board),
	ResultBoard#board{ reinforcements = [ NewColor | ResultBoard#board.reinforcements], 
						 triggered_abilities = add_ability_block( Block, Board#board.triggered_abilities) }.



pop_cloner( ClonerBlock, Board = #board{}) ->

	X = ClonerBlock#block.x,
	Y = ClonerBlock#block.y,

	FunShiftPieces = fun( Piece_y, ShiftResultBoard ) ->
		case board:get_block( X, Piece_y, Board) of
			empty ->
				ShiftResultBoard;
			Block ->
				board:set_block( Block, X, Piece_y + 1, board:remove_block(X, Piece_y, ShiftResultBoard))
		end
	end,
	ShiftedBoard = lists:foldl( FunShiftPieces, Board, lists:seq(Board#board.height, Y + 1, -1)),
	ResultBoard = board:set_block( #block{ type = color, color = ClonerBlock#block.color }, X, Y + 1, ShiftedBoard ),
	ResultBoard.



pop_ghost( Block = #block{}, Board = #board{} ) ->
	BoardWithoutGhost = board:remove_block( Block#block.x, Block#block.y , Board),
	BoardWithoutGhost#board{ ghosts_to_trigger = BoardWithoutGhost#board.ghosts_to_trigger + 1,
								 triggered_abilities = add_ability_block( Block, Board#board.triggered_abilities) }.



trigger_ghosts( Board = #board{ ghosts_to_trigger = NumberGhosts}, _GameRules = #game_logic_rules{} ) when NumberGhosts == 0 ->
	Board;
trigger_ghosts( Board = #board{}, GameRules = #game_logic_rules{} ) ->

	GarbageOnlyFilter = fun( Current_Block = #block{} ) ->
		Current_Block#block.type == garbage orelse 
			Current_Block#block.type == garbage_hard orelse 
				Current_Block#block.type == garbage_color
	end,

	FunOrderGarbages = fun( GarbageBlock = #block{}, GarbageBlock2 = #block{}) ->
		case GarbageBlock#block.type == GarbageBlock2#block.type of
			false ->
				get_block_type_order( GarbageBlock#block.type) > get_block_type_order( GarbageBlock2#block.type);

			true when GarbageBlock#block.type == garbage_hard andalso GarbageBlock#block.hardness =/= GarbageBlock2#block.hardness ->
				GarbageBlock#block.hardness > GarbageBlock2#block.hardness;

			true ->
				case GarbageBlock#block.x == GarbageBlock2#block.x of
					true ->			GarbageBlock#block.y < GarbageBlock2#block.y;
					false ->		GarbageBlock#block.x < GarbageBlock2#block.x
				end
		end
	end,

	GarbageList = lists:sort( FunOrderGarbages, lists:filter( GarbageOnlyFilter , board:get_all_blocks( Board ) ) ),
	ResultBoard = trigger_ghosts( 1, GarbageList, Board, GameRules),
	ResultBoard#board{ ghosts_to_trigger = 0 }.



get_block_type_order( Type ) ->
	case Type of
		garbage_hard ->			3;
		garbage_color ->		2;
		garbage ->				1
	end.





trigger_ghosts(_,[], Board = #board{}, _GameRules = #game_logic_rules{}) ->
	Board;
trigger_ghosts(0,_, Board = #board{}, GameRules = #game_logic_rules{}) ->
	Board;
trigger_ghosts(HowMany, [ Garbage | RestGarbageList], Board = #board{}, GameRules = #game_logic_rules{}) ->
	BoardWithShapeshifter = board:set_block( #block{ type = shapeshifter }, Garbage#block.x, Garbage#block.y, 
												board:remove_block( Garbage#block.x, Garbage#block.y, Board ) ),

	trigger_ghosts(HowMany - 1, RestGarbageList, BoardWithShapeshifter, GameRules).






get_all_same_color( Color, Board = #board{} ) ->
	Fun = fun( Block = #block{}, ResultList )->
		case Block#block.type of
			color when Block#block.color == Color ->
				[ Block | ResultList];
			_other ->
				ResultList
		end
	end,
	lists:foldl( Fun, [], board:get_all_blocks(Board) ).





simulate_gravity( Board = #board{} )->
	simulate_gravity_by_column( Board, 0).






release_garbage_list( Board = #board{}, [] ) ->
	Board;

release_garbage_list( Board = #board{}, [{{garbage_color, Color}, GarbagePosition} | Rest ] ) ->
	NewBoard = board:set_block( #block{ type = garbage_color, color = Color }, GarbagePosition , board:get_column_height( GarbagePosition, Board ), Board ),
	release_garbage_list( NewBoard, Rest );
	
release_garbage_list( Board = #board{}, [{{garbage_hard, Hardness}, GarbagePosition} | Rest ] ) ->
	NewBoard = board:set_block( #block{ type = garbage_hard, hardness = Hardness }, GarbagePosition , board:get_column_height( GarbagePosition, Board ), Board ),
	release_garbage_list( NewBoard, Rest );

release_garbage_list( Board = #board{}, [{garbage, GarbagePosition} | Rest ] ) ->
	NewBoard = board:set_block( #block{ type = garbage }, GarbagePosition , board:get_column_height( GarbagePosition, Board ), Board ),
	release_garbage_list( NewBoard, Rest ).



calculate_garbage_from_combos( Combos, Board = #board{}, OpponentBoard = #board{}, GameRules = #game_logic_rules{} ) ->
	{ NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber } = case Combos of 
		[] -> { 0,0,0 };
		_ -> game_rules:get_garbage_number( Combos, GameRules )
	end,
	
	io:format("\nWTF ~p\n",[Board#board.triggered_abilities]),

	TripleGarbageNumber = length(Board#board.triggered_abilities),

	{ TripleGarbageNumber2, NormalGarbageNumber2, ColorGarbageNumber2, HardGarbageNumber2 } = 
		case powers_logic:trigger_barrier_blow(Board, OpponentBoard, GameRules) of 

			true when NormalGarbageNumber > 0 ->
				{ TripleGarbageNumber, NormalGarbageNumber - 1, ColorGarbageNumber, HardGarbageNumber };

			_ ->
				{ TripleGarbageNumber, NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber }
		end,


	{ TripleGarbageNumber3, NormalGarbageNumber3, ColorGarbageNumber3, HardGarbageNumber3 } =
		case Board#board.thrash_turns > 0 of
			false ->
				{ TripleGarbageNumber2, HardGarbageNumber2, ColorGarbageNumber2, NormalGarbageNumber2};
			true ->
				{ case TripleGarbageNumber2 of 0 -> 0; _ -> TripleGarbageNumber2 + 1 end,
					case HardGarbageNumber2 of 0 -> 0; _ -> HardGarbageNumber2 + 1 end,
						case ColorGarbageNumber2 of 0 -> 0; _ -> ColorGarbageNumber2 + 1 end,
							case NormalGarbageNumber2 of 0 -> 0; _ -> NormalGarbageNumber2 + 1 end }
		end,


	{ TripleGarbageNumber4, NormalGarbageNumber4, ColorGarbageNumber4, HardGarbageNumber4 } = 
		case powers_logic:trigger_killing_blow( Board, OpponentBoard, GameRules) of
			false ->	{ TripleGarbageNumber3, NormalGarbageNumber3, ColorGarbageNumber3, HardGarbageNumber3 };
			true ->		{ TripleGarbageNumber3 * 2, NormalGarbageNumber3 * 2, ColorGarbageNumber3 * 2, HardGarbageNumber3 * 2 }
		end,

	generate_garbage_positions( TripleGarbageNumber4, NormalGarbageNumber4, ColorGarbageNumber4, HardGarbageNumber4, Board ).
	





calculate_next_piece( Gamestate = #user_gamestate{}, Combos, GameRules = #game_logic_rules{} ) ->
	{ NewRandomState, Random } = get_next_random( Gamestate#user_gamestate.random_state ),
	{ NewRandomState2, Random2 } = get_next_random( NewRandomState ),

	{ Color, Type } = get_block_color_type(Random, Combos, Gamestate#user_gamestate.board, GameRules),

	{ Color2, Type2 } = case powers_logic:trigger_overload( Gamestate#user_gamestate.board, GameRules ) of
		true ->			{ undefined, shapeshifter};
		false ->		get_block_color_type(Random2)
	end,

	{ Gamestate#user_gamestate{ random_state = NewRandomState2 }, 
		#piece{ block1 = #block{ type = Type, color = Color }, 
				block2 = #block{ type = Type2, color = Color2 } }}.

calculate_next_piece( InitialRandomState ) ->
	{ NewRandomState, Random } = get_next_random( InitialRandomState ),
	{ NewRandomState2, Random2 } = get_next_random( NewRandomState ),

	{ Color, Type } = get_block_color_type(Random),
	{ Color2, Type2 } = get_block_color_type(Random2),

	{  NewRandomState2,
		#piece{ block1 = #block{ type = Type, color = Color }, 
				block2 = #block{ type = Type2, color = Color2 } }}.




get_block_color_type( Random, Combos, Board = #board{}, GameRules = #game_logic_rules{} ) ->
	Color = case Random rem 6 of
		0 ->		red;
		1 ->		yellow;
		2 ->		blue;
		3 ->		green;
		4 ->		purple;
		5 ->		white
	end,
	
	Type = case Board#board.frenzy_turns > 0 of
		true ->			game_rules:get_next_piece_type_with_frenzy( Combos, GameRules );
		false ->		game_rules:get_next_piece_type( Combos, GameRules)
	end,
	
	{Color, Type}.


get_block_color_type( Random ) ->
	case Random rem 6 of
		0 ->		{red, color};
		1 ->		{yellow, color};
		2 ->		{blue, color};
		3 ->		{green, color};
		4 ->		{purple, color};
		5 ->		{white, color}
	end.






get_next_random( X ) ->
	Random = (1103515245 * X + 12345) rem 2147483648,
	{ Random, Random }.








generate_garbage_positions( TripleGarbageNumber, HardGarbageNumber, ColorGarbageNumber, NormalGarbageNumber, Board = #board{} ) ->
	%lager:debug("generating garbage: ~p hard , ~p color , ~p normal",[HardGarbageNumber,ColorGarbageNumber,NormalGarbageNumber]),
	generate_garbage_positions( TripleGarbageNumber, HardGarbageNumber, ColorGarbageNumber, NormalGarbageNumber, 
									Board, lists:seq( 0, Board#board.width - 1 ) ).


generate_garbage_positions( TripleGarbageNumber, HardGarbageNumber, ColorGarbageNumber, NormalGarbageNumber, Board = #board{}, [] ) ->
	generate_garbage_positions( TripleGarbageNumber, HardGarbageNumber, ColorGarbageNumber, NormalGarbageNumber, 
									Board, lists:seq( 0 , Board#board.width - 1 ) );


generate_garbage_positions( 0, 0, 0, 0, _ , _ ) ->
	[];


generate_garbage_positions( 0, 0, 0, NormalGarbageNumber, Board = #board{}, ColumnList ) ->
	Random = random:uniform( length(ColumnList) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, ColumnList),
	NewColumnList = lists:append( List1 , List2 ),

	[ {garbage , Position} | 
		generate_garbage_positions( 0, 0, 0, NormalGarbageNumber - 1, Board, NewColumnList )];


generate_garbage_positions( 0, 0, ColorGarbageNumber, NormalGarbageNumber, Board = #board{}, ColumnList ) ->
	Random = random:uniform( length(ColumnList) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, ColumnList),
	NewColumnList = lists:append( List1 , List2 ),

	[ {{garbage_color, generate_random_garbage_color() }, Position} | 
		generate_garbage_positions( 0, 0, ColorGarbageNumber - 1, NormalGarbageNumber, Board, NewColumnList )];


generate_garbage_positions( 0, HardGarbageNumber, ColorGarbageNumber, NormalGarbageNumber, Board = #board{}, ColumnList ) ->
	Random = random:uniform( length(ColumnList) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, ColumnList),
	NewColumnList = lists:append( List1 , List2 ),

	[ {{garbage_hard, 2}, Position} | 
		generate_garbage_positions( 0, HardGarbageNumber -1, ColorGarbageNumber, NormalGarbageNumber, Board, NewColumnList )];



generate_garbage_positions( TripleGarbageNumber, HardGarbageNumber, ColorGarbageNumber, NormalGarbageNumber, Board = #board{}, ColumnList ) ->
	Random = random:uniform( length(ColumnList) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, ColumnList),
	NewColumnList = lists:append( List1 , List2 ),

	[ {{garbage_hard, 3}, Position} | 
		generate_garbage_positions( TripleGarbageNumber - 1, HardGarbageNumber, ColorGarbageNumber, NormalGarbageNumber, Board, NewColumnList )].




generate_random_garbage_color() ->
	case random:uniform( 6 ) of
		1 ->	purple;
		2 ->	blue;
		3 ->	green;
		4 ->	yellow;
		5 ->	red;
		6 ->	white
	end.











%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										combos helper functions
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




simulate_gravity_by_column( Board = #board{}, X ) when X >= Board#board.width ->
	Board;

simulate_gravity_by_column( Board = #board{}, X ) ->
	NewBoard = move_column_down( Board, X, 0, 0),
	simulate_gravity_by_column( NewBoard, X + 1 ).



move_column_down( Board = #board{}, _X, Y, _HowMuch ) when Y >= Board#board.height ->
	Board;

move_column_down( Board = #board{}, X, Y, 0) ->
	case board:get_block( X, Y, Board) of
		empty ->
			move_column_down( Board, X, Y + 1, 1);
		_block ->
			move_column_down( Board, X, Y + 1, 0)
	end;

move_column_down( Board = #board{}, X, Y, HowMuch ) ->
	case board:get_block( X, Y, Board) of
		empty ->
			move_column_down( Board, X, Y + 1, HowMuch + 1);

		Block ->
			BoardWithoutBlock = board:remove_block( X, Y, Board ),
			BoardWithBlockInPlace = board:set_block( Block, X, Y - HowMuch, BoardWithoutBlock ),

			move_column_down( BoardWithBlockInPlace, X, Y + 1, HowMuch)
	end.













calculate_combo_for_piece( Block = #block{ }, X, Y, Board = #board{} ) ->
	calculate_combo_for_piece( Block, X, Y, sets:new(), sets:new(), Board ).

calculate_combo_for_piece( Block = #block{ }, X, Y, Combo, Visited, Board = #board{} ) ->

	case sets:is_element( {X,Y}, Visited) of
		true ->
			{ Combo, Visited };
		false ->
			NewVisited = sets:add_element( {X,Y}, Visited),

			case board:get_block( X, Y, Board ) of
				empty ->
					{ Combo, NewVisited };

				CurrentBlock when (CurrentBlock#block.type =/= garbage andalso
										CurrentBlock#block.type =/= garbage_hard andalso
											CurrentBlock#block.type =/= garbage_color andalso
												CurrentBlock#block.color == Block#block.color) orelse
													CurrentBlock#block.type == shapeshifter ->

					NewCombo = sets:add_element( CurrentBlock, Combo ),

					{NewCombo2, NewVisited2 } = calculate_combo_for_piece( Block, X , Y + 1 , NewCombo, NewVisited, Board),
					{NewCombo3, NewVisited3 } = calculate_combo_for_piece( Block, X + 1 , Y , NewCombo2, NewVisited2, Board),
					{NewCombo4, NewVisited4 } = calculate_combo_for_piece( Block, X - 1 , Y , NewCombo3, NewVisited3, Board),
					calculate_combo_for_piece( Block, X, Y - 1 , NewCombo4, NewVisited4, Board);

				_CurrentBlock ->
					{ Combo, NewVisited }
			end
	end.



%%-------------------------------------------------------------------------------------------------------------------------------------------------
%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										UNIT TESTS 
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%---------------------------------------------------------------------------------------






-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%% --------------------         GOOGLE DOCS             ------------------------------------------


get_power_type_from_test_type( TestType ) ->
	case TestType of

		<<"none">> ->				color;
		
		<<"tornado">> ->			tornado;
		<<"paint">> ->				paint;
		<<"reinforcements">> ->		reinforcements;
		<<"chromatic">> ->			chromatic_bomb;
		<<"shapeshifter">> ->		shapeshifter;
		<<"cloner">> ->				cloner;
		<<"bomb">> ->				bomb;
		<<"ghost">>	->				ghost
	end.


create_board( BlockList, XOffset ) ->
	Fun = fun(  {BlockProperties}, Result) ->

		Value = proplists:get_value(<<"block">>, BlockProperties),
		X = proplists:get_value(<<"x">>, BlockProperties) - 1 - XOffset,
		Y = 14 - proplists:get_value(<<"y">>, BlockProperties),

		{RealBlockType, Color, Hardness} = 
		case Value of
			<<"w">> -> 	{color,white, 2};
			<<"p">> -> 	{color,purple, 2};
			<<"b">> -> 	{color,blue, 2};
			<<"r">> -> 	{color,red, 2};
			<<"g">> -> 	{color,green, 2};
			<<"y">> -> 	{color,yellow, 2};

			<<"@">> -> 	{garbage,red, 2};

			<<"#1">> -> {garbage_hard,red, 1};
			<<"#2">> -> {garbage_hard,red, 2};
			<<"#">> -> 	{garbage_hard,red, 2};

			<<"#w">> -> {garbage_color,white, 2};
			<<"#p">> -> {garbage_color,purple, 2};
			<<"#b">> -> {garbage_color,blue, 2};
			<<"#r">> -> {garbage_color,red, 2};
			<<"#g">> -> {garbage_color,green, 2};
			<<"#y">> -> {garbage_color,yellow, 2};
			
			<<"?">> -> 	{shapeshifter,undefined, 2};

			<<"Tw">> -> {tornado,white, 2};
			<<"Tp">> -> {tornado,purple, 2};
			<<"Tg">> -> {tornado,green, 2};
			<<"Tr">> -> {tornado,red, 2};
			<<"Ty">> -> {tornado,yellow, 2};
			<<"Tb">> -> {tornado,blue, 2};

			<<"Pw">> -> {paint,white, 2};
			<<"Pp">> -> {paint,purple, 2};
			<<"Pg">> -> {paint,green, 2};
			<<"Pr">> -> {paint,red, 2};
			<<"Py">> -> {paint,yellow, 2};
			<<"Pb">> -> {paint,blue, 2};

			<<"Bw">> -> {bomb,white, 2};
			<<"Bp">> -> {bomb,purple, 2};
			<<"Bb">> -> {bomb,blue, 2};
			<<"Bg">> -> {bomb,green, 2};
			<<"Br">> -> {bomb,red, 2};
			<<"By">> -> {bomb,yellow, 2};

			<<"Gw">> -> {ghost,white, 2};
			<<"Gp">> -> {ghost,purple, 2};
			<<"Gb">> -> {ghost,blue, 2};
			<<"Gg">> -> {ghost,green, 2};
			<<"Gr">> -> {ghost,red, 2};
			<<"Gy">> -> {ghost,yellow, 2};

			<<"Cw">> -> {cloner,white, 2};
			<<"Cp">> -> {cloner,purple, 2};
			<<"Cb">> -> {cloner,blue, 2};
			<<"Cg">> -> {cloner,green, 2};
			<<"Cr">> -> {cloner,red, 2};
			<<"Cy">> -> {cloner,yellow, 2};

			<<"CRw">> -> {chromatic_bomb,white, 2};
			<<"CRp">> -> {chromatic_bomb,purple, 2};
			<<"CRb">> -> {chromatic_bomb,blue, 2};
			<<"CRg">> -> {chromatic_bomb,green, 2};
			<<"CRr">> -> {chromatic_bomb,red, 2};
			<<"CRy">> -> {chromatic_bomb,yellow, 2};

			<<"Rw">> -> {reinforcements,white, 2};
			<<"Rp">> -> {reinforcements,purple, 2};
			<<"Rb">> -> {reinforcements,blue, 2};
			<<"Rg">> -> {reinforcements,green, 2};
			<<"Rr">> -> {reinforcements,red, 2};
			<<"Ry">> -> {reinforcements,yellow, 2}

		end,

		board:set_block( #block{ type = RealBlockType, color = Color, hardness = Hardness}, X, Y, Result)
	end,

	lists:foldl( Fun , board:new_empty( ?BOARD_WIDTH, ?BOARD_HEIGHT), BlockList ).



create_near_death_board() ->
	board:set_block( #block{ color = red }, 0, 0,
	board:set_block( #block{ color = white }, 1, 0,
	board:set_block( #block{ color = purple }, 2, 0,
	board:set_block( #block{ color = yellow }, 3, 0,
	board:set_block( #block{ color = blue }, 4, 0,
	board:set_block( #block{ color = green }, 5, 0,

		board:set_block( #block{ color = green }, 0, 1,
		board:set_block( #block{ color = red }, 1, 1,
		board:set_block( #block{ color = white }, 2, 1,
		board:set_block( #block{ color = purple }, 3, 1,
		board:set_block( #block{ color = yellow }, 4, 1,
		board:set_block( #block{ color = blue }, 5, 1,

			board:set_block( #block{ color = blue }, 0, 2,
			board:set_block( #block{ color = green }, 1, 2,
			board:set_block( #block{ color = red }, 2, 2,
			board:set_block( #block{ color = white }, 3, 2,
			board:set_block( #block{ color = purple }, 4, 2,
			board:set_block( #block{ color = yellow }, 5, 2,
			
				board:set_block( #block{ color = yellow }, 0, 3,
				board:set_block( #block{ color = blue }, 1, 3,
				board:set_block( #block{ color = green }, 2, 3,
				board:set_block( #block{ color = red }, 3, 3,
				board:set_block( #block{ color = white }, 4, 3,
				board:set_block( #block{ color = purple }, 5, 3,
				
					board:set_block( #block{ color = purple }, 0, 4,
					board:set_block( #block{ color = yellow }, 1, 4,
					board:set_block( #block{ color = blue }, 2, 4,
					board:set_block( #block{ color = green }, 3, 4,
					board:set_block( #block{ color = red }, 4, 4,
					board:set_block( #block{ color = white }, 5, 4,

						board:set_block( #block{ color = white }, 0, 5,
						board:set_block( #block{ color = purple }, 1, 5,
						board:set_block( #block{ color = yellow }, 2, 5,
						board:set_block( #block{ color = blue }, 3, 5,
						board:set_block( #block{ color = green }, 4, 5,
						board:set_block( #block{ color = red }, 5, 5,

							board:set_block( #block{ color = red }, 0, 6,
							board:set_block( #block{ color = white }, 1, 6,
							board:set_block( #block{ color = purple }, 2, 6,
							board:set_block( #block{ color = yellow }, 3, 6,
							board:set_block( #block{ color = blue }, 4, 6,
							board:set_block( #block{ color = green }, 5, 6,

								board:set_block( #block{ color = green }, 0, 7,
								board:set_block( #block{ color = red }, 1, 7,
								board:set_block( #block{ color = white }, 2, 7,
								board:set_block( #block{ color = purple }, 3, 7,
								board:set_block( #block{ color = yellow }, 4, 7,
								board:set_block( #block{ color = blue }, 5, 7,

									board:set_block( #block{ color = blue }, 0, 8,

										board:new_empty(6,10)))))))))))))))))))))))))))))))))))))))))))))))))).



setup_tests() ->
	application:start(inets),
	application:start(httpc),
	ok.


tear_down_tests( _ ) ->
	ok.


google_docs_tests(GameRules) ->

	{ok, Dir} = file:get_cwd(),
	{ok, Binary} = file:read_file(Dir ++ "/../scripts/RESULT.json"),
	
	Fun = fun( {TestData}, Result) ->

		TestName = proplists:get_value(<<"name">>,TestData),

		[ {binary_to_list(TestName),
			fun () ->

				Final = proplists:get_value(<<"final">>,TestData),
				Start = proplists:get_value(<<"origin">>,TestData),
				
				{GeneratedGarbage} = proplists:get_value(<<"garbage">>,TestData),

				ColorGarbage = proplists:get_value(<<"colorGarbage">>,GeneratedGarbage),
				HardGarbage = proplists:get_value(<<"hardGarbage">>,GeneratedGarbage),
				NormalGarbage = proplists:get_value(<<"normalGarbage">>,GeneratedGarbage),
				Triple_HardGarbage = case proplists:get_value(<<"tripleGarbage">>,GeneratedGarbage) of undefined -> 0; Other -> Other end,

				SecondaryGeneratedPower = proplists:get_value(<<"secPowerGenerated">>,GeneratedGarbage),
				SecondaryGeneratedType = get_power_type_from_test_type( SecondaryGeneratedPower ),

				PowerGenerated = proplists:get_value(<<"PowerGenerated">>,GeneratedGarbage),
				PowerType = get_power_type_from_test_type( PowerGenerated ),
				

				PassivePower = proplists:get_value(<<"passivePower">>,GeneratedGarbage),
				PowerTrigger = proplists:get_value(<<"triggerPower">>,GeneratedGarbage),

				FinalBoard = create_board( Final, 9 ),
				TemporaryBoard = create_board( Start, 0 ),

				StartBoard1 = case PowerTrigger of
					<<"none">> ->				TemporaryBoard;
					<<"redbutton">> ->			TemporaryBoard#board{ red_button_pressed = true };
					<<"frenzy">> ->				TemporaryBoard#board{ frenzy_turns = 5 };
					<<"trash">> ->				TemporaryBoard#board{ thrash_turns = 5 }
				end,

				StartBoard = case PassivePower of
					<<"none">> ->			StartBoard1;
					<<"overload">> ->		StartBoard1#board{ is_overload_active = true };
					<<"killingBlow">> ->	StartBoard1#board{ killing_blow_active = true };
					<<"barrier">> ->		StartBoard1#board{ barrier_active = true }
				end,

				OpponentBoard = case PassivePower of
									<<"killingBlow">> -> 	create_near_death_board();
									_ -> 					#board{}
								end,


				{ GarbagePositionList, 
					_, 
						ResultGamestate,
							_ResultOpponentGamestate} = execute_turn( #user_gamestate{ board = StartBoard, random_state = 0 },
																			#user_gamestate{ board = OpponentBoard, random_state = 0},
																				GameRules,
																					0 ),

				ResultBoard = ResultGamestate#user_gamestate.board,
				NextPiece = ResultGamestate#user_gamestate.current_piece,

				case board:are_boards_equal(ResultBoard,FinalBoard) of
					true ->			do_nothing;
					false ->
									io:format("\n expected result\n"),
									board:print_board(FinalBoard),
									io:format("\n actual result \n"),
									board:print_board(ResultBoard)
				end,

				?assertMatch( true , board:are_boards_equal(ResultBoard,FinalBoard) ),

				CalculatedNumberColorGarbages = length( lists:filter( fun( {Garbage,_} )-> 
																case Garbage of 
																	{garbage_color, _color} -> 	true;
																	_other ->					false 
																end
															end, GarbagePositionList)),

				CalculatedNumberHardGarbages = length( lists:filter( fun( {Garbage,_} )-> 
																			case Garbage of 
																				{garbage_hard , 2 } -> 		true;
																				_ -> 						false
																			end
																		end, GarbagePositionList)),

				CalculatedNumberTripleGarbages = length( lists:filter( fun( {Garbage,_} )-> 
																			case Garbage of 
																				{garbage_hard , 3 } -> 		true;
																				_ -> 						false
																			end
																		end, GarbagePositionList)),

				CalculatedNumberNormalGarbages = length( lists:filter( fun( {Garbage,_} )-> 
																			Garbage == garbage 
																		end, GarbagePositionList)),

				NextBlock = NextPiece#piece.block1,
				NextSecBlock = NextPiece#piece.block2,

				CalculatedBlockType = NextBlock#block.type,
				CalculatedSecBlockType = NextSecBlock#block.type,

				io:format("calculated power [<~p>,<~p>], tripleGarbage ~p garbage ~p, garbage_hard ~p,  garbage_color ~p",[	CalculatedBlockType,
																											CalculatedSecBlockType,
																											CalculatedNumberTripleGarbages,
																											CalculatedNumberNormalGarbages,
																											CalculatedNumberHardGarbages,	
									 																		CalculatedNumberColorGarbages ] ),

				?assertMatch( CalculatedNumberNormalGarbages, NormalGarbage ),
				?assertMatch( CalculatedNumberHardGarbages, HardGarbage ),
				?assertMatch( CalculatedNumberColorGarbages, ColorGarbage ),
				?assertMatch( CalculatedNumberTripleGarbages,Triple_HardGarbage),

				?assertMatch( CalculatedBlockType, PowerType ),
				?assertMatch( CalculatedSecBlockType, SecondaryGeneratedType),


				ok
				
			end } | Result]

	end,
	lists:foldl( Fun,[], ejson:decode(Binary)).


full_game_logic_test_() ->

		setup_tests(),
		GameRules = game_rules:get_offline_current_rules(<<"Normal">>),

		GoogleTestList = google_docs_tests(GameRules),
		{spawn,
			{setup,	
				fun()-> ok end,
				fun tear_down_tests/1,
      			GoogleTestList
			}
		}.







%% --------------------         POWERS             ------------------------------------------

%% --------------------         GARBAGE              ------------------------------------------



test_garbage_position( GarbagePositionList, Board = #board{} ) ->
	
	Fun = fun( { _ , Position }, { Cache , Max } ) ->

		case Position >= Board#board.width of
			true ->
				throw( {out_of_bounds, Position} );
			false -> 
				nothing_bad
		end,
		case Position < 0 of
			true ->
				throw( {out_of_bounds, Position} );
			false -> 
				nothing_bad
		end,

		case proplists:get_value( Position , Cache) of
			undefined ->
				NewValue = 0,
				NewCache = [ { Position, 1 } | Cache];
			Value ->
				NewValue = Value + 1,
				NewCache =  [ { Position, NewValue } | proplists:delete( Position ,Cache) ]
		end,

		NewMax = case NewValue > Max of
			true ->		NewValue;
			false ->	Max
		end,

		{ NewCache, NewMax }

	end,
	{ Cache , Max } = lists:foldl(Fun, { [], 0 }, GarbagePositionList),

	Min = lists:foldl( fun( {_Position, Value}, Min ) -> 
		case Value < Min of  
			true ->	Value;
			false -> Min
		end
	end, 999, Cache ),


	Max =< Min +1.










%% --------------------                       ------------------------------------------





-endif.







