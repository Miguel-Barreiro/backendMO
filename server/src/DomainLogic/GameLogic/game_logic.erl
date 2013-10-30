-module(game_logic).

-include("include/softstate.hrl").

-export([ handle_place_piece/5, handle_update_piece/5, create_new_game/3 ]).

-define( BOARD_WIDTH , 6).
-define( BOARD_HEIGHT , 13).
-define( STARTING_PIECE_X , 3).


%-------------- PUBLIC -------------------------


create_new_game( User1_pid, User2_pid, Initial_seed  ) ->

	{ New_random_state, Piece } = calculate_next_piece( Initial_seed ),

	User1_gamestate = #user_gamestate{ user_pid = User1_pid,
										board = board:new_empty( ?BOARD_WIDTH, ?BOARD_HEIGHT ),
											current_piece = Piece,
												random_state = New_random_state },

	User2_gamestate = #user_gamestate{ user_pid = User2_pid,
										board = board:new_empty( ?BOARD_WIDTH, ?BOARD_HEIGHT ),
											current_piece = Piece,
												random_state = New_random_state },

	Game_rules = game_rules:get_current_rules(<<"Easy">>),

	#game{ user1_gamestate = User1_gamestate, user2_gamestate = User2_gamestate, initial_seed = Initial_seed, game_rules = Game_rules }.






%throws out_of_bounds (in case the user has lost)
%throws invalid_move (in case of an invalid move)
handle_place_piece( User_pid, X, Y, Angle,  Game = #game{}  ) when User_pid == (Game#game.user1_gamestate)#user_gamestate.user_pid->
	Opponent_pid = (Game#game.user2_gamestate)#user_gamestate.user_pid,
	{New_gamestate, New_opponent_gamestate} = handle_place_piece( User_pid, 
																	Opponent_pid,
																		(Game#game.user1_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user1_gamestate, 
																					Game#game.user2_gamestate,
																						Game#game.game_rules ),

	io:format("-------------------------------- ",[]),
	io:format(" USER 1 ~p PLACE A PIECE ",[User_pid]),
	board:print_board( New_gamestate#user_gamestate.board ),
	io:format("-------------------------------- ",[]),

	Game#game{ user1_gamestate = New_gamestate, user2_gamestate = New_opponent_gamestate };

handle_place_piece( User_pid, X, Y, Angle, Game = #game{} ) when User_pid == (Game#game.user2_gamestate)#user_gamestate.user_pid->

	Opponent_pid = (Game#game.user1_gamestate)#user_gamestate.user_pid,
	{New_gamestate, New_opponent_gamestate} = handle_place_piece( User_pid, 
																	Opponent_pid,  
																		(Game#game.user2_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user2_gamestate, 
																					Game#game.user1_gamestate,
																						Game#game.game_rules ),

	io:format("-------------------------------- ",[]),
	io:format(" USER 2 ~p PLACE A PIECE ",[User_pid]),
	board:print_board( New_gamestate#user_gamestate.board ),
	io:format("-------------------------------- ",[]),

	Game#game{ user2_gamestate = New_gamestate, user1_gamestate = New_opponent_gamestate }.




handle_place_piece( User_pid, Opponent_pid, 
						Piece = #piece{}, X, Y, 
							Angle, Gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{}, 
								Game_rules = #game_logic_rules{} ) ->

	lager:debug("placed the piece in ~p,~p",[X,Y]),
	case Piece == Gamestate#user_gamestate.current_piece of	
		false ->
			lager:debug("invalid piece place: wrong piece",[]),
			throw( invalid_move );
		true ->

			Board_after_reset_reinforcements = (Gamestate#user_gamestate.board)#board{ reinforcements = [] },
			Board_after_place_piece = place_piece( Piece, X, Y, Angle, Board_after_reset_reinforcements),

			{ Combos , Result_loop_board } = apply_gravity_combo_loop( Board_after_place_piece, Game_rules ),
			Board_after_release_garbage = release_garbage_list( Result_loop_board, Gamestate#user_gamestate.garbage_position_list ),
			{ New_gamestate_after_piece, Next_piece} = calculate_next_piece( Gamestate , Combos, Game_rules ),
			Generated_garbage_position_list = calculate_garbage_from_combos( Combos, Result_loop_board, Game_rules ),

			case length(Generated_garbage_position_list) of
				0 ->
					do_nothing;
				_other ->
					gen_server:cast( User_pid , { send_message, message_processor:create_generated_garbage_message( Generated_garbage_position_list ) } )
			end,
			gen_server:cast( Opponent_pid , { send_message, message_processor:create_opponent_place_piece_message( Generated_garbage_position_list, Piece, X, Y, Angle ) } ),

			New_gamestate = New_gamestate_after_piece#user_gamestate{ board = Board_after_release_garbage,
											garbage_position_list = [],
												current_piece = Next_piece,
												current_piece_angle = down,
												current_piece_x = ?STARTING_PIECE_X,
												current_piece_y = Board_after_release_garbage#board.height - 1,
												piece_generation_step = New_gamestate_after_piece#user_gamestate.piece_generation_step + 1 },

			New_opponent_garbage_list = lists:append( Opponent_gamestate#user_gamestate.garbage_position_list, Generated_garbage_position_list ),
			New_opponent_gamestate = Opponent_gamestate#user_gamestate{ garbage_position_list = New_opponent_garbage_list },


			{ New_gamestate, New_opponent_gamestate }
	end.








%throws out_of_bounds (in case the user has lost)
%throws invalid_move (in case of an invalid move)
handle_update_piece( User_pid, X, Y, Angle,  Game = #game{}  ) when User_pid == (Game#game.user1_gamestate)#user_gamestate.user_pid->
	Opponent_pid = (Game#game.user2_gamestate)#user_gamestate.user_pid,
	{New_gamestate, New_opponent_gamestate} = handle_update_piece( User_pid, 
																	Opponent_pid,
																		(Game#game.user1_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user1_gamestate, 
																					Game#game.user2_gamestate ),

	Game#game{ user1_gamestate = New_gamestate, user2_gamestate = New_opponent_gamestate };

handle_update_piece( User_pid, X, Y, Angle, Game = #game{} ) when User_pid == (Game#game.user2_gamestate)#user_gamestate.user_pid->

	Opponent_pid = (Game#game.user1_gamestate)#user_gamestate.user_pid,
	{New_gamestate, New_opponent_gamestate} = handle_update_piece( User_pid, 
																	Opponent_pid,
																		(Game#game.user2_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user2_gamestate, 
																					Game#game.user1_gamestate ),

	Game#game{ user2_gamestate = New_gamestate, user1_gamestate = New_opponent_gamestate }.





handle_update_piece( _User_pid, Opponent_pid, Piece = #piece{}, X, Y, Angle, Gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{} ) ->

	case Piece == Gamestate#user_gamestate.current_piece of	
		false ->
			lager:debug("invalid piece update: wrong piece",[]),
			throw( invalid_move );
		true ->
			gen_server:cast( Opponent_pid , { send_message, message_processor:create_update_piece_message( Angle, X, Y) } ),
			New_gamestate = Gamestate#user_gamestate{  current_piece_angle = Angle,
															current_piece_x = X,
																current_piece_y = Y },
			{ New_gamestate, Opponent_gamestate }
	end.







%-------------- PRIVATE -------------------------


apply_gravity_combo_loop( Board = #board{} , Game_rules = #game_logic_rules{} ) ->
	Board_after_gravity = simulate_gravity( Board ),
	lager:debug("apply gravity"),
	Combos = calculate_combos( Board_after_gravity, Game_rules ),
	case Combos of
		[] ->
			{ [], Board_after_gravity};
		_other ->
			Board_after_pop_combos = pop_combos( Board_after_gravity, Combos ),
			lager:debug("poped combos"),

			Board_with_reinforcements = release_reinforcements( Board_after_pop_combos ),

			Board_after_ability_chain = reset_board( Board_with_reinforcements ),

			{ New_Combos , New_board} = apply_gravity_combo_loop( Board_after_ability_chain, Game_rules ),
			{ [ Combos | New_Combos ] , New_board}
	end.




reset_board( Board = #board{} ) ->
	Board#board{ painted = [], reinforcements = [] }.



release_reinforcements( Board = #board{} ) ->

	X_list = lists:seq(0, Board#board.width - 1),

	Fun_foreach_reinforcement = fun( Color, { Y, Result_Board }) ->
		
		Fun = fun( X , Inner_result_Board) ->
			case board:get_block( X, Y, Inner_result_Board) of
				empty ->
					board:set_block( #block{ type = color, color = Color }, X, Y, Inner_result_Board);
				Block ->
					throw(out_of_bounds)
			end		
		end,
		{ Y - 1, lists:foldl( Fun, Result_Board, X_list )}

	end,

	{_,Final_board} = lists:foldl( Fun_foreach_reinforcement, { Board#board.height -1, Board},  Board#board.reinforcements ),
	Final_board.



place_piece( Piece = #piece{}, X, Y, up, Board = #board{} ) ->
	Real_y = get_column_height( X, Board),
	case Real_y == Y - 1 of 
		true ->
			 board:set_block( Piece#piece.block1, X , Y, 
			 					board:set_block( Piece#piece.block2, X , Y - 1, Board ) );
		false ->
			lager:error("piece2 was supposed to be in ~p,~p but was in ~p,~p",[X,Real_y,X,Y - 1]),
			board:print_board(Board),
			throw( invalid_move )
	end;


place_piece( Piece = #piece{}, X, Y, down, Board = #board{} ) ->
	Real_y = get_column_height( X, Board),
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
	Real_y = get_column_height( X, Board),
	Real_y2 = get_column_height( X + 1, Board),
	case Y == Real_y orelse Y == Real_y2 of
		true ->
			board:set_block( Piece#piece.block1, X , Real_y,
			 					board:set_block( Piece#piece.block2, X + 1, Real_y2, Board ) );
		false ->
			lager:error("piece1 was supposed to be in ~p,~p but was in ~p,~p",[X,Real_y,X,Y]),
			lager:error("piece2 was supposed to be in ~p,~p but was in ~p,~p",[X + 1 ,Real_y2,X + 1,Y]),
			board:print_board(Board),
			throw( invalid_move )
	end;


place_piece( Piece = #piece{}, X, Y, right, Board = #board{} ) ->
	Real_y = get_column_height( X, Board),
	Real_y2 = get_column_height( X - 1, Board),
	case Y == Real_y orelse Y == Real_y2 of
		true ->
			board:set_block( Piece#piece.block1, X , Real_y,
			 					board:set_block( Piece#piece.block2, X - 1, Real_y2, Board ) );
		false ->
			lager:error("piece1 was supposed to be in ~p,~p but was in ~p,~p",[X,Real_y,X,Y]),
			lager:error("piece2 was supposed to be in ~p,~p but was in ~p,~p",[X -1 ,Real_y2,X -1,Y]),
			board:print_board(Board),
			throw( invalid_move )
	end.










calculate_combos( Board = #board{}, Game_rules = #game_logic_rules{} )->
	Blocks = board:get_all_blocks(Board),

	Fun = fun( Block = #block{} , Combos ) ->
		%lets ignore blocks already in combos
		case lists:any( fun( Combo ) -> sets:is_element( Block, Combo ) end , Combos) of
			true ->
				Combos;
			false ->
				{New_combo, _ } = calculate_combo_for_piece( Block, Block#block.x, Block#block.y, Board),
				case sets:size(New_combo) of
					0 ->		Combos;
					_other ->	[New_combo | Combos]
				end
		end
	end,
	All_Combos = lists:foldl(Fun, [], Blocks),

	Pred_minimum_combo_size = fun( Combo ) ->
		sets:size( Combo ) >= Game_rules#game_logic_rules.min_combo_size
	end,
	lists:filter(Pred_minimum_combo_size, All_Combos).










pop_combos( Board = #board{}, Combo_list ) ->
	Fun = fun( Combo, { List , All_combo_blocks} )->
		{ lists:append( List, get_afected_by_combo_proximity(Combo , Board)),
			lists:append( All_combo_blocks, sets:to_list(Combo) ) }
	end,
	{ Blocks_by_proximity, All_blocks_in_combos } = lists:foldl( Fun, {[],[]} , Combo_list),
	Board_after_remove_combos_proximity = activate_blocks( Blocks_by_proximity, Board),
	activate_blocks( order_activated_block_list(All_blocks_in_combos), Board_after_remove_combos_proximity).










get_afected_by_combo_proximity( Combo , Board = #board{} ) ->
	Fun = fun ( Block, Set) ->
		add_garbages_around_to_set( Block#block.x, Block#block.y, Board, Set)
	end,
	sets:to_list( lists:foldl(Fun , sets:new(), sets:to_list(Combo) ) ).

add_garbages_around_to_set( X, Y, Board = #board{}, Set) ->
	add_garbage_to_set( X + 1, Y, Board,
		add_garbage_to_set( X, Y + 1, Board,
			add_garbage_to_set( X - 1, Y, Board,
				add_garbage_to_set( X, Y - 1, Board, Set )))).

add_garbage_to_set( X, Y, Board = #board{}, Set ) ->
	case board:get_block( X, Y, Board ) of
		empty ->	
			Set;
		Block when	Block#block.type == garbage_hard orelse 
						Block#block.type == garbage_color orelse 
							Block#block.type == garbage ->
			sets:add_element(Block, Set);
		_other ->
			Set
	end.
	








get_activated_by_blocks_abilities( Block_list , Board = #board{} ) ->
	Fun = fun( Block, List ) ->
		lists:append( get_activated_by_block_abilities( Block, Board ) , List)
	end,
	lists:foldl( Fun, [], Block_list ).




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



order_activated_block_list( Block_list ) ->
	Fun = fun( Block = #block{} , Block2 = #block{} ) ->
		get_color_order(Block#block.color) > get_color_order(Block2#block.color)
	end,
	lists:sort( Fun , Block_list ).






activate_blocks( [], Board = #board{} ) ->
	Board;
activate_blocks( Block_list, Board = #board{} ) ->
	Fun = fun( Block, Result_Board = #board{} ) ->
		activate_block( Block, Result_Board )
	end,
	New_board = lists:foldl( Fun , Board, Block_list ),
	New_activated_list = order_activated_block_list( get_activated_by_blocks_abilities(Block_list , Board) ),
	activate_blocks( New_activated_list, New_board).



activate_block( Block = #block{}, Board = #board{} ) ->
	pop_block( Block#block.x, Block#block.y, Board ).







get_activated_by_block_abilities( Block = #block{}, Board = #board{} ) ->
	X = Block#block.x,
	Y = Block#block.y,
	case Block#block.type of
		bomb ->
			add_block_to_list( X + 1, Y + 1, Board,
				add_block_to_list( X + 1, Y, Board,
					add_block_to_list( X + 1, Y - 1, Board,

						add_block_to_list( X, Y + 1, Board,
							add_block_to_list( X, Y - 1, Board,

								add_block_to_list( X - 1, Y + 1, Board,
									add_block_to_list( X - 1, Y, Board,
										add_block_to_list( X - 1, Y - 1, Board, [] ))))))));
		chromatic_bomb ->
			get_all_same_color( Block#block.color, Board );

		tornado ->
			get_tornado_activated_blocks( X, Y, Board );

		_other ->
			[]
	end.


add_block_to_list( X, Y, Board = #board{}, List) ->
	case board:get_block( X, Y, Board) of
		empty ->			List;
		Block ->			[Block | List]
	end.






pop_block(  X, Y, Board = #board{} ) ->
	case board:get_block( X, Y , Board) of
		empty ->
			Board;

		Paint_block when Paint_block#block.type == paint ->
			pop_paint( X, Y, Board );

		Garbage_block when Garbage_block#block.type == garbage_hard, Garbage_block#block.hardness > 1 ->
			board:set_block( #block{ type = garbage_hard , hardness = Garbage_block#block.hardness - 1 }, X, Y, board:remove_block( X, Y , Board));

		Garbage_block when Garbage_block#block.type == garbage_color ->
			board:set_block( #block{ type = color , color = Garbage_block#block.color }, X, Y, board:remove_block( X, Y , Board) );

		Reinforcement_block when Reinforcement_block#block.type == reinforcements ->
			pop_reinforcement( X, Y, Board );

		_all_other ->
			board:remove_block( X, Y, Board)
	end.






get_tornado_activated_blocks( X, Y, Board = #board{}) ->
	io:format("getting tornado blocks in ~p,~p \n",[X, Y]),

	First_ring = [ {1,0}, {-1,1}, {-1,-1}, {-1,0}, {1,-1}, {0,1}, {0,-1} ],
	Second_ring = [ {0,2}, {-2,-2}, {2,-2}, {2,2}, {0,-2}, {-2,2}, {2,0}, {-1,2}, {-1,-2}, {2,1}, {-2,0}, {2,-1}, {1,-2}, {-2,1}, {1,2}, {-2,-1} ],

	First_ring_pieces = get_tornado_pieces_at( 3, First_ring, X, Y, Board ),
	Second_ring_pieces = get_tornado_pieces_at( 5, Second_ring, X, Y, Board ),
	lists:append( First_ring_pieces, Second_ring_pieces).


get_tornado_pieces_at( 0 , _, _, _, _ ) ->
	[];

get_tornado_pieces_at( _, [], _, _, _ ) ->
	[];

get_tornado_pieces_at( Amount, [ {Dx,Dy} | Rest], X, Y, Board = #board{} ) ->
	case board:get_block( X + Dx , Y + Dy, Board) of
		empty ->
			get_tornado_pieces_at( Amount, Rest, X, Y, Board); 
		Block ->
			[ Block | get_tornado_pieces_at( Amount - 1, Rest, X, Y, Board) ]
	end.





change_block_color( X, Y, New_color, Board = #board{} ) ->
	case board:get_block( X, Y , Board) of
		empty ->
			Board;
		Block when Block#block.type == garbage_color->
			Board;
		Block ->
			case proplists:get_value( {X,Y}, Board#board.painted ) of
				undefined ->
					New_board = board:set_block( Block#block{ color = New_color }, X, Y, board:remove_block( X, Y, Board)),
					New_board#board{ painted = [ {{X,Y}, New_color} | New_board#board.painted ] };
				Color when Color == New_color ->
					board:set_block( Block#block{ color = New_color }, X, Y, board:remove_block( X, Y, Board));
				_color  ->
					board:set_block( Block#block{ type = shapeshifter }, X, Y, board:remove_block( X, Y, Board))
			end
	end.

pop_paint( X, Y, Board = #board{}) ->
	io:format("poping paint in ~p,~p \n",[X, Y]),
	
	Block = board:get_block( X, Y , Board),
	New_color = Block#block.color,
	Board_without_paint = board:remove_block( X, Y , Board),

	change_block_color( X + 1, Y, New_color, 
		change_block_color( X, Y + 1, New_color, 
			change_block_color( X - 1, Y, New_color, 
				change_block_color( X, Y -1, New_color, 
					change_block_color( X + 1, Y + 1, New_color, 
						change_block_color( X - 1, Y + 1, New_color,
							change_block_color( X + 1, Y - 1, New_color,
								change_block_color( X - 1, Y - 1, New_color, Board_without_paint)))))))).




pop_reinforcement( X, Y, Board= #board{}) ->
	Block = board:get_block( X, Y , Board),
	New_color = Block#block.color,
	Board_without_paint = board:remove_block( X, Y , Board),
	Board_without_paint#board{ reinforcements = [ New_color | Board_without_paint#board.reinforcements] }.



	


get_all_same_color( Color, Board = #board{} ) ->
	Fun = fun( Block = #block{}, Result_list )->
		case Block#block.type of
			color when Block#block.color == Color ->
				[ Block | Result_list];
			_other ->
				Result_list
		end
	end,
	lists:foldl( Fun, [], board:get_all_blocks(Board)).








simulate_gravity( Board = #board{} )->
	simulate_gravity_by_column( Board, 0).






release_garbage_list( Board = #board{}, [] ) ->
	Board;

release_garbage_list( Board = #board{}, [{{garbage_color, Color}, Garbage_position} | Rest ] ) ->
	New_board = board:set_block( #block{ type = garbage_color, color = Color }, Garbage_position , get_column_height( Garbage_position, Board ), Board ),
	release_garbage_list( New_board, Rest );
	
release_garbage_list( Board = #board{}, [{garbage_hard, Garbage_position} | Rest ] ) ->
	New_board = board:set_block( #block{ type = garbage_hard }, Garbage_position , get_column_height( Garbage_position, Board ), Board ),
	release_garbage_list( New_board, Rest );

release_garbage_list( Board = #board{}, [{garbage, Garbage_position} | Rest ] ) ->
	New_board = board:set_block( #block{ type = garbage }, Garbage_position , get_column_height( Garbage_position, Board ), Board ),
	release_garbage_list( New_board, Rest ).







calculate_garbage_from_combos( [], _, _ ) ->
	[];

calculate_garbage_from_combos( Combos, Board = #board{}, Game_rules = #game_logic_rules{} ) ->
	{ Normal_garbage_number, Color_garbage_number, Hard_garbage_number } = game_rules:get_garbage_number( Combos, Game_rules ),
	generate_garbage_positions( Hard_garbage_number, Color_garbage_number, Normal_garbage_number, Board ).








calculate_next_piece( Gamestate = #user_gamestate{} , Combos, Game_rules = #game_logic_rules{} ) ->
	{ New_random_state, Random } = get_next_random( Gamestate#user_gamestate.random_state ),
	{ New_random_state2, Random2 } = get_next_random( New_random_state ),

	{ Color, Type } = get_block_color_type(Random, Combos, Game_rules),
	{ Color2, Type2 } = get_block_color_type(Random2),

	{ Gamestate#user_gamestate{ random_state = New_random_state2 }, 
		#piece{ block1 = #block{ type = Type, color = Color }, 
				block2 = #block{ type = Type2, color = Color2 } }}.

calculate_next_piece( Initial_random_state ) ->
	{ New_random_state, Random } = get_next_random( Initial_random_state ),
	{ New_random_state2, Random2 } = get_next_random( New_random_state ),

	{ Color, Type } = get_block_color_type(Random),
	{ Color2, Type2 } = get_block_color_type(Random2),

	{  New_random_state2,
		#piece{ block1 = #block{ type = Type, color = Color }, 
				block2 = #block{ type = Type2, color = Color2 } }}.








get_block_color_type( Random, Combos, Game_rules = #game_logic_rules{} ) ->
	Color = case Random rem 6 of
		0 ->		red;
		1 ->		yellow;
		2 ->		blue;
		3 ->		green;
		4 ->		purple;
		5 ->		white
	end,

	Type = game_rules:get_next_piece_type( Combos, Game_rules),

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












generate_garbage_positions( Hard_garbage_number, Color_garbage_number, Normal_garbage_number, Board = #board{} ) ->
	lager:debug("generating garbage: ~p hard , ~p color , ~p normal",[Hard_garbage_number,Color_garbage_number,Normal_garbage_number]),
	generate_garbage_positions( Hard_garbage_number, Color_garbage_number, Normal_garbage_number, Board, lists:seq( 0, Board#board.width - 1 ) ).


generate_garbage_positions( Hard_garbage_number, Color_garbage_number, Normal_garbage_number, Board = #board{}, [] ) ->
	generate_garbage_positions( Hard_garbage_number, Color_garbage_number, Normal_garbage_number, Board, lists:seq( 0 , Board#board.width - 1 ) );


generate_garbage_positions( 0, 0, 0, _ , _ ) ->
	[];


generate_garbage_positions( 0, 0, Normal_garbage_number, Board = #board{}, Column_list ) ->
	Random = random:uniform( length(Column_list) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, Column_list),
	New_column_list = lists:append( List1 , List2 ),

	[ {garbage , Position} | 
		generate_garbage_positions( 0, 0, Normal_garbage_number - 1, Board, New_column_list )];



generate_garbage_positions( 0, Color_garbage_number, Normal_garbage_number, Board = #board{}, Column_list ) ->
	Random = random:uniform( length(Column_list) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, Column_list),
	New_column_list = lists:append( List1 , List2 ),

	[ {{garbage_color, generate_random_garbage_color() }, Position} | 
		generate_garbage_positions( 0, Color_garbage_number - 1, Normal_garbage_number, Board, New_column_list )];



generate_garbage_positions( Hard_garbage_number, Color_garbage_number, Normal_garbage_number, Board = #board{}, Column_list ) ->
	Random = random:uniform( length(Column_list) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, Column_list),
	New_column_list = lists:append( List1 , List2 ),

	[ {garbage_hard , Position} | 
		generate_garbage_positions( Hard_garbage_number -1, Color_garbage_number, Normal_garbage_number, Board, New_column_list )].




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




get_column_height( Column, Board = #board{} ) ->
	get_column_height( Board, Column, 0 ).

get_column_height( Board = #board{}, _X, Y ) when Y >= Board#board.height ->
	throw( out_of_bounds );

get_column_height( Board = #board{}, X, Y ) ->
	case board:get_block( X , Y, Board ) of
		empty ->		Y;
		_other ->		get_column_height( Board, X , Y + 1)
	end.














simulate_gravity_by_column( Board = #board{}, X ) when X >= Board#board.width ->
	Board;

simulate_gravity_by_column( Board = #board{}, X ) ->
	New_board = move_column_down( Board, X, 0, 0),
	simulate_gravity_by_column( New_board, X + 1 ).



move_column_down( Board = #board{}, _X, Y, _How_much ) when Y >= Board#board.height ->
	Board;

move_column_down( Board = #board{}, X, Y, 0) ->
	case board:get_block( X, Y, Board) of
		empty ->
			move_column_down( Board, X, Y + 1, 1);
		_block ->
			move_column_down( Board, X, Y + 1, 0)
	end;

move_column_down( Board = #board{}, X, Y, How_much ) ->
	case board:get_block( X, Y, Board) of
		empty ->
			move_column_down( Board, X, Y + 1, How_much + 1);

		Block ->
			Board_without_block = board:remove_block( X, Y, Board ),
			Board_with_block_in_place = board:set_block( Block, X, Y - How_much, Board_without_block ),

			move_column_down( Board_with_block_in_place, X, Y + 1, How_much)
	end.













calculate_combo_for_piece( Block = #block{ }, X, Y, Board = #board{} ) ->
	calculate_combo_for_piece( Block, X, Y, sets:new(), sets:new(), Board ).

calculate_combo_for_piece( Block = #block{ }, X, Y, Combo, Visited, Board = #board{} ) ->

	case sets:is_element( {X,Y}, Visited) of
		true ->
			{ Combo, Visited };
		false ->
			New_visited = sets:add_element( {X,Y}, Visited),

			case board:get_block( X, Y, Board ) of
				empty ->
					{ Combo, New_visited };

				Current_block when (Current_block#block.type =/= garbage andalso
										Current_block#block.type =/= garbage_hard andalso
											Current_block#block.type =/= garbage_color andalso
												Current_block#block.color == Block#block.color) orelse
													Current_block#block.type == shapeshifter ->

					New_combo = sets:add_element( Current_block, Combo ),

					{New_combo2, New_visited2 } = calculate_combo_for_piece( Block, X , Y + 1 , New_combo, New_visited, Board),
					{New_combo3, New_visited3 } = calculate_combo_for_piece( Block, X + 1 , Y , New_combo2, New_visited2, Board),
					{New_combo4, New_visited4 } = calculate_combo_for_piece( Block, X - 1 , Y , New_combo3, New_visited3, Board),
					calculate_combo_for_piece( Block, X, Y - 1 , New_combo4, New_visited4, Board);

				_Current_block ->
					{ Combo, New_visited }
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




create_board( Block_list, X_offset ) ->
	Fun = fun(  {Block_properties}, Result) ->

		Value = proplists:get_value(<<"block">>, Block_properties),
		X = proplists:get_value(<<"x">>, Block_properties) - 1 - X_offset,
		Y = ?BOARD_HEIGHT - proplists:get_value(<<"y">>, Block_properties) + 1,

		{Real_block_type, Color} = 
		case Value of
			<<"w">> -> {color,white};
			<<"p">> -> {color,purple};
			<<"b">> -> {color,blue};
			<<"r">> -> {color,red};
			<<"g">> -> {color,green};
			<<"y">> -> {color,yellow};

			<<"@">> -> {garbage,red};
			<<"#">> -> {garbage_hard,red};

			<<"#w">> -> {garbage_color,white};
			<<"#p">> -> {garbage_color,purple};
			<<"#b">> -> {garbage_color,blue};
			<<"#r">> -> {garbage_color,red};
			<<"#g">> -> {garbage_color,green};
			<<"#y">> -> {garbage_color,yellow};
			
			<<"?">> -> {shapeshifter,undefined};

			<<"Tw">> -> {tornado,white};
			<<"Tp">> -> {tornado,purple};
			<<"Tg">> -> {tornado,green};
			<<"Tr">> -> {tornado,red};
			<<"Ty">> -> {tornado,yellow};
			<<"Tb">> -> {tornado,blue};

			<<"Pw">> -> {paint,white};
			<<"Pp">> -> {paint,purple};
			<<"Pg">> -> {paint,green};
			<<"Pr">> -> {paint,red};
			<<"Py">> -> {paint,yellow};
			<<"Pb">> -> {paint,blue};

			<<"Bw">> -> {bomb,white};
			<<"Bp">> -> {bomb,purple};
			<<"Bb">> -> {bomb,blue};
			<<"Bg">> -> {bomb,green};
			<<"Br">> -> {bomb,red};
			<<"By">> -> {bomb,yellow};

			<<"Rw">> -> {reinforcements,white};
			<<"Rp">> -> {reinforcements,purple};
			<<"Rb">> -> {reinforcements,blue};
			<<"Rg">> -> {reinforcements,green};
			<<"Rr">> -> {reinforcements,red};
			<<"Ry">> -> {reinforcements,yellow}

		end,

		board:set_block( #block{ type = Real_block_type, color = Color}, X, Y, Result)
	end,




	lists:foldl( Fun , board:new_empty( ?BOARD_WIDTH, ?BOARD_HEIGHT), Block_list ).



setup_tests() ->
	application:start(inets),
	application:start(httpc),
	ok.


tear_down_tests( _ ) ->
	ok.


google_docs_tests(Game_rules) ->

	{ok, Dir} = file:get_cwd(),
	{ok, Binary} = file:read_file(Dir ++ "/../scripts/RESULT.json"),
	
	Fun = fun( {Test_data}, Result) ->

		Test_name = proplists:get_value(<<"name">>,Test_data),

		[ {binary_to_list(Test_name),
			fun () ->

				Final = proplists:get_value(<<"final">>,Test_data),
				Start = proplists:get_value(<<"origin">>,Test_data),
				
				{Generated_garbage} = proplists:get_value(<<"garbage">>,Test_data),

				Color_garbage = proplists:get_value(<<"colorGarbage">>,Generated_garbage),
				Hard_garbage = proplists:get_value(<<"hardGarbage">>,Generated_garbage),
				Normal_garbage = proplists:get_value(<<"normalGarbage">>,Generated_garbage),
				%Spawns_bomb = proplists:get_value(<<"spawnsBomb">>,Generated_garbage),

				Start_board = create_board( Start, 0 ),
				Final_board = create_board( Final, 9 ),




				{ Combos , Result_loop_board } = apply_gravity_combo_loop( Start_board, Game_rules ),
				Garbage_position_list = calculate_garbage_from_combos( Combos, Result_loop_board, Game_rules ),

				io:format("\n expected result\n"),
				board:print_board(Final_board),
				io:format("\n actual result \n"),
				board:print_board(Result_loop_board),
				io:format("\n"),

				?assertMatch( true , board:are_boards_equal(Result_loop_board,Final_board) ),

				Calculated_number_color_garbages = length( lists:filter( fun( {Garbage,_} )-> 
																case Garbage of 
																	{garbage_color, _color} -> 	true;
																	_other ->					false 
																end
															end, Garbage_position_list)),

				Calculated_number_hard_garbages = length( lists:filter( fun( {Garbage,_} )-> 
																			Garbage == garbage_hard 
																		end, Garbage_position_list)),

				Calculated_number_normal_garbages = length( lists:filter( fun( {Garbage,_} )-> 
																			Garbage == garbage 
																		end, Garbage_position_list)),

				io:format("calculated garbage ~p , garbage_hard ~p,  garbage_color ~p",[Calculated_number_normal_garbages,
																						Calculated_number_hard_garbages,	
									 													Calculated_number_color_garbages ] ),

				?assertMatch( Calculated_number_normal_garbages, Normal_garbage ),
				?assertMatch( Calculated_number_hard_garbages, Hard_garbage ),
				?assertMatch( Calculated_number_color_garbages, Color_garbage ),
				?assert( test_garbage_position( Garbage_position_list, Final_board ) )
			end } | Result]

	end,
	lists:foldl( Fun,[], ejson:decode(Binary)).


normal_tests_(Game_rules) ->
	[
		{"test_multiple_bombs",?_test(test_multiple_bombs(Game_rules))},
		{"test_simple_chromatic_bomb",?_test(test_simple_chromatic_bomb(Game_rules))},
		{"test_simple2_chromatic_bomb",?_test(test_simple2_chromatic_bomb(Game_rules))},
		{"test_simple_bomb",?_test(test_simple_bomb(Game_rules))},
		{"test_simple_double_bomb_chain",?_test(test_simple_double_bomb_chain(Game_rules))},
		{"test_simple_double_bomb_chain2",?_test(test_simple_double_bomb_chain2(Game_rules))},
		{"test_simple_bomb_chain_chromatic_bom",?_test(test_simple_bomb_chain_chromatic_bom(Game_rules))},
		{"test_release_garbage_into_full_column",?_test(test_release_garbage_into_full_column(Game_rules))},
		{"test_simple_gameplay_loop",?_test(test_simple_gameplay_loop(Game_rules))},
		{"test_simple2_gameplay_loop",?_test(test_simple2_gameplay_loop(Game_rules))},
		{"test_simple2_gravity",?_test(test_simple2_gravity(Game_rules))},
		{"test_simple_gravity",?_test(test_simple_gravity(Game_rules))},
		{"test_simple_combo",?_test(test_simple_combo(Game_rules))},
		{"test_simple_combo_with_chromatic",?_test(test_simple_combo_with_chromatic(Game_rules))},
		{"test_double_combo",?_test(test_double_combo(Game_rules))},
		{"test_no_combos",?_test(test_no_combos(Game_rules))},
		{"test_simple_garbage_poping_combos",?_test(test_simple_garbage_poping_combos(Game_rules))},
		{"test_simple_poping_combos",?_test(test_simple_poping_combos(Game_rules))},
		{"test_simple_poping_combos",?_test(test_simple_poping_combos(Game_rules))},
		{"test_simple_down_place_piece",?_test(test_simple_down_place_piece(Game_rules))},
		{"test_simple_right_piece",?_test(test_simple_right_piece(Game_rules))},
		{"test_simple_left_place_piece",?_test(test_simple_left_place_piece(Game_rules))},
		{"test_simple_invalid_place_piece",?_test(test_simple_invalid_place_piece(Game_rules))},
		{"test_simple_up_place_piece",?_test(test_simple_up_place_piece(Game_rules))}		
	].


full_game_logic_test_() ->

		setup_tests(),
		Game_rules = game_rules:get_offline_current_rules(<<"Easy">>),

		Google_test_list = google_docs_tests(Game_rules),
		Normal_test_list = normal_tests_(Game_rules),
		{spawn,
			{setup,	
				fun()-> ok end,
				fun tear_down_tests/1,
      			 lists:append( Google_test_list, Normal_test_list )
			}
		}.







%% --------------------         POWERS             ------------------------------------------


test_multiple_bombs(Game_rules) ->

	BoardT = board:new_empty(5,12),
	board:print_board( BoardT),
	board:print_board(board:set_block( #block{ color = white }, 4 , 5,BoardT)),

	Board = board:set_block( #block{ color = red , type = bomb }, 0 , 0, 
			board:set_block( #block{ color = yellow}, 0 , 1, 
			board:set_block( #block{ color = green, type = bomb }, 0 , 2, 
			board:set_block( #block{ color = red }, 0 , 3, 

				board:set_block( #block{ color = red }, 1 , 0, 
				board:set_block( #block{ color = red }, 1 , 1, 
				board:set_block( #block{ color = red }, 1 , 2, 

					board:set_block( #block{ color = green, type = bomb }, 2 , 0,
					board:set_block( #block{ color = green }, 2 , 1,
					board:set_block( #block{ color = red }, 2 , 2,
					board:set_block( #block{ color = green }, 2 , 3,
					board:set_block( #block{ color = green }, 2 , 4,
					board:set_block( #block{ color = red }, 2 , 5,
					board:set_block( #block{ color = yellow }, 2 , 6,

						board:set_block( #block{ color = yellow }, 3 , 0 ,
						board:set_block( #block{ color = purple, type = bomb }, 3, 1,
						board:set_block( #block{ color = green }, 3 , 2,
						board:set_block( #block{ color = white }, 3 , 3,
						board:set_block( #block{ color = purple }, 3 , 4,
						board:set_block( #block{ color = yellow }, 3 , 5,

							board:set_block( #block{ color = purple }, 4 , 0,
							board:set_block( #block{ color = white }, 4 , 1,
							board:set_block( #block{ color = yellow }, 4 , 2,
							board:set_block( #block{ color = green }, 4 , 3,
							board:set_block( #block{ color = yellow }, 4 , 4,
							board:set_block( #block{ color = white }, 4 , 5,

								board:new_empty(5,12))))))))))))))))))))))))))),

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( #block{ color = green, type = bomb, x = 0, y = 0 }, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = red, x = 0, y = 1 }, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 4, Result_loop_board ) ),

	?assertMatch( #block{ color = red, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = yellow, x = 2, y = 1 }, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 4, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 5, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 6, Result_loop_board ) ),


	?assertMatch( #block{ color = white, x = 3, y = 0 }, board:get_block( 3, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = purple, x = 3, y = 1 }, board:get_block( 3, 1, Result_loop_board ) ),
	?assertMatch( #block{ color = yellow, x = 3, y = 2 }, board:get_block( 3, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 4, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 5, Result_loop_board ) ),

	?assertMatch( #block{ color = green, x = 4, y = 0 }, board:get_block( 4, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = yellow, x = 4, y = 1 }, board:get_block( 4, 1, Result_loop_board ) ),
	?assertMatch( #block{ color = white, x = 4, y = 2 }, board:get_block( 4, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 4, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 4, 4, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 4, 5, Result_loop_board ) ),

	ok.






test_simple_chromatic_bomb(Game_rules) ->

	Board = board:set_block( #block{ color = red }, 0 , 0, 
				board:set_block( #block{ color = red }, 0 , 1, 
					board:set_block( #block{ color = red }, 0 , 2, 
						board:set_block( #block{ color = red }, 0, 3, 
							board:set_block( #block{ color = red }, 0 , 4,

								board:set_block( #block{ type = chromatic_bomb, color = red }, 1 , 0,
									board:set_block( #block{ color = yellow }, 1 , 1,
										board:set_block( #block{ color = green }, 1 , 2,

											board:set_block( #block{ color = green }, 2 , 0 ,
												board:set_block( #block{ color = green }, 2 , 1,
													board:set_block( #block{ color = blue }, 2 , 3,

														board:set_block( #block{ color = red }, 3 , 0,
															board:new_empty(5,12))))))))))))),

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( #block{ color = yellow, x = 1, y = 0 }, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = green, x = 1, y = 1 }, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = green, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = green, x = 2, y = 1 }, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( #block{ color = blue, x = 2, y = 2 }, board:get_block( 2, 2, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 3, 0, Result_loop_board ) ),

	ok.




test_simple2_chromatic_bomb(Game_rules) ->
	Board = board:set_block( #block{ color = red }, 0 , 0, 
			board:set_block( #block{ color = red }, 0 , 1, 
			board:set_block( #block{ color = red }, 0 , 2, 
			board:set_block( #block{ type = chromatic_bomb, color = red }, 0, 3, 
			board:set_block( #block{ color = red }, 0 , 4,

				board:set_block( #block{ color = green }, 1 , 0,
				board:set_block( #block{ color = yellow }, 1 , 1,
				board:set_block( #block{ color = green }, 1 , 2,

					board:set_block( #block{ color = green }, 2 , 0 ,
					board:set_block( #block{ color = red }, 2 , 1 ,
					board:set_block( #block{ color = green }, 2 , 2,
					board:set_block( #block{ color = blue }, 2 , 3,
					board:set_block( #block{ color = yellow }, 2 , 4,
					board:set_block( #block{ color = blue }, 2 , 5,
					board:set_block( #block{ color = white }, 2 , 6,

						board:set_block( #block{ color = green }, 3 , 0,
						board:set_block( #block{ color = purple }, 3 , 1,
						board:set_block( #block{ color = red }, 3 , 2,
						board:set_block( #block{ color = green }, 3 , 3,

							board:set_block( #block{ color = white }, 4 , 0,
							board:set_block( #block{ color = white }, 4 , 1,
							board:set_block( #block{ color = green }, 4 , 2,
							board:set_block( #block{ color = purple }, 4 , 3,
								board:new_empty(5,12)))))))))))))))))))))))),

	
	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Board),
	io:format("\nafter\n"),
	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( #block{ color = yellow, x = 1, y = 0 }, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = green, x = 1, y = 1 }, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = blue, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = yellow, x = 2, y = 1 }, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( #block{ color = blue, x = 2, y = 2 }, board:get_block( 2, 2, Result_loop_board ) ),
	?assertMatch( #block{ color = white, x = 2, y = 3 }, board:get_block( 2, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 4, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 5, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 6, Result_loop_board ) ),

	?assertMatch( #block{ color = purple, x = 3, y = 0 }, board:get_block( 3, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = green, x = 3, y = 1 }, board:get_block( 3, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 3, Result_loop_board ) ),

	?assertMatch( #block{ color = white, x = 4, y = 0 }, board:get_block( 4, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = white, x = 4, y = 1 }, board:get_block( 4, 1, Result_loop_board ) ),
	?assertMatch( #block{ color = green, x = 4, y = 2 }, board:get_block( 4, 2, Result_loop_board ) ),
	?assertMatch( #block{ color = purple, x = 4, y = 3 }, board:get_block( 4, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 4, 4, Result_loop_board ) ),

	ok.





test_simple_bomb(Game_rules) ->
	Board = board:set_block( #block{ color = red }, 0 , 0, 
				board:set_block( #block{ color = red }, 0 , 1, 
					board:set_block( #block{ color = red }, 0 , 2, 
						board:set_block( #block{ color = red }, 0, 3, 
							board:set_block( #block{ color = red }, 0 , 4,

								board:set_block( #block{ color = red, type = bomb }, 1 , 0,
									board:set_block( #block{ color = yellow }, 1 , 1,
										board:set_block( #block{ color = green }, 1 , 2,

											board:set_block( #block{ color = green }, 2 , 0 ,
												board:set_block( #block{ color = green }, 2 , 1,
													board:set_block( #block{ color = blue }, 2 , 3,

														board:set_block( #block{ color = green }, 3 , 0,
															board:new_empty(5,12))))))))))))),

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( #block{ color = green, x = 1, y = 0 }, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = blue, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = green, x = 3, y = 0 }, board:get_block( 3, 0, Result_loop_board ) ),

	ok.






test_simple_double_bomb_chain(Game_rules) ->
	Board = board:set_block( #block{ color = red }, 0 , 0, 
				board:set_block( #block{ color = red }, 0 , 1, 
					board:set_block( #block{ color = red }, 0 , 2, 
						board:set_block( #block{ color = red }, 0, 3, 
							board:set_block( #block{ color = red }, 0 , 4,

								board:set_block( #block{ color = red, type = bomb }, 1 , 0,
									board:set_block( #block{ color = yellow }, 1 , 1,
										board:set_block( #block{ color = green }, 1 , 2,

											board:set_block( #block{ color = green }, 2 , 0 ,
												board:set_block( #block{ color = white, type = bomb }, 2 , 1,
													board:set_block( #block{ color = blue }, 2 , 3,

														board:set_block( #block{ color = white }, 3 , 0,
															board:set_block( #block{ color = red }, 3 , 1,
																board:set_block( #block{ color = green }, 3 , 2,
																	board:new_empty(5,12))))))))))))))),

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 2, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 3, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 2, Result_loop_board ) ),

	ok.





test_simple_double_bomb_chain2(Game_rules) ->
	Board = board:set_block( #block{ color = red }, 0 , 0, 
				board:set_block( #block{ color = red }, 0 , 1, 
					board:set_block( #block{ color = red }, 0 , 2, 
						board:set_block( #block{ color = red }, 0, 3, 
							board:set_block( #block{ color = red }, 0 , 4,

								board:set_block( #block{ color = red, type = bomb }, 1 , 0,
									board:set_block( #block{ color = yellow }, 1 , 1,
										board:set_block( #block{ color = green }, 1 , 2,

											board:set_block( #block{ color = green }, 2 , 0 ,
												board:set_block( #block{ color = white, type = bomb }, 2 , 1,
													board:set_block( #block{ color = blue }, 2 , 3,
														board:set_block( #block{ color = purple }, 2 , 4,

															board:set_block( #block{ color = purple }, 3 , 0,
																board:set_block( #block{ color = white }, 3 , 1,
																	board:set_block( #block{ color = red }, 3 , 2,
																		board:set_block( #block{ color = red }, 3 , 3,
																			board:new_empty(5,12))))))))))))))))),

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = purple, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 3, Result_loop_board ) ),

	?assertMatch( #block{ color = red, x = 3, y = 0 }, board:get_block( 3, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 3, Result_loop_board ) ),

	ok.







test_simple_bomb_chain_chromatic_bom(Game_rules) ->
	Board = board:set_block( #block{ color = red }, 0 , 0, 
				board:set_block( #block{ color = red }, 0 , 1, 
					board:set_block( #block{ color = red }, 0 , 2, 
						board:set_block( #block{ color = red }, 0, 3, 
							board:set_block( #block{ color = red }, 0 , 4,

								board:set_block( #block{ color = red, type = bomb }, 1 , 0,
									board:set_block( #block{ color = yellow }, 1 , 1,
										board:set_block( #block{ color = green }, 1 , 2,

											board:set_block( #block{ color = green }, 2 , 0 ,
												board:set_block( #block{ type = chromatic_bomb, color = purple }, 2 , 1,
													board:set_block( #block{ color = blue }, 2 , 3,
														board:set_block( #block{ color = purple }, 2 , 4,

															board:set_block( #block{ color = purple }, 3 , 0,
																board:set_block( #block{ color = white }, 3 , 1,
																	board:set_block( #block{ color = red }, 3 , 2,
																		board:set_block( #block{ color = red }, 3 , 3,
																			board:new_empty(5,12))))))))))))))))),

	{ Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( #block{ color = green, x = 1, y = 0 }, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = blue, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 3, Result_loop_board ) ),

	?assertMatch( #block{ color = white, x = 3, y = 0 }, board:get_block( 3, 0, Result_loop_board ) ),
	?assertMatch( #block{ color = red, x = 3, y = 1 }, board:get_block( 3, 1, Result_loop_board ) ),
	?assertMatch( #block{ color = red, x = 3, y = 2 }, board:get_block( 3, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 3, Result_loop_board ) ),


	_Garbage_position_list = calculate_garbage_from_combos( Combos, Result_loop_board, Game_rules ),


	ok.


%% --------------------         GARBAGE              ------------------------------------------



test_garbage_position( Garbage_position_list, Board = #board{} ) ->
	
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
				New_value = 0,
				New_cache = [ { Position, 1 } | Cache];
			Value ->
				New_value = Value + 1,
				New_cache =  [ { Position, New_value } | proplists:delete( Position ,Cache) ]
		end,

		New_max = case New_value > Max of
			true ->		New_value;
			false ->	Max
		end,

		{ New_cache, New_max }

	end,
	{ Cache , Max } = lists:foldl(Fun, { [], 0 }, Garbage_position_list),

	Min = lists:foldl( fun( {_Position, Value}, Min ) -> 
		case Value < Min of  
			true ->	Value;
			false -> Min
		end
	end, 999, Cache ),


	Max =< Min +1.







test_release_garbage_into_full_column( _Game_rules )->
	
	Board = board:set_block( #block{ color = red }, 1 , 11, 
				board:set_block( #block{ color = yellow }, 1 , 10, 
					board:set_block( #block{ color = blue }, 1 , 9, 
						board:set_block( #block{ color = green }, 1 , 8, 
							board:set_block( #block{ color = yellow }, 1 , 7, 
								board:set_block( #block{ color = green }, 1 , 6,
									board:set_block( #block{ color = yellow }, 1 , 5,
										board:set_block( #block{ color = green }, 1 , 4,
											board:set_block( #block{ color = blue }, 1 , 3 ,
												board:set_block( #block{ color = green }, 1 , 2,
													board:set_block( #block{ color = blue }, 1 , 1,
														board:set_block( #block{ color = green }, 1 , 0,
															board:new_empty(5,12))))))))))))),

	?assertThrow(out_of_bounds, release_garbage_list( Board, [{garbage,1}] ) ),

	ok.




%% --------------------         GAMEPLAY LOOP              ------------------------------------------





test_simple_gameplay_loop(Game_rules) ->
	Board = board:set_block( #block{ color = red }, 0 , 0, 
			board:set_block( #block{ color = red }, 0 , 1, 
			board:set_block( #block{ color = red }, 0 , 2, 
			board:set_block( #block{ color = red }, 0, 3, 
			board:set_block( #block{ color = red }, 0 , 4,
			board:set_block( #block{ color = green }, 0 , 5,

				board:set_block( #block{ color = green }, 1 , 0,
				board:set_block( #block{ color = blue }, 1 , 1,
				board:set_block( #block{ color = yellow }, 1 , 2,

					board:set_block( #block{ color = green }, 2 , 0 ,
					board:set_block( #block{ color = green }, 2 , 1,
					board:set_block( #block{ color = blue }, 2 , 3,
					board:set_block( #block{ color = purple }, 2 , 4,

						board:set_block( #block{ color = blue }, 3 , 0,
						board:set_block( #block{ color = white }, 3 , 1,

							board:set_block( #block{ color = blue }, 4 , 0,
										
								board:new_empty(5,12))))))))))))))))),

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( #block{ color = yellow, x = 1, y = 0 }, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = purple, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 3, Result_loop_board ) ),

	?assertMatch( #block{ color = white, x = 3, y = 0 }, board:get_block( 3, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 2, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 4, 0, Result_loop_board ) ),

	ok.






test_simple2_gameplay_loop(Game_rules) ->
	Board = board:set_block( #block{ color = red }, 0 , 0, 
			board:set_block( #block{ color = red }, 0 , 1, 
			board:set_block( #block{ color = red }, 0 , 2, 
			board:set_block( #block{ color = red }, 0, 3, 
			board:set_block( #block{ color = red }, 0 , 4,
			board:set_block( #block{ color = green }, 0 , 5,

				board:set_block( #block{ color = green }, 1 , 0,
				board:set_block( #block{ color = blue }, 1 , 1,
				board:set_block( #block{ color = yellow }, 1 , 2,

					board:set_block( #block{ color = green }, 2 , 0 ,
					board:set_block( #block{ color = green }, 2 , 1,
					board:set_block( #block{ color = blue }, 2 , 3,
					board:set_block( #block{ color = purple }, 2 , 4,

						board:set_block( #block{ color = blue }, 3 , 0,
						board:set_block( #block{ color = white }, 3 , 1,

							board:set_block( #block{ color = blue }, 4 , 0,
										
								board:new_empty(5,12))))))))))))))))),

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board, Game_rules ),

	board:print_board(Result_loop_board),

	?assertMatch( empty, board:get_block( 0, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 3, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 0, 4, Result_loop_board ) ),

	?assertMatch( #block{ color = yellow, x = 1, y = 0 }, board:get_block( 1, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 1, 2, Result_loop_board ) ),

	?assertMatch( #block{ color = purple, x = 2, y = 0 }, board:get_block( 2, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 2, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 2, 3, Result_loop_board ) ),

	?assertMatch( #block{ color = white, x = 3, y = 0 }, board:get_block( 3, 0, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 1, Result_loop_board ) ),
	?assertMatch( empty, board:get_block( 3, 2, Result_loop_board ) ),

	?assertMatch( empty, board:get_block( 4, 0, Result_loop_board ) ),

	ok.



%% --------------------         GRAVITY              ------------------------------------------





test_simple2_gravity(_Game_rules) ->

	Board = board:set_block( #block{ color = green }, 5 , 5,
				board:set_block( #block{ color = red }, 5 , 0,
					board:set_block( #block{ color = green }, 1 , 4,
						board:set_block( #block{ color = blue }, 2 , 3,
							board:set_block( #block{ color = yellow }, 3 , 7,
								board:set_block( #block{ color = yellow }, 3 , 6,
									board:set_block( #block{ color = red }, 3 , 2,
										board:new_empty(6,12)))))))),

	Board_after_gravity = simulate_gravity( Board ),

	?assertMatch( #block{ color = red, x = 3, y = 0 }, board:get_block( 3, 0, Board_after_gravity ) ),
	?assertMatch( #block{ color = yellow, x = 3, y = 1 }, board:get_block( 3, 1, Board_after_gravity ) ),
	?assertMatch( #block{ color = yellow, x = 3, y = 2 }, board:get_block( 3, 2, Board_after_gravity ) ),
	?assertMatch( #block{ color = blue, x = 2, y = 0 }, board:get_block( 2, 0, Board_after_gravity ) ),
	?assertMatch( #block{ color = green, x = 1, y = 0 }, board:get_block( 1, 0, Board_after_gravity ) ),
	?assertMatch( #block{ color = red, x = 5, y = 0 }, board:get_block( 5, 0, Board_after_gravity ) ),
	?assertMatch( #block{ color = green, x = 5, y = 1 }, board:get_block( 5, 1, Board_after_gravity ) ),

	ok.





test_simple_gravity(_Game_rules) ->
	
	Board = board:set_block( #block{ color = green }, 1 , 1,
				board:set_block( #block{ color = blue }, 2 , 0,
					board:set_block( #block{ color = yellow }, 3 , 3,
				 		board:set_block( #block{ color = red }, 3 , 1,
				 			board:new_empty(5,12))))),

	Board_after_gravity = simulate_gravity( Board ),

	?assertMatch( #block{ color = red, x = 3, y = 0 }, board:get_block( 3, 0, Board_after_gravity ) ),
	?assertMatch( #block{ color = yellow, x = 3, y = 1 }, board:get_block( 3, 1, Board_after_gravity ) ),
	?assertMatch( #block{ color = blue, x = 2, y = 0 }, board:get_block( 2, 0, Board_after_gravity ) ),
	?assertMatch( #block{ color = green, x = 1, y = 0 }, board:get_block( 1, 0, Board_after_gravity ) ),

	ok.






%% --------------------         COBMOS              ------------------------------------------

test_simple_combo(Game_rules) ->

	Board = board:set_block( #block{ color = yellow }, 3 , 2,
				board:set_block( #block{ color = green }, 2 , 2,
					board:set_block( #block{ color = yellow }, 2 , 0,
						board:set_block( #block{ color = blue }, 4 , 0,
							board:set_block( #block{ color = red }, 4 , 2,
								board:set_block( #block{ color = red }, 4 , 1,
									board:set_block( #block{ color = red }, 3 , 1,
										board:set_block( #block{ color = red }, 3 , 0,
											board:new_empty(5,12))))))))),

	Combos = calculate_combos( Board, Game_rules ),

	?assertMatch( 1, length( Combos ) ),
	
	[Combo] = Combos,

	?assertMatch(4, sets:size(Combo) ),

	?assert( sets:is_element(#block{ color = red, x = 3, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 2},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 3, y = 0},Combo) ),

	ok.






test_simple_combo_with_chromatic(Game_rules) ->

	Board = board:set_block( #block{ color = yellow }, 1 , 2,
			board:set_block( #block{ color = green }, 2 , 2,
			board:set_block( #block{ color = yellow }, 2 , 0,
				
				board:set_block( #block{ color = blue }, 4 , 0,
				board:set_block( #block{ color = red }, 4 , 1,
				board:set_block( #block{ color = red }, 4 , 2,
				
					board:set_block( #block{ color = red }, 3 , 0,
					board:set_block( #block{ color = red }, 3 , 1,
					board:set_block( #block{ color = red, type = chromatic_bomb }, 3 , 2,
											board:new_empty(5,12)))))))))),

	Combos = calculate_combos( Board, Game_rules ),

	?assertMatch( 1, length( Combos ) ),
	
	[Combo] = Combos,

	?assertMatch(5, sets:size(Combo) ),

	?assert( sets:is_element(#block{ color = red, x = 3, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 2},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 3, y = 0},Combo) ),

	ok.





test_double_combo(Game_rules) ->

	Board = board:set_block( #block{ color = yellow }, 1 , 2,
				board:set_block( #block{ color = red }, 5 , 1,
					board:set_block( #block{ color = yellow }, 5 , 0,
						board:set_block( #block{ color = blue }, 3 , 2,

							board:set_block( #block{ color = green }, 4 , 1,
								board:set_block( #block{ color = green }, 3 , 1,
									board:set_block( #block{ color = green }, 2 , 2,
										board:set_block( #block{ color = green }, 2 , 1,

											board:set_block( #block{ color = blue }, 1 , 1,
												board:set_block( #block{ color = blue }, 4 , 0,
													board:set_block( #block{ color = blue }, 3 , 0,
														board:set_block( #block{ color = blue }, 2 , 0,
															board:set_block( #block{ color = blue }, 1 , 0,
																board:new_empty(6,12)))))))))))))),

	Combos = calculate_combos( Board, Game_rules ),

%	Lists = lists:map( fun( Set ) ->  sets:to_list(Set) end, Combos),
%	io:format("All_Combos ~p",[Lists]),	

	?assertMatch( 2, length( Combos ) ),

	[Combo1 , Combo2] = Combos,

	case sets:size(Combo1) of
		5 ->
			?assert( sets:is_element(#block{ color = blue, x = 1, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 2, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 3, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 4, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 1, y = 1},Combo1) ),

			?assertMatch( 4, sets:size(Combo2) ),
			?assert( sets:is_element(#block{ color = green, x = 2, y = 1},Combo2) ),
			?assert( sets:is_element(#block{ color = green, x = 2, y = 2},Combo2) ),
			?assert( sets:is_element(#block{ color = green, x = 3, y = 1},Combo2) ),
			?assert( sets:is_element(#block{ color = green, x = 4, y = 1},Combo2) );

		4 ->
			?assert( sets:is_element(#block{ color = blue, x = 1, y = 0},Combo2) ),
			?assert( sets:is_element(#block{ color = blue, x = 2, y = 0},Combo2) ),
			?assert( sets:is_element(#block{ color = blue, x = 3, y = 0},Combo2) ),
			?assert( sets:is_element(#block{ color = blue, x = 4, y = 0},Combo2) ),
			?assert( sets:is_element(#block{ color = blue, x = 1, y = 1},Combo2) ),

			?assertMatch( 4, sets:size(Combo1) ),
			?assert( sets:is_element(#block{ color = green, x = 2, y = 1},Combo1) ),
			?assert( sets:is_element(#block{ color = green, x = 2, y = 2},Combo1) ),
			?assert( sets:is_element(#block{ color = green, x = 3, y = 1},Combo1) ),
			?assert( sets:is_element(#block{ color = green, x = 4, y = 1},Combo1) );

		_other ->
			throw("one of the combos isnt the correct size") 
	end,
	ok.





test_no_combos(Game_rules) ->
	
	Board = board:set_block( #block{ color = yellow }, 1 , 2,
				board:set_block( #block{ color = blue }, 1 , 1,
					board:set_block( #block{ color = green }, 4 , 0,
						board:set_block( #block{ color = yellow }, 3 , 0,
							board:set_block( #block{ color = blue }, 2 , 0,
								board:set_block( #block{ color = red }, 1 , 0,
									board:new_empty(5,12))))))),

	Combos = calculate_combos( Board, Game_rules ),

	?assert( length( Combos ) == 0),
	ok.






test_simple_garbage_poping_combos(Game_rules) ->
	Board = board:set_block( #block{ color = yellow }, 3 , 2,
				board:set_block( #block{ color = green }, 2 , 2,
					board:set_block( #block{ color = yellow }, 2 , 0,
						board:set_block( #block{ color = blue }, 4 , 0,

							board:set_block( #block{ color = red }, 4 , 2,
								board:set_block( #block{ color = red }, 4 , 1,
									board:set_block( #block{ color = red }, 3 , 1,
										board:set_block( #block{ color = red }, 3 , 0,

											board:set_block( #block{ type = garbage }, 4 , 3,
												board:set_block( #block{ type = garbage }, 2 , 1,
													board:new_empty(5,12))))))))))),

	Combos = calculate_combos( Board, Game_rules ),

	?assertMatch( 1, length( Combos ) ),
	
	[Combo] = Combos,

	?assertMatch(4, sets:size(Combo) ),

	?assert( sets:is_element(#block{ color = red, x = 3, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 2},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 3, y = 0},Combo) ),


	New_board = pop_combos( Board, Combos ),

	?assertMatch( #block{ color = yellow, x = 3, y = 2 }, board:get_block( 3, 2, New_board ) ),
	?assertMatch( #block{ color = green, x = 2, y = 2 }, board:get_block( 2, 2, New_board ) ),
	?assertMatch( #block{ color = yellow, x = 2, y = 0 }, board:get_block( 2, 0, New_board ) ),
	?assertMatch( #block{ color = blue, x = 4, y = 0 }, board:get_block( 4, 0, New_board ) ),


	?assertMatch( empty, board:get_block( 4, 2, New_board ) ),
	?assertMatch( empty, board:get_block( 4, 1, New_board ) ),
	?assertMatch( empty, board:get_block( 3, 1, New_board ) ),
	?assertMatch( empty, board:get_block( 3, 0, New_board ) ),

	?assertMatch( empty, board:get_block( 4, 3, New_board ) ),
	?assertMatch( empty, board:get_block( 2, 1, New_board ) ),
	
	ok.






test_simple_poping_combos(Game_rules) ->
	Board = board:set_block( #block{ color = yellow }, 3 , 2,
				board:set_block( #block{ color = green }, 2 , 2,
					board:set_block( #block{ color = yellow }, 2 , 0,
						board:set_block( #block{ color = blue }, 4 , 0,
							board:set_block( #block{ color = red }, 4 , 2,
								board:set_block( #block{ color = red }, 4 , 1,
									board:set_block( #block{ color = red }, 3 , 1,
										board:set_block( #block{ color = red }, 3 , 0,
											board:new_empty(5,12))))))))),

	Combos = calculate_combos( Board, Game_rules ),

	?assertMatch( 1, length( Combos ) ),
	
	[Combo] = Combos,

	?assertMatch(4, sets:size(Combo) ),

	?assert( sets:is_element(#block{ color = red, x = 3, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 2},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 3, y = 0},Combo) ),


	New_board = pop_combos( Board, Combos ),

	?assertMatch( #block{ color = yellow, x = 3, y = 2 }, board:get_block( 3, 2, New_board ) ),
	?assertMatch( #block{ color = green, x = 2, y = 2 }, board:get_block( 2, 2, New_board ) ),
	?assertMatch( #block{ color = yellow, x = 2, y = 0 }, board:get_block( 2, 0, New_board ) ),
	?assertMatch( #block{ color = blue, x = 4, y = 0 }, board:get_block( 4, 0, New_board ) ),


	?assertMatch( empty, board:get_block( 4, 2, New_board ) ),
	?assertMatch( empty, board:get_block( 4, 1, New_board ) ),
	?assertMatch( empty, board:get_block( 3, 1, New_board ) ),
	?assertMatch( empty, board:get_block( 3, 0, New_board ) ),


	ok.


%% --------------------         PLACE PIECE              ------------------------------------------





test_simple_up_place_piece(_Game_rules) ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 1, down, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch( #block{ color = green, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 1, y = 2 }, board:get_block( 1, 2, Board_result ) ),

	ok.



test_simple_down_place_piece(_Game_rules) ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 2, up, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch( #block{ color = green, x = 1, y = 2 }, board:get_block( 1, 2, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),

	ok.




test_simple_right_piece(_Game_rules) ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 1, left, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch( #block{ color = green, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 2, y = 0 }, board:get_block( 2, 0, Board_result ) ),

	ok.




test_simple_left_place_piece(_Game_rules) ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 2, 1, right, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch(  #block{ color = green, x = 2, y = 0 }, board:get_block( 2, 0, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),

	ok.





test_simple_invalid_place_piece(_Game_rules) ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),

	Piece = #piece{ block1 = #block{ color = red }, block2 = #block{ color = blue } },

	?assertThrow(invalid_move, place_piece(  Piece, 1, 0, down, Board )  ),

	ok.





%% --------------------                       ------------------------------------------





-endif.







