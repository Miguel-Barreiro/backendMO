-module(game_logic).

-include("include/softstate.hrl").

-export([ handle_place_piece/5, handle_update_piece/5, create_new_game/3, test_random/1 ]).

-define( BOARD_WIDTH , 6).
-define( BOARD_HEIGHT , 13).
-define( STARTING_PIECE_X , 3).


-define(HARD_GARBAGE_RATIO, 1).
-define(COLOR_GARBAGE_RATIO, 2).

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
	#game{ user1_gamestate = User1_gamestate, user2_gamestate = User2_gamestate, initial_seed = Initial_seed }.






%throws out_of_bounds (in case the user has lost)
%throws invalid_move (in case of an invalid move)
handle_place_piece( User_pid, X, Y, Angle,  Game = #game{}  ) when User_pid == (Game#game.user1_gamestate)#user_gamestate.user_pid->
	Opponent_pid = (Game#game.user2_gamestate)#user_gamestate.user_pid,
	{New_gamestate, New_opponent_gamestate} = handle_place_piece( User_pid, 
																	Opponent_pid,
																		(Game#game.user1_gamestate)#user_gamestate.current_piece, 
																			X, Y, Angle, 
																				Game#game.user1_gamestate, 
																					Game#game.user2_gamestate ),

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
																					Game#game.user1_gamestate ),

	io:format("-------------------------------- ",[]),
	io:format(" USER 2 ~p PLACE A PIECE ",[User_pid]),
	board:print_board( New_gamestate#user_gamestate.board ),
	io:format("-------------------------------- ",[]),

	Game#game{ user2_gamestate = New_gamestate, user1_gamestate = New_opponent_gamestate }.


handle_place_piece( User_pid, Opponent_pid, Piece = #piece{}, X, Y, Angle, Gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{} ) ->
	lager:debug("placed the piece in ~p,~p",[X,Y]),
	case Piece == Gamestate#user_gamestate.current_piece of	
		false ->
			lager:debug("invalid piece place: wrong piece",[]),
			throw( invalid_move );
		true ->
			Board_after_place_piece = place_piece( Piece, X, Y, Angle, Gamestate#user_gamestate.board),

			{ Combos , Result_loop_board } = apply_gravity_combo_loop( Board_after_place_piece ),
			
			Board_after_release_garbage = release_garbage_list( Result_loop_board, Gamestate#user_gamestate.garbage_position_list ),

			{ New_gamestate_after_piece, Next_piece} = calculate_next_piece( Gamestate , Combos ),

			Generated_garbage_position_list = calculate_garbage_from_combos( Combos, Result_loop_board ),

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
												current_piece_y = ?BOARD_HEIGHT - 1,
													piece_generation_step = New_gamestate_after_piece#user_gamestate.piece_generation_step + 1 },

			New_opponent_garbage_list = lists:append( Generated_garbage_position_list, Opponent_gamestate#user_gamestate.garbage_position_list ),
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


apply_gravity_combo_loop( Board = #board{} ) ->
	Board_after_gravity = simulate_gravity( Board ),
	lager:debug("apply gravity"),
	Combos = calculate_combos( Board_after_gravity ),
	case Combos of
		[] ->
			{ [], Board_after_gravity};
		_other ->
			Board_after_pop_combos = pop_combos( Board_after_gravity, Combos ),
			lager:debug("poped combos"),
			{ New_Combos , New_board} = apply_gravity_combo_loop( Board_after_pop_combos ),
			{ [ Combos | New_Combos ] , New_board}
	end.







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










calculate_combos( Board = #board{} )->
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

	Pred_minimum_4 = fun( Combo ) ->
		sets:size( Combo ) >= 4
	end,
	lists:filter(Pred_minimum_4, All_Combos).






pop_combos( Board = #board{}, Combo_list ) ->
	Fun = fun( Combo, New_board )->
		pop_combo( New_board, Combo )
	end,
	lists:foldl( Fun, Board, Combo_list ).


pop_combo( Board = #board{}, Combo ) ->
	Fun = fun( Block = #block{}, New_board )->
		pop_combo_block(  Block#block.x, Block#block.y, New_board )
	end,
	lists:foldl( Fun, Board, sets:to_list(Combo)).



pop_combo_block(  X, Y, Board = #board{} ) ->
	New_board_witout_block = pop_block(  X, Y, Board ),
	pop_garbages_around( New_board_witout_block, X, Y ).





pop_garbages_around( Board = #board{}, X, Y) ->
	pop_garbage_in( X + 1, Y, pop_garbage_in( X - 1, Y, pop_garbage_in( X, Y + 1, pop_garbage_in( X, Y - 1, Board)))).

pop_garbage_in( X, Y, Board = #board{}) ->
	case board:get_block( X, Y , Board) of
		
		empty ->
			Board;

		Garbage_block when Garbage_block#block.type == garbage ->
			board:remove_block( X, Y, Board);

		Garbage_block when Garbage_block#block.type == garbage_hard, Garbage_block#block.hardness == 1 ->
			board:remove_block( X, Y, Board);

		Garbage_block when Garbage_block#block.type == garbage_hard ->
			board:set_block( #block{ type = garbage_hard , hardness = Garbage_block#block.hardness - 1 }, X, Y,board:remove_block( X, Y, Board));

		Garbage_block when Garbage_block#block.type == garbage_color ->
			board:set_block( #block{ type = color , color = Garbage_block#block.color }, X, Y, board:remove_block( X, Y , Board) );

		_other_blocks ->
			Board
	end.





pop_block(  X, Y, Board = #board{} ) ->
	case board:get_block( X, Y , Board) of
		empty ->
			Board;

		Garbage_block when Garbage_block#block.type == garbage_hard, Garbage_block#block.hardness == 1 ->
			board:remove_block( X, Y , Board);			

		Garbage_block when Garbage_block#block.type == garbage_hard ->
			board:set_block( #block{ type = garbage_hard , hardness = Garbage_block#block.hardness - 1 }, X, Y,
							 	board:remove_block( X, Y , Board));

		Garbage_block when Garbage_block#block.type == garbage ->
			board:remove_block( X, Y , Board);

		Garbage_block when Garbage_block#block.type == garbage_color ->
			board:set_block( #block{ type = color , color = Garbage_block#block.color }, X, Y, board:remove_block( X, Y , Board) );

		Bomb_block when Bomb_block#block.type == bomb ->
			pop_bomb( X, Y, Board);

		Chromatic_block when Chromatic_block#block.type == chromatic_bomb ->
			pop_chromatic_bomb(X, Y, Chromatic_block#block.color, Board);

		Color_block when Color_block#block.type == color ->
			board:remove_block( X, Y , Board)
	end.




pop_bomb( X, Y, Board = #board{}) ->
	io:format("poping bomb in ~p,~p \n",[X, Y]),
	pop_block( X + 1, Y ,
		pop_block( X - 1, Y ,
			pop_block( X, Y + 1,
				pop_block( X, Y -1,

					pop_block( X + 1, Y + 1,
						pop_block( X + 1, Y - 1,
							pop_block( X - 1, Y + 1,
								pop_block( X - 1, Y -1,
									board:remove_block( X, Y , Board))))))))).




remove_all_same_color( Color, Board = #board{} ) ->	
	Fun = fun( Block = #block{}, New_board = #board{} )->
		case Block#block.type of
			color when Block#block.color == Color ->
				io:format("\nremoving same color ~p block in ~p,~p",[Color,Block#block.x, Block#block.y]),
				board:remove_block( Block#block.x, Block#block.y , New_board);
			_other ->
				New_board
		end
	end,
	lists:foldl( Fun, Board, board:get_all_blocks(Board)).


pop_chromatic_bomb( X, Y, Color, Board = #board{}) ->
	io:format("poping ~p chromatic in ~p,~p \n",[Color,X,Y]),
	New_board = remove_all_same_color( Color, Board),
	board:remove_block( X, Y, New_board).







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




calculate_garbage_from_combos( [], _ ) ->
	[];

calculate_garbage_from_combos( Combos, Board = #board{} ) ->
	Sum_garbage_from_combos = 
	fun( Combo_sequence, { Combo_number_garbage, Combo_sequence_length_garbage } )->
		{ Combo_number_garbage + lists:foldl( fun( Combo, Acc2 ) -> Acc2 + calculate_garbage_from_combo( Combo ) end, 0, Combo_sequence) ,
			Combo_sequence_length_garbage + (length( Combo_sequence ) - 1 ) * ?HARD_GARBAGE_RATIO }
	end,

	Combo_sequence_garbage = (length( Combos ) - 1) * ?COLOR_GARBAGE_RATIO,

	{ Combo_number_garbage, Combo_sequence_length_garbage } = lists:foldl( Sum_garbage_from_combos, {0,0}, Combos),

	generate_garbage_positions( Combo_sequence_length_garbage, Combo_sequence_garbage, Combo_number_garbage, Board ).






calculate_next_piece( Gamestate = #user_gamestate{} , Combos ) ->
	{ New_random_state, Random } = get_next_random( Gamestate#user_gamestate.random_state ),
	{ New_random_state2, Random2 } = get_next_random( New_random_state ),

	{ Color, Type } = get_block_color_type(Random, Combos),
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




get_block_color_type( Random, Combos ) ->
	Color = case Random rem 6 of
		0 ->		red;
		1 ->		yellow;
		2 ->		blue;
		3 ->		green;
		4 ->		purple;
		5 ->		white
	end,
	Type = case lists:any( fun( Combo_Sequence ) -> 
								lists:any( fun(Combo) ->sets:size(Combo) > 4 end, Combo_Sequence) 
							end, Combos) of
		true ->			bomb;
		false ->		color
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







test_random( Number ) ->
	test_random(1999, 1, Number).

test_random( _ , Current, Max) when Current == Max ->
	ok;
test_random(Random_state, Current, Max) ->
	{ Random_state2, Random}  = get_next_random( Random_state ),
	lager:debug("~p -> ~p",[Current,Random]),
	test_random(Random_state2, Current + 1, Max).
	










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




generate_random_garbage_color() ->
	case random:uniform( 6 ) of
		1 ->	purple;
		2 ->	blue;
		3 ->	green;
		4 ->	yellow;
		5 ->	red;
		6 ->	white
	end.



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

				Current_block when Current_block#block.type =/= garbage,
										Current_block#block.type =/= garbage_hard,
											Current_block#block.type =/= garbage_color, Current_block#block.color == Block#block.color ->

					New_combo = sets:add_element( Current_block, Combo ),

					{New_combo2, New_visited2 } = calculate_combo_for_piece( Block, X , Y + 1 , New_combo, New_visited, Board),
					{New_combo3, New_visited3 } = calculate_combo_for_piece( Block, X + 1 , Y , New_combo2, New_visited2, Board),
					{New_combo4, New_visited4 } = calculate_combo_for_piece( Block, X - 1 , Y , New_combo3, New_visited3, Board),
					calculate_combo_for_piece( Block, X, Y - 1 , New_combo4, New_visited4, Board);

				_Current_block ->
					{ Combo, New_visited }
			end
	end.







calculate_garbage_from_combo( Combo ) ->
	case sets:size(Combo) of
		4 ->		1;
		5 ->		2;
		6 ->		3;
		7 ->		4;
		8 ->		5;
		_other ->	5
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
		X = ?BOARD_WIDTH - proplists:get_value(<<"x">>, Block_properties) + 1 + X_offset,
		Y = ?BOARD_HEIGHT - proplists:get_value(<<"y">>, Block_properties) + 1,

		{Real_block_type, Color} = 
		case Value of
			<<"w">> -> {color,white};
			<<"p">> -> {color,purple};
			<<"b">> -> {color,blue};
			<<"r">> -> {color,red};
			<<"g">> -> {color,green};
			<<"y">> -> {color,yellow};

			<<"+">> -> {garbage,red};
			<<"#">> -> {garbage_hard,red};

			<<"#w">> -> {garbage_color,white};
			<<"#p">> -> {garbage_color,purple};
			<<"#b">> -> {garbage_color,blue};
			<<"#r">> -> {garbage_color,red};
			<<"#g">> -> {garbage_color,green};
			<<"#y">> -> {garbage_color,yellow};
			
			<<"W">> -> {bomb,white};
			<<"P">> -> {bomb,purple};
			<<"B">> -> {bomb,blue};
			<<"G">> -> {bomb,green};
			<<"R">> -> {bomb,red};
			<<"Y">> -> {bomb,yellow}
		end,

		board:set_block( #block{ type = Real_block_type, color = Color}, X, Y, Result)
	end,

	lists:foldl( Fun , board:new_empty( ?BOARD_WIDTH, ?BOARD_HEIGHT), Block_list ).



google_docs_test_() ->

		{ok, Dir} = file:get_cwd(),
		io:format("filename is ~p",[Dir ++ "/../scripts/RESULT.json"]),
		{ok, Binary} = file:read_file(Dir ++ "/../scripts/RESULT.json"),
		
		Fun = fun( {Test_data}, Result) ->

			Final = proplists:get_value(<<"final">>,Test_data),
			Start = proplists:get_value(<<"origin">>,Test_data),
			Test_name = proplists:get_value(<<"name">>,Test_data),
			{Generated_garbage} = proplists:get_value(<<"garbage">>,Test_data),

			Color_garbage = proplists:get_value(<<"colorGarbage">>,Generated_garbage),
			Hard_garbage = proplists:get_value(<<"hardGarbage">>,Generated_garbage),
			Normal_garbage = proplists:get_value(<<"normalGarbage">>,Generated_garbage),
			Spawns_bomb = proplists:get_value(<<"spawnsBomb">>,Generated_garbage),

			Start_board = create_board( Start, 0 ),
			Final_board = create_board( Final, 9 ),

		%	io:format("----------\n ~p  \n-----",[Start]),

			{ Combos , Result_loop_board } = apply_gravity_combo_loop( Start_board ),
			Garbage_position_list = calculate_garbage_from_combos( Combos, Result_loop_board ),

		%	io:format("\nanswer is ~p\n",[Result_loop_board]),
		%	io:format("final is ~p",[Final_board]),
		%	io:format("\ncomparison is ~p\n",[board:are_boards_equal(Result_loop_board,Final_board)])
		%io:format("garbage list is ~p",[Garbage_position_list]),

			[ {binary_to_list(Test_name),
				fun () ->  			
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
					io:format("garbage ~p , garbage_hard ~p,  garbage_color ~p",[Calculated_number_normal_garbages,
																					Calculated_number_hard_garbages,	
										 												Calculated_number_color_garbages ] ),

					?assertMatch( Normal_garbage, Calculated_number_normal_garbages ),
					?assertMatch( Hard_garbage, Calculated_number_hard_garbages ),
					?assertMatch( Color_garbage, Calculated_number_color_garbages ),
					?assert( test_garbage_position( Garbage_position_list, Final_board ))
				end } | Result]

		end,
		lists:foldl( Fun,[], ejson:decode(Binary)).



%% --------------------         POWERS             ------------------------------------------


multiple_bombs_test() ->

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

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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




simple_chromatic_bomb_test() ->
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

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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


simple2_chromatic_bomb_test() ->
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

	

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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


simple_bomb_test() ->
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

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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


simple_double_bomb_chain_test() ->
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

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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


simple_double_bomb_chain2_test() ->
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

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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



simple_bomb_chain_chromatic_bom_test() ->
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

	{ Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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


	Garbage_position_list = calculate_garbage_from_combos( Combos, Result_loop_board ),


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





release_garbage_into_full_column_test()->
	
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

	?assertThrow(out_of_bounds, release_garbage_list( Board, [1] ) ),

	ok.



no_combo_garbage_test() ->
	Board = board:new_empty(6,12),

	Garbage_position_list = calculate_garbage_from_combos( [], Board ),

	?assertMatch( 0, length( Garbage_position_list ) ),
	?assert( test_garbage_position( Garbage_position_list, Board )),

	ok.


five_combo_garbage_test() ->
	Board = board:new_empty(6,12),
	Combo = sets:add_element( #block{ color = red, x = 3, y = 0 } ,
				sets:add_element( #block{ color = red, x = 3, y = 1 },
					sets:add_element( #block{ color = red, x = 3, y = 2 },
						sets:add_element( #block{ color = red, x = 3, y = 3 }, sets:new())))),

	Garbage_position_list = calculate_garbage_from_combos( [[Combo, Combo, Combo, Combo, Combo]], Board ),

	io:format("garbage list is ~p",[Garbage_position_list]),

	?assertMatch( 9, length( Garbage_position_list ) ),
	?assertMatch( 4, length( lists:filter( fun( {Garbage,_} )-> Garbage == garbage_hard end, Garbage_position_list) ) ),
	?assertMatch( 5, length( lists:filter( fun( {Garbage,_} )-> Garbage == garbage end, Garbage_position_list) ) ),
	?assert( test_garbage_position( Garbage_position_list, Board )),

	ok.


single_combo_garbage_test() ->
	Board = board:new_empty(6,12),
	Combo = sets:add_element( #block{ color = red, x = 3, y = 0 } ,
				sets:add_element( #block{ color = red, x = 3, y = 1 },
					sets:add_element( #block{ color = red, x = 3, y = 2 },
						sets:add_element( #block{ color = red, x = 3, y = 3 }, sets:new())))),

	Garbage_position_list = calculate_garbage_from_combos( [[Combo]], Board ),

	?assertMatch( 1, length( Garbage_position_list ) ),
	?assertMatch( 1, length( lists:filter( fun( {Garbage,_} )-> Garbage == garbage end, Garbage_position_list) ) ),
	?assert( test_garbage_position( Garbage_position_list, Board )),

	ok.


double_combo_garbage_test() ->

	Board = board:new_empty(6,12),

	Combo = sets:add_element( #block{ color = red, x = 3, y = 0 } ,
				sets:add_element( #block{ color = red, x = 3, y = 1 },
					sets:add_element( #block{ color = red, x = 3, y = 2 },
						sets:add_element( #block{ color = red, x = 3, y = 3 }, sets:new())))),

	Combo1 = sets:add_element( #block{ color = blue, x = 3, y = 0 } ,
				sets:add_element( #block{ color = blue, x = 3, y = 1 },
					sets:add_element( #block{ color = blue, x = 3, y = 2 },
						sets:add_element( #block{ color = blue, x = 4, y = 2 }, 
							sets:add_element( #block{ color = blue, x = 4, y = 0 }, 
								sets:add_element( #block{ color = blue, x = 5, y = 0 }, sets:new())))))),

%	Garbage_position_list = calculate_garbage_from_combos( [[Combo,Combo1]] , Board ),

	ok.



%% --------------------         GAMEPLAY LOOP              ------------------------------------------


simple_gameplay_loop_test() ->
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

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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



simple2_gameplay_loop_test() ->
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

	{ _Combos , Result_loop_board } = apply_gravity_combo_loop( Board ),

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


simple2_gravity_test() ->

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



simple_gravity_test() ->
	
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

simple_combo_test() ->

	Board = board:set_block( #block{ color = yellow }, 3 , 2,
				board:set_block( #block{ color = green }, 2 , 2,
					board:set_block( #block{ color = yellow }, 2 , 0,
						board:set_block( #block{ color = blue }, 4 , 0,
							board:set_block( #block{ color = red }, 4 , 2,
								board:set_block( #block{ color = red }, 4 , 1,
									board:set_block( #block{ color = red }, 3 , 1,
										board:set_block( #block{ color = red }, 3 , 0,
											board:new_empty(5,12))))))))),

	Combos = calculate_combos( Board ),

	?assertMatch( 1, length( Combos ) ),
	
	[Combo] = Combos,

	?assertMatch(4, sets:size(Combo) ),

	?assert( sets:is_element(#block{ color = red, x = 3, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 2},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 3, y = 0},Combo) ),

	ok.




simple_combo_with_chromatic_test() ->

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

				

	Combos = calculate_combos( Board ),

	?assertMatch( 1, length( Combos ) ),
	
	[Combo] = Combos,

	?assertMatch(5, sets:size(Combo) ),

	?assert( sets:is_element(#block{ color = red, x = 3, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 2},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 3, y = 0},Combo) ),

	ok.


double_combo_test() ->

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
																board:new_empty(5,12)))))))))))))),

	Combos = calculate_combos( Board ),

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


no_combos_test() ->
	
	Board = board:set_block( #block{ color = yellow }, 1 , 2,
				board:set_block( #block{ color = blue }, 1 , 1,
					board:set_block( #block{ color = green }, 4 , 0,
						board:set_block( #block{ color = yellow }, 3 , 0,
							board:set_block( #block{ color = blue }, 2 , 0,
								board:set_block( #block{ color = red }, 1 , 0,
									board:new_empty(5,12))))))),

	Combos = calculate_combos( Board ),

	?assert( length( Combos ) == 0),
	ok.



simple_garbage_poping_combos_test() ->
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

	Combos = calculate_combos( Board ),

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



simple_poping_combos_test() ->
	Board = board:set_block( #block{ color = yellow }, 3 , 2,
				board:set_block( #block{ color = green }, 2 , 2,
					board:set_block( #block{ color = yellow }, 2 , 0,
						board:set_block( #block{ color = blue }, 4 , 0,
							board:set_block( #block{ color = red }, 4 , 2,
								board:set_block( #block{ color = red }, 4 , 1,
									board:set_block( #block{ color = red }, 3 , 1,
										board:set_block( #block{ color = red }, 3 , 0,
											board:new_empty(5,12))))))))),

	Combos = calculate_combos( Board ),

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


simple_up_place_piece_test() ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 1, down, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch( #block{ color = green, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 1, y = 2 }, board:get_block( 1, 2, Board_result ) ),

	ok.

simple_down_place_piece_test() ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 2, up, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch( #block{ color = green, x = 1, y = 2 }, board:get_block( 1, 2, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),

	ok.


simple_right_piece_test() ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 1, left, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch( #block{ color = green, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 2, y = 0 }, board:get_block( 2, 0, Board_result ) ),

	ok.


simple_left_place_piece_test() ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 2, 1, right, Board ),

	?assertMatch( #block{ color = red, x = 1, y = 0 }, board:get_block( 1, 0, Board_result ) ),
	?assertMatch(  #block{ color = green, x = 2, y = 0 }, board:get_block( 2, 0, Board_result ) ),
	?assertMatch( #block{ color = blue, x = 1, y = 1 }, board:get_block( 1, 1, Board_result ) ),

	ok.



simple_invalid_place_piece_test() ->

	Board = board:set_block( #block{ color = red }, 1 , 0, board:new_empty(5,12) ),

	Piece = #piece{ block1 = #block{ color = red }, block2 = #block{ color = blue } },

	?assertThrow(invalid_move, place_piece(  Piece, 1, 0, down, Board )  ),

	ok.





%% --------------------                       ------------------------------------------





-endif.







