-module(game_logic).

-include("include/softstate.hrl").

-export([ handle_place_piece/6 ]).




%-------------- PUBLIC -------------------------




%throws out_of_bounds (in case the user has lost)
%throws invalid_move (in case of an invalid move)
handle_place_piece( User_pid, Piece = #piece{}, X, Y, Angle,  Game = #game{}  ) when User_pid == (Game#game.user1_gamestate)#user_gamestate.user_pid->
	Opponent_pid = (Game#game.user2_gamestate)#user_gamestate.user_pid,
	{New_gamestate, New_opponent_gamestate} = handle_place_piece( User_pid, Opponent_pid, Piece, X, Y, Angle, Game#game.user1_gamestate, Game#game.user2_gamestate ),
	Game#game{ user1_gamestate = New_gamestate, user2_gamestate = New_opponent_gamestate };

handle_place_piece( User_pid, Piece = #piece{}, X, Y, Angle, Game = #game{} ) when User_pid == (Game#game.user2_gamestate)#user_gamestate.user_pid->
	Opponent_pid = (Game#game.user1_gamestate)#user_gamestate.user_pid,
	{New_gamestate, New_opponent_gamestate} = handle_place_piece( User_pid, Opponent_pid,  Piece, X, Y, Angle, Game#game.user2_gamestate, Game#game.user1_gamestate ),
	Game#game{ user2_gamestate = New_gamestate, user1_gamestate = New_opponent_gamestate }.


handle_place_piece( User_pid, Opponent_pid, Piece = #piece{}, X, Y, Angle, Gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{} ) ->
	case Piece == Gamestate#user_gamestate.current_piece of	
		false ->
			throw( invalid_piece );
		true ->
			Board_after_place_piece = place_piece( Piece, X, Y, Angle, Gamestate#user_gamestate.board),
			Combos = calculate_combos( Board_after_place_piece ),
			Board_after_pop_combos = pop_combos( Board_after_place_piece, Combos ),
			Board_after_gravity = simulate_gravity( Board_after_pop_combos ),
			Board_after_release_garbage = release_garbage_list( Board_after_gravity, Gamestate#user_gamestate.garbage_position_list ),
			Next_piece = calculate_next_piece( Gamestate ),

			Generated_garbage_position_list = calculate_garbage_from_combos( Combos, Board_after_gravity ),			
			
			case length(Generated_garbage_position_list) of
				0 ->
						dont_send_anything;
				_other ->
						gen_server:cast( User_pid , { send_message, message_processor:create_generated_garbage_message( Generated_garbage_position_list ) } ),
						gen_server:cast( Opponent_pid , { send_message, message_processor:create_place_garbage_message( Generated_garbage_position_list ) } )
			end,

			New_gamestate = Gamestate#user_gamestate{ board = Board_after_release_garbage,
											garbage_position_list = [],
												current_piece = Next_piece,
													piece_generation_step = Gamestate#user_gamestate.piece_generation_step + 1 },

			New_opponent_garbage_list = lists:append( Generated_garbage_position_list, Opponent_gamestate#user_gamestate.garbage_position_list ),
			New_opponent_gamestate = Opponent_gamestate#user_gamestate{ garbage_position_list = New_opponent_garbage_list },

			{ New_gamestate, New_opponent_gamestate }
	end.













%-------------- PRIVATE -------------------------


place_piece( Piece = #piece{}, X, Y, Angle, Board = #board{} ) ->
	New_board = board:set_block( Piece#piece.block1, Board, X , Y ),
	case Angle of
		up ->
			board:set_block( Piece#piece.block2, New_board, X , Y + 1 );	
		down ->
			board:set_block( Piece#piece.block2, New_board, X , Y - 1 );
		left ->
			board:set_block( Piece#piece.block2, New_board, X -1 , Y );
		right ->			
			board:set_block( Piece#piece.block2, New_board, X + 1 , Y )
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
		board:remove_block( New_board, Block#block.x, Block#block.y )
	end,
	lists:foldl( Fun, Board, sets:to_list(Combo)).






simulate_gravity( Board = #board{} )->
	simulate_gravity_by_column( Board, 0).






release_garbage_list( Board = #board{}, [] ) ->
	Board;
release_garbage_list( Board = #board{}, [Garbage_position | Rest ] ) ->
	New_board = board:set_block( #block{ type = garbage }, Board, Garbage_position , get_column_height( Garbage_position, Board ) ),
	release_garbage_list( New_board, Rest ).








calculate_garbage_from_combos( Combos, Board = #board{} ) ->
	Sum_garbage_from_combos = fun( Combo, Acc )->
		Acc + calculate_garbage_from_combo( Combo )
	end,
	Garbage_number = ( length( Combos ) - 1) * 2 + lists:foldl( Sum_garbage_from_combos, 0, Combos),
	generate_garbage_positions( Garbage_number, Board ).





%TODO
calculate_next_piece( Gamestate = #user_gamestate{} ) ->
	#piece{ block1 = #block{ type = color , color = red }, block2 = #block{ type = color , color = green } }.








%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										combos helper functions
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




get_column_height( Column, Board = #board{} ) ->
	get_column_height( Board, Column, 0 ).


get_column_height( Board = #board{}, X, Y ) ->
	case board:get_block( Board , X , Y ) of
		empty ->		Y;
		_other ->		get_column_height( Board, X , Y + 1)
	end.









generate_garbage_positions( Garbage_number, Board = #board{} ) ->
	generate_garbage_positions( Garbage_number, Board, lists:seq( 0, Board#board.width ) ).



generate_garbage_positions( 0, _ , _ ) ->
	[];

generate_garbage_positions( Garbage_number, Board = #board{}, [] ) ->
	generate_garbage_positions( Garbage_number, Board, lists:seq( 0 , Board#board.width ) );

generate_garbage_positions( Garbage_number, Board = #board{}, Column_list ) ->
	Random = random:uniform( length(Column_list) ) - 1,

	{ List1 , [ Position | List2] } = lists:split( Random, Column_list),
	New_column_list = lists:append( List1 , List2 ),

	[ Position | generate_garbage_positions( Garbage_number - 1, Board, New_column_list )].










simulate_gravity_by_column( Board = #board{}, X ) when X >= Board#board.width ->
	Board;

simulate_gravity_by_column( Board = #board{}, X ) ->
	New_board = move_column_down( Board, X, 0, 0),
	simulate_gravity_by_column( New_board, X + 1 ).



move_column_down( Board = #board{}, _X, Y, _How_much ) when Y >= Board#board.height ->
	Board;

move_column_down( Board = #board{}, X, Y, 0) ->
	case board:get_block( Board, X, Y) of
		empty ->
			move_column_down( Board, X, Y + 1, 1);
		_block ->
			move_column_down( Board, X, Y + 1, 0)
	end;

move_column_down( Board = #board{}, X, Y, How_much ) ->
	case board:get_block( Board, X, Y) of
		empty ->
			move_column_down( Board, X, Y + 1, How_much + 1);

		Block ->
			Board_withouth_block = board:remove_block( Board, X, Y ),
			Board_with_block_in_place = board:set_block( Block, Board_withouth_block, X, Y - How_much ),

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

			case board:get_block( Board, X, Y ) of
				empty ->
					{ Combo, New_visited };

				Current_block when Current_block#block.type == color, Current_block#block.color == Block#block.color ->

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
		5 ->		1;
		6 ->		2;
		7 ->		2;
		8 ->		3;
		9 ->		3;
		10 ->		4;
		11 ->		4;
		12 ->		5;
		_other ->	6
	end.




%%-------------------------------------------------------------------------------------------------------------------------------------------------
%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										UNIT TESTS 
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%---------------------------------------------------------------------------------------




-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").



%% --------------------         GARBAGE              ------------------------------------------







test_garbage_position( Garbage_position_list ) ->
	
	Fun = fun( Position, { Cache , { Position_max , Max} , { Position_min , Min} } ) ->
		case proplists:get_value( Position , Cache) of
			undefined ->
				New_value = 0,
				New_cache = [ { Position, 1 } | Cache];
			Value ->
				New_value = Value + 1,
				New_cache =  [ { Position, New_value } | proplists:delete( Position ,Cache) ]
		end,

		New_max = case New_value > Max of
			true ->		{ Position , New_value };
			false ->	{ Position_max , Max}
		end,

		New_min = case New_value < Min of
			true ->		{ Position , New_value };
			false ->	{ Position_min , Min}
		end,

		{ New_cache, New_max, New_min }

	end,
	{ _ , { _Position_max , Max} , { _Position_min , Min} } = lists:foldl(Fun, { [], { 0 ,0 } , { 0, 999} }, Garbage_position_list),

	Max =< Min +1.





release_garbage_into_full_column_test()->
	
	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = green }, Board , 1 , 0 ),
	Board3 = board:set_block( #block{ color = blue }, Board2 , 1 , 1 ),
	Board4 = board:set_block( #block{ color = green }, Board3 , 1 , 2 ),
	Board5 = board:set_block( #block{ color = blue }, Board4 , 1 , 3 ),
	Board6 = board:set_block( #block{ color = green }, Board5 , 1 , 4 ),
	Board7 = board:set_block( #block{ color = yellow }, Board6 , 1 , 5 ),
	Board8 = board:set_block( #block{ color = green }, Board7 , 1 , 6 ),
	Board9 = board:set_block( #block{ color = yellow }, Board8 , 1 , 7 ),
	Board10 = board:set_block( #block{ color = green }, Board9 , 1 , 8 ),
	Board11 = board:set_block( #block{ color = blue }, Board10 , 1 , 9 ),
	Board12 = board:set_block( #block{ color = yellow }, Board11 , 1 , 10 ),
	Board13 = board:set_block( #block{ color = red }, Board12 , 1 , 11 ),

	?assertThrow(out_of_bounds, release_garbage_list( Board13, [1] ) ),

	ok.




single_combo_garbage_test() ->
	Board = board:new_empty(6,12),
	Combo = sets:add_element( #block{ color = red, x = 3, y = 0 } ,
				sets:add_element( #block{ color = red, x = 3, y = 1 },
					sets:add_element( #block{ color = red, x = 3, y = 2 },
						sets:add_element( #block{ color = red, x = 3, y = 3 }, sets:new())))),

	Garbage_position_list = calculate_garbage_from_combos( [Combo], Board ),

	?assert( length( Garbage_position_list ) == 1 ),
	?assert( test_garbage_position( Garbage_position_list )),

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

	Garbage_position_list = calculate_garbage_from_combos( [Combo,Combo1] , Board ),

	?assert( length( Garbage_position_list ) == 5 ),
	?assert( test_garbage_position( Garbage_position_list )),

	ok.



%% --------------------         GRAVITY              ------------------------------------------


simple2_gravity_test() ->

	Board = board:new_empty(6,12),
	Board2 = board:set_block( #block{ color = red }, Board , 3 , 2 ),
	Board3 = board:set_block( #block{ color = yellow }, Board2 , 3 , 6 ),
	Board4 = board:set_block( #block{ color = yellow }, Board3 , 3 , 7 ),
	Board5 = board:set_block( #block{ color = blue }, Board4 , 2 , 3 ),
	Board6 = board:set_block( #block{ color = green }, Board5 , 1 , 4 ),
	Board7 = board:set_block( #block{ color = red }, Board6 , 5 , 0 ),
	Board8 = board:set_block( #block{ color = green }, Board7 , 5 , 5 ),

	Board_after_gravity = simulate_gravity( Board8 ),

	?assert( board:get_block( Board_after_gravity , 3, 0 ) == #block{ color = red, x = 3, y = 0 } ),
	?assert( board:get_block( Board_after_gravity , 3, 1 ) == #block{ color = yellow, x = 3, y = 1 } ),
	?assert( board:get_block( Board_after_gravity , 3, 2 ) == #block{ color = yellow, x = 3, y = 2 } ),
	?assert( board:get_block( Board_after_gravity , 2, 0 ) == #block{ color = blue, x = 2, y = 0 } ),
	?assert( board:get_block( Board_after_gravity , 1, 0 ) == #block{ color = green, x = 1, y = 0 } ),
	?assert( board:get_block( Board_after_gravity , 5, 0 ) == #block{ color = red, x = 5, y = 0 } ),
	?assert( board:get_block( Board_after_gravity , 5, 1 ) == #block{ color = green, x = 5, y = 1 } ),

	ok.



simple_gravity_test() ->
	
	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 3 , 1 ),
	Board3 = board:set_block( #block{ color = yellow }, Board2 , 3 , 3 ),
	Board4 = board:set_block( #block{ color = blue }, Board3 , 2 , 0 ),
	Board5 = board:set_block( #block{ color = green }, Board4 , 1 , 1 ),

	Board_after_gravity = simulate_gravity( Board5 ),

	?assert( board:get_block( Board_after_gravity , 3, 0 ) == #block{ color = red, x = 3, y = 0 } ),
	?assert( board:get_block( Board_after_gravity , 3, 1 ) == #block{ color = yellow, x = 3, y = 1 } ),
	?assert( board:get_block( Board_after_gravity , 2, 0 ) == #block{ color = blue, x = 2, y = 0 } ),
	?assert( board:get_block( Board_after_gravity , 1, 0 ) == #block{ color = green, x = 1, y = 0 } ),

	ok.



%% --------------------         COBMOS              ------------------------------------------

simple_combo_test() ->

	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 3 , 0 ),
	Board3 = board:set_block( #block{ color = red }, Board2 , 3 , 1 ),
	Board4 = board:set_block( #block{ color = red }, Board3 , 4 , 1 ),
	Board5 = board:set_block( #block{ color = red }, Board4 , 4 , 2 ),

	Board6 = board:set_block( #block{ color = blue }, Board5 , 4 , 0 ),
	Board7 = board:set_block( #block{ color = yellow }, Board6 , 2 , 0 ),
	Board8 = board:set_block( #block{ color = green }, Board7 , 2 , 2 ),
	Board9 = board:set_block( #block{ color = yellow }, Board8 , 3 , 2 ),

	Combos = calculate_combos( Board9 ),

	?assert( length( Combos ) == 1),
	
	[Combo] = Combos,

	?assert( sets:size(Combo) == 4),

	?assert( sets:is_element(#block{ color = red, x = 3, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 1},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 4, y = 2},Combo) ),
	?assert( sets:is_element(#block{ color = red, x = 3, y = 0},Combo) ),

	ok.


double_combo_test() ->

	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = blue }, Board , 1 , 0 ),
	Board3 = board:set_block( #block{ color = blue }, Board2 , 2 , 0 ),
	Board4 = board:set_block( #block{ color = blue }, Board3 , 3 , 0 ),
	Board5 = board:set_block( #block{ color = blue }, Board4 , 4 , 0 ),
	Board6 = board:set_block( #block{ color = blue }, Board5 , 1 , 1 ),
	
	Board7 = board:set_block( #block{ color = green }, Board6 , 2 , 1 ),
	Board8 = board:set_block( #block{ color = green }, Board7 , 2 , 2 ),
	Board9 = board:set_block( #block{ color = green }, Board8 , 3 , 1 ),
	Board10 = board:set_block( #block{ color = green }, Board9 , 4 , 1 ),

	Board11 = board:set_block( #block{ color = blue }, Board10 , 3 , 2 ),
	Board12 = board:set_block( #block{ color = yellow }, Board11 , 5 , 0 ),
	Board13 = board:set_block( #block{ color = red }, Board12 , 5 , 1 ),
	Board14 = board:set_block( #block{ color = yellow }, Board13 , 1 , 2 ),

	Combos = calculate_combos( Board14 ),

%	Lists = lists:map( fun( Set ) ->  sets:to_list(Set) end, Combos),
%	io:format("All_Combos ~p",[Lists]),	

	?assert( length( Combos ) == 2),

	[Combo1 , Combo2] = Combos,

	case sets:size(Combo1) of
		5 ->
			?assert( sets:is_element(#block{ color = blue, x = 1, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 2, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 3, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 4, y = 0},Combo1) ),
			?assert( sets:is_element(#block{ color = blue, x = 1, y = 1},Combo1) ),

			?assert( sets:size(Combo2) == 4),
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

			?assert( sets:size(Combo1) == 4),
			?assert( sets:is_element(#block{ color = green, x = 2, y = 1},Combo1) ),
			?assert( sets:is_element(#block{ color = green, x = 2, y = 2},Combo1) ),
			?assert( sets:is_element(#block{ color = green, x = 3, y = 1},Combo1) ),
			?assert( sets:is_element(#block{ color = green, x = 4, y = 1},Combo1) );

		_other ->
			throw("one of the combos isnt the correct size") 
	end,
	ok.


no_combos_test() ->
	
	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 1 , 0 ),
	Board3 = board:set_block( #block{ color = blue }, Board2 , 2 , 0 ),
	Board4 = board:set_block( #block{ color = yellow }, Board3 , 3 , 0 ),
	Board5 = board:set_block( #block{ color = green }, Board4 , 4 , 0 ),
	Board6 = board:set_block( #block{ color = blue }, Board5 , 1 , 1 ),	
	Board7 = board:set_block( #block{ color = yellow }, Board6 , 1 , 2 ),

	Combos = calculate_combos( Board7 ),

	?assert( length( Combos ) == 0),
	ok.



simple_up_place_piece_test() ->

	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 1 , 0 ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 1, up, Board2 ),

	?assert( board:get_block( Board_result , 1, 0 ) == #block{ color = red, x = 1, y = 0 } ),
	?assert( board:get_block( Board_result , 1, 1 ) == #block{ color = green, x = 1, y = 1 } ),
	?assert( board:get_block( Board_result , 1, 2 ) == #block{ color = blue, x = 1, y = 2 } ),

	ok.

simple_down_place_piece_test() ->

	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 1 , 0 ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 2, down, Board2 ),

	?assert( board:get_block( Board_result , 1, 0 ) == #block{ color = red, x = 1, y = 0 } ),
	?assert( board:get_block( Board_result , 1, 2 ) == #block{ color = green, x = 1, y = 2 } ),
	?assert( board:get_block( Board_result , 1, 1 ) == #block{ color = blue, x = 1, y = 1 } ),

	ok.


simple_right_piece_test() ->

	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 1 , 0 ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 1, 1, right, Board2 ),

	?assert( board:get_block( Board_result , 1, 0 ) == #block{ color = red, x = 1, y = 0 } ),
	?assert( board:get_block( Board_result , 1, 1 ) == #block{ color = green, x = 1, y = 1 } ),
	?assert( board:get_block( Board_result , 2, 1 ) == #block{ color = blue, x = 2, y = 1 } ),

	ok.


simple_left_place_piece_test() ->

	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 1 , 0 ),
	Piece = #piece{ block1 = #block{ color = green }, block2 = #block{ color = blue } },

	Board_result = place_piece(  Piece, 2, 1, left, Board2 ),

	?assert( board:get_block( Board_result , 1, 0 ) == #block{ color = red, x = 1, y = 0 } ),
	?assert( board:get_block( Board_result , 2, 1 ) == #block{ color = green, x = 2, y = 1 } ),
	?assert( board:get_block( Board_result , 1, 1 ) == #block{ color = blue, x = 1, y = 1 } ),

	ok.



simple_invalid_place_piece_test() ->

	Board = board:new_empty(5,12),
	Board2 = board:set_block( #block{ color = red }, Board , 1 , 0 ),

	Piece = #piece{ block1 = #block{ color = red }, block2 = #block{ color = blue } },

	?assertThrow(invalid_move, place_piece(  Piece, 1, 0, up, Board2 )  ),

	ok.








%% --------------------                       ------------------------------------------





-endif.







