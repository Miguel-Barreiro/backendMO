-module(game_logic).

-include("include/softstate.hrl").

-export([ place_piece/4 ]).
-export([ simulate_gravity/1, calculate_combos/1, pop_combo/2 ]).


place_piece( Piece = #piece{} , Board = #board{} , X, Y) ->
	New_board = board:set_block( Piece#piece.block1, Board, X , Y ),
	case Piece#piece.angle of
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


pop_combo( Board = #board{}, Combo ) ->
	Fun = fun( Block = #block{}, New_board )->
		board:remove_block( New_board, Block#block.x, Block#block.y )
	end,
	lists:foldl( Fun, Board, sets:to_list(Combo)).



simulate_gravity( Board = #board{} )->
	simulate_gravity( Board = #board{}, 0, 0 ).



%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										combos helper functions
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

simulate_gravity_by_column( Board = #board{}, X ) when X >= Board#board.width ->
	Board;

simulate_gravity_by_column( Board = #board{}, X ) ->
	
	simulate_gravity_by_column( Board = #board{}, X + 1 ).



simulate_gravity( Board = #board{}, X, Y ) ->
	case board:get_block( Board, X, Y) of
		empty ->
			move_column_down( Board = #board{}, X, Y, 1 );
		_block ->
			Board
	end.


move_column_down( Board = #board{}, X, Y, How_much ) when Y >= Board#board.height ->
	Board;

move_column_down( Board = #board{}, X, Y, How_much ) ->
	Board.





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









%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										UNIT TESTS 
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").



%% --------------------         Combos              ------------------------------------------

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

	Combos = game_logic:calculate_combos( Board9 ),

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

	Combos = game_logic:calculate_combos( Board14 ),

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

	Combos = game_logic:calculate_combos( Board7 ),

	?assert( length( Combos ) == 0),
	ok.












%% --------------------                       ------------------------------------------





-endif.







