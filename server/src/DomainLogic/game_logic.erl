-module(game_logic).

-export([ place_piece/4 ]).
-export([ simulate_gravity/1, calculate_combos/1, pop_combo/2 ]).


-include("include/softstate.hrl").




place_piece( #piece{} , Board = #board{} , x, y) ->
	Board.

simulate_gravity( Board = #board{} )->
	Board.

calculate_combos( #board{} )->
	[].

pop_combo( Board = #board{}, Combo) ->
	Board.

