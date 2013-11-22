-module(powers_logic).
-include("include/softstate.hrl").


-export([ handle_use_power/3, handle_turn_passed/2, handle_turn_begin/2 ]).

-export([trigger_overload/1, trigger_killing_blow/2]).


-define( FRENZY_POWER, 1).
-define( THRASH_POWER, 2).
-define( RED_BUTTON_POWER, 3).


-spec handle_turn_begin(User_board::#board{}, Opponent_board::#board{}) -> {#board{},#board{}}.
handle_turn_begin( User_board = #board{}, Opponent_board = #board{} ) -> 
	{ User_board, Opponent_board }.


-spec handle_turn_passed(User_board::#board{}, Opponent_board::#board{}) -> {#board{},#board{}}.
handle_turn_passed( User_board = #board{}, Opponent_board = #board{} ) -> 

	Board_after_red_button = case User_board#board.red_button_pressed of
		true ->				execute_red_button(User_board);
		false ->			User_board
	end,
	
	New_board = Board_after_red_button#board{
					thrash_turns = decrease_turn( Board_after_red_button#board.thrash_turns ), 
					frenzy_turns = decrease_turn( Board_after_red_button#board.frenzy_turns )
				},

	{New_board, Opponent_board}.


-spec handle_use_power( Power::integer(), User_board::#board{}, Opponent_board::#board{}) -> {#board{},#board{}}.
handle_use_power( ?THRASH_POWER, User_board = #board{}, Opponent_board = #board{}) ->
	{User_board#board{thrash_turns = 5}, Opponent_board};

handle_use_power( ?RED_BUTTON_POWER, User_board = #board{}, Opponent_board = #board{}) ->
	{User_board#board{red_button_pressed = true}, Opponent_board};

handle_use_power( ?FRENZY_POWER, User_board = #board{}, Opponent_board = #board{}) ->
	{User_board#board{frenzy_turns = 5}, Opponent_board}.




-spec trigger_overload( Board::#board{} ) -> false | true.
trigger_overload(Board = #board{}) ->
	case Board#board.is_overload_active of
		false ->	false;
		true ->		all_blocks_same_height( Board )
	end.


-spec trigger_killing_blow( Board ::#board{}, Opponent_board ::#board{}) -> false | true.
trigger_killing_blow(Board = #board{}, Opponent_board = #board{}) ->
	board:get_number_blocks(Opponent_board) > 66.









execute_red_button( Board = #board{} ) ->
	New_board = game_logic:activate_ability_blocks( Board ),
	New_board#board{ red_button_pressed = false }.





%--------------------------------------
%		 PRIVATE UTIL FUNCTIONS
%--------------------------------------



all_blocks_same_height( Board = #board{}) ->
	case board:get_column_height( 0, Board) of
		0 ->			false;
		Height ->		lists:all( fun( Col ) -> board:get_column_height( Col, Board) == Height end, lists:seq(1,Board#board.width - 1))
	end.




decrease_turn( Turn ) ->
	case Turn > 1 of
		false ->		0;
		true ->			Turn - 1
	end.

