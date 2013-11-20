-module(powers_logic).
-include("include/softstate.hrl").


-export([ handle_use_power/3, handle_turn_passed/2, handle_turn_begin/2 ]).


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





execute_red_button( Board = #board{} ) ->
	New_board = game_logic:activate_ability_blocks( Board ),
	New_board#board{ red_button_pressed = false }.














%--------------------------------------
%		 PRIVATE UTIL FUNCTIONS
%--------------------------------------

decrease_turn( Turn ) ->
	case Turn > 1 of
		false ->		0;
		true ->			Turn - 1
	end.