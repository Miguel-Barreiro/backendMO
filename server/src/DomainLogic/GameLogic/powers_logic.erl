-module(powers_logic).
-include("include/softstate.hrl").


-export([ handle_use_power/4, handle_turn_passed/3 ]).

-export([trigger_red_button/2]).

-export([trigger_overload/2, trigger_killing_blow/3]).



-spec handle_turn_passed(User_board::#board{}, Opponent_board::#board{}, Game_rules::#game_logic_rules{}) -> {#board{},#board{}}.
handle_turn_passed( User_board = #board{}, Opponent_board = #board{}, _Game_rules = #game_logic_rules{} ) -> 

	New_board = User_board#board{
					thrash_turns = decrease_turn( User_board#board.thrash_turns ), 
					frenzy_turns = decrease_turn( User_board#board.frenzy_turns ),
					triggered_abilities = false
				},

	{New_board, Opponent_board}.


-spec handle_use_power( atom(), User_board::#board{}, Opponent_board::#board{}, Game_rules::#game_logic_rules{}) -> {#board{},#board{}}.
handle_use_power( trash, User_board = #board{}, Opponent_board = #board{}, _Game_rules = #game_logic_rules{}) ->
	{User_board#board{thrash_turns = 5}, Opponent_board};

handle_use_power( redbutton, User_board = #board{}, Opponent_board = #board{}, _Game_rules = #game_logic_rules{} ) ->
	{User_board#board{red_button_pressed = true}, Opponent_board};

handle_use_power( frenzy, User_board = #board{}, Opponent_board = #board{}, _Game_rules = #game_logic_rules{} ) ->
	{User_board#board{frenzy_turns = 5}, Opponent_board}.




-spec trigger_overload( Board::#board{}, Game_rules::#game_logic_rules{} ) -> false | true.
trigger_overload(Board = #board{}, Game_rules = #game_logic_rules{} ) ->
	case Board#board.is_overload_active of
		false ->	false;
		true ->		all_blocks_same_height( Board )
	end.


-spec trigger_killing_blow( Board ::#board{}, Opponent_board ::#board{}, Game_rules::#game_logic_rules{}) -> false | true.
trigger_killing_blow(Board = #board{}, Opponent_board = #board{}, Game_rules = #game_logic_rules{} ) ->
	case Board#board.killing_blow_active of
		false ->			false;
		true ->				board:get_number_blocks(Opponent_board) > 66
	end.


-spec trigger_barrier_blow( Board ::#board{}, Opponent_board ::#board{}, Game_rules::#game_logic_rules{}) -> false | true.
trigger_barrier_blow(Board = #board{}, Opponent_board = #board{}, Game_rules = #game_logic_rules{} ) ->
	case Opponent_board#board.barrier_active of
		false ->			false;
		true ->				Board#board.triggered_abilities
	end.


-spec trigger_red_button( Board::#board{}, Game_rules::#game_logic_rules{} ) -> { [[set]] , #board{} }.
trigger_red_button( Board = #board{}, Game_rules = #game_logic_rules{} ) ->
	case Board#board.red_button_pressed of
		true ->				New_board = game_logic:activate_ability_blocks( Board ),
							game_logic:apply_gravity_combo_loop(  New_board#board{ red_button_pressed = false }, Game_rules );
		false ->			{ [], Board}
	end.





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

