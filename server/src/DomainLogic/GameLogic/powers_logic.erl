-module(powers_logic).
-include("include/softstate.hrl").


-export([ handle_use_power/4, handle_turn_passed/3 ]).

-export([trigger_red_button/2]).

-export([trigger_overload/2, trigger_killing_blow/3, trigger_barrier_blow/3]).



-spec handle_turn_passed(UserBoard::#board{}, OpponentBoard::#board{}, GameRules::#game_logic_rules{}) -> {#board{},#board{}}.
handle_turn_passed( UserBoard = #board{}, OpponentBoard = #board{}, _GameRules = #game_logic_rules{} ) -> 

	NewBoard = UserBoard#board{
					thrash_turns = decrease_turn( UserBoard#board.thrash_turns ), 
					frenzy_turns = decrease_turn( UserBoard#board.frenzy_turns )
				},

	{NewBoard, OpponentBoard}.


-spec handle_use_power( atom(), UserBoard::#board{}, OpponentBoard::#board{}, GameRules::#game_logic_rules{}) -> {#board{},#board{}}.
handle_use_power( trash, UserBoard = #board{}, OpponentBoard = #board{}, _GameRules = #game_logic_rules{}) ->
	{UserBoard#board{thrash_turns = 5}, OpponentBoard};

handle_use_power( redbutton, UserBoard = #board{}, OpponentBoard = #board{}, _GameRules = #game_logic_rules{} ) ->
	{UserBoard#board{red_button_pressed = true}, OpponentBoard};

handle_use_power( frenzy, UserBoard = #board{}, OpponentBoard = #board{}, _GameRules = #game_logic_rules{} ) ->
	{UserBoard#board{frenzy_turns = 5}, OpponentBoard}.




-spec trigger_overload( Board::#board{}, GameRules::#game_logic_rules{} ) -> false | true.
trigger_overload(Board = #board{}, _GameRules = #game_logic_rules{} ) ->
	case Board#board.is_overload_active of
		false ->	false;
		true ->		all_blocks_same_height( Board )
	end.


-spec trigger_killing_blow( Board ::#board{}, OpponentBoard ::#board{}, GameRules::#game_logic_rules{}) -> false | true.
trigger_killing_blow(Board = #board{}, OpponentBoard = #board{}, _GameRules = #game_logic_rules{} ) ->
	case Board#board.killing_blow_active of
		false ->			false;
		true ->				board:get_number_blocks(OpponentBoard) > 48
	end.


-spec trigger_barrier_blow( Board ::#board{}, OpponentBoard ::#board{}, GameRules::#game_logic_rules{}) -> false | true.
trigger_barrier_blow(Board = #board{}, OpponentBoard = #board{}, _GameRules = #game_logic_rules{} ) ->
	case OpponentBoard#board.barrier_active of
		false ->			false;
		true ->				length( Board#board.triggered_abilities ) > 0
	end.




-spec trigger_red_button( Board::#board{}, GameRules::#game_logic_rules{} ) -> { [[set]] , #board{} }.
trigger_red_button( Board = #board{}, GameRules = #game_logic_rules{} ) ->
	case Board#board.red_button_pressed of
		true ->				{ Combos , ResultLoopBoard } = game_logic:activate_ability_blocks( Board, GameRules ),
							{ Combos , ResultLoopBoard#board{ red_button_pressed = false } };
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

