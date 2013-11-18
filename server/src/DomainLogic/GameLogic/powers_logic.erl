-module(powers_logic).
-include("include/softstate.hrl").


-export([ handle_use_power/3, handle_turn_passed/2 ]).


-define( FRENZY_POWER, 1).
-define( THRASH_POWER, 2).
-define( RED_BUTTON_POWER, 3).



-spec handle_turn_passed(User_gamestate::#user_gamestate{}, Opponent_gamestate::#user_gamestate{}) -> {#user_gamestate{},#user_gamestate{}}.
handle_turn_passed( User_gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{} ) -> 
	New_user_gamestate = User_gamestate#user_gamestate{ thrash_turns = decrease_turn( User_gamestate#user_gamestate.thrash_turns ), 
															frenzy_turns = decrease_turn( User_gamestate#user_gamestate.frenzy_turns )
														},
	{ New_user_gamestate, Opponent_gamestate }.



-spec handle_use_power( Power::integer(), User_gamestate::#user_gamestate{}, Opponent_gamestate::#user_gamestate{} ) -> {#user_gamestate{},#user_gamestate{}}.
handle_use_power( ?THRASH_POWER, User_gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{}) ->
	{User_gamestate#user_gamestate{ thrash_turns = 5 }, Opponent_gamestate};

handle_use_power( ?RED_BUTTON_POWER, User_gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{}) ->
	{User_gamestate#user_gamestate{ red_button_pressed = true }, Opponent_gamestate};

handle_use_power( ?FRENZY_POWER, User_gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{}) ->
	{User_gamestate#user_gamestate{ frenzy_turns = 5 }, Opponent_gamestate};

handle_use_power( Power, User_gamestate = #user_gamestate{}, Opponent_gamestate = #user_gamestate{}) ->
	{User_gamestate, Opponent_gamestate}.


decrease_turn( Turn ) ->
	case Turn > 1 of
		false ->		0;
		true ->			Turn - 1
	end.