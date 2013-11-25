-module(game_rules).

-include("include/softstate.hrl").

-export([ get_current_rules/1, get_next_piece_type/2, get_next_piece_type_with_frenzy/2, get_garbage_number/2 ]).


-export([ get_offline_current_rules/1]).


-spec get_offline_current_rules( Tier::binary() ) -> #game_logic_rules{}.
get_offline_current_rules( Tier ) ->

	{ _Values , _Products, Ability_rules, 
		Garbage_combo_rule, Garbage_chain_rule, Garbage_simultaneous_combo_rule, 
			Combo_max, Total_color_number, Min_combo_size } = configurations_serv:get_offline_configuration(Tier),

	#game_logic_rules{

		abilities_rule = Ability_rules,
		garbage_combo_rule = Garbage_combo_rule,
		garbage_chain_rule = Garbage_chain_rule,
		garbage_simultaneous_combo_rule = Garbage_simultaneous_combo_rule,

		total_color_number = Total_color_number,
		min_combo_size = Min_combo_size,

		garbage_combo_max = Combo_max
	}.




-spec get_current_rules( Tier::binary() ) -> #game_logic_rules{}.
get_current_rules( Tier ) ->
	#game_logic_rules{

		%version = configurations_serv:get_current_version(),

		abilities_rule = configurations_serv:get_abilities_generation_rules( Tier ),
		garbage_combo_rule = configurations_serv:get_garbage_combo_generation_rules( Tier ),
		garbage_chain_rule = configurations_serv:get_garbage_chain_generation_rules( Tier ),
		garbage_simultaneous_combo_rule = configurations_serv:get_garbage_simultaneous_combo_generation_rules( Tier ),

		total_color_number = configurations_serv:get_total_color_number( Tier ),
		min_combo_size = configurations_serv:get_min_combo_size( Tier ),

		garbage_combo_max = configurations_serv:get_max_combo_size( Tier )
	}.
	

-spec get_next_piece_type_with_frenzy( Combos:: [[[#block{}]]], Game_rules::#game_logic_rules{} ) -> block_type().
get_next_piece_type_with_frenzy(  Combos, Game_rules = #game_logic_rules{}) ->
	get_type_from_rule_with_frenzy(  Game_rules#game_logic_rules.abilities_rule, Combos).


-spec get_next_piece_type( Combos:: [[[#block{}]]] , Game_rules::#game_logic_rules{} ) -> block_type().
get_next_piece_type( Combos, Game_rules = #game_logic_rules{} ) ->
	get_type_from_rule(  Game_rules#game_logic_rules.abilities_rule , Combos).




-spec get_garbage_number( Combos:: [[[#block{}]]] , Game_rules::#game_logic_rules{} ) -> { integer(), integer(), integer() }.
get_garbage_number( Combos, Game_rules = #game_logic_rules{} ) ->
	get_garbage_from_combo_chain( Combos , Game_rules ).










get_random_garbage_from_types( _, [] ) ->
	[];
get_random_garbage_from_types( 0, _Types ) ->
	[];
get_random_garbage_from_types( Number, Types ) ->
	Garbage = lists:nth( random:uniform( length(Types) ), Types), 
	[Garbage | get_random_garbage_from_types( Number - 1, Types )].
	



get_garbage_from_single_combo( Combo_size , Game_rules = #game_logic_rules{} ) ->

	case proplists:get_value( Combo_size, Game_rules#game_logic_rules.garbage_combo_rule ) of
	
		undefined when Combo_size > Game_rules#game_logic_rules.garbage_combo_max ->
				get_garbage_from_single_combo( Game_rules#game_logic_rules.garbage_combo_max , Game_rules );

		undefined ->					{ 0,0,0 };
		{garbage, Value} ->				{ Value,0,0 };
		{garbage_color, Value} ->		{ 0,Value,0 };
		{garbage_hard, Value} ->		{ 0,0,Value }
	end.




get_garbage_from_combo_sequence( Combo_sequence, Game_rules = #game_logic_rules{}  ) ->

	Combo_sum = fun( Combo, { Normal_garbage_number, Color_garbage_number, Hard_garbage_number } ) -> 
			
			{ New_normal_garbage_number, New_color_garbage_number, New_hard_garbage_number } = get_garbage_from_single_combo( sets:size(Combo), Game_rules ),
			
			{Normal_garbage_number + New_normal_garbage_number,
				Color_garbage_number + New_color_garbage_number, 
					Hard_garbage_number + New_hard_garbage_number}
		end,

	{ Normal_garbage_number, Color_garbage_number, Hard_garbage_number } = lists:foldl( Combo_sum, { 0,0,0 } , Combo_sequence),
	
	{ Ratio , Garbage_types } = Game_rules#game_logic_rules.garbage_simultaneous_combo_rule,

	Garbage_list = get_random_garbage_from_types( (length(Combo_sequence) - 1) * Ratio, Garbage_types ),

	Fun = 
	fun( Garbage_type, { Normal_garbage_number_acc, Color_garbage_number_acc, Hard_garbage_number_acc } ) ->
		case Garbage_type of
			garbage ->			{ Normal_garbage_number_acc + 1, Color_garbage_number_acc, Hard_garbage_number_acc };
			garbage_color ->	{ Normal_garbage_number_acc, Color_garbage_number_acc + 1, Hard_garbage_number_acc };
			garbage_hard ->		{ Normal_garbage_number_acc, Color_garbage_number_acc, Hard_garbage_number_acc + 1 }
		end
	end,
	lists:foldl( Fun, { Normal_garbage_number, Color_garbage_number, Hard_garbage_number }, Garbage_list).






get_garbage_from_combo_chain( Combos, Game_rules = #game_logic_rules{} ) ->
	Sum_sequence = fun( Combo_sequence, { Normal_garbage_number, Color_garbage_number, Hard_garbage_number } ) -> 
			
			{ New_normal_garbage_number, New_color_garbage_number, New_hard_garbage_number } = get_garbage_from_combo_sequence( Combo_sequence, Game_rules ),
			
			{Normal_garbage_number + New_normal_garbage_number,
				Color_garbage_number + New_color_garbage_number, 
					Hard_garbage_number + New_hard_garbage_number}
		end,

	{ Normal_garbage_number, Color_garbage_number, Hard_garbage_number } = lists:foldl( Sum_sequence, { 0,0,0 } , Combos),
	{ Ratio , Garbage_types } = Game_rules#game_logic_rules.garbage_chain_rule,

	Garbage_list = get_random_garbage_from_types( (length(Combos) - 1) * Ratio, Garbage_types ),

	Fun = 
	fun( Garbage_type, { Normal_garbage_number_acc, Color_garbage_number_acc, Hard_garbage_number_acc } ) ->
		case Garbage_type of
			garbage ->			{ Normal_garbage_number_acc + 1, Color_garbage_number_acc, Hard_garbage_number_acc };
			garbage_color ->	{ Normal_garbage_number_acc, Color_garbage_number_acc + 1, Hard_garbage_number_acc };
			garbage_hard ->		{ Normal_garbage_number_acc, Color_garbage_number_acc, Hard_garbage_number_acc + 1 }
		end
	end,
	lists:foldl( Fun, { Normal_garbage_number, Color_garbage_number, Hard_garbage_number }, Garbage_list).



get_combo_color( [] ) ->
	shapeshifter;

get_combo_color( Combo_list ) ->
	[ Block | Rest_blocks ] = Combo_list,
	case Block#block.type of
		shapeshifter ->		get_combo_color( Rest_blocks );
		_other ->			Block#block.color
	end.


get_minimum_combo_power( _, []) ->
	color;
get_minimum_combo_power( [], _combos) -> 
	color;
get_minimum_combo_power( Game_rules, Combos) -> 
	Reverse_rules = lists:reverse(Game_rules),
	get_minimum_combo_power_rec( Reverse_rules, lists:flatten(Combos)).


get_minimum_combo_power_rec( [{{ _combo_size, Rule_color }, Power } | Rest], Combos) -> 
	Fun = 
	fun( Combo, { Found, Didnt_match } ) ->
		Combo_list = sets:to_list(Combo),
		case Rule_color == any orelse Rule_color == get_combo_color( Combo_list ) of
			true ->			{ true, Didnt_match};
			false ->		{ Found, [ Combo | Didnt_match]}
		end
	end,
	case lists:foldl( Fun, {false, []}, Combos ) of
		{ true, [] } ->				Power;
		{ _, Not_matched } ->		get_minimum_combo_power_rec( Rest, Not_matched)
	end.



get_type_from_rule_with_frenzy( Game_rules , Combos) ->
	case get_type_from_rule( Game_rules , Combos) of
		color ->	get_minimum_combo_power( Game_rules, Combos);
		Other -> 	Other
	end.

get_type_from_rule( [] , _combos) ->
	color;

get_type_from_rule( [{{ Combo_size, Rule_color }, Power } | Rest] , Combos) ->
	Combo_fits_rule = 
	fun( Combo ) ->
		Combo_list = sets:to_list(Combo),
		length(Combo_list) >= Combo_size andalso ( Rule_color == any orelse Rule_color == get_combo_color( Combo_list ) )
	end,

	case lists:any( fun( Combo_sequence )-> lists:any( Combo_fits_rule, Combo_sequence ) end, Combos ) of
		false ->		get_type_from_rule( Rest, Combos);
		true ->			Power
	end.








	
