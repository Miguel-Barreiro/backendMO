-module(game_rules).

-include("include/softstate.hrl").

-export([ get_current_rules/1, get_next_piece_type/2, get_garbage_number/2 ]).


-export([ get_offline_current_rules/1]).


-spec get_offline_current_rules( Tier::binary() ) -> #game_logic_rules{}.
get_offline_current_rules( Tier ) ->

	{ _Values , _Products, Ability_rules, 
		Garbage_combo_rule, Garbage_chain_rule, Garbage_simultaneous_combo_rule, 
			Total_color_number, Min_combo_size } = configurations_serv:get_offline_configuration(Tier),

	#game_logic_rules{

		abilities_rule = Ability_rules,
		garbage_combo_rule = Garbage_combo_rule,
		garbage_chain_rule = Garbage_chain_rule,
		garbage_simultaneous_combo_rule = Garbage_simultaneous_combo_rule,

		total_color_number = Total_color_number,
		min_combo_size = Min_combo_size
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
		min_combo_size = configurations_serv:get_min_combo_size( Tier )
	}.
	




-spec get_next_piece_type( Combos:: [[[#block{}]]] , Game_rules::#game_logic_rules{} ) -> block_type().
get_next_piece_type( Combos, Game_rules = #game_logic_rules{} ) ->
	get_type_from_rule(  Game_rules#game_logic_rules.abilities_rule , Combos).




-spec get_garbage_number( Combos:: [[[#block{}]]] , Game_rules::#game_logic_rules{} ) -> { integer(), integer(), integer() }.
get_garbage_number( Combos, Game_rules = #game_logic_rules{} ) ->
	get_garbage_from_combo_chain( Combos , Game_rules ).










get_random_garbage_from_types( _, [] ) ->
	[];
get_random_garbage_from_types( 0, Types ) ->
	[];
get_random_garbage_from_types( Number, Types ) ->
	Garbage = lists:nth( random:uniform( length(Types) ), Types), 
	[Garbage | get_random_garbage_from_types( Number - 1, Types )].
	



get_garbage_from_single_combo( Combo , Game_rules = #game_logic_rules{} ) ->
	case proplists:get_value( sets:size(Combo), Game_rules#game_logic_rules.garbage_combo_rule ) of
		undefined ->					{ 0,0,0 };
		{garbage, Value} ->				{ Value,0,0 };
		{garbage_color, Value} ->		{ 0,Value,0 };
		{garbage_hard, Value} ->		{ 0,0,Value }
	end.




get_garbage_from_combo_sequence( Combo_sequence, Game_rules = #game_logic_rules{}  ) ->

	Combo_sum = fun( Combo, { Normal_garbage_number, Color_garbage_number, Hard_garbage_number } ) -> 
			
			{ New_normal_garbage_number, New_color_garbage_number, New_hard_garbage_number } = get_garbage_from_single_combo( Combo, Game_rules ),
			
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










get_type_from_rule( [] , Combos) ->
	color;

get_type_from_rule( [{{ Combo_size, Rule_color }, Power } | Rest] , Combos) ->
	
	Combo_fits_rule = 
	fun( Combo = [ Block | _Rest_blocks ] ) -> 
		length( Combo) == Combo_size andalso ( Rule_color == any orelse Rule_color == Block#block.color )
	end,

	case lists:any( fun( Combo_sequence )-> lists:any( Combo_fits_rule, Combo_sequence) end, Combos ) of
		false ->
				get_type_from_rule( Rest, Combos);
		true ->
				get_type_from_power( Power )
	end.



get_type_from_power( generate_bomb ) ->
	bomb;
get_type_from_power( _other ) ->
	bomb.