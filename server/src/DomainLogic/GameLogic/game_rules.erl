-module(game_rules).

-include("include/softstate.hrl").

-export([ get_current_rules/1, get_next_piece_type/2, get_next_piece_type_with_frenzy/2, get_garbage_number/2 ]).


-export([ get_offline_current_rules/1]).


-spec get_offline_current_rules( Tier::binary() ) -> #game_logic_rules{}.
get_offline_current_rules( Tier ) ->

	{ _Values , _Products, AbilityRules, 
		GarbageComboRule, GarbageChainRule, GarbageSimultaneousComboRule, 
			ComboMax, TotalColorNumber, MinComboSize } = configurations_serv:get_offline_configuration(Tier),

	#game_logic_rules{

		abilities_rule = AbilityRules,
		garbage_combo_rule = GarbageComboRule,
		garbage_chain_rule = GarbageChainRule,
		garbage_simultaneous_combo_rule = GarbageSimultaneousComboRule,

		total_color_number = TotalColorNumber,
		min_combo_size = MinComboSize,

		garbage_combo_max = ComboMax
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
	

-spec get_next_piece_type_with_frenzy( Combos:: [[[#block{}]]], GameRules::#game_logic_rules{} ) -> block_type().
get_next_piece_type_with_frenzy(  Combos, GameRules = #game_logic_rules{}) ->
	get_type_from_rule_with_frenzy(  GameRules#game_logic_rules.abilities_rule, Combos).


-spec get_next_piece_type( Combos:: [[[#block{}]]] , GameRules::#game_logic_rules{} ) -> block_type().
get_next_piece_type( Combos, GameRules = #game_logic_rules{} ) ->
	get_type_from_rule(  GameRules#game_logic_rules.abilities_rule , Combos).




-spec get_garbage_number( Combos:: [[[#block{}]]] , GameRules::#game_logic_rules{} ) -> { integer(), integer(), integer() }.
get_garbage_number( Combos, GameRules = #game_logic_rules{} ) ->
	get_garbage_from_combo_chain( Combos , GameRules ).










get_random_garbage_from_types( _, [] ) ->
	[];
get_random_garbage_from_types( 0, _Types ) ->
	[];
get_random_garbage_from_types( Number, Types ) ->
	Garbage = lists:nth( random:uniform( length(Types) ), Types), 
	[Garbage | get_random_garbage_from_types( Number - 1, Types )].
	



get_garbage_from_single_combo( ComboSize , GameRules = #game_logic_rules{} ) ->

	case proplists:get_value( ComboSize, GameRules#game_logic_rules.garbage_combo_rule ) of
	
		undefined when ComboSize > GameRules#game_logic_rules.garbage_combo_max ->
				get_garbage_from_single_combo( GameRules#game_logic_rules.garbage_combo_max , GameRules );

		undefined ->					{ 0,0,0 };
		{garbage, Value} ->				{ Value,0,0 };
		{garbage_color, Value} ->		{ 0,Value,0 };
		{garbage_hard, Value} ->		{ 0,0,Value }
	end.




get_garbage_from_combo_sequence( ComboSequence, GameRules = #game_logic_rules{}  ) ->

	ComboSum = fun( Combo, { NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber } ) -> 
			
			{ NewNormalGarbageNumber, NewColorGarbageNumber, NewHardGarbageNumber } = get_garbage_from_single_combo( sets:size(Combo), GameRules ),
			
			{NormalGarbageNumber + NewNormalGarbageNumber,
				ColorGarbageNumber + NewColorGarbageNumber, 
					HardGarbageNumber + NewHardGarbageNumber}
		end,

	{ NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber } = lists:foldl( ComboSum, { 0,0,0 } , ComboSequence),
	
	{ Ratio , GarbageTypes } = GameRules#game_logic_rules.garbage_simultaneous_combo_rule,

	GarbageList = get_random_garbage_from_types( (length(ComboSequence) - 1) * Ratio, GarbageTypes ),

	Fun = 
	fun( GarbageType, { NormalGarbageNumberAcc, ColorGarbageNumberAcc, HardGarbageNumberAcc } ) ->
		case GarbageType of
			garbage ->			{ NormalGarbageNumberAcc + 1, ColorGarbageNumberAcc, HardGarbageNumberAcc };
			garbage_color ->	{ NormalGarbageNumberAcc, ColorGarbageNumberAcc + 1, HardGarbageNumberAcc };
			garbage_hard ->		{ NormalGarbageNumberAcc, ColorGarbageNumberAcc, HardGarbageNumberAcc + 1 }
		end
	end,
	lists:foldl( Fun, { NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber }, GarbageList).






get_garbage_from_combo_chain( Combos, GameRules = #game_logic_rules{} ) ->
	SumSequence = fun( ComboSequence, { NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber } ) -> 
			
			{ NewNormalGarbageNumber, NewColorGarbageNumber, NewHardGarbageNumber } = get_garbage_from_combo_sequence( ComboSequence, GameRules ),
			
			{NormalGarbageNumber + NewNormalGarbageNumber,
				ColorGarbageNumber + NewColorGarbageNumber, 
					HardGarbageNumber + NewHardGarbageNumber}
		end,

	{ NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber } = lists:foldl( SumSequence, { 0,0,0 } , Combos),
	{ Ratio , GarbageTypes } = GameRules#game_logic_rules.garbage_chain_rule,

	GarbageList = get_random_garbage_from_types( (length(Combos) - 1) * Ratio, GarbageTypes ),

	Fun = 
	fun( GarbageType, { NormalGarbageNumberAcc, ColorGarbageNumberAcc, HardGarbageNumberAcc } ) ->
		case GarbageType of
			garbage ->			{ NormalGarbageNumberAcc + 1, ColorGarbageNumberAcc, HardGarbageNumberAcc };
			garbage_color ->	{ NormalGarbageNumberAcc, ColorGarbageNumberAcc + 1, HardGarbageNumberAcc };
			garbage_hard ->		{ NormalGarbageNumberAcc, ColorGarbageNumberAcc, HardGarbageNumberAcc + 1 }
		end
	end,
	lists:foldl( Fun, { NormalGarbageNumber, ColorGarbageNumber, HardGarbageNumber }, GarbageList).



get_combo_color( [] ) ->
	shapeshifter;

get_combo_color( ComboList ) ->
	[ Block | RestBlocks ] = ComboList,
	case Block#block.type of
		shapeshifter ->		get_combo_color( RestBlocks );
		_other ->			Block#block.color
	end.


get_minimum_combo_power( _, []) ->
	color;
get_minimum_combo_power( [], _combos) -> 
	color;
get_minimum_combo_power( GameRules, Combos) -> 
	ReverseRules = lists:reverse(GameRules),
	get_minimum_combo_power_rec( ReverseRules, lists:flatten(Combos)).


get_minimum_combo_power_rec( [{{ _combo_size, RuleColor }, Power } | Rest], Combos) -> 
	Fun = 
	fun( Combo, { Found, DidntMatch } ) ->
		ComboList = sets:to_list(Combo),
		case RuleColor == any orelse RuleColor == get_combo_color( ComboList ) of
			true ->			{ true, DidntMatch};
			false ->		{ Found, [ Combo | DidntMatch]}
		end
	end,
	case lists:foldl( Fun, {false, []}, Combos ) of
		{ true, [] } ->				Power;
		{ _, NotMatched } ->		get_minimum_combo_power_rec( Rest, NotMatched)
	end.



get_type_from_rule_with_frenzy( GameRules , Combos) ->
	case get_type_from_rule( GameRules , Combos) of
		color ->	get_minimum_combo_power( GameRules, Combos);
		Other -> 	Other
	end.

get_type_from_rule( [] , _combos) ->
	color;

get_type_from_rule( [{{ ComboSize, RuleColor }, Power } | Rest] , Combos) ->
	ComboFitsRule = 
	fun( Combo ) ->
		ComboList = sets:to_list(Combo),
		length(ComboList) >= ComboSize andalso ( RuleColor == any orelse RuleColor == get_combo_color( ComboList ) )
	end,

	case lists:any( fun( ComboSequence )-> lists:any( ComboFitsRule, ComboSequence ) end, Combos ) of
		false ->		get_type_from_rule( Rest, Combos);
		true ->			Power
	end.








	
