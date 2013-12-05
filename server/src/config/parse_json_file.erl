-module(parse_json_file).

-export([get_values_from_json/1 , get_tiers_from_json/1 ]).


-include("include/softstate.hrl").






get_values_from_json( Proplist ) ->
	{AllValues} = proplists:get_value(<<"values">>,Proplist),

	Fun = fun( { Key, Value} , Tree ) ->  
		lager:debug("configuration value ~p -> ~p",[ binary_to_list(Key),Value]),
		gb_trees:insert(Key, Value, Tree) 
	end,
	lists:foldl( Fun, gb_trees:empty(), AllValues ).






get_real_garbage_type_from_json( JsonGarbageType ) ->
	case JsonGarbageType of
		<<"BlockTypeGarbage">> ->		garbage;
		<<"BlockTypeColor">> ->			garbage_color;
		<<"BlockTypeHard">> ->			garbage_hard
	end.



get_combo_rules_from_json( List ) ->
	Fun = fun( {[{ <<"blockColor">>,Type}, { <<"comboSize">>, ComboSize}, { <<"quantityGenerated">>, Quantity} ]}, {Result,Max} ) ->
		NextMax = case Max > ComboSize of true -> Max; false -> ComboSize end,
		{ [{ComboSize, {get_real_garbage_type_from_json(Type), Quantity}} | Result], NextMax}
	end,
	lists:foldl( Fun, { [], 0 }, List ).


get_simultaneous_rules_from_json( List ) ->
	Fun = fun( {[{ <<"blockColor">>,Type}, { <<"quantityGenerated">>, Quantity} ]} , { _ , RestList } ) ->
		{ Quantity, [ get_real_garbage_type_from_json(Type) | RestList] }
	end,
	lists:foldl( Fun, { 0 , [] }, List ).


get_chain_rules_from_json( List ) ->
	Fun = fun( {[{ <<"blockColor">>,Type}, { <<"quantityGenerated">>, Quantity} ]}, { _ , RestList } ) ->
		{ Quantity, [ get_real_garbage_type_from_json(Type) | RestList] }
	end,
	lists:foldl( Fun, { 0 , [] }, List ).



get_block_type_from_json( Type ) ->
	case Type of
		<<"BlockColorBlue">> ->			blue;
		<<"BlockColorRed">> ->			red;
		<<"BlockColorOrange">> ->		yellow;
		<<"BlockColorGreen">> ->		green;
		<<"BlockColorPurple">> ->		purple;
		<<"BlockColorWhite">> ->		white;
		<<"BlockColorAny">>	->			shapeshifter;
		_ -> 							any
	end.

get_power_from_json( Power )->
	case Power of
		<<"BlockTypeBomb">> ->					bomb;
		<<"BlockTypePaint">> ->					paint;
		<<"BlockTypeShapeshifter">> ->			shapeshifter;
		<<"BlockTypeTornado">> ->				tornado;
		<<"BlockTypeClone">> ->					cloner;
		<<"BlockTypeGhost">> ->					ghost;
		<<"BlockTypeReinforcements">> ->		reinforcements;
		<<"BlockTypeChromatic">> ->				chromatic_bomb
	end.


get_abilities_generation_rules_from_json( List ) ->
	Fun = fun( {[ {<<"blockColor">>,Type}, {<<"comboSizeMin">>, ComboSize}, { <<"powerType">>, Power} ]}, RestList ) ->
		[ {{ComboSize,get_block_type_from_json(Type)}, get_power_from_json( Power ) } | RestList]
	end,
	lists:reverse(lists:foldl( Fun, [], List )).
	




get_tier_from_json( TierProperties ) ->
	{GarbageRules} = proplists:get_value(<<"garbage">>,TierProperties),

	GarbageChainRules = proplists:get_value( <<"garbagePerChain">>, GarbageRules),
	GarbageComboRules = proplists:get_value( <<"garbagePerCombo">>, GarbageRules),
	GarbageSimultaneousRules = proplists:get_value( <<"garbagePerSimultaneous">>, GarbageRules),

	AbilitiesGenerationRules = proplists:get_value( <<"powersGeneration">>, TierProperties),

	{GarbagesCombo, MaxCombo} = get_combo_rules_from_json( GarbageComboRules ),
	GarbagesChain = get_chain_rules_from_json( GarbageChainRules ),
	GarbagesSimultaneous = get_simultaneous_rules_from_json( GarbageSimultaneousRules ),

	#configuration_tier{
		board_width = proplists:get_value( <<"boardSizeX">>, TierProperties),
		board_height = proplists:get_value( <<"boardSizeY">>, TierProperties),

		abilities_generation_rules = get_abilities_generation_rules_from_json(AbilitiesGenerationRules),

		garbage_combo_rules = GarbagesCombo,
		garbage_chain_rules = GarbagesChain,
		garbage_simultaneous_rules = GarbagesSimultaneous	,

		garbage_combo_max = MaxCombo
	}.



get_tiers_from_json( Proplist ) ->
	{AllValues} = proplists:get_value(<<"tiers">>,Proplist),

%	[{ TierName, TierProperties } | _] = AllValues,
%	lager:info("tier properties for ~p are ~p",[TierName, TierProperties]),

	Fun = fun( { TierName, {TierProperties} }, Result ) ->  
		%lager:info("tier properties for ~p are ~p",[TierName, TierProperties]),
		Tier = get_tier_from_json( TierProperties ),
		lager:info("\n\ntier ~p has \n\n ~p \n\n",[TierName,Tier]),
		gb_trees:insert(TierName, Tier, Result)
	end,

	lists:foldl( Fun, gb_trees:empty(), AllValues ).

