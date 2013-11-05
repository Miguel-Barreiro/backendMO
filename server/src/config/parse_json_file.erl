-module(parse_json_file).

-export([get_values_from_json/1 , get_tiers_from_json/1 ]).


-include("include/softstate.hrl").






get_values_from_json( Proplist ) ->
	{All_values} = proplists:get_value(<<"values">>,Proplist),

	Fun = fun( { Key, Value} , Tree ) ->  
		lager:debug("configuration value ~p -> ~p",[ binary_to_list(Key),Value]),
		gb_trees:insert(Key, Value, Tree) 
	end,
	lists:foldl( Fun, gb_trees:empty(), All_values ).






get_real_garbage_type_from_json( Json_garbage_type ) ->
	case Json_garbage_type of
		<<"BlockTypeGarbage">> ->		garbage;
		<<"BlockTypeColor">> ->			garbage_color;
		<<"BlockTypeHard">> ->			garbage_hard
	end.



get_combo_rules_from_json( List ) ->
	Fun = fun( {[{ <<"blockColor">>,Type}, { <<"comboSize">>, Combo_size}, { <<"quantityGenerated">>, Quantity} ]}, Result ) ->
		[ {Combo_size, {get_real_garbage_type_from_json(Type), Quantity} } | Result]
	end,
	lists:foldl( Fun, [], List ).


get_simultaneous_rules_from_json( List ) ->
	Fun = fun( {[{ <<"blockColor">>,Type}, { <<"quantityGenerated">>, Quantity} ]} , { _ , Rest_list } ) ->
		{ Quantity, [ get_real_garbage_type_from_json(Type) | Rest_list] }
	end,
	lists:foldl( Fun, { 0 , [] }, List ).


get_chain_rules_from_json( List ) ->
	Fun = fun( {[{ <<"blockColor">>,Type}, { <<"quantityGenerated">>, Quantity} ]}, { _ , Rest_list } ) ->
		{ Quantity, [ get_real_garbage_type_from_json(Type) | Rest_list] }
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
		<<"BlockTypeReinforcements">> ->		reinforcements
	end.


get_abilities_generation_rules_from_json( List ) ->
	Fun = fun( {[ {<<"blockColor">>,Type}, {<<"comboSizeMin">>, Combo_size}, { <<"powerType">>, Power} ]}, Rest_list ) ->
		[ {{Combo_size,get_block_type_from_json(Type)}, get_power_from_json( Power ) } | Rest_list]
	end,
	lists:reverse(lists:foldl( Fun, [], List )).
	




get_tier_from_json( Tier_properties ) ->
	{Garbage_rules} = proplists:get_value(<<"garbage">>,Tier_properties),

	Garbage_chain_rules = proplists:get_value( <<"garbagePerChain">>, Garbage_rules),
	Garbage_combo_rules = proplists:get_value( <<"garbagePerCombo">>, Garbage_rules),
	Garbage_simultaneous_rules = proplists:get_value( <<"garbagePerSimultaneous">>, Garbage_rules),

	Abilities_generation_rules = proplists:get_value( <<"powersGeneration">>, Tier_properties),

	#configuration_tier{
		board_width = proplists:get_value( <<"boardSizeX">>, Tier_properties),
		board_height = proplists:get_value( <<"boardSizeY">>, Tier_properties),

		abilities_generation_rules = get_abilities_generation_rules_from_json(Abilities_generation_rules),

		garbage_combo_rules = get_combo_rules_from_json( Garbage_combo_rules ),
		garbage_chain_rules = get_chain_rules_from_json( Garbage_chain_rules ),
		garbage_simultaneous_rules = get_simultaneous_rules_from_json( Garbage_simultaneous_rules ) 
	}.



get_tiers_from_json( Proplist ) ->
	{All_values} = proplists:get_value(<<"tiers">>,Proplist),

%	[{ Tier_name, Tier_properties } | _] = All_values,
%	lager:info("tier properties for ~p are ~p",[Tier_name, Tier_properties]),

	Fun = fun( { Tier_name, {Tier_properties} }, Result ) ->  
		%lager:info("tier properties for ~p are ~p",[Tier_name, Tier_properties]),
		Tier = get_tier_from_json( Tier_properties ),
		lager:info("\n\ntier ~p has \n\n ~p \n\n",[Tier_name,Tier]),
		gb_trees:insert(Tier_name, Tier, Result)
	end,

	lists:foldl( Fun, gb_trees:empty(), All_values ).

