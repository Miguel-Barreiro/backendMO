-include_lib("mc_user_store/include/user_store.hrl").

-record(user, {
	user_id :: binary(),
	user_process_pid = undefined :: pid()
}).


%%-------------------
%	DATABASE 
%-----------------------

-record(  persistent_user, {
	id,
	name,
	guest_id
}).


%-------------------
%	GAME LOGIC
%----------------------

-define(DEFAULT_BOARD_WIDTH, 6).
-define(DEFAULT_BOARD_HEIGHT, 11 ).



-record( board, {
	blocks = gb_trees:empty() :: gb_tree(),
	width :: integer(),
	height :: integer(),

	abilities_random_state = 0,

	ghosts_to_trigger = 0 :: integer(),
	reinforcements = [] :: [color_type()],
	painted = [],
	
	triggered_abilities = [],

	frenzy_turns = 0 :: integer(),
	thrash_turns = 0 :: integer(),
	red_button_pressed = false,
	
	is_overload_active = false,
	killing_blow_active = false,
	barrier_active = false,

	active_powers_equiped = []

}).

-type garbage_type() :: garbage_hard | garbage_color | garbage.
-type ability_type() :: bomb | chromatic_bomb | paint | shapeshifter | tornado | reinforcements | cloner | ghost.
-type block_type() :: color | ability_type() | garbage_type().

-type color_type() :: red | yellow | blue | green | purple | white.



-record( block, {
	type = color :: block_type(),
	color = undefined :: color_type(),
	hardness = 2,
	x = 0 :: integer(),
	y = 0 :: integer()
}).


-type angle_type() :: up | down | right | left.

-record( piece, {
	block1 :: #block{},
	block2 :: #block{}
}).


-record( user_gamestate,{
	user_pid = undefined :: pid(),
	board :: #board{},
	current_piece :: #piece{},
	
	current_piece_angle :: angle_type(),
	current_piece_x = 0 :: integer(),
	current_piece_y = 0 :: integer(),

	current_garbage_id = 1 :: integer(),
	garbage_position_list = [],

	piece_generation_step = 0,
	random_state = undefined
	
}).



-record ( logic_user,{
	lifes_generate_timer = undefined,
	user = undefined :: mc_user(),
	session_start_time = undefined
}).



-record( game_logic_rules,{
	version :: string(),

	abilities_rule = undefined :: [ { {integer(), color_type() | any}, ability_type() } ],
	
	garbage_combo_rule = undefined :: [ { {integer()}, {garbage_type(),integer()}  } ],
	garbage_chain_rule = undefined :: [ { {integer()}, {garbage_type(),integer()}  } ],
	garbage_simultaneous_combo_rule = undefined :: [ { {integer()}, {garbage_type(),integer()}  } ],

	garbage_combo_max = 0 :: integer(),

	total_color_number = 6 :: integer(),
	min_combo_size = 4 :: integer()
}).




-record( game, {

	user1_gamestate = #user_gamestate{},
	user2_gamestate = #user_gamestate{},

	initial_seed = undefined,
	difficult_level = 0,

	game_rules = undefined :: #game_logic_rules{}
}).



-type league_name() :: beginner | skilled | expert | master.

%% CONFIGURATION MODULE

-record( configuration_tier,{
	board_width,
	board_height,

	abilities_generation_rules :: [ { {integer(), color_type() | any}, ability_type() } ] ,

	garbage_combo_rules :: [ { integer(), {garbage_type(),integer()}  } ],
	garbage_chain_rules :: { integer(), [ garbage_type() ]},
	garbage_simultaneous_rules :: { integer(), [ garbage_type() ]},


	garbage_combo_max = 0 :: integer()

}).

