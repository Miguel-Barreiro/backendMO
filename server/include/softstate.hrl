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

-record( board, {
	blocks = gb_trees:empty() :: gb_tree(),
	width :: integer(),
	height :: integer()
}).

-type block_type() :: color | garbage | bomb | chromatic_bomb | garbage_hard | garbage_color.
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

	garbage_position_list = [],
	piece_generation_step = 0,
	random_state = undefined
}).

-record( game, {

	user1_gamestate = #user_gamestate{},
	user2_gamestate = #user_gamestate{},
	initial_seed = undefined,
	difficult_level = 0
}).


-record ( logic_user,{
	lifes_generate_timer = undefined,
	user = undefined :: mc_user(),
	session_start_time = undefined
}).

-type power_type() :: generate_bomb.


-record( game_logic_power_rule,{
	combo_size,
	color = red :: color_type(),
	power = generate_bomb :: power_type()
}).