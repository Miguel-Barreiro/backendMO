
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

-type block_type() :: color | garbage.
-type color_type() :: red | yellow | blue | green | purple | white.

-record( block, {
	type = color :: block_type(),
	color :: color_type(),
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
	garbage_position_list = [],
	piece_generation_step = 0
}).

-record( game, {

	user1_gamestate = #user_gamestate{},
	user2_gamestate = #user_gamestate{},
	initial_seed = undefined

}).