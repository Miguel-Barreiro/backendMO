
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
-type color_type() :: red | yellow | blue | green.

-record( block, {
	type = color :: block_type(),
	color :: color_type(),
	x = 0 :: integer(),
	y = 0 :: integer()
}).


-type angle_type() :: up | down | right | left.

-record( piece, {
	block1 :: #block{},
	block2 :: #block{},
	angle = up :: angle_type()
}).


-record( game, {

	user1_pid = undefined :: pid(),
	user2_pid = undefined :: pid(),

	user1_current_piece :: #piece{},
	user2_current_piece :: #piece{},

	user1_board :: #board{},
	user2_board :: #board{}
}).