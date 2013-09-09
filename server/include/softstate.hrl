
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
	blocks = gb_tree:empty() :: gb_tree()
}).

-type block_type() :: color | garbage.


-record( block, {
	type = color :: block_type()
}).


-record( piece, {
	block1 :: #block{},
	block2 :: #block{},
	angle
}).


-record( game, {

	user1_pid = undefined :: pid(),
	user2_pid = undefined :: pid(),

	user1_board :: #board{},
	user2_board :: #board{}
}).