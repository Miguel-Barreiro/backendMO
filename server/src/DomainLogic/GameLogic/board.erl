-module(board).

-export([ new_empty/0 , get_block/3, set_block/4, remove_block/3 ]).

-include("include/softstate.hrl").


new_empty() ->
	#board{}.

%returns the block record
get_block( #board{ blocks = Blocks } , x , y ) ->
	case gb_tree:lookup( {x,y}, Blocks) of
		none ->					empty;
		{value, Block} ->		Block
	end.


%returns the new board record
set_block( Block = #block{}, Board = #board{ blocks = Blocks } , x , y ) ->
	Board#board{ blocks = gb_tree:insert({x,y}, Block , Blocks) }.


%returns the new board record
remove_block( Board = #board{ blocks = Blocks } , x , y ) ->
	Board#board{ blocks = gb_tree:delete_any({x,y}, Blocks) }.


