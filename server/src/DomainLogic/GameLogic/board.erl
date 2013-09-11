-module(board).

-export([ new_empty/2 , get_all_blocks/1 ]).

-export([ get_block/3, set_block/4, remove_block/3, is_valid_position/3 ]).

-include("include/softstate.hrl").


new_empty( Width, Height ) ->
	#board{ height = Height, width = Width }.



get_all_blocks( Board = #board{} ) ->
	gb_trees:values( Board#board.blocks ).



is_valid_position( Board = #board{} , X , Y )
				when Board#board.height > Y, Board#board.height > Y, Y >= 0, X >= 0 ->
	true;
is_valid_position( #board{} , _X , _Y ) ->
	false.




%returns the block record
get_block( #board{ blocks = Blocks } , X , Y ) ->
	case gb_trees:lookup( {X,Y}, Blocks) of
		none ->					empty;
		{value, Block} ->		Block
	end.




%returns the new board record
%may throw out_of_bounds exception
set_block( Block = #block{}, Board = #board{ blocks = Blocks } , X , Y )  ->
	case is_valid_position( Board, X, Y) of 
		true ->
			Board#board{ blocks = gb_trees:insert({X,Y}, Block#block{ x = X , y = Y } , Blocks) };
		false ->
			throw( out_of_bounds )
	end.




%returns the new board record
remove_block( Board = #board{ blocks = Blocks } , X , Y ) ->
	Board#board{ blocks = gb_trees:delete_any({X,Y}, Blocks) }.


