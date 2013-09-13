-module(board).

-export([ new_empty/2 , get_all_blocks/1 ]).

-export([ get_block/3, set_block/4, remove_block/3, is_valid_position/3 ]).

-include("include/softstate.hrl").


new_empty( Width, Height ) ->
	#board{ height = Height, width = Width }.



get_all_blocks( Board = #board{} ) ->
	gb_trees:values( Board#board.blocks ).



is_valid_position( X , Y, Board = #board{} )
				when Board#board.height > Y, Board#board.height > Y, Y >= 0, X >= 0 ->
	true;
is_valid_position( _X , _Y, #board{} ) ->
	false.




%returns the block record
get_block( X , Y , #board{ blocks = Blocks }) ->
	case gb_trees:lookup( {X,Y}, Blocks) of
		none ->					empty;
		{value, Block} ->		Block
	end.




%returns the new board record
%may throw out_of_bounds exception
set_block( Block = #block{}, X , Y, Board = #board{ blocks = Blocks } )  ->
	case is_valid_position( X, Y, Board) of 
		true ->
			case get_block( X , Y, Board ) of
				empty ->
					Board#board{ blocks = gb_trees:insert({X,Y}, Block#block{ x = X , y = Y } , Blocks) };
				_other ->
					throw( invalid_move )
			end;
		false ->
			throw( out_of_bounds )
	end.




%returns the new board record
remove_block( X , Y, Board = #board{ blocks = Blocks } ) ->
	Board#board{ blocks = gb_trees:delete_any({X,Y}, Blocks) }.


