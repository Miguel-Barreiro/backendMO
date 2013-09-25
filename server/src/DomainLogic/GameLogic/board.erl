-module(board).

-export([ new_empty/2 , get_all_blocks/1 ]).
-export([ get_block/3, set_block/4, remove_block/3, is_valid_position/3 ]).
-export([ print_board/1, test_print/0 ]).


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
				Other ->
					lager:info("invalid piece place: trying to place the piece ~p in ~p,~p",[Other,X,Y]),
					throw( invalid_move )
			end;
		false ->
			lager:info("Out of bounds in ~p,~p when board is ~p,~p",[X,Y,Board#board.width,Board#board.height]),
			throw( out_of_bounds )
	end.




%returns the new board record
remove_block( X , Y, Board = #board{ blocks = Blocks } ) ->
	Board#board{ blocks = gb_trees:delete_any({X,Y}, Blocks) }.








test_print() ->
	Board = board:set_block( #block{ type = color, color = red } , 0, 0,  
				board:set_block( #block{ type = color, color = green } , 0, 1,  
					board:set_block( #block{ type = color, color = yellow } , 1, 0,  
						board:set_block( #block{ type = color, color = white } , 1, 1,  
							board:set_block( #block{ type = color, color = purple } , 2, 0,  
								board:set_block( #block{ type = color, color = blue } , 3, 0,  
									board:set_block( #block{ type = garbage } , 4, 0,  
										board:new_empty( 5, 14 )))))))),

	print_board(Board).





print_board( Board = #board{} ) ->
	print_board( Board, Board#board.height ).

print_board( Board = #board{}, 0 ) ->
	io:format( get_print_string( Board#board.width ) ++ "\n", get_column_chars( 0, Board));
print_board( Board = #board{}, Row ) ->
	io:format( get_print_string( Board#board.width ) ++ "\n", get_column_chars( Row, Board)),
	print_board( Board, Row - 1 ).


get_column_chars( Row, Board = #board{}) ->
	get_column_chars( 0, Row, Board).



get_column_chars( X, _, Board = #board{})  when X == Board#board.width ->
	[];
get_column_chars( X, Y, Board = #board{}) ->
	[ get_board_position( Board, X, Y ) | get_column_chars( X + 1, Y, Board) ].



get_board_position( Board = #board{}, X, Y ) ->
	case board:get_block( X , Y , Board) of
		empty ->										" ";
		#block{ type = color, color = red } ->			"R";
		#block{ type = color, color = yellow } ->		"Y";
		#block{ type = color, color = white } ->		"W";
		#block{ type = color, color = blue } ->			"B";
		#block{ type = color, color = purple } ->		"P";
		#block{ type = color, color = green } ->		"G";
		#block{ type = garbage } ->						"@"
	end.

	


get_print_string( 0 ) ->
	"";
get_print_string( Number ) ->
	get_print_string( Number - 1 ) ++ "~p".

