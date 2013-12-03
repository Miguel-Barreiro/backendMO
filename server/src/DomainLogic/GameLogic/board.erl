-module(board).

-export([ new_empty/2 , get_all_blocks/1 ]).
-export([ get_block/3, set_block/4, remove_block/3, is_valid_position/3 ]).
-export([ print_board/1, lager_print_board/1, are_boards_equal/2, get_block_representation/1 ]).

-export([get_column_height/2, get_number_blocks/1]).

-include("include/softstate.hrl").


new_empty( Width, Height ) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	Seed = random:uniform(2147483646),
	#board{ height = Height, width = Width, abilities_random_state = Seed }.


get_number_blocks(Board = #board{}) ->
	gb_trees:size(Board#board.blocks).


get_all_blocks( Board = #board{} ) ->
	gb_trees:values( Board#board.blocks ).



is_valid_position( X , Y, Board = #board{} )
				when Board#board.width > X, Board#board.height > Y, Y >= 0, X >= 0 ->
	true;
is_valid_position( _X , _Y, #board{} ) ->
	false.




%returns the block record
get_block( X , Y , #board{ blocks = Blocks }) ->
	case gb_trees:lookup( {X,Y}, Blocks) of
		none ->					empty;
		{value, Block} ->		Block
	end.



are_blocks_equal( Block1 = #block{type=garbage_hard}, Block2 = #block{type=garbage_hard} ) 
-> 
	Block1 =:= Block2;

are_blocks_equal( Block1 = #block{type=garbage_color}, Block2 = #block{type=garbage_color} ) 
-> 
	Block1 =:= Block2;

are_blocks_equal( Block1, Block2 ) when is_record(Block1,block) andalso is_record(Block2,block) ->
	Block1#block{hardness=undefined} =:= Block2#block{hardness=undefined}.





are_boards_equal(Board = #board{}, Board2 = #board{} ) ->

	Board1_keys = gb_trees:keys(Board#board.blocks),
	Board2_keys = gb_trees:keys(Board2#board.blocks),

	Compare_fun = fun( Block_key ) ->
		{value,Block1} = gb_trees:lookup( Block_key, Board#board.blocks),
		{value,Block2} = gb_trees:lookup( Block_key, Board2#board.blocks),
		are_blocks_equal( Block1, Block2 )
	end,

	case length(Board2_keys) == length(Board1_keys) of
		false ->	false;
		true ->		lists:all( Compare_fun, Board1_keys)
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
					lager:debug("invalid piece place: trying to place the piece ~p in ~p,~p",[Other,X,Y]),
					throw( invalid_move )
			end;
		false ->
			lager:debug("Out of bounds in ~p,~p when board is ~p,~p",[X,Y,Board#board.width,Board#board.height]),
			print_board(Board),
			throw(out_of_bounds)
	end.




%returns the new board record
remove_block( X , Y, Board = #board{ blocks = Blocks } ) ->
	Board#board{ blocks = gb_trees:delete_any({X,Y}, Blocks) }.






get_column_height( Column, Board = #board{} ) ->
	get_column_height( Board, Column, 0 ).

get_column_height( Board = #board{}, _X, Y ) when Y >= Board#board.height ->
	throw( out_of_bounds );

get_column_height( Board = #board{}, X, Y ) ->
	case board:get_block( X , Y, Board ) of
		empty ->		Y;
		_other ->		get_column_height( Board, X , Y + 1)
	end.




print_board( Board = #board{} ) ->
	print_board( Board, Board#board.height ).

print_board( Board = #board{}, 0 ) ->
	io:format( get_print_string( Board#board.width ) ++ "\n", get_column_chars( 0, Board));
print_board( Board = #board{}, Row ) ->
	io:format( get_print_string( Board#board.width ) ++ "\n", get_column_chars( Row, Board)),
	print_board( Board, Row - 1 ).


lager_print_board( Board = #board{} ) ->
	lager_print_board( Board, Board#board.height ).

lager_print_board( Board = #board{}, 0 ) ->
	lager:debug( get_print_string( Board#board.width ) ++ "\n", get_column_chars( 0, Board));
lager_print_board( Board = #board{}, Row ) ->
	lager:debug( get_print_string( Board#board.width ) ++ "\n", get_column_chars( Row, Board)),
	lager_print_board( Board, Row - 1 ).



get_column_chars( Row, Board = #board{}) ->
	get_column_chars( 0, Row, Board).



get_column_chars( X, _, Board = #board{})  when X == Board#board.width ->
	[];
get_column_chars( X, Y, Board = #board{}) ->
	[ get_board_position( Board, X, Y ) | get_column_chars( X + 1, Y, Board) ].



get_block_representation( Block ) ->
	case Block of
		
		empty ->												"  ";

		#block{ type = color, color = red } ->					"-r";
		#block{ type = color, color = yellow } ->				"-y";
		#block{ type = color, color = white } ->				"-w";
		#block{ type = color, color = blue } ->					"-b";
		#block{ type = color, color = purple } ->				"-p";
		#block{ type = color, color = green } ->				"-g";

		#block{ type = chromatic_bomb, color = red } ->			"Cr";
		#block{ type = chromatic_bomb, color = yellow } ->		"Cy";
		#block{ type = chromatic_bomb, color = white } ->		"Cw";
		#block{ type = chromatic_bomb, color = blue } ->		"Cb";
		#block{ type = chromatic_bomb, color = purple } ->		"Cp";
		#block{ type = chromatic_bomb, color = green } ->		"Cg";

		#block{ type = bomb, color = red } ->					"Br";
		#block{ type = bomb, color = yellow } ->				"By";
		#block{ type = bomb, color = white } ->					"Bw";
		#block{ type = bomb, color = blue } ->					"Bb";
		#block{ type = bomb, color = purple } ->				"Bp";
		#block{ type = bomb, color = green } ->					"Bg";


		#block{ type = reinforcements, color = red } ->			"Rr";
		#block{ type = reinforcements, color = yellow } ->		"Ry";
		#block{ type = reinforcements, color = white } ->		"Rw";
		#block{ type = reinforcements, color = blue } ->		"Rb";
		#block{ type = reinforcements, color = purple } ->		"Rp";
		#block{ type = reinforcements, color = green } ->		"Rg";

		#block{ type = shapeshifter, color = red } ->			"?r";
		#block{ type = shapeshifter, color = yellow } ->		"?y";
		#block{ type = shapeshifter, color = white } ->			"?w";
		#block{ type = shapeshifter, color = blue } ->			"?b";
		#block{ type = shapeshifter, color = purple } ->		"?p";
		#block{ type = shapeshifter, color = green } ->			"?g";
		#block{ type = shapeshifter, color = undefined } ->		"?u";

		#block{ type = paint, color = red } ->					"Pr";
		#block{ type = paint, color = yellow } ->				"Py";
		#block{ type = paint, color = white } ->				"Pw";
		#block{ type = paint, color = blue } ->					"Pb";
		#block{ type = paint, color = purple } ->				"Pp";
		#block{ type = paint, color = green } ->				"Pg";

		#block{ type = tornado, color = red } ->				"Tr";
		#block{ type = tornado, color = yellow } ->				"Ty";
		#block{ type = tornado, color = white } ->				"Tw";
		#block{ type = tornado, color = blue } ->				"Tb";
		#block{ type = tornado, color = purple } ->				"Tp";
		#block{ type = tornado, color = green } ->				"Tg";

		#block{ type = cloner, color = red } ->					"Dr";
		#block{ type = cloner, color = yellow } ->				"Dy";
		#block{ type = cloner, color = white } ->				"Dw";
		#block{ type = cloner, color = blue } ->				"Db";
		#block{ type = cloner, color = purple } ->				"Dp";
		#block{ type = cloner, color = green } ->				"Dg";
		
		#block{ type = ghost, color = red } ->					"Gr";
		#block{ type = ghost, color = yellow } ->				"Gy";
		#block{ type = ghost, color = white } ->				"Gw";
		#block{ type = ghost, color = blue } ->					"Gb";
		#block{ type = ghost, color = purple } ->				"Gp";
		#block{ type = ghost, color = green } ->				"Gg";

		#block{ type = garbage } ->								"##";
		
		#block{ type = garbage_hard, hardness = 1 } ->			"#1";
		#block{ type = garbage_hard, hardness = 2 } ->			"#2";
		#block{ type = garbage_hard, hardness = 3 } ->			"#3";
		#block{ type = garbage_hard, hardness = 4 } ->			"#4";

		#block{ type = garbage_color, color = red } ->			"#r";
		#block{ type = garbage_color, color = yellow } ->		"#y";
		#block{ type = garbage_color, color = white } ->		"#w";
		#block{ type = garbage_color, color = blue } ->			"#b";
		#block{ type = garbage_color, color = purple } ->		"#p";
		#block{ type = garbage_color, color = green } ->		"#g"

	end.

get_board_position( Board = #board{}, X, Y ) ->
	get_block_representation( board:get_block( X , Y , Board) ).	


get_print_string( 0 ) ->
	"";
get_print_string( Number ) ->
	get_print_string( Number - 1 ) ++ "~p".

