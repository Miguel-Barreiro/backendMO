-module( block ).

-export([ new_color/1 ]).
-export([ new_garbage/0 ]).
-export([ new_garbage_hard/1 ]).
-export([ new_garbage_color/1 ]).

-include("include/softstate.hrl").



-spec new_color(Color::color_type()) -> #block{}.
new_color( red ) ->
	#block{ type = color, color = red };
new_color( blue ) ->
	#block{ type = color, color = blue };
new_color( yellow ) ->
	#block{ type = color, color = yellow };
new_color( green ) ->
	#block{ type = color, color = green };
new_color( white ) ->
	#block{ type = color, color = white };
new_color( purple ) ->
	#block{ type = color, color = purple }.




-spec new_garbage() -> #block{}.
new_garbage() ->
	#block{ type = garbage }.


-spec new_garbage_hard( Hardness::integer() ) -> #block{}.
new_garbage_hard(Hardness) ->
	#block{ type = garbage, hardness = Hardness }.




-spec new_garbage_color(Color::color_type()) -> #block{}.
new_garbage_color( red ) ->
	#block{ type = garbage_color, color = red };
new_garbage_color( blue ) ->
	#block{ type = garbage_color, color = blue };
new_garbage_color( yellow ) ->
	#block{ type = garbage_color, color = yellow };
new_garbage_color( green ) ->
	#block{ type = garbage_color, color = green };
new_garbage_color( white ) ->
	#block{ type = garbage_color, color = white };
new_garbage_color( purple ) ->
	#block{ type = garbage_color, color = purple }.
