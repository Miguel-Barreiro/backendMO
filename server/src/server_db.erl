-module(server_db).

-include("include/softstate.hrl").


-export([ start/0 ,get_user_data/1, push_user_data/1, get_unmatched_all_users/0]).

get_unmatched_all_users() ->
	case mnesia:sync_transaction(fun () -> mnesia:match_object(user, {user, '_', '_', 'in_queue'}, read) end)  of
		{atomic, Result } ->  			{ ok , Result };
		{aborted, Reason} ->			{ error, Reason }
	end.


get_user_data( UserId )->
	case mnesia:sync_transaction(fun () -> mnesia:read({user, UserId}) end) of
		{atomic, []} ->  				{ error, no_user };
		{atomic, [UserData | _ ]} -> 	{ ok, UserData };
		{aborted, Reason} ->			{ error, Reason }
	end.

push_user_data( User = #user{ } ) ->
	case mnesia:sync_transaction(fun() -> mnesia:write(User) end) of
		{atomic, ok } -> 			ok;
		{aborted, Reason} ->		{ error, Reason }
	end.


start()->
	create_tables().



create_tables() ->
	mnesia:stop(),
	mnesia:create_schema([node()]),
	mnesia:start(),

	mnesia:create_table(user, [ 
		{type, set},
		{index, [pid]},
		{attributes, record_info(fields, user)},
		{disc_copies, [node()]}
	]),
%	mnesia:create_table(broker_order, [
%		{type, set},
%		{index, [node, opponent, userpid]},
%		{attributes, record_info(fields, broker_order)},
%		{disc_copies, [node()]}
%	]),

	mnesia:wait_for_tables([user], 60000).

add_mem_node(Master) ->
	ok = mnesia:start(),
	mnesia:change_config(extra_db_nodes, [Master]),
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	mnesia:add_table_copy(user, node(), disc_copies),
	mnesia:add_table_copy(broker_order, node(), disc_copies),
	mnesia:wait_for_tables([user, broker_order], 60000).
