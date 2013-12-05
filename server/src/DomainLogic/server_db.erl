-module(server_db).


-include("include/softstate.hrl").

-export([ start/0 ,get_user_data/1, push_user_data/1, delete_user_data/1, login_user/3]).


get_user_data( UserId )->
	case mnesia:sync_transaction(fun () -> mnesia:read({user, UserId}) end) of
		{atomic, []} ->  				{ error, no_user };
		{atomic, [UserData | _ ]} -> 	{ ok, UserData };
		{aborted, Reason} ->			{ error, Reason }
	end.

push_user_data( User = #user{ } ) ->
	case mnesia:sync_transaction(fun() -> mnesia:write(User) end) of
		{atomic, ok } -> 				ok;
		{aborted, Reason} ->			{ error, Reason }
	end.

delete_user_data( UserId ) ->
	lager:debug("delete user from server_db"),
	case mnesia:sync_transaction(fun() -> mnesia:delete(user, UserId ) end) of
		{atomic, ok } -> 				ok;
		{aborted, Reason} ->			{ error, Reason }
	end.



login_user( UserId , LoginFunction, ReloginFunction) ->
	Function = fun() ->
		case mnesia:read({user, UserId}) of
			[] ->
				mnesia:write(LoginFunction());

			[PreviousUserData | _ ] ->
				case ReloginFunction( PreviousUserData ) of 

					{save, User = #user{} } -> 	
						mnesia:write(User);
					_other ->
						ok
				end
		end
	end,

	case mnesia:sync_transaction( Function ) of
		{atomic, ok } -> 				ok;
		{aborted, Reason} ->			{ error, Reason }
	end.




start()->
	create_tables().


create_tables() ->
	mnesia:stop(),
	mnesia:create_schema([node()]),
	mnesia:start(),

	CreateTableRes = mnesia:create_table(user, [ 
		{type, set},
		{index, [user_process_pid]},
		{attributes, record_info(fields, user)},
		{disc_copies, [node()]}
	]),
	lager:debug("creating mnesia tables result is ~p ", [CreateTableRes]),

%	mnesia:create_table(broker_order, [
%		{type, set},
%		{index, [node, opponent, userpid]},
%		{attributes, record_info(fields, broker_order)},
%		{disc_copies, [node()]}
%	]),

	mnesia:wait_for_tables([user], 60000),
	lager:debug("finished creating mnesia tables", []).

%add_mem_node(Master) ->
%	ok = mnesia:start(),
%	mnesia:change_config(extra_db_nodes, [Master]),
%	mnesia:change_table_copy_type(schema, node(), disc_copies),
%	mnesia:add_table_copy(user, node(), disc_copies),
%	mnesia:add_table_copy(broker_order, node(), disc_copies),
%	mnesia:wait_for_tables([user, broker_order], 60000).
