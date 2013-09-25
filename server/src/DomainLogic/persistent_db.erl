-module(persistent_db).

-include("../deps/emysql/include/emysql.hrl").
-include("include/softstate.hrl").

-export([ database_connect/0, create_user/1 ]).
-export([ get_user_by_name/1, get_user_by_guest_id/1 ]).


-define( DB_POOL_NAME , miniorbs_db_pool).






database_connect() ->

	Db_host = "development.cgroqtenwniw.us-west-2.rds.amazonaws.com",
	Db_user = "devuser",
	Db_password = "devpassword",
	Db_name = "miniorbs_dev",
	Db_connections_number = 2,

	%{ok , Db_host} = application:get_env( apn_creator, db_host),
	%{ok , Db_user} = application:get_env( apn_creator, db_user),
	%{ok , Db_password} = application:get_env( apn_creator, db_password),
	%{ok , Db_name} = application:get_env( apn_creator, db_name),
	%{ok , Db_connections_number} = application:get_env( apn_creator, db_connections_number),

	Return = (catch emysql:add_pool(?DB_POOL_NAME, Db_connections_number, Db_user, Db_password, Db_host, 3306, Db_name, utf8)),
		
	case Return of
		{'EXIT', { failed_to_authenticate, Reason}} ->
			io:format("failed to authenticate to database: ~p", [Reason]),
			{error,Reason};
		{'EXIT', Reason} ->
			io:format(" failed to connect to database: ~p", [Reason]),
			{error,Reason};
		_Ok ->
			io:format("i have connected to database: ~p", [Db_host]),
			ok
	end.



%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										SETTERS
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


%persistent_db:create_user("Name")

%returns {ok, guest_id} or {error, _ }
create_user( Name ) ->
	lager:info("creating new user with name ~p",[Name]),
	Query = "call user_create(\'" ++ Name ++ "\' )",
	case  (catch emysql:execute(?DB_POOL_NAME, list_to_binary(Query) )) of
		Ok_packet when is_record(Ok_packet, ok_packet) ->		{error, user_not_created};
		[Result | _ ] ->
			case Result#result_packet.rows of
				[] ->											{error, user_not_created};
				[ [New_user_id] ] ->							{ok, New_user_id}
			end
	end.






%%::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
%%
%%										GETTERS
%%:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


get_user_by_guest_id( Guest_id ) ->
	Query = "call user_search_by_guest_id(\'" ++ Guest_id ++ "\' )",
	case (catch emysql:execute(?DB_POOL_NAME, list_to_binary(Query) )) of

		Ok_packet when is_record(Ok_packet, ok_packet) ->
			lager:error("user with guest_id ~p wasnt found",[Guest_id]),
			{error, user_not_found};

		[Result | _ ] ->
			case emysql_util:as_record(Result, persistent_user, record_info(fields, persistent_user) ) of
				[]->				lager:error("user with guest_id ~p wasnt found",[Guest_id]),
									{error, user_not_found};
				[ User | _ ] ->		{ok, User}
			end;
		{'EXIT',{mysql_timeout, _ , _ } } ->
			lager:error("couldnt query to database , timeout reached"),
			{error, query_timeout}
	end.






get_user_by_name( Name ) ->
	Query = "call user_search_by_name(\'" ++ Name ++ "\' )",
	case (catch emysql:execute(?DB_POOL_NAME, list_to_binary(Query) )) of

		Ok_packet when is_record(Ok_packet, ok_packet) ->
			lager:error("user ~p wasnt found",[Name]),
			{error, user_not_found};

		[Result | _ ] ->
			case emysql_util:as_record(Result, persistent_user, record_info(fields, persistent_user) ) of
				[]->				lager:error("user ~p wasnt found",[Name]),
									{error, user_not_found};
				[ User | _ ] ->		{ok, User}
			end;
		{'EXIT',{mysql_timeout, _ , _ } } ->
			lager:error("couldnt query to database , timeout reached"),
			{error, query_timeout}
	end.













