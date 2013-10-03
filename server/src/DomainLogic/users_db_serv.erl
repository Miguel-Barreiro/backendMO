-module(users_db_serv).
-behaviour(gen_server).

-include("include/softstate.hrl").
-include_lib("mc_user_store/include/user_store.hrl").

-export([ create_user/1 ]).
-export([ get_user_by_guest_id/1 ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/0]).


-record(users_db_state, {
	context :: store_context()
}).


-define(MAX_GUEST_USER_TIME, 9999999).

-spec create_user( Name :: binary()) -> {error, not_found} | {ok, binary()}.
create_user( Name ) ->
	gen_server:call(whereis(?MODULE), {create_user, Name}).

-spec get_user_by_guest_id(binary()) -> {error, not_found} | {ok, mc_user()}.
get_user_by_guest_id( Guest_id ) ->
	gen_server:call(whereis(?MODULE), {get_user_by_guest_id, Guest_id}).




start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	gen_server:cast(self(), init),
	{ok, #users_db_state{}}.




handle_call( {create_user, Name}, _From, State = #users_db_state{}) ->
	{ok, {Guest_id, _User}} = user_store:create_local_user( State#users_db_state.context, Name, ?MAX_GUEST_USER_TIME),
	{reply, {ok, Guest_id}, State};


handle_call( {get_user_by_guest_id, Guest_id}, _From, State = #users_db_state{}) ->
	{reply,user_store:login_local_user(State#users_db_state.context, Guest_id),State};


handle_call(_E, _From, State) ->
	{noreply, State}.








handle_cast( init, State = #users_db_state{} ) ->
	{noreply, State#users_db_state{ context = user_store:new() }};

handle_cast( Msg, State) ->
	lager:error("users_db_serv: unhandled cast ~p", [Msg]),
	{noreply, State}.


handle_info(Msg, State) ->
	lager:error("users_db_serv: unhandled info ~p", [Msg]),
	{noreply, State}.


terminate(Reason, _State) ->
	lager:error("users_db_serv: terminate reason: ~p", [Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.








