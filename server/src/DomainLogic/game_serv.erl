-module(game_serv).
-behaviour(gen_server).

-define(CONNECTION_TIMEOUT, 40000).

-define(DIFFICULT_CHANGE_SECONDS, 30).

-include("include/softstate.hrl").

-define( COUNTDOWN_TO_START_SECONDS , 5).

-type game_state_type() :: running | waiting_payers.

-record(game_state, {
	difficult_level = 0,
	user1_pid :: pid(),
	user2_pid :: pid(),
	user1_monitor,
	user2_monitor,
	user1_victories = 0,
	user2_victories = 0,
	is_user1_ready = true,
	is_user2_ready = true,
	state = waiting_payers :: game_state_type()
}).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/2]).

start_link( User_pid, User_pid2  ) ->
    gen_server:start_link(?MODULE, [ User_pid, User_pid2 ], []).

init(InitData) ->
	gen_server:cast(self(), InitData),
	{ok, #game_state{ }}.


handle_cast([User_pid, User_pid2], State = #game_state{ }) ->

	lager:info("new game with user_id ~p user ~p",[User_pid,User_pid2]),

	gen_server:cast( User_pid , {register_game_process,self()}),
	Connection_monitor1 = monitor(process, User_pid),
	
	gen_server:cast( User_pid2 , {register_game_process,self()}),
	Connection_monitor2 = monitor(process, User_pid2),

	gen_server:cast(self() , start_game ),

	{noreply, State#game_state{
				user1_pid = User_pid,
				user2_pid = User_pid2,
				user1_monitor = Connection_monitor1,
				user2_monitor = Connection_monitor2,
				is_user1_ready = true,
				is_user2_ready = true,
				state = waiting_payers
			}
	};



handle_cast( start_game, State = #game_state{ 	user1_pid = User_pid, 
												user2_pid = User_pid2, 
												state = Game_State, 
												is_user1_ready = User1_ready, 
												is_user2_ready = User2_ready} ) when Game_State == waiting_payers,
																					 User1_ready == true,
																					 User2_ready == true ->
	
	{ ok, Name1 } = gen_server:call( User_pid, get_user_id ),
	{ ok, Name2 } = gen_server:call( User_pid2, get_user_id ),

	StartTime = swiss:unix_timestamp() + ?COUNTDOWN_TO_START_SECONDS, 	%the game will start in <COUNTDOWN_TO_START_SECONDS> seconds
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3), 
	Seed = random:uniform(2147483646),

	gen_server:cast( User_pid , {game_start , Name2 , StartTime, Seed } ),
	gen_server:cast( User_pid2 , {game_start , Name1 , StartTime, Seed } ),

	erlang:send_after(timer:seconds(?DIFFICULT_CHANGE_SECONDS), self(), difficult_change),

	{ noreply, State#game_state{ difficult_level = 0, state = running, is_user1_ready = false, is_user2_ready = false } };





handle_cast( { user_ready_rematch, User_pid} , State = #game_state{ state = Game_State, is_user2_ready = User2_ready, user1_pid = User1_pid } ) 
				when User1_pid == User_pid, Game_State == waiting_payers ->
	case User2_ready of
		true ->
			lager:info("game ~p is going to start (rematch)",[self()]),
			gen_server:cast(self() , start_game );
		false ->
			nothing_happens
	end,
	{ noreply, State#game_state{ is_user1_ready = true } };

handle_cast( { user_ready_rematch, User_pid} , State = #game_state{ state = Game_State, is_user1_ready = User1_ready, user2_pid = User2_pid } ) 
				when User2_pid == User_pid, Game_State == waiting_payers ->
	case User1_ready of
		true ->
			lager:info("game ~p is going to start (rematch)",[self()]),
			gen_server:cast(self() , start_game );
		false ->
			nothing_happens
	end,
	{ noreply, State#game_state{ is_user2_ready = true} };









handle_cast( { user_lost_game, Lost_user_pid } , State = #game_state{ user1_pid = User1_pid, user2_pid = User2_pid } ) ->
	
	lager:info("game ~p is going to end",[self()]),

	case Lost_user_pid of
		User1_pid ->
			gen_server:cast(User2_pid, {send_won_message , no_reason}),
			gen_server:cast(User1_pid, {send_lost_message , no_reason});
		User2_pid ->
			gen_server:cast(User2_pid, {send_lost_message , no_reason}),
			gen_server:cast(User1_pid, {send_won_message , no_reason});
		_->
			ok
	end,
	
	{noreply, State#game_state{ state = waiting_payers, is_user1_ready = false, is_user2_ready = false } };






handle_cast( { send_message_to_other, Msg, From_pid }, State = #game_state{ user1_pid = User1_pid, user2_pid = User2_pid })->
	case From_pid of
		User1_pid ->
			gen_server:cast(User2_pid, {send_message, Msg });
		User2_pid ->
			gen_server:cast(User1_pid, {send_message, Msg });
		_->
			ok
	end,
	{noreply, State};





handle_cast(Msg, State ) ->
	lager:error("game_serv: unknown cast ~p received when state was ~p", [Msg, State]),
	{noreply, State}.





handle_info( difficult_change , State = #game_state{ state = Game_State, user2_pid = User2_pid, user1_pid = User1_pid , difficult_level = Level } ) ->

	New_level = Level + 1,

	gen_server:cast( User1_pid , {game_difficult_change , New_level } ),
	gen_server:cast( User2_pid , {game_difficult_change , New_level } ),

	lager:info("game_serv: GAME DIFFICULT CHANGED TO ~p",[New_level]),

	erlang:send_after(timer:seconds(?DIFFICULT_CHANGE_SECONDS), self(), difficult_change),

	{ noreply, State#game_state{ is_user1_ready = true , difficult_level = New_level } };







%%
%	called when the user1 connection stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #game_state{user1_monitor = Connection_monitor, user2_pid = User2_pid})
		 when Reference == Connection_monitor ->

	lager:info("user ~p connection went down", [Pid]),

	demonitor(Connection_monitor),
	message_processor:process_user_disconect(Pid, User2_pid, self()),
	
	{stop, normal, State};

%%
%	called when the user2 connection stops
%%
handle_info({'DOWN', Reference, process, Pid, _Reason}, State = #game_state{user2_monitor = Connection_monitor, user1_pid = User1_pid }) 
		when Reference == Connection_monitor ->

	lager:info("user ~p connection went down", [Pid]),

	demonitor(Connection_monitor),
	message_processor:process_user_disconect(Pid, User1_pid, self()),

	{stop, normal, State};




%%
%	called when the user disconect timeouts
%%
handle_info(connection_timeout, State = #game_state{}) ->
    lager:debug("connection timeout", []),
    {stop, normal, State};








handle_info(M,S) ->
	lager:error("unhandled info ~p", [M]),
	{noreply, S}.

handle_call(_E, _From, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	lager:error("game_serv: ~p terminate with reason: ~p", [self(),Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
