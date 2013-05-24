
-record(user, {
	user_id :: binary(),
	user_process_pid = undefined :: pid(),
	user_connection_pid = undefined :: pid(),
	game_pid = undefined :: pid(),
	state :: 'in_queue' | 'matched' | 'out_queue'
}).
