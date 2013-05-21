
-record(user, {
	user_id :: binary(),
	user_process_pid :: pid(),
	user_connection_pid :: pid(),
	state :: 'in_queue' | 'matched' | 'out_queue'
}).
