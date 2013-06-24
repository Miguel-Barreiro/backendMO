
-record(user, {
	user_id :: binary(),
	user_process_pid = undefined :: pid()
}).
