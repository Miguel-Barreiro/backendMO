
-record(user, {
	userid :: binary(),
	pid :: pid(),
	state :: 'in_queue' | 'matched' | 'out_queue'
}).
