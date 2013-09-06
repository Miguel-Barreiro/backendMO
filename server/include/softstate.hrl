
-record(user, {
	user_id :: binary(),
	user_process_pid = undefined :: pid()
}).





%%-------------------
%	DATABASE 
%-----------------------


-record(  persistent_user, {
	id,
	name,
	guest_id
}).