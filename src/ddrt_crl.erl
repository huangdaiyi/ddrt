-module (ddrt_crl).


-define (INSERT_DAILYHOUR, "INSERT INTO [DailyHours] ([TaskExecuteId],[Date],[SpendTime],[Activity],[Created],[CreatedUserId],[CreatedUserName],[ProcessModelId],[TaskId],[TaskName],[CommonProcessId],[Phase],[TaskType]) VALUES(0,?,?,?,GETDATE(),?,?,?,?,?,?,?,?)").
-define (UPDATE_DAILYHOUR, "UPDATE [DailyHours] SET [TaskExecuteId] = 0, [Date] = ? ,[SpendTime] = ? ,[Activity] = ?,[CreatedUserId] = ?,[CreatedUserName] = ?,[Phase] = ? WHERE [Activity] LIKE '%[AUTO#?]%'' ESCAPE '['").
-define (DELETE_DAILYHOUR, "DELETE FROM [DailyHours] WHERE [Activity] like '[AUTO#?]%' ESCAPE '['").
-export ([add_dailyhour/1, update_dailyhour/1, delete_dailyhour/1]).

-record (dailyhours, {task_execute_id, date, spend_time, activity, created, created_user_id, created_user_name, process_model_id, task_id, task_name, common_process_id, phase, task_type}).


add_dailyhour(DailyHours) ->
	odbc:param_query(getRef(), ?INSERT_DAILYHOUR, [
		{sql_datetime, DailyHours#dailyhours.date},
		{sql_integer, DailyHours#dailyhours.spend_time},
		{sql_nvarchar, DailyHours#dailyhours.activity},
		{sql_integer, DailyHours#dailyhours.created_user_id},
		{sql_varchar, DailyHours#dailyhours.created_user_name},
		{sql_integer, DailyHours#dailyhours.process_model_id},
		{sql_integer, DailyHours#dailyhours.task_id},
		{sql_varchar, DailyHours#dailyhours.task_name},
		{sql_integer, DailyHours#dailyhours.common_process_id},
		{sql_integer, DailyHours#dailyhours.phase},
		{sql_integer, DailyHours#dailyhours.task_type}
		]).

update_dailyhour(DailyHours)
	odbc:param_query(getRef(), ?UPDATE_DAILYHOUR, [
		{sql_datetime, DailyHours#dailyhours.date},
		{sql_integer, DailyHours#dailyhours.spend_time},
		{sql_nvarchar, DailyHours#dailyhours.activity},
		{sql_integer, DailyHours#dailyhours.created_user_id},
		{sql_varchar, DailyHours#dailyhours.created_user_name},
		{sql_integer, DailyHours#dailyhours.phase}
		]).

delete_dailyhour(WorklogId)
	odbc:param_query(getRef(), ?UPDATE_DAILYHOUR, [
		{sql_varchar, DailyHours#dailyhours.WorklogId}
		]).

getRef() ->
	{ok, ConnectStr} = neg_hydra:get_env(crl_connect),
	connect(ConnectStr).

connect(ConnectStr) ->
	{ok, ConnectRef} = odbc:connect(ConnectStr, [{sql}]),
	ConnectStr.

	
