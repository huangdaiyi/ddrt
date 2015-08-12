-module (ddrt_crl).
-export ([add_dailyhour/1, update_dailyhour/1, delete_dailyhour/1, build_create_params/3]).
-include ("include/ddrt.hrl").
-include ("include/crl_script.hrl").

-record (dailyhours, {task_execute_id, date, spend_time, activity, created, created_user_id, created_user_name, process_model_id, task_id, task_name, common_process_id, phase, task_type}).


add_dailyhour(DailyHours) ->
	%%Pool = execute_sync:get_pool(),
	ddrt_mssql_mgr:execute_sync(?INSERT_DAILYHOUR, [
		{sql_datetime, DailyHours#dailyhours.date},
		{sql_integer, DailyHours#dailyhours.spend_time},
		{sql_nvarchar, DailyHours#dailyhours.activity},
		{sql_integer, DailyHours#dailyhours.created_user_id},
		{sql_varchar, DailyHours#dailyhours.created_user_name},
		{sql_integer, DailyHours#dailyhours.process_model_id},%2
		{sql_integer, DailyHours#dailyhours.task_id},% 105
		{sql_varchar, DailyHours#dailyhours.task_name}, %Developers Coding
		{sql_integer, DailyHours#dailyhours.common_process_id},%
		{sql_integer, DailyHours#dailyhours.phase}, %%2
		{sql_integer, DailyHours#dailyhours.task_type}
		]).


update_dailyhour(DailyHours) ->
	ddrt_mssql_mgr:execute_sync(?UPDATE_DAILYHOUR, [
		{sql_datetime, DailyHours#dailyhours.date},
		{sql_integer, DailyHours#dailyhours.spend_time},
		{sql_nvarchar, DailyHours#dailyhours.activity},
		{sql_integer, DailyHours#dailyhours.created_user_id},
		{sql_varchar, DailyHours#dailyhours.created_user_name},
		{sql_integer, DailyHours#dailyhours.phase}
		]).

delete_dailyhour(WorklogId) ->
	ddrt_mssql_mgr:execute_sync(?DELETE_DAILYHOUR, [{sql_varchar, WorklogId}]).


%%fill_dailyhours() ->

build_create_params(CreateReports, UserId, UserName) ->
	 %% ([TaskExecuteId],[Date],[SpendTime],[Activity],[Created],[CreatedUserId],[CreatedUserName],
            %%[ProcessModelId],[TaskId],[TaskName],[CommonProcessId],[Phase],[TaskType]
        %%ddrt_crl:add_dailyhour(ddrt_utils:get_str_today(), TimeSpent, ddrt_utils:get_crl_comment(Content,WorklogId),user,  ),
    Date = ddrt_utils:get_str_today(),

    [begin 
    	TimeSpent = proplists:get_value("timeSpent", R, "8"),
    	WorklogId = proplists:get_value("id", R),
    	Content = proplists:get_value("comment", R, ""),
    	Activity = ddrt_utils:get_crl_comment(Content, WorklogId),
        Params = [{sql_datetime, Date},
		{sql_numeric, TimeSpent},
		{sql_nvarchar, Activity},
		{sql_integer, UserId},
		{sql_varchar, UserName},
		{sql_integer, 2},%2
		{sql_integer, 105},% 105
		{sql_varchar, "Developers Coding"}, %Developers Coding
		{sql_integer, 1009292},%
		{sql_integer, 24}, %%2
		{sql_integer, 31}],
		ddrt_mssql_mgr:execute_sync(?INSERT_DAILYHOUR, Params)
    end || {obj, R} <- CreateReports].

% private int setDailyHoursByCommonProcId(int commonProcId) {
% 		dailyHourEntity.setTaskName("Developers Coding");
% 		dailyHourEntity.setCommonProcessId(commonProcId);
% 		dailyHourEntity.setProcessModelId(5733);
% 		dailyHourEntity.setTaskId(105);
% 		dailyHourEntity.setTaskType(31);

% 		return commonProcId;
% 	}

% 	private int setDailyHoursByCrlNoAsCommonProcId(int crlNo) {
% 		dailyHourEntity.setTaskName("Key User");
% 		dailyHourEntity.setCommonProcessId(crlNo);
% 		dailyHourEntity.setProcessModelId(2);
% 		dailyHourEntity.setTaskId(0);
% 		dailyHourEntity.setTaskType(-1);
% 		dailyHourEntity.setPhase(-1);

% 		return crlNo;
% 	}