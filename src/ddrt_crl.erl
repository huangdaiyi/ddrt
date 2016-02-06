-module  (ddrt_crl).
-export  ([add_dailyhour/5, update_dailyhour/3, delete_dailyhour/1, get_internal_key_crlno/1]).
-include ("include/ddrt.hrl").
-include ("include/crl_script.hrl").
-define  (PHASE_MAP(Activity), proplists:get_value(Activity, 
				[{"Requirements", 22},
				 {"Design", 23},
				 {"Development", 24},
				 {"Testing", 25},
				 {"Deployment", 26},
				 {"UAT Phase", 27}, 
				 {"Support", 28},
				 {"Documentation", 23}, 
				 {"Management", 28}, 
				 {"Study & Tranning", 24}, 
				 {"Research", 24},
				 {"Meeting", 22},
				 {"Cooperation", 28}], -1)).


update_dailyhour(TimeSpent, Content, WorklogId) ->
	ddrt_mssql_mgr:execute_sync(?UPDATE_DAILYHOUR(ddrt_utils:binary_to_string(WorklogId)), [
		{{sql_varchar, 23},[list_to_binary(ddrt_utils:get_mssql_day_string())]},
		{{sql_decimal,15,2},[ddrt_utils:to_float(TimeSpent)]},
		{{sql_wvarchar,200},  [ddrt_utils:to_sql_wvarchar(ddrt_utils:get_crl_comment(Content, WorklogId))]}]).
	
delete_dailyhour(WorklogId) ->
	ddrt_mssql_mgr:execute_sync(?DELETE_DAILYHOUR(ddrt_utils:binary_to_string(WorklogId)), []).


add_dailyhour(DailyHour, LoginId, UserName, UserId, Req) ->
    NewUserName = re:replace(UserName, "\\.", " ", [global, {return, list}]),
	TimeSpent = ddrt_utils:to_float(ddrt_utils:get_value("timeSpent", DailyHour, 8.0)),
	WorklogId = proplists:get_value("id", DailyHour),
	Content = proplists:get_value("comment", DailyHour, ""),
	Activity = ddrt_utils:get_crl_comment(Content, WorklogId),
	
    Params = [{{sql_varchar, 23},[list_to_binary(ddrt_utils:get_mssql_day_string())]},
			  {{sql_decimal,15,2},[TimeSpent]},
              {{sql_wvarchar,200},  [ddrt_utils:to_sql_wvarchar(Activity)]},
              {{sql_char,20},[ddrt_utils:string_to_binary(LoginId)]},
              {{sql_wvarchar,50}, [ddrt_utils:to_sql_wvarchar(NewUserName)]}],

    case get_params(ddrt_utils:get_value("key", DailyHour), LoginId ,Req) of
              	{ok, OtherParams} ->
              		ddrt_mssql_mgr:execute_sync(?INSERT_DAILYHOUR, Params ++ OtherParams);
              	{error, Reason} ->
              		ddrt_mail:error_remind(Reason, UserId)
    end.




get_params(IssueKey, LoginId ,Req) ->
	
	{_IssueKey, ActivityBin, Fields} = get_parent_issue(IssueKey, Req),
	CrlNo = case proplists:get_value("customfield_11205", Fields) of
		[Val] when is_binary(Val) -> list_to_integer(binary_to_list(Val));
		_  -> 0
	end,

	Activity = case ActivityBin of
		undefined -> undefined;
		ActivityBin -> binary_to_list(ActivityBin)
	end,

	Phase = ?PHASE_MAP(Activity),
	case log_decide(CrlNo) of
	 	{internal, CommonProcessId} ->
	 		{ok, get_params_common_procid(CommonProcessId, Phase)};
	 	{process_id, CrlNo} -> 
	 		{ok, get_params_by_crlno_as_common_procid(CrlNo)};
	 	{error, _} ->
	 		FixVersion = case proplists:get_value("fixVersions", Fields, []) of
	 		 	[] -> [];
	 		 	[{obj, Fv}|_] -> Fv
	 		end,
	 		FixVersionName = proplists:get_value("name", FixVersion),
	 		%DefaultCrlNo = get_common_crlno(FixVersionName, LoginId, Activity),
	 		case get_common_crlno(FixVersionName, LoginId, Activity) of
	 			{ok, DefaultCrlNo} -> 
	 				case log_decide(DefaultCrlNo) of
			 			{internal, CommonProcessId2}  ->
			 				{ok, get_params_common_procid(CommonProcessId2, Phase)};
			 			{process_id, DefaultCrlNo} -> 
					 		{ok, get_params_by_crlno_as_common_procid(DefaultCrlNo)};
					 	Error1 -> Error1
					 		%throw({termination, 400, [], "CRL# or CommonProcessId is not found"})
			 		end;
			 	Error2 -> Error2
	 		end
	end.


get_parent_issue(Key, Req) ->
	get_parent_issue(Key, Req, undefined).

get_parent_issue(Key, Req, Activity) ->
	Data =  [{"jql", list_to_binary("key='" ++ Key ++ "'")},
	{"fields",[<<"parent">>, <<"issuetype">>, <<"customfield_11205">>, <<"customfield_10401">>, <<"fixVersions">>]}],
	{200, _, Content} = ddrt_jira:search(Data, Req),
	{ok, {obj, ParentInfo}, _} = rfc4627:decode(Content),
	[{obj, Issue}] = proplists:get_value("issues", ParentInfo),
	{obj, Fields} = proplists:get_value("fields", Issue),
	{obj, TypeObj} = proplists:get_value("issuetype", Fields, []),
	NewActivity = case Activity of
			undefined -> 
				case proplists:get_value("customfield_10401", Fields) of
				 	undefined  -> undefined;
				 	{obj, ActivityList} -> proplists:get_value("value", ActivityList);
				 	_ -> undefined
				end; 
			Any -> Any
		end,
	case proplists:get_value("subtask", TypeObj, false) of
		false -> {Key, NewActivity, Fields};
		true  ->   
			{obj, ParentObj} = proplists:get_value("parent", Fields),
			ParentKey = ddrt_utils:get_value("key", ParentObj),
			get_parent_issue(ParentKey, Req, NewActivity)
	end.


get_params_common_procid(CommonProcessId, Phase) ->
	[{sql_integer,[5733]},
	{sql_integer,[105]},
	{{sql_varchar,50},[ <<"Developers Coding">>]},
	{sql_integer,[CommonProcessId]},
    {sql_integer,[ Phase ]},
    {sql_integer,[31]}].

get_params_by_crlno_as_common_procid(CrlNo) ->
	[{sql_integer,[2]},
	{sql_integer,[0]},
	{{sql_varchar,50},[<<"Key User">>]},
	{sql_integer,[CrlNo]},
    {sql_integer,[-1]},
    {sql_integer,[-1]}].


get_common_crlno(undefined, Author, Activity) ->
	get_common_crlno(Author, Activity);
get_common_crlno(FixVersion, Author, Activity) ->
	case ddrt_mssql_mgr:execute(?GET_COMMON_PROCESSID, [{{sql_wvarchar, 200}, [ddrt_utils:to_sql_wvarchar(FixVersion)]}]) of
		{selected, _, []} -> add_commonprocess_map(FixVersion, Author, Activity);
		{selected, _, [{CrlNo}]} -> {ok, ddrt_utils:to_integer(CrlNo)};
		Error -> {error, Error}

	end.

get_common_crlno(Author, undefined) -> 
	get_common_crlno(Author, "Internal"); %not_synchronized;
get_common_crlno(Author, Activity) ->
	case ddrt_mssql_mgr:execute(?GET_TEAM_BY_MEMBER(Author), []) of
		{selected, _, []}-> {error, "member ("++ ddrt_utils:binary_to_string(Author) ++") does't has a group"}; %"N/A";
		{selected, _, [{GroupName}|_]} -> get_activity_crlno_mapping_by_group(GroupName, string:to_upper(Activity));
		Error -> {error, Error}
	end.

%% add mapping
add_commonprocess_map(FixVersion, Author, Activity) ->
	case ddrt_mssql_mgr:execute(?ADD_COMMONPROCESS_MAP, [{{sql_char,20},[ddrt_utils:string_to_binary(Author)]}, 
		{{sql_wvarchar, 200}, [ddrt_utils:to_sql_wvarchar("Project For Jira:" ++ FixVersion)]},
		{{sql_wvarchar, 200}, [ddrt_utils:to_sql_wvarchar(FixVersion)]}])  of
		{updated, _} -> get_common_crlno(FixVersion, Author, Activity);
		Error -> {error, Error}
	end.



get_activity_crlno_mapping_by_group(GroupName, []) ->
	get_internal_key_crlno(GroupName);
get_activity_crlno_mapping_by_group(GroupName, "STUDY & TRAINING")->
	get_activity_crlno_mapping_by_group(GroupName, "StudyAndTraining");
get_activity_crlno_mapping_by_group(GroupName, Activity) ->
	case ddrt_mssql_mgr:execute(?GET_CRLNO_BY_GROUP, [{{sql_varchar, 40}, [GroupName]}, {{sql_varchar, 40}, [ddrt_utils:string_to_binary(Activity)]}]) of
		{selected, _, []} -> get_internal_key_crlno(GroupName);
		{selected, _, [{CrlNo}|_]} -> {ok, ddrt_utils:to_integer(CrlNo)};
		Error -> {error, Error}
	end.

get_internal_key_crlno(GroupName) ->
	case ddrt_mssql_mgr:execute(?GET_CRLNO_BY_GROUP, [{{sql_varchar, 40}, [GroupName]}, {{sql_varchar, 40}, [<<"Internal">>]}]) of
		{selected, _, []} -> {error,   "group (" ++ ddrt_utils:binary_to_string(GroupName) ++ ") does't has a internal crlno"}; %throw({termination, 400, [], "The activityMap does not contain internal key"});
		{selected, _, [{CrlNo}|_]} -> {ok, ddrt_utils:to_integer(CrlNo)};
		Error -> {error, Error}
	end.


%log_decide
log_decide(CrlNo) when CrlNo < 1 -> {error, "bad crlNo"};
log_decide(CrlNo) ->
	case ddrt_mssql_mgr:execute(?GET_PROCESSID_BY_INTERNAL, [{sql_integer, [CrlNo]}])  of
		{selected, _, [{CommonProcessId}|_]} -> {internal, CommonProcessId};
		{selected, _, []} -> 
			case ddrt_mssql_mgr:execute(?EXIST_COMMONPROCESS, [{sql_integer, [CrlNo]}])  of
				{selected, _, [{_Any}]} -> {process_id, CrlNo};
				Error -> {error, Error}
			end;
		Error2 -> {error, Error2}
	end.


%%% ===================================================================
%%% Tests  
%%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_dailyhour_test() ->
	ok = meck:new(ddrt_mssql_mgr, [non_strict]),
	ok = meck:expect(ddrt_mssql_mgr, execute_sync, fun(Sql, Params) -> 
		?assertEqual("UPDATE [DailyHours] SET [Date] = ?, [SpendTime] = ? ,[Activity] = ? WHERE [Activity] LIKE '%[AUTO#123]%' ESCAPE '['", Sql),
		?assert(proplists:is_defined({sql_varchar, 23}, Params)),
		?assert(proplists:is_defined({sql_decimal,15,2}, Params)),
		?assert(proplists:is_defined({sql_wvarchar,200}, Params)), ok
	 end),
	?assertEqual(ok, update_dailyhour("5.0", <<"test content">>, <<"123">>)),

	true = meck:validate(ddrt_mssql_mgr),
	ok = meck:unload(ddrt_mssql_mgr).

delete_dailyhour_test() ->
	ok = meck:new(ddrt_mssql_mgr, [non_strict]),
	ok = meck:expect(ddrt_mssql_mgr, execute_sync, fun(Sql, Params) -> 
		?assertEqual("DELETE FROM [DailyHours] WHERE [Activity] LIKE '%[AUTO#123]%' ESCAPE '['" , Sql),
		?assertEqual([], Params), ok
	 end),
	?assertEqual(ok, delete_dailyhour(<<"123">>)),

	true = meck:validate(ddrt_mssql_mgr),
	ok = meck:unload(ddrt_mssql_mgr).	

% add_dailyhour_test() ->
% 	DailyHour = [{"timeSpent", 3.0}, {"id", <<"12345">>}, {"comment", "test comment"}, {"key", "No_CrlNo"}],
% 	LoginId = "ab43",
% 	UserName = "test.user",
% 	% ok = meck:new(ddrt_crl, [passthrough]),
% 	% ok = meck:expect(ddrt_crl, get_params, fun
% 	% 	("No_CrlNo", _, _)  -> [];
% 	% 	(_, _, _) -> not_synchronized
% 	% end),

% 	ok = meck:expect(ddrt_jira, search, fun
% 		(_Any) -> search_result
% 	end),
% 	ok = meck:expect(rfc4627, decode, fun
% 		(_Any) when guard ->
% 			body
% 	end),
% 	?assertEqual(ok, add_dailyhour(DailyHour, LoginId, UserName, req)),

% 	ok = meck:expect(ddrt_mssql_mgr, execute_sync, fun(_Sql, _Params) ->  ok end),

% 	%%ddrt_jira:search(Data, Req)

% 	NewDailyHour = [{"timeSpent", 3.0}, {"id", <<"12345">>}, {"comment", "test comment"}, {"key", "CrlNo"}],
% 	?assertEqual(ok, add_dailyhour(NewDailyHour, LoginId, UserName, req)).


	%ok = meck:new()

-endif.