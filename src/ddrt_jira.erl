-module (ddrt_jira).
-define (JIRA_AUTH_URL,  string:concat(ddrt_utils:get_jira_url(), "/rest/auth/1/session")).
-define (JIRA_API(Resource), lists:flatten(io_lib:format("~s/rest/api/2/~s", [ ddrt_utils:get_jira_url(), proplists:get_value(Resource, [{project,"project"}, {user,"user"}, {search, "search"},{status,"status"}]) ]))).
-define (JIRA_LOG_URL(IssueId), lists:flatten(io_lib:format("~s/rest/api/2/issue/~s/worklog", [ddrt_utils:get_jira_url(),IssueId]))).
-define (JIRA_EDIT_URL(IssueId, LogId), lists:flatten(io_lib:format("~s/rest/api/2/issue/~s/worklog/~s", [ddrt_utils:get_jira_url(),IssueId, LogId]))).

-define (JIRA_TOKEN, "atlassian.xsrf.token").
-define (JIRA_SESSION, "JSESSIONID").
%%-define (JIRA_WORKLOG, "http://jira/secure/CreateWorklog.jspa").
-export ([login/3, login_info/1, login_out/1, project/1]).
-export ([user_info/2, parse_cookie/1, search/2, get_all_status/1, worklog/2, delete_log/3, edit_log/2]).


login(Username, Password, _Req) ->
	Data = rfc4627:encode({obj, [{username, list_to_binary(Username)}, {password, list_to_binary(Password)}] }),  
    Headers = [{"content-type", "application/json"}],                            
			   %{"authorization", base64:encode_to_string(string:join([Username,Password], ":"))}],
	{ok, StatusCode, ResposneHeaders, Content} = ddrt_utils:send_http(post, ?JIRA_AUTH_URL, Headers, list_to_binary(Data)),
    %%httpc:store_cookies(ResposneHeaders, ?JIRA_API("project")),
    {StatusCode, ResposneHeaders, Content}.

login_info(Req) ->
    Headers = [jira_cookie(Req)],
    {ok, StatusCode, _, Content} = ddrt_utils:send_http(get, ?JIRA_AUTH_URL, Headers, <<>>),
    {StatusCode, [], Content}.

login_out(Req) ->
    Headers = [jira_cookie(Req)],
    {ok, StatusCode, _, _} = ddrt_utils:send_http(delete, ?JIRA_AUTH_URL, Headers, <<>>),
    {StatusCode, [], <<>>}.

user_info(Username, AuthHeaders) when is_list(AuthHeaders) ->
    Url = string:join([?JIRA_API(user), "?username=", Username], ""),
    {ok, StatusCode, _, Content} = ddrt_utils:send_http(get, Url, AuthHeaders, <<>>),
    {StatusCode, [], Content};
user_info(Username, Req) ->
    Headers = [jira_cookie(Req)],
    user_info(Username, Headers).

get_all_status(Req) ->
    Headers = [jira_cookie(Req)],
    {ok, StatusCode, _, Content} = ddrt_utils:send_http(get, ?JIRA_API(status), Headers, <<>>),
    {StatusCode, [], Content}.

search(Data, Req) ->
    Body = rfc4627:encode({obj, Data}),
    Headers = [{"content-type", "application/json"}, jira_cookie(Req)],
    {ok, StatusCode, ResposneHeaders, Content} = ddrt_utils:send_http(post, ?JIRA_API(search), Headers, ddrt_utils:string_to_binary(Body)),
    {StatusCode, ResposneHeaders, Content}.

worklog(Report, Req) ->
    Headers = [{"content-type", "application/json"}, jira_cookie(Req)],
    Started = ddrt_utils:time_to_utc_string(erlang:now()),
    worklog_2(Report, list_to_binary(Started), Headers).

edit_log(Report, Req) ->
    Headers = [{"content-type", "application/json"}, jira_cookie(Req)],
    Started = ddrt_utils:time_to_utc_string(erlang:now()),
    edit_log_2(Report, list_to_binary(Started), Headers).

delete_log(IssueId, LogId, Req) ->
    Headers = [jira_cookie(Req)],
    delete_log_2(IssueId, LogId, Headers).



project(Req) ->
    Headers = [{"content-type", "application/json"}, jira_cookie(Req)],
    {ok, StatusCode, ResposneHeaders, Content} = ddrt_utils:send_http(get, ?JIRA_API(project), Headers, <<>>),
    {StatusCode, ResposneHeaders, Content}.


worklog_2({obj, R}, _Started, AuthHeaders) ->
    worklog_2(R, _Started, AuthHeaders);
worklog_2(R, _Started, AuthHeaders) ->
    IssueId = proplists:get_value("key", R),
    Url = ?JIRA_LOG_URL(IssueId),
    Body = rfc4627:encode({obj, proplists:delete("key", R)}),
    {ok, StatusCode, ResposneHeaders, Content} = ddrt_utils:send_http(post, Url, AuthHeaders, ddrt_utils:string_to_binary(Body)),
    {StatusCode, ResposneHeaders, Content}.


edit_log_2({obj, R},  _Started, AuthHeaders) -> 
    edit_log_2(R, _Started, AuthHeaders);
edit_log_2(R, _Started, AuthHeaders) ->
    IssueId = proplists:get_value("key", R),
    LogId = proplists:get_value("id", R),
    Url = ?JIRA_EDIT_URL(IssueId, LogId),
    Body = rfc4627:encode({obj, proplists:delete("key", R)}),
    {ok, StatusCode, ResposneHeaders, Content} = ddrt_utils:send_http(put, Url, AuthHeaders, ddrt_utils:string_to_binary(Body)),
    {StatusCode, ResposneHeaders, Content}.

delete_log_2(IssueId, LogId, AuthHeaders) -> 
    Url = ?JIRA_EDIT_URL(IssueId, LogId),
    {ok, StatusCode, _, _} = ddrt_utils:send_http(delete, Url, AuthHeaders, <<>>),
    {StatusCode, [], <<>>}.


parse_cookie(Headers) ->
    {"cookie", string:join(proplists:get_all_values("set-cookie",Headers), ";")}.

jira_cookie(Req) ->
    jira_cookie(Req:call(get_cookie_value, ?JIRA_TOKEN), Req:call(get_cookie_value, ?JIRA_SESSION)).
jira_cookie(Token, Session) ->
    {"cookie", lists:flatten(io_lib:format("atlassian.xsrf.token=~s; Path=/;JSESSIONID=~s; Path=/; HttpOnly",[Token, Session]))}.