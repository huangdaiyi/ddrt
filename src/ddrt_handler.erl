-module (ddrt_handler).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).
-include("include/ddrt.hrl").

-define(LOGIN_PAGE, "/login.html").

%%%================================================
%%% request callback
%%%================================================
% request(get, Paths, DocRoot, Req) ->
%     do_get([string:to_lower(P) || P <- Paths], DocRoot, Req);

% request(post, Paths, DocRoot, Req) ->
%     do_post([string:to_lower(P) || P <- Paths], DocRoot, Req);

% request(put, Paths, DocRoot, Req) ->
%     do_put([string:to_lower(P) || P <- Paths], DocRoot, Req);

% request(delete, Paths, DocRoot, Req) ->
%     do_delete(Paths, DocRoot, Req);

% request(head, Paths, DocRoot, Req) ->
%     do_head([string:to_lower(P) || P <- Paths], DocRoot, Req).
    
responsed(_Code, _Req) ->
    ok.

request(Method, Paths, DocRoot, Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    try 
        validate(Method, SafePaths, DocRoot, Req),
        case Method of
            get    -> do_get   (SafePaths, DocRoot, Req);
            post   -> do_post  (SafePaths, DocRoot, Req);
            put    -> do_put   (SafePaths, DocRoot, Req);
            delete -> do_delete(SafePaths, DocRoot, Req);
            head   -> do_head  (SafePaths, DocRoot, Req);
            _Other -> {404, [], <<>>}

        end
    catch
        throw:{termination, StatusCode, Headers, Body} ->
            {StatusCode, Headers, ddrt_utils:string_to_binary(Body)}
    end.
    

validate(Method, SafePaths, _DocRoot, Req) ->
    case {Method, SafePaths} of
        {get, P} when P =:=[]; P =:= ["index.html"] ->
            check_login_jira(Req);
        _Any ->
            ok
    end.

check_login_jira(Req) ->
    case ddrt_jira:login_info(Req) of
        {200, _, Content} ->
             Content;
        _ ->
            throw({termination, 401,  [], <<>>})
            %throw({termination, 302,  [{"Location", Page},{"Content-Type", "text/html; charset=UTF-8"}], <<>>})
    end.


%%%================================================
%%% get request
%%%================================================
do_get(["api", "v1", "users"|_], _DocRoot, _Req) ->
    Result = ddrt_db:select(get_users,userentity,[]),
    Json = ddrt_utils:user_format(Result),
    {200, [], list_to_binary(rfc4627:encode(Json))};

do_get(["api", "v1", "user", UserId], _DocRoot, _Req) ->
    Result = ddrt_db:select(get_user,userentity,[UserId]),
    Json = ddrt_utils:user_format(Result),
    {200, [], list_to_binary(rfc4627:encode(Json))};


do_get(["api", "v1", "reports", GroupID, Date], _DocRoot, _Req) ->
    Result = ddrt_db:get_report(list_to_binary(Date), <<"7">>, list_to_binary(GroupID)),
    Body = ddrt_utils:build_report_body(Result),
    {200, [{"Content-Type","JSON"}], rfc4627:encode(Body)};

do_get(["api", "v1", "report", "user", UserId, DayNum], _DocRoot, _Req)->
    do_get(["api", "v1", "report", "user", UserId, ddrt_utils:get_str_today(), DayNum], _DocRoot, _Req);
    
do_get(["api", "v1", "report", "user", UserId, Date, DayNum], _DocRoot, _Req) ->
    Result = ddrt_db:get_user_report(list_to_binary(Date), DayNum, list_to_binary(UserId)),
    Body = ddrt_utils:build_report_body(Result),
    {200, [{"Content-Type","JSON"}], rfc4627:encode(Body)};

do_get(["api", "v1", "issue", "prev", UserId], _DocRoot, _Req) ->
    BinUser = list_to_binary(UserId),
    Result = ddrt_db:get_prev_issues(BinUser),
    Body = [ {obj, [{"user_id", BinUser}, {"issue",Issue} ]} || #history_issue{issue = Issue} <- Result],
    {200, [{"Content-Type","JSON"}], rfc4627:encode(Body)};


do_get(["api", "v1", "db", "refresh"], _DocRoot, _Req) ->
    case erlang:whereis(ddrt_sup) of
        undefined -> pass;
        Pid -> erlang:exit(Pid, kill)
    end,
    {200, [{"Content-Type", "text/plain"}], ""};

do_get(["api", "v1", "jira", "project"], _DocRoot, Req) ->
    ddrt_jira:project(Req);

do_get(["api", "v1", "jira", "login"], _DocRoot, Req) ->
    ddrt_jira:login_info(Req);

do_get(["api", "v1", "jira", "user"], _DocRoot, Req) ->
    {ok, {obj, Data}, _} = rfc4627:decode(check_login_jira(Req)),
    Username = proplists:get_value("name", Data),
    {200, _, UserInfo} = ddrt_jira:user_info(binary_to_list(Username), Req),
    {200, [], make_user_reponse(UserInfo)};

do_get(["api", "v1", "jira", "status"], _DocRoot, Req) ->
    ddrt_jira:get_all_status(Req);

do_get(_Any, _DocRoot, _Req) ->
    {404, [], <<>>}.
        

%%%================================================
%%% post request
%%%================================================
do_post(["api", "v1", "jira", "login"], _DocRoot, Req) ->
    %{obj, Data} = Req:json_body(),
    Data = Req:call(parse_post),
    Username = proplists:get_value("username", Data),
    Password = proplists:get_value("password", Data),
    {StatusCode, Headers, Content} = ddrt_jira:login(Username, Password, Req),
    case {StatusCode, proplists:get_value("info", Data)} of
        {200, Info} when Info =:= "true"; Info =:= true ->
            {200, _, UserInfo} = ddrt_jira:user_info(Username, [ddrt_jira:parse_cookie(Headers)]),
            {StatusCode, Headers, make_user_reponse(UserInfo)};
        _ ->
            {StatusCode, Headers, Content}
    end;

do_post(["api", "v1", "jira", "search"], _DocRoot, Req) ->
    {obj, Data} = Req:json_body(),
    Params1 = [{jql, proplists:get_value("jql", Data)}, {startAt, proplists:get_value("startAt", Data, 0)},
        {maxResults, proplists:get_value("maxResults", Data, 500)}],
    Params2 = case proplists:get_value("fields", Data) of
        undefined -> Params1;
        F -> [{fields, F} | Params1]
    end,
    ddrt_jira:search(Params2, Req);

do_post(["api", "v1", "jira", "worklog"], _DocRoot, Req) ->
    {obj, Data} = Req:json_body(),
    %%[{Data, _}] = Req:call(parse_post),
    UserId =  ddrt_utils:get_value("userId", Data),
    LoginId = ddrt_utils:get_value("loginId", Data),
    Username = proplists:get_value("username", Data, ""), 
    CreateReports = proplists:get_value("create", Data, []),
    UpdateReports = proplists:get_value("update", Data, []),
    DeleteReprots = proplists:get_value("delete", Data, []),
    Datetime = proplists:get_value("datetime", Data, calendar:local_time()),

    ReprotCreate = create_reports(CreateReports, UserId, LoginId, Username, Datetime, Req),
    ReprotUpdate = update_reports(UpdateReports, UserId, LoginId, Username, Datetime, Req),
    ok = delete_reports(DeleteReprots, UserId, Req),

    {200, [{"Content-Type","JSON"}], rfc4627:encode(ReprotCreate ++ ReprotUpdate)};
    %{200, [], <<"Success">>};


    
do_post(_Any, _DocRoot, _Req) ->
    {404, [], <<>>}.



%%%================================================
%%% put request
%%%================================================
do_put(["api", _V, "group"], _DocRoot, Req) ->
    {obj, Data} = Req:json_body(),
    GroupName = proplists:get_value("name", Data, ""),
    Template = proplists:get_value("template", Data, ""),
    case ddrt_db:add_group([GroupName, Template]) of
        ok ->
            {200, [], <<"Success">>};
        _ ->
            {500, [], <<"Failed">>}
    end;

do_put(["api", _V, "report"], _DocRoot, Req) ->
    %{obj, Data} = Req:json_body(),
    Data = Req:call(parse_post),
    
    case proplists:get_value("userid", Data) of
        undefined ->
            {200, [], <<"userid can not be empty">>};

        UserId ->
            case ddrt_db:check_today_report(UserId) of 
                [] -> 
                    Content = ddrt_utils:format_data_line(proplists:get_value("comment", Data, "")),
                    Datetime = proplists:get_value("datetime", Data, calendar:local_time()),
                    case ddrt_db:add_report(UserId, Content, Datetime, "unnokown") of
                        ok ->
                             {200, [], <<"Success">>};
                        _ ->
                             {500, [], <<"Failed">>}
                    end;
                _Any ->
                    {200, [], <<"You have already submitted">>}
            end
    end;

do_put(["api", "v1","adduser"], _DocRoot, Req) ->
    {obj, Data} = Req:json_body(),
    Email = proplists:get_value("email", Data, ""),
    Type = proplists:get_value("type", Data, ""),
    EmailStr=binary_to_list(Email),
    TypeStr=binary_to_list(Type),
    case ddrt_db:update(add_user, [EmailStr,TypeStr]) of
         ok ->
            {200, [], <<"Success">>};
        _ ->
            {500, [], <<"Failed">>}
     end;

do_put(_, _DocRoot, _Req) ->
    {404, [], <<>> }.



%%%================================================
%%% put request
%%%================================================
do_delete(["api", "v1", "jira", "login"], _DocRoot, Req) ->
    ddrt_jira:login_out(Req);
do_delete(_, _DocRoot, _Req) ->
    {404, [], <<>> }.
    



%%%================================================
%%% head request
%%%================================================

do_head(_Any, _DocRoot, _Req) ->
    {404, [], <<>>}.


%%%================================================
%%% private method
%%%================================================

get_user_by_email(Email)->
    case ddrt_db:get_user_by_email(Email) of
        [] -> throw({termination, 403,  [], <<"user not exist">>});
        [User] -> User
    end.

make_user_reponse(JiraUserInfo) ->
    {ok, {obj, JiraUser}, _} = rfc4627:decode(JiraUserInfo),
    DdrtUser = get_user_by_email(proplists:get_value("emailAddress", JiraUser)),
    rfc4627:encode({obj, [{"userId",DdrtUser#userentity.id},
             {"groupName",DdrtUser#userentity.group_name},
             {"domainName",DdrtUser#userentity.domain_name} | JiraUser]}).


create_reports(CreateReports, UserId, LoginId, Username, Datetime, Req) ->
    [begin 
            {ok, {obj, Worklog}, _} = case ddrt_jira:worklog(R, Req) of
                {201, _, LogInfo}  ->  rfc4627:decode(LogInfo);
                {StatusCode, Headers, Message} -> throw({termination, StatusCode, Headers, Message})
            end,
            WorklogId = proplists:get_value("id", Worklog),
            Content = proplists:get_value("comment", R, ""),
            TimeSpent = proplists:get_value("timeSpent", R, 8),
            Key = ddrt_utils:get_orignal_value("key", R),
            case ddrt_db:add_report(UserId, Content, Datetime,TimeSpent, Key, WorklogId) of
                ok ->  ddrt_db:create_history_issue(Key, UserId);
                 _ ->  throw({termination, 500, [], <<"Failed">>})
            end,
            ddrt_crl:add_dailyhour([{"id", WorklogId} | R], LoginId, Username, Req),
            {obj, [{"issue", Key}, {"id", WorklogId}]}
    end || {obj, R} <- CreateReports].



update_reports(UpdateReports, UserId, LoginId, Username, Datetime, Req) ->
    [ begin 

        WorklogId = ddrt_utils:get_orignal_value("id", R),
        Key = ddrt_utils:get_orignal_value("key", R),
        case ddrt_jira:edit_log(R, Req) of
            {200, _, _} ->
                Content = proplists:get_value("comment", R, ""),
                TimeSpent = proplists:get_value("timeSpent", R, 8),
                case ddrt_db:update_report(Content, Datetime,TimeSpent, WorklogId, UserId) of
                    ok -> 
                        ddrt_crl:update_dailyhour(TimeSpent, Content, WorklogId),
                        {obj, [{"issue", Key}, {"id", WorklogId}]};
                     _ -> throw({termination, 500, [], <<"Failed">>})
                end; 
            {404, _, _} -> 
                [Item] = create_reports([{obj, proplists:delete("id", R)}], UserId, LoginId, Username, Datetime, Req),
                ddrt_db:delete_report([WorklogId], UserId), Item;
            {StatusCode, Headers, Message} -> throw({termination, StatusCode, Headers, Message})
        end
    end || {obj, R} <- UpdateReports].


delete_reports(DeleteReprots, UserId, Req) ->
    WorklogIds =  [ begin 
            IssueId = ddrt_utils:get_value("key", R),
            LogId = ddrt_utils:get_value("id", R),
            case ddrt_jira:delete_log(IssueId, LogId, Req) of
                {StatusCode, _, _} when StatusCode =:=204; StatusCode =:= 404 ->
                    ddrt_crl:delete_dailyhour(LogId),
                    LogId;
                {StatusCode, _, _} -> throw({termination, StatusCode, [],  
                    list_to_binary(lists:flatten(io_lib:format("delete failed, issue ~s, worklog id ~s", [IssueId, LogId])))})
            end
        end || {obj, R} <- DeleteReprots],
    ddrt_db:delete_report(WorklogIds, UserId), ok.