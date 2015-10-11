-module (ddrt_handler).
-author("hardy.d.huang@newegg.com").
-export([request/4,responsed/2]).
-include("include/ddrt.hrl").
    
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
        {post, [_, _, _, "login"]} -> ok;
        {Method, _} when Method =:= post; Method =:= put; Method =:= delete ->
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
    end.


%%%================================================
%%% get request
%%%================================================
do_get(["logs", Dir, Log], _DocRoot, Req) ->
    Req:call(serve_file, Log, filename:join("logs", Dir)),
    {200, [], <<>>};

do_get(["logs" | Name], _DocRoot, _Req) ->
    Headers = [{"Content-Type", "text/html"} ],
    Html = <<"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
    "<html><head>"
    "<title>DDRT Logs</title>"
    "</head><body>"
    "<h3>DDRT Logs</h3>">>,
    LogDir = filename:join("logs", Name),
    {ok, Files} = file:list_dir(LogDir),
    FileList = lists:foldr(fun (F, Acct) ->
        <<Acct/binary, 
        <<"<li><a href=\"/">>/binary, 
        (list_to_binary(filename:join(LogDir, F)))/binary,
        <<"\">">>/binary, 
        (list_to_binary(F))/binary, 
        <<"</a></li>">>/binary >> end,
        <<"<p><ul>">>, Files),
    {200, Headers, << Html/binary, FileList/binary, <<"</ul></p></body></html>\n">>/binary >>};

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
    Result = ddrt_db:get_user_report(list_to_binary(Date), list_to_binary(DayNum), list_to_binary(UserId)),
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
            ddrt_crl:add_dailyhour([{"id", WorklogId} | R], LoginId, Username, UserId, Req),
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
                ddrt_db:delete_report([WorklogId], UserId),
                [Item] = create_reports([{obj, proplists:delete("id", R)}], UserId, LoginId, Username, Datetime, Req), Item;
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


%%% ===================================================================
%%% Tests  
%%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

responsed_test() ->
   ?assertEqual(ok, responsed(200, "req")).

request_test() ->
%Method, Paths, DocRoot, Req
    ok = meck:new(ddrt_jira, [non_strict]),
    ok = meck:expect(ddrt_jira, login_info, fun 
        (bad_req) ->  {400, [], <<>>};
        (req) -> {200, [], <<>>} 
    end),

    ?assertEqual({404, [], <<>>}, request(get, [], "www", req)),  
    ?assertEqual({404, [], <<>>}, request(head, [], "www", req)), 

    ?assertEqual({404, [], <<>>}, request(post, [], "www", req)),    
    ?assertEqual({401,  [], <<>>},  request(post, [], "www", bad_req)), 

    ?assertEqual({404, [], <<>>}, request(put, [], "www", req)),    
    ?assertEqual({401,  [], <<>>},  request(put, [], "www", bad_req)), 

    ?assertEqual({404, [], <<>>}, request(delete, [], "www", req)),    
    ?assertEqual({401,  [], <<>>},  request(delete, [], "www", bad_req)),

    ?assertEqual({404,  [], <<>>},  request(other, [], "www", bad_req)),

    true = meck:validate(ddrt_jira),
    ok = meck:unload(ddrt_jira).

validate_test() ->
    ok = meck:new(ddrt_jira, [non_strict]),
    ok = meck:expect(ddrt_jira, login_info, fun 
        (bad_req) ->  {400, [], <<>>};
        (req) -> {200, [], <<>>} 
    end),

    ?assertEqual(ok, validate(post,["api", "v1", "jira", "login"], "www", req)),
    ?assertEqual(ok, validate(get, [], "www", req)),
    ?assertEqual(ok, validate(head, [], "www", req)),

    ?assertEqual(<<>>, validate(post, [], "www", req)),
    ?assertEqual(<<>>, validate(put, [], "www", req)),
    ?assertEqual(<<>>, validate(delete, [], "www", req)),

    ?assertThrow({termination, 401,  [], <<>>},  validate(post, [], "www", bad_req)), 
    ?assertThrow({termination, 401,  [], <<>>},  validate(put, [], "www", bad_req)), 
    ?assertThrow({termination, 401,  [], <<>>},  validate(delete, [], "www", bad_req)), 

    true = meck:validate(ddrt_jira),
    ok = meck:unload(ddrt_jira).

check_login_jira_test() ->
    ok = meck:new(ddrt_jira, [non_strict]),
    ok = meck:expect(ddrt_jira, login_info, fun 
        (bad_req) ->  {400, [], <<>>};
        (req) -> {200, [], <<"logininfo">>} 
    end),

    ?assertEqual(<<"logininfo">>,  check_login_jira(req)), 
    ?assertThrow({termination, 401,  [], <<>>},  check_login_jira(bad_req)),

    true = meck:validate(ddrt_jira),
    ok = meck:unload(ddrt_jira).

do_get_users_test() ->
    %api/v1/users
    ok = meck:new(ddrt_db, [non_strict]),
    ok = meck:expect(ddrt_db, select, fun(get_users,userentity, _) -> [#userentity{id = "1"}] end),

    ok = meck:new(rfc4627, [non_strict]),
    ok = meck:expect(rfc4627, encode, fun(_Any) -> "[json_obj]" end),

    ?assertMatch({200, [], _}, do_get(["api", "v1", "users"], "www", req)),
    
    true = meck:validate(ddrt_db),
    true = meck:validate(rfc4627),

    ok = meck:unload(ddrt_db),
    ok = meck:unload(rfc4627).

do_get_user_test() ->
    %"api/v1/user/{UserId}
    ok = meck:new(ddrt_db, [non_strict]),
    ok = meck:expect(ddrt_db, select, fun(get_user,userentity, [Id]) -> 
        ?assertEqual("1", Id),
        [#userentity{id = Id}] end),

    ok = meck:new(rfc4627, [non_strict]),
    ok = meck:expect(rfc4627, encode, fun(_Any) -> "json_obj" end),

    ?assertMatch({200, [], _}, do_get(["api", "v1", "user", "1"], "www", req)),

    true = meck:validate(ddrt_db),
    true = meck:validate(rfc4627),

    ok = meck:unload(ddrt_db),
    ok = meck:unload(rfc4627).

do_get_reports_test() ->
    %"api/v1/reports/{GroupID}/{Date[options]}" ["api", "v1", "reports", GroupID, Date
    ok = meck:new(ddrt_db, [non_strict]),
    ok = meck:expect(ddrt_db, get_report, fun(Date,DayNum, GroupId) -> 
        ?assertEqual(<<"2015-8-25">>, Date),
        ?assertEqual(<<"7">>, DayNum),
        ?assertEqual(<<"1">>, GroupId),
        [#report_mode{worklog_id = "123"}] end),

    ok = meck:new(rfc4627, [non_strict]),
    ok = meck:expect(rfc4627, encode, fun(_Any) -> "[json_obj]" end),

    ?assertMatch({200, [{"Content-Type","JSON"}], _}, do_get(["api", "v1", "reports", "1", "2015-8-25"], "www", req)),

    true = meck:validate(ddrt_db),
    true = meck:validate(rfc4627),

    ok = meck:unload(ddrt_db),
    ok = meck:unload(rfc4627).


do_get_report_user_test() ->
    %"api/v1/report/user/{UserId}/{Date[option]}/{DayNum}
    ok = meck:new(ddrt_db, [non_strict]),
    ok = meck:expect(ddrt_db, get_user_report, fun
        (<<"2015-8-24">>,DayNum, UserId) -> 
            ?assertEqual(<<"7">>, DayNum),
            ?assertEqual(<<"1">>, UserId),
            [#report_mode{}];
        (Date, DayNum, UserId) ->
            ?assertEqual(list_to_binary(ddrt_utils:get_str_today()), Date),
            ?assertEqual(<<"7">>, DayNum),
            ?assertEqual(<<"1">>, UserId),
            [#report_mode{}]
    end),

    ok = meck:new(rfc4627, [non_strict]),
    ok = meck:expect(rfc4627, encode, fun(_Any) -> "[json_obj]" end),
    %["api", "v1", "report", "user", UserId, Date, DayNum]
    ?assertMatch({200, [{"Content-Type","JSON"}], _}, do_get(["api", "v1", "report", "user", "1", "2015-8-24", "7"], "www", req)),
    ?assertMatch({200, [{"Content-Type","JSON"}], _}, do_get(["api", "v1", "report", "user", "1", "7"], "www", req)),

    true = meck:validate(ddrt_db),
    true = meck:validate(rfc4627),

    ok = meck:unload(ddrt_db),
    ok = meck:unload(rfc4627).

do_get_issue_prev_test() ->
    ok = meck:new(ddrt_db, [non_strict]),
    ok = meck:expect(ddrt_db, get_prev_issues, fun
            (UserId) -> 
                ?assertEqual(<<"1">>, UserId),
                [#history_issue{}]
    end),

    ok = meck:new(rfc4627, [non_strict]),
    ok = meck:expect(rfc4627, encode, fun(_Any) -> "[json_obj]" end),

    ?assertMatch({200, [{"Content-Type","JSON"}], _}, do_get(["api", "v1", "issue", "prev", "1"], "www", req)),

    true = meck:validate(ddrt_db),
    true = meck:validate(rfc4627),

    ok = meck:unload(ddrt_db),
    ok = meck:unload(rfc4627).

do_get_refresh__test() ->
    %"api/v1/db/refresh"
    ?assertMatch({200, [{"Content-Type", "text/plain"}], ""}, do_get(["api", "v1", "db", "refresh"],  "www", req)),
    spawn(fun()  -> register(ddrt_sup, self()), timer:sleep(3000) end),
    ?assertMatch({200, [{"Content-Type", "text/plain"}], ""}, do_get(["api", "v1", "db", "refresh"],  "www", req)).


do_get_jira_project_test() ->
    ok = meck:new(ddrt_jira, [non_strict]),
    ok = meck:expect(ddrt_jira, project, fun
            (req) -> {200, [], <<>>}
    end),
    ?assertEqual({200, [], <<>>}, do_get(["api", "v1", "jira", "project"],  "www", req)),

    true = meck:validate(ddrt_jira),
    ok = meck:unload(ddrt_jira).

do_get_jira_login_info_test() ->
    ok = meck:new(ddrt_jira, [non_strict]),
    ok = meck:expect(ddrt_jira, login_info, fun
            (req) -> {200, [], <<"user_login_info">>}
    end),
    ?assertMatch({200, [], <<"user_login_info">>}, do_get(["api", "v1", "jira", "login"],  "www", req)),

    true = meck:validate(ddrt_jira),
    ok = meck:unload(ddrt_jira).

do_get_jira_user_info_test() ->
    %"api/v1/jira/user
    ok = meck:new(ddrt_jira, [non_strict]),
    ok = meck:expect(ddrt_jira, login_info, fun
            (req) -> {200, [], <<"[{\"name\", \"testuser\"}]">>}
    end),
   

    ok = meck:expect(ddrt_jira, user_info, fun
        ("testuser", req) -> {200, [], <<"[{\"emailAddress\", \"test\"}]">>}
    end),

    ok = meck:expect(ddrt_db, get_user_by_email, fun
        (<<"test">>)  -> [#userentity{id = <<"1">>, group_name = "test group name", domain_name = "test domain name"}]
    end),

    ok = meck:new(rfc4627, [non_strict]),
    ok = meck:expect(rfc4627,decode, fun
        (<<"[{\"name\", \"testuser\"}]">>) ->   {ok, {obj, [{"name", <<"testuser">>}]}, []};
        (<<"[{\"emailAddress\", \"test\"}]">>) -> {ok, {obj, [{"emailAddress", <<"test">>}]}, []}
    end),

    ok = meck:expect(rfc4627, encode, fun
        (_Any) -> "user_info_json_string"
    end),

    ?assertMatch({200, [], _}, do_get(["api", "v1", "jira", "user"],  "www", req)),

    true = meck:validate(ddrt_db),
    true = meck:validate(rfc4627),
    true = meck:validate(ddrt_jira),

    ok = meck:unload(ddrt_db),
    ok = meck:unload(rfc4627),
    ok = meck:unload(ddrt_jira).

do_get_jira_jira_status_test() ->
    ok = meck:new(ddrt_jira, [non_strict]),
    ok = meck:expect(ddrt_jira, get_all_status, fun
            (req) -> {200, [], <<"issue_status">>}
    end),
    ?assertEqual({200, [], <<"issue_status">>}, do_get(["api", "v1", "jira", "status"],  "www", req)),

    true = meck:validate(ddrt_jira),
    ok = meck:unload(ddrt_jira).

% do_post_login_test() ->
%     %"api"/v1/jira/login"
%     ok = meck:new()

-endif.