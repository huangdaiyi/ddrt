-module (ddrt_handler).
-compile([debug_info]).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).
-include ("include/ddrt.hrl").


% get the users
request(get, Paths, _DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1", "users"|_] ->
            % io:format("~p~n", ddrt_db:select(get_users,usercontainer, [])),
            % emysql:execute(mysql_pool,get_users,[]),
            Result = emysql:execute(mysql_pool,get_users,[]),
            io:format("~p~n", [Result]),
            User = emysql:as_record(Result, usercontainer, record_info(fields, usercontainer)),
            io:format("~p~n", [User]),
            {200, [], <<"ok,users list">>};
        ["api", _V, "reports", GroupID, Date] ->
            case ddrt_db:get_report(list_to_binary(Date), <<"7">>, list_to_binary(GroupID)) of
                [] ->
                    {200, [], <<>>};
                Result ->
                    Body = ddrt_utils:build_report_body(Result),
                    {200, [], rfc4627:encode(Body)}
            end;
        ["rest", "api", "v1"|_] -> {200, [], <<"not match">>};
        _ -> {404, [], <<>>}
    end;


request(post, Paths, _DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end;


request(put, Paths, _DocRoot, Req) ->
    io:format("~nput request...~n"),
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["api", _V, "group"] ->
            {obj, Data} = Req:json_body(),
            GroupName = proplists:get_value("name", Data, ""),
            Template = proplists:get_value("template", Data, ""),
            case ddrt_db:add_group([GroupName, Template]) of
                ok ->
                    {200, [], <<"Success">>};
                _ ->
                    {500, [], <<"Failed">>}
             end;
        ["api", _V, "report"] ->
            {obj, Data} = Req:json_body(),
            UserID = proplists:get_value("userid", Data), 
            Content = proplists:get_value("content", Data),
            Datetime = proplists:get_value("datetime", Data),
            case ddrt_db:add_report([UserID, Content, Datetime]) of
                ok ->
                     {200, [], <<"Success">>};
                _ ->
                     {500, [], <<"Failed">>}
            end;
        ["api", "v1"|_] -> {200, [], <<"rest full api">>};

        _ -> {404, [], <<>>}
    end;
request(head, Paths, _DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end.



responsed(_Code, _Req) ->
    ok.





