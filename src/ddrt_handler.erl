-module (ddrt_handler).
-compile([debug_info]).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).


% get the users
request(get, Paths, DocRoot, _Req) ->
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
        ["rest", "api", "v1"|_] -> {200, [], <<"not match">>};
        _ -> {404, [], <<>>}
    end;
    
request(post, Paths, DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end;
request(put, Paths, DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end;
request(head, Paths, DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end.

responsed(_Code, _Req) ->
    ok.
