-module (ddrt_handler).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).

% get the users
request(get, Paths, DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1", "users"|_] ->
            ddrt_db:select(get_users,usercontainer, []),
            {200, [], <<"ok,users list">>};
        ["rest", "api", "v1"|_] -> {200, [], <<"not match">>};
        _ -> {404, [], <<>>}
    end;
    
% get the user
request(get, Paths, DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1", "user"|_] ->
            ddrt_db:update(get_user, []),
            {200, [], <<"this is the user">>};
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end;

request(get, Paths, DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1", "groups"|_] ->
            ddrt_db:update(add_group, [<<"testing group">>]),
            {200, [], <<"rest full api">>};
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
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
