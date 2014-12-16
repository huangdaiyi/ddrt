-module (ddrt_handler).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).

request(get, Paths, DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1", "groups"|_] ->
            ddrt_db:update(add_group, [<<"testing group">>]),
            {200, [], <<"rest full api">>};
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end.

responsed(_Code, _Req) ->
    ok.
