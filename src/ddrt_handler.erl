-module (ddrt_handler).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).

request(get, Paths, _DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1", "groups"|_] ->
            ddrt_db:update(add_group, [<<"testing group">>]),
            {200, [], <<"rest full api">>};
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
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
        ["api", _V, "report"] ->
            Data = Req:parse_post(),
            GroupName = proplists:get_value("name", Data, ""),
            Template = proplists:get_value("template", Data, ""),
            case ddrt_db:add_report(add_report, [GroupName, Template]) of
                 ok ->
                    responsed(Req, 200, [], <<"Success">>);
                _ ->
                    responsed(Req, 500, [], <<"Failed">>)
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



generate_cors_headers() ->
    [ {"Access-Control-Allow-Origin", "*"},
    {"Accept-Ranges", "bytes"},
    {"Access-Control-Allow-Headers", "Range, Content-Type, Accept, Accept-Encoding, Accept-Language, User-Agent, authorization"},
    {"Access-Control-Expose-Headers", "Range"},
    {"Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT"}].

responsed(_Code, _Req) ->
    ok.

responsed(Req, Code, Headers, Body) ->
  Req:respond({Code, generate_cors_headers() ++ Headers, Body}).
