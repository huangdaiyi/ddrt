-module (ddrt_handler).
-compile([debug_info]).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).
-include ("include/ddrt.hrl").


% get all users or a special user
request(get, Paths, _DocRoot,Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of   
        ["rest", "api", "v1", "users"|_] ->%get all user
            Result = ddrt_db:select(get_users,userentity,[]),
            Json = lists:map(fun(#userentity{dname = Dname, email = Email,type=Type,receive_type=Receivetype,gname=GroupName,template=Template}) -> 
                {obj, [{domainname, Dname},{email,Email},{type,Type},{receivetype,Receivetype},{groupname,GroupName},{template,Template}]}
            end, Result),
            {200, [], list_to_binary(rfc4627:encode(Json))};
        ["rest", "api", "v1", "user",UserID] -> % get a user
            Result = ddrt_db:select(get_user,userentity,[UserID]),
            Json = lists:map(fun(#userentity{dname = Dname, email = Email,type=Type,receive_type=Receivetype,gname=GroupName,template=Template}) -> 
                {obj, [{domainname, Dname},{email,Email},{type,Type},{receivetype,Receivetype},{groupname,GroupName},{template,Template}]}
            end, Result),
            {200, [], list_to_binary(rfc4627:encode(Json))};
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
