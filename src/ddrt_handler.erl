-module (ddrt_handler).
-compile([debug_info]).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).
-include ("include/ddrt.hrl").


% get all users or a special user
request(get, Paths, _DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["api", "v1", "users"|_] ->%get all user
            Result = ddrt_db:select(get_users,userentity,[]),
            Json = lists:map(fun(#userentity{dname = Dname, email = Email,type=Type,receive_type=Receivetype,gname=GroupName,template=Template}) -> 
                {obj, [{email,Email},{groupname,GroupName},{domainname, Dname},{type,Type},{receivetype,Receivetype},{template,Template}]}
            end, Result),
            {200, [], list_to_binary(rfc4627:encode(Json))};
        ["api", "v1", "user",UserID] -> % get a user
            Result = ddrt_db:select(get_user,userentity,[UserID]),
            Json = lists:map(fun(#userentity{dname = Dname, email = Email,type=Type,receive_type=Receivetype,gname=GroupName,template=Template}) -> 
                 {obj, [{email,Email},{groupname,GroupName},{domainname, Dname},{type,Type},{receivetype,Receivetype},{template,Template}]}
            end, Result),
            {200, [], list_to_binary(rfc4627:encode(Json))};
       
        ["api", _V, "reports", GroupID, Date] ->
            case ddrt_db:get_report(list_to_binary(Date), <<"7">>, list_to_binary(GroupID)) of
                [] ->
                    {200, [], <<>>};
                Result ->
                    Body = ddrt_utils:build_report_body(Result),
                    {200, [], rfc4627:encode(Body)}
            end;

        ["api", "v1"|_] -> {200, [], <<"not match">>};
        _ -> {404, [], <<>>}
    end;



request(post, Paths, _DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["api", "v1"|_] -> {200, [], <<"rest full api">>};
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
        ["api", "v1","adduser"] ->
            {obj,Data} = Req:json_body(),
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

        ["api", "v1"|_] ->
            {404, [], <<>>}
    end;

request(head, Paths, _DocRoot, _Req) ->
    SafePaths = [string:to_lower(P) || P <- Paths],
    case SafePaths of
        ["rest", "api", "v1"|_] -> {200, [], <<"rest full api">>};
        _ -> {404, [], <<>>}
    end.


responsed(_Code, _Req) ->
    ok.





