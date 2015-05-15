-module (ddrt_handler).
-author("benjamin.c.yan@newegg.com").
-export([request/4,responsed/2]).
-include ("include/ddrt.hrl").

%%%================================================
%%% request callback
%%%================================================
request(get, Paths, DocRoot, Req) ->
    do_get([string:to_lower(P) || P <- Paths], DocRoot, Req);

request(post, Paths, DocRoot, Req) ->
    do_post([string:to_lower(P) || P <- Paths], DocRoot, Req);

request(put, Paths, DocRoot, Req) ->
    do_put([string:to_lower(P) || P <- Paths], DocRoot, Req);
 
request(head, Paths, DocRoot, Req) ->
    do_head([string:to_lower(P) || P <- Paths], DocRoot, Req).
    
responsed(_Code, _Req) ->
    ok.


%%%================================================
%%% get request
%%%================================================
do_get(["api", "v1", "users"|_], _DocRoot, _Req) ->
    Result = ddrt_db:select(get_users,userentity,[]),
    Json = ddrt_utils:user_format(Result),
    {200, [], list_to_binary(rfc4627:encode(Json))};

do_get(["api", "v1", "user", UserID], _DocRoot, _Req) ->
    Result = ddrt_db:select(get_user,userentity,[UserID]),
    Json = ddrt_utils:user_format(Result),
    {200, [], list_to_binary(rfc4627:encode(Json))};


do_get(["api", _V, "reports", GroupID, Date], _DocRoot, _Req) ->
    Result = ddrt_db:get_report(list_to_binary(Date), <<"7">>, list_to_binary(GroupID)),
    Body = ddrt_utils:build_report_body(Result),
    {200, [{"Content-Type","JSON"}], rfc4627:encode(Body)};

do_get(["api", "v1", "db", "refresh"], _DocRoot, _Req) ->
    case erlang:whereis(ddrt_sup) of
        undefined -> pass;
        Pid -> erlang:exit(Pid, kill)
    end,
    {200, [{"Content-Type", "text/plain"}], ""};
do_get(_Any, _DocRoot, _Req) ->
    {404, [], <<>>}.
        

%%%================================================
%%% post request
%%%================================================
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

        UserID ->
            case ddrt_db:check_today_report(UserID) of 
                [] -> 
                    Content = ddrt_utils:format_data_line(proplists:get_value("content", Data, "")),
                    Datetime = proplists:get_value("datetime", Data, calendar:local_time()),
                    case ddrt_db:add_report([UserID, Content, Datetime]) of
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

do_put(_, _DocRoot, _Req) ->
    {404, [], <<>> }.


%%%================================================
%%% head request
%%%================================================
do_head(_Any, _DocRoot, _Req) ->
    {404, [], <<>>}.
