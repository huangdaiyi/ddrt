-module(ddrt_db).
-author("benjamin.c.yan@newegg.com").
-include ("include/ddrt.hrl").
-include ("include/ddrt_db.hrl").
-export ([start/0, stop/0]).
-export ([update/2, select/3]).

%%
% public method
%%
start() ->
    ok = init_db(),
    ok = init_prepare(),
    ok.

stop() ->
    application:stop(emysql).

%%
% private methods
%%
init_db() ->
    io:format("ok"),
    crypto:start(),
    application:start(emysql),
    io:format("ok"),
    io:format("ok ~p", [neg_hydra:get_env(mysql_pool)]),
    {ok,Config} = neg_hydra:get_env(mysql_pool),
    io:format("ok"),
    emysql:add_pool(mysql_pool, Config),
    ok.

init_prepare() ->
    io:format("ok"),
    [ok = emysql:prepare(K,V) || {K,V} <- ?DB_SCRIPT],
    ok.


get_record_info(groups) ->
    record_info(fields, groups);
get_record_info(users) ->
    record_info(fields, users).

update(Pre,Params) when is_atom(Pre),is_list(Params) ->
    case emysql:execute(mysql_pool,Pre,Params) of
        {ok_packet,_,_,_,_,_,_} -> ok;
        [{result_packet,_,_,[[<<"SQLEXECPTION">>]],_},_] ->
            error_logger:error_report(["execute update error",{pre,Pre},{params,Params},"SQLEXECPTION"]),
            faild;
        [{result_packet,_,_,[[<<"USAGEINSUFFICIENT">>]],_},_] ->
            more_than_total;
        [{result_packet,_,_,[[<<"FORBIDDEN">>]],_},_] ->
            exists;
        [{result_packet,_,_,[[<<"NOT_AUTH">>]],_},_] ->
            faild;
        [{result_packet,_,_,[[<<"EXISTS">>]],_},_] ->
            exists;
        [{result_packet,_,_,[[<<"NOTFOUND">>]],_},_] ->
            not_found;
        W ->
            error_logger:error_report(["execute update error",{pre,Pre},{params,Params},W]),
            faild
    end.

select(Pre,Record,Params) when is_atom(Pre),is_atom(Record),is_list(Params) ->
    case emysql:execute(mysql_pool,Pre,Params) of
        [Result,_] -> emysql:as_record(Result, Record, get_record_info(Record));
        [[],_] -> [];
        [] -> [];
        {ok_packet,_,_,_,_,_,[]} -> [];
        {result_packet,_,_,[],<<>>} -> [];
        Result -> emysql:as_record(Result, Record, get_record_info(Record))
    end.
