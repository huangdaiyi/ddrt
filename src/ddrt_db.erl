-module(ddrt_db).
-author("benjamin.c.yan@newegg.com").
-include ("include/ddrt.hrl").
-include ("include/ddrt_db.hrl").
-export ([start/0, stop/0]).
-export ([update/2, select/3, add_report/1, add_group/1,
         get_not_report_emails/2, get_report/3, check_today_report/1, 
         get_all_reports/3, get_send_users/1, get_groups/0, get_dommember/1,get_notdommember/1]).



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
    crypto:start(),
    application:start(emysql),
    {ok,Config} = neg_hydra:get_env(mysql_pool),
    emysql:add_pool(mysql_pool, Config),
    ok.

init_prepare() ->
    [ok = emysql:prepare(K,V) || {K,V} <- ?DB_SCRIPT],
    ok.

get_record_info(groups) ->
    record_info(fields, groups);

get_record_info(userentity) ->
    record_info(fields, userentity);

get_record_info(id) ->
    record_info(fields, id);
get_record_info(users) ->
    record_info(fields, users);
get_record_info(email_list) ->
    record_info(fields, email_list);
get_record_info(report_mode) ->
    record_info(fields, report_mode);
get_record_info(send_user) ->
    record_info(fields, send_user).



update(Pre,Params) when is_atom(Pre),is_list(Params) ->
    case emysql:execute(mysql_pool, Pre, Params) of
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


add_report(Params) ->
    update(add_report, Params).

add_group(Params) ->
    update(add_group, Params).

    
check_today_report(UserID) ->
    BinDay = ddrt_utils:datetime_format(calendar:local_time()),
    Result = select(check_today_report, id, [UserID, BinDay, BinDay]),
    Result.

get_send_users(GroupID) ->
    select(get_send_users, send_user, [GroupID]).

get_dommember(Params)->%Params::[GroupId,DomainName]
    select(get_dommeber,member,Params).

get_notdommember(Params)->
    select(get_notdommeber,member,Params).%Param::[GroupId]


%-spec get_report(Date :: datetime(), DayNum :: integer(), GroupID:: integer()) -> any().
get_report(Date, DayNum, GroupID) ->
    BinDay = ddrt_utils:datetime_format(Date),
    Params = [BinDay, DayNum, BinDay, GroupID],
    Result = select(get_report, report_mode, Params),
    Result.

get_all_reports(Date, DayNum, GroupID) ->
    BinDay = ddrt_utils:datetime_format(Date),
    Params = [BinDay, DayNum, BinDay, GroupID],
    Result = select(get_all_reports, report_mode, Params),
    Result.


get_groups() ->
    select(get_groups, groups, []).
    
%-spec get_not_report_emails(Date :: datetime(), DayNum :: integer()) -> any().
get_not_report_emails(Date, DayNum) ->
    BinDay = ddrt_utils:datetime_format(Date),
    Params = [BinDay, BinDay, DayNum],
    Reuslt = select(get_not_report_emails, email_list, Params),
    Reuslt.

select(Pre,Record,Params) when is_atom(Pre),is_atom(Record),is_list(Params) ->
    case emysql:execute(mysql_pool,Pre,Params) of
        [Result,_] -> emysql:as_record(Result, Record, get_record_info(Record));
        [[],_] -> [];
        [] -> [];
        {ok_packet,_,_,_,_,_,[]} -> [];
        {result_packet,_,_,[],<<>>} -> [];
        Result -> emysql:as_record(Result, Record, get_record_info(Record))
    end.


