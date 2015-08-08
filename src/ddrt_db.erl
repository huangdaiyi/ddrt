-module(ddrt_db).
-author("benjamin.c.yan@newegg.com").
-include ("include/ddrt.hrl").
-include ("include/ddrt_db.hrl").
-export ([start/0, stop/0]).
-export([update/2, select/3, add_report/6, add_group/1, get_report/3, check_today_report/1, get_all_reports/3,
         get_group_users/1, get_groups/0, get_dommember/1,get_notdommember/1]).

-export([get_not_report_emails/1,get_not_report_emails/2, get_group_report_user/1]).
-export([get_scheduling/1, check_today_report_by_email/1, get_user_by_email/1, get_user_report/3]).
-export ([update_report/5, delete_report/2, create_history_issue/2, get_prev_issues/1]).


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

get_record_info(groups) -> record_info(fields, groups);
get_record_info(userentity) ->
    record_info(fields, userentity);
get_record_info(id) -> record_info(fields, id);
get_record_info(users) -> record_info(fields, users);
get_record_info(email_list) ->
    record_info(fields, email_list);
get_record_info(report_mode) ->
    record_info(fields, report_mode);
get_record_info(group_user) ->
    record_info(fields, group_user);
get_record_info(group_report_user) ->
    record_info(fields, group_report_user);
get_record_info(scheduling) ->
    record_info(fields, scheduling);
get_record_info(history_issue) ->
    record_info(fields, history_issue).

update(Pre, Params)
    when is_atom(Pre), is_list(Params) ->
    case emysql:execute(mysql_pool, Pre, Params) of
        {ok_packet, _, _, _, _, _, _} -> ok;
        [{result_packet, _, _, [[<<"SQLEXECPTION">>]], _}, _] ->
            error_logger:error_report(["execute update error",{pre, Pre}, {params, Params}, "SQLEXECPTION"]),
            faild;
        [{result_packet, _, _, [[<<"USAGEINSUFFICIENT">>]], _}, _] ->
            more_than_total;
        [{result_packet, _, _, [[<<"FORBIDDEN">>]], _}, _] ->
            exists;
        [{result_packet, _, _, [[<<"NOT_AUTH">>]], _}, _] ->
            faild;
        [{result_packet, _, _, [[<<"EXISTS">>]], _}, _] ->
            exists;
        [{result_packet, _, _, [[<<"NOTFOUND">>]], _}, _] ->
            not_found;
        W ->
            error_logger:error_report(["execute update error", {pre, Pre}, {params, Params}, W]),
            faild
    end.

select(Pre, Record, Params)
    when is_atom(Pre), is_atom(Record), is_list(Params) ->
    case emysql:execute(mysql_pool, Pre, Params) of
        [Result, _] -> emysql:as_record(Result, Record, get_record_info(Record));
        [[], _] -> [];
        [] -> [];
        {ok_packet, _, _, _, _, _, []} -> [];
        {result_packet, _, _, [], <<>>} -> [];
    Result ->
          emysql:as_record(Result, Record, get_record_info(Record))
    end.

add_report(UserId, Content, Datetime, TimeSpent, Issue, WorklogId) -> update(add_report, [UserId, Content, Datetime,TimeSpent, Issue, WorklogId]).

update_report(Content, Date, TimeSpent, WorklogId, UserId) ->
    update(update_report, [Content, Date, TimeSpent, WorklogId, UserId]).

delete_report([], _UserId) -> ok;
delete_report(WorklogId, UserId) when is_number(WorklogId) ->
    update(delete_report, [WorklogId, UserId]);

delete_report([F | R], UserId) ->
    update(delete_report, [ddrt_utils:string_to_binary(F), UserId]),
    delete_report(R, UserId).


create_history_issue(Issue, UserId) -> 
    update(create_history, [Issue, UserId, ddrt_utils:get_today_days()]).

get_prev_issues(UserId) -> 
    select(get_prev_issues, history_issue, [UserId, UserId]).


add_group(Params) -> update(add_group, Params).

check_today_report(UserId) ->
    BinDay =
        ddrt_utils:datetime_format(calendar:local_time()),
    select(check_today_report, id,
           [UserId, BinDay, BinDay]).

check_today_report_by_email(Email) ->
    BinDay =
        ddrt_utils:datetime_format(calendar:local_time()),
    select(check_today_report_by_email, id,
           [Email, BinDay, BinDay]).

get_group_report_user(GroupID) ->
    select(get_group_report_user, group_report_user,
           [GroupID]).

get_group_users(GroupID) ->
    select(get_group_user, group_user, [GroupID]).

get_dommember(Params) ->
    select(get_dommeber, member, Params).

get_notdommember(Params) ->
    select(get_notdommeber, member, Params).

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

get_user_report(Date, DayNum, UserId) ->
    BinDay = ddrt_utils:datetime_format(Date),
    Params = [BinDay, BinDay, DayNum, UserId],
    Result = select(get_user_report, report_mode, Params),
    Result.


get_user_by_email(Email) ->
    select(get_user_by_email, userentity, [Email]).

get_groups() -> select(get_groups, groups, []).

get_not_report_emails(Date, DayNum) ->
    BinDay = ddrt_utils:datetime_format(Date),
    Params = [BinDay, BinDay, DayNum],
    Reuslt = select(get_not_report_emails, email_list,
                    Params),
    Reuslt.

get_not_report_emails(GroupId) ->
    BinDay =
        ddrt_utils:datetime_format(calendar:local_time()),
    Params = [GroupId, BinDay, BinDay, 1],
    select(get_not_report_emails, email_list, Params).

get_scheduling(Name) ->
    Schedulings = select(get_scheduling, scheduling,
                         [Name]),
    {RemindSpan, ReportSpan} = lists:partition(fun
                                                 (#scheduling{type =
                                                                  <<"remind_time">>}) ->
                                                     true;
                                                 (_) -> false
                                               end,
                                               Schedulings),
    {extract_time(RemindSpan), extract_time(ReportSpan)}.


%% %%%%%%%%%%%%
%% private methods
%% %%%%%%%%%%%%

extract_time(Schedulings) ->
    [{H, M} || #scheduling{scheduling_time={time, {H, M, _}}} <- Schedulings].



