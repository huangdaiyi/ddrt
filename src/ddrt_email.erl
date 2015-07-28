-module(ddrt_email).
-include ("include/ddrt.hrl").

-export([send_mail/1, send_remind/1, send_mail_test/2]).

send_remind(#groups{id = Id, scheduling_name = Name}) ->
    Users = ddrt_db:get_not_report_emails(Id),
    {_, ReportSpan} = ddrt_db:get_scheduling(Name),
    {H, M} = lists:last(ReportSpan),
    SendTime = lists:flatten(io_lib:format("~2..0B:~2..0B", [H, M])),
    send_remind_2(Users, SendTime).

send_remind_2([], _) -> ok;
send_remind_2([#email_list{email = Email, user_name = User}| RestUsers],  SendTime) ->
    {ok, ReportUrl} = neg_hydra:get_env(report_address),
    {ok, TemplateName} = neg_hydra:get_env(remind_template),
    T = #template_opts{module = template_parser,
                       source = string:join(["template", TemplateName], "/"),
                       compile_opts = [],
                       render_vars =
                           [{title, <<"Daily Report Remind">>}, {user, User},
                            {report_url, list_to_binary(ReportUrl)},
                            {send_time, list_to_binary(SendTime)}]},
    Body = ddrt_runder:run(T),
    Subject = "(Info)Submit Daily Report Remind " ++
                ddrt_utils:get_str_today(),
    Cc = "",
    Mail = #mail{to = binary_to_list(Email), cc = Cc,
                 subject = Subject, body = Body},
    spawn(ddrt_mail, send_mail, [Mail]),
    send_remind_2(RestUsers, SendTime).

send_mail_test(ID, Name) ->
    send_mail(#groups{id = ID, group_name = Name}).

send_mail(#groups{id = ID, group_name = Name}) ->
    StrName = binary_to_list(Name),
    Now = calendar:local_time(),
    DayNum = get_report_num(),
    Reports = ddrt_db:get_all_reports(Now, DayNum, ID),
    GroupUsers = ddrt_db:get_group_users(ID),
    To = string:join([binary_to_list(Email)
                      || #group_user{email = Email, receive_type = Type}
                             <- GroupUsers,
                         string:to_lower(binary_to_list(Type)) == "to"],
                     ";"),
    Cc = string:join([binary_to_list(Email)
                      || #group_user{email = Email, receive_type = Type}
                             <- GroupUsers,
                         string:to_lower(binary_to_list(Type)) == "cc"],
                     ";"),
    Today = ddrt_utils:get_today_days(),
    ReportDays = get_report_days(Today, DayNum),
    NewReports = lists:flatten([fill_reports([R
                                              || #report_mode{email = Email} = R
                                                     <- Reports,
                                                 Email =:= U#group_user.email],
                                             U, ReportDays)
                                || U <- GroupUsers,
                                   U#group_user.report_type =:= <<"R">>]),
    Subject = "(Report)" ++
                StrName ++
                  " Daily Report " ++ ddrt_utils:get_str_today(),
    {ok, TemplateName} = neg_hydra:get_env(report_template),
    T = #template_opts{module = template_parser,
                       source = string:join(["template", TemplateName], "/"),
                       compile_opts =
                           [{record_info,
                             [{report_mode, record_info(fields, report_mode)}]}
                            | (#template_opts{})#template_opts.compile_opts],
                       render_vars =
                           [{reports, NewReports}, {group_name, Name},
                            {columns,
                             generate_colums(Today, DayNum, ["Team Member"])}]},
    Body = ddrt_runder:run(T),
    Mail = #mail{to = "Hardy.D.Huang@newegg.com", cc = "",
                 subject = Subject, body = Body},
    %Mail = #mail{to=To, cc=Cc, subject=Subject, body=Body},
    spawn(ddrt_mail, send_mail, [Mail]).

get_report_num() ->
    {ok, DayNum} = neg_hydra:get_env(report_day, 7), DayNum.

generate_colums(Today, Num, Acct) ->
    generate_colums(Today, 1, Num, Acct).

generate_colums(_Today, DayNo, Num, Acct) when DayNo > Num ->
    lists:reverse(Acct);
generate_colums(Today, DayNo, Num, Acct)when DayNo =:= 1 ->
    generate_colums(Today - 1, DayNo + 1, Num, [list_to_binary(string:concat("Today<br/>", ddrt_utils:days_to_str_date(Today))) | Acct]);
generate_colums(Today, DayNo, Num, Acct) when DayNo =:= 2 ->
    generate_colums(Today - 1, DayNo + 1, Num, [list_to_binary(string:concat("Yesterday<br/>", ddrt_utils:days_to_str_date(Today))) | Acct]);
generate_colums(Today, DayNo, Num, Acct) ->
    generate_colums(Today - 1, DayNo + 1, Num,[list_to_binary(string:concat(lists:flatten(io_lib:format("D~p<br/>", [DayNo])),ddrt_utils:days_to_str_date(Today))) | Acct]).

get_report_days(Today, Num) when Num > 0 ->
    get_report_days(Today, Num, []).

get_report_days(_Day, 0, Acct) -> Acct;
get_report_days(Day, Num, Acct) ->
    get_report_days(Day - 1, Num - 1, [Day | Acct]).

flatten_reports_by_day(Reports) ->
  lists:foldl(fun(#report_mode{date = Date} = R, Acct) ->
      Key = ddrt_utils:get_days(Date),
      dict:update(ddrt_utils:get_days(Date), 
        fun(V) -> V#report_mode{content = V#report_mode.content ++ "<br />" ++ "<b>"++ R#report_mode.issue ++<"/b> <br />" ++ R#report_mode.content} end,
        R#report_mode{content = "<b>" ++ R#report_mode.issue ++<"/b> <br />" ++ R#report_mode.content}, Acct)
  end, dict:new(), Reports).

fill_reports(Reports, User, Days) ->
    FlattenReports = flatten_reports_by_day(Reports),
    NewReports = lists:foldl(fun (Day, Acct) ->
                                            case dict:find(Day, FlattenReports) of
                                              {ok, V} -> [V | Acct];
                                              error ->
                                                  [#report_mode{user_id =
                                                                    User#group_user.user_id,
                                                                user_name =
                                                                    User#group_user.user_name,
                                                                email =
                                                                    User#group_user.email,
                                                                content =
                                                                    <<"">>,
                                                                date =
                                                                    ddrt_utils:days_to_date(Day),
                                                                receive_type =
                                                                    User#group_user.receive_type,
                                                                domain_name =
                                                                    User#group_user.domain_name}
                                                   | Acct]
                                            end
                                    end,
                                    [], Days),

    lists:sort(fun (#report_mode{date = Date1},
                    #report_mode{date = Date2}) ->
                       ddrt_utils:get_days(Date1) > ddrt_utils:get_days(Date2)
               end,
               NewReports).

% fill_reports(Reports, User, Days) ->
%     DictReport = flatten_reports_by_day(Reports),
%     NewReports = if dict:size(DictReport) =:= length(Days) ->
%                         Reports;
%                     true ->
%                         lists:foldl(fun (Day, Acct) ->
%                                             case lists:any(fun
%                                                              (#report_mode{date = Date}) ->
%                                                                  ddrt_utils:get_days(Date) =:= Day;
%                                                              (_Any) -> false
%                                                            end,
%                                                            Reports)
%                                                 of
%                                               true -> Acct;
%                                               false ->
%                                                   [#report_mode{user_id =
%                                                                     User#group_user.user_id,
%                                                                 user_name =
%                                                                     User#group_user.user_name,
%                                                                 email =
%                                                                     User#group_user.email,
%                                                                 content =
%                                                                     <<"">>,
%                                                                 date =
%                                                                     ddrt_utils:days_to_date(Day),
%                                                                 receive_type =
%                                                                     User#group_user.receive_type,
%                                                                 domain_name =
%                                                                     User#group_user.domain_name}
%                                                    | Acct]
%                                             end
%                                     end,
%                                     [], Days)
%                           ++ Reports
%                  end,
%     lists:sort(fun (#report_mode{date = Date1},
%                     #report_mode{date = Date2}) ->
%                        ddrt_utils:get_days(Date1) > ddrt_utils:get_days(Date2)
%                end,
%                NewReports).
