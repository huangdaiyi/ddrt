-module(ddrt_email).
-include ("include/ddrt.hrl").

-export([send_mail/1, send_remind/1]).

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
                           [{user, User},
                            {report_url, list_to_binary(ReportUrl)},
                            {send_time, list_to_binary(SendTime)}]},
    %{title, <<"Daily Report Remind">>},
    Body = ddrt_runder:run(T),
    Subject = "(Info)Submit Daily Report Remind " ++ ddrt_utils:get_str_today(),
    Cc = "",
    Mail = #mail{to = binary_to_list(Email), cc = Cc,
                  subject = Subject, body = Body},
    %Mail = #mail{to = "Hardy.D.Huang@newegg.com", cc = "",
    %             subject = Subject, body = Body},
    spawn(ddrt_mail, send_mail, [Mail]),
    send_remind_2(RestUsers, SendTime).

% send_mail_test(ID, Name) ->
%     send_mail(#groups{id = ID, group_name = Name}).

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
    Subject = "(Report)" ++ StrName ++ " Daily Report " ++ ddrt_utils:get_str_today(),
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
    % Mail = #mail{to = "Hardy.D.Huang@newegg.com", cc = "",
    %             subject = Subject, body = Body},
    Mail = #mail{to=To, cc=Cc, subject=Subject, body=Body},
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
      dict:update(ddrt_utils:get_days(Date), 
        fun(V) -> V#report_mode{content = << (list_to_binary(ddrt_utils:format_data_line(V#report_mode.content)))/bits, <<"<br />">>/bits, <<"<a class='issue' href='">>/bits, (list_to_binary(ddrt_utils:get_jira_url()))/bits, <<"/browse/">>/bits, (R#report_mode.issue)/bits,  << "'>" >>/bits, (R#report_mode.issue)/bits, <<"</a> <br />">>/bits, (list_to_binary(ddrt_utils:format_data_line(R#report_mode.content)))/bits >>} end,
        R#report_mode{content = << <<"<a class='issue' href='">>/bits, (list_to_binary(ddrt_utils:get_jira_url()))/bits, <<"/browse/">>/bits, (R#report_mode.issue)/bits,  << "'>" >>/bits, (R#report_mode.issue)/bits, <<"</a> <br />">>/bits, (list_to_binary(ddrt_utils:format_data_line(R#report_mode.content)))/bits >>}, Acct)
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
                                                                    User#group_user.domain_name,
                                                                domain_id =  
                                                                    User#group_user.domain_id}
                                                   | Acct]
                                            end
                                    end,
                                    [], Days),

    lists:sort(fun (#report_mode{date = Date1},
                    #report_mode{date = Date2}) ->
                       ddrt_utils:get_days(Date1) > ddrt_utils:get_days(Date2)
               end,
               NewReports).

%%% ===================================================================
%%% Tests  
%%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
send_mail_test() ->
  Group = #groups{id = 1, group_name = <<"test group name">>},
  ok = meck:new(ddrt_db, [non_strict]),
  ok = meck:expect(ddrt_db, get_all_reports, fun
    %{worklog_id, user_id, user_name, email, content, date, group_name, template, receive_type, domain_name, domain_id, time_spent ,issue}).
    (_, _, _)  -> 
      [#report_mode{user_id=1, email = <<"test1@email">>, date={datetime, {{2015,8,20},{12,0,0}}}, group_name = <<"test group name">>, receive_type = <<"cc">>, issue = <<"issue1">>},
       #report_mode{user_id=1, email = <<"test1@email">>, date={datetime, {{2015,8,21},{12,0,0}}}, group_name = <<"test group name">>, receive_type = <<"cc">>, issue = <<"issue1">>},
       #report_mode{user_id=1, email = <<"test1@email">>, date={datetime, {{2015,8,22},{12,0,0}}}, group_name = <<"test group name">>, receive_type = <<"cc">>, issue = <<"issue1">>},
       #report_mode{user_id=1, email = <<"test1@email">>, date={datetime, {{2015,8,22},{12,0,0}}}, group_name = <<"test group name">>, receive_type = <<"cc">>, issue = <<"issue2">>},
    
       #report_mode{user_id=2, email = <<"test2@email">>, date={datetime, {{2015,8,21},{12,0,0}}}, group_name = <<"test group name">>, receive_type = <<"cc">>, issue = <<"issue">>},
       #report_mode{user_id=2, email = <<"test2@email">>, date={datetime, {{2015,8,22},{12,0,0}}}, group_name = <<"test group name">>, receive_type = <<"cc">>, issue = <<"issue">>},
      
       #report_mode{user_id=3, email = <<"test3@email">>, date={datetime, {{2015,8,21},{12,0,0}}}, group_name = <<"test group name">>, receive_type = <<"cc">>, issue = <<"issue">>}]
      end),
  ok = meck:expect(ddrt_db, get_group_users, fun
    (_) -> %(group_user, {user_id, email, user_name, domain_name, report_type, receive_type}).
        [
        #group_user{user_id=1, email = <<"test1@email">>,  user_name = <<"test1">>, receive_type = <<"cc">>, report_type = <<"R">>},
        #group_user{user_id=2, email = <<"test2@email">>,  user_name = <<"test2">>, receive_type = <<"cc">>, report_type = <<"R">>},
        #group_user{user_id=3, email = <<"test3@email">>,  user_name = <<"test3">>, receive_type = <<"cc">>, report_type = <<"R">>},
        #group_user{user_id=4, email = <<"test4@email">>,  user_name = <<"test4">>, receive_type = <<"cc">>, report_type = <<"R">>},
        #group_user{user_id=4, email = <<"test5@email">>,  user_name = <<"test5">>, receive_type = <<"to">>, report_type = <<"NR">>}
        ]
  end),


  ok = meck:new(neg_hydra, [non_strict]),

  ok = meck:expect(neg_hydra, get_env, fun
    (report_template) -> {ok, "test_template"}
  end),

  ok = meck:expect(neg_hydra, get_env, fun
    (report_day, _) -> {ok, 3}
  end),


  %%736195 736196, 736197
  ok = meck:new(ddrt_utils, [non_strict]),
  ok = meck:expect(ddrt_utils, get_today_days, fun () -> 736197 end),
  ok = meck:expect(ddrt_utils, get_str_today, fun() -> "2015-8-22" end),
  ok = meck:expect(ddrt_utils, days_to_str_date, fun
    (DayNums) ->
      io:format("error: ~p", [DayNums]),
      {Y, M, D} = calendar:gregorian_days_to_date(DayNums),
      string:join([integer_to_list(Y), integer_to_list(M), integer_to_list(D)], "-")
    end),

  ok = meck:expect(ddrt_utils, get_days, fun
      ({Y, M, D}) -> calendar:date_to_gregorian_days(Y, M, D);
      ({datetime, {{Y, M, D}, _}}) -> calendar:date_to_gregorian_days(Y, M, D)
    end),

  ok = meck:expect(ddrt_utils, days_to_date, fun(DayNums) -> calendar:gregorian_days_to_date(DayNums) end),

  ok = meck:expect(ddrt_utils, format_data_line, fun(_) -> "test data" end),
  ok = meck:expect(ddrt_utils, get_jira_url, fun() -> "http://jira" end),


  ok = meck:new(ddrt_runder, [non_strict]),
  ok = meck:expect(ddrt_runder, run, fun(_) -> <<"test body">> end),

  ok = meck:new(ddrt_mail, [non_strict]),
  ok = meck:expect(ddrt_mail, send_mail, fun(#mail{to = To, cc = Cc, subject = Subject, body = Body})  -> 
    ?assertEqual("test5@email", To),
    ?assertEqual("test1@email;test2@email;test3@email;test4@email", Cc),
    ?assertEqual(<<"test body">>, Body),
    ?assert(is_list(Subject)),
    true
   end),

  ?assert(is_pid(send_mail(Group))),

  ok = meck:unload(ddrt_db),
  ok = meck:unload(neg_hydra),
  ok = meck:unload(ddrt_utils),
  ok = meck:unload(ddrt_runder),
  ok = meck:unload(ddrt_mail).


send_remind_test() ->
  Group = #groups{id = 1, scheduling_name = <<"test">>},
  %(email_list, {email, user_name}).
  ok = meck:new(ddrt_db, [non_strict]),
  ok = meck:expect(ddrt_db, get_not_report_emails, fun(1) -> 
    [#email_list{email= <<"test@email1.com">>, user_name = <<"test_user1">>}, 
     #email_list{email= <<"test@email2.com">>, user_name = <<"test_user2">>}, 
     #email_list{email= <<"test@email3.com">>, user_name = <<"test_user3">>},
     #email_list{email= <<"test@email4.com">>, user_name = <<"test_user4">>}] 
  end),

  %ddrt_db:get_scheduling(Name)
  ok = meck:expect(ddrt_db, get_scheduling, fun(<<"test">>)  -> {[], [{9, 30}]} end),

  ok = meck:new(neg_hydra, [non_strict]),

  ok = meck:expect(neg_hydra, get_env, fun
    (report_address) -> {ok, "http://www.test.com"};
    (remind_template) -> {ok, "test_remind_template"}
  end),

  ok = meck:new(ddrt_runder, [non_strict]),
  ok = meck:expect(ddrt_runder, run, fun(_) -> <<"test remind">> end),

  ok = meck:new(ddrt_utils, [non_strict]),
  ok = meck:expect(ddrt_utils, get_str_today, fun() -> "2015-8-22" end),

  ok = meck:new(ddrt_mail, [non_strict]),
  ok = meck:expect(ddrt_mail, send_mail, fun
    (#mail{to = "test@email1.com", cc = Cc, subject = Subject, body = Body}) -> 
        ?assertEqual("", Cc),
        ?assertEqual(<<"test remind">>, Body),
        ?assert(is_list(Subject)),true;
    (#mail{to = "test@email2.com", cc = Cc, subject = Subject, body = Body}) ->
        ?assertEqual("", Cc),
        ?assertEqual(<<"test remind">>, Body),
        ?assert(is_list(Subject)),true;  
    (#mail{to = "test@email3.com", cc = Cc, subject = Subject, body = Body}) ->
        ?assertEqual("", Cc),
        ?assertEqual(<<"test remind">>, Body),
        ?assert(is_list(Subject)),true;  
    (#mail{to = "test@email4.com", cc = Cc, subject = Subject, body = Body}) ->
        ?assertEqual("", Cc),
        ?assertEqual(<<"test remind">>, Body),
        ?assert(is_list(Subject)),true
   end),

  ?assertEqual(ok, send_remind(Group)),

  true = meck:validate(ddrt_db),
  true = meck:validate(neg_hydra),
  true = meck:validate(ddrt_utils),
  true = meck:validate(ddrt_runder),
  true = meck:validate(ddrt_mail),

  ok = meck:unload(ddrt_db),
  ok = meck:unload(neg_hydra),
  ok = meck:unload(ddrt_utils),
  ok = meck:unload(ddrt_runder),
  ok = meck:unload(ddrt_mail).

-endif.