-module(ddrt_email).
-include ("include/ddrt.hrl").

-define (REPORTDAYS, 7).
-export([send_mail/1, send_remind/1]).


send_remind(#groups{id=Id, scheduling_name=Name}) ->
    Users = ddrt_db:get_not_report_emails(Id),
    {_, ReportSpan} = ddrt_db:get_scheduling(Name),
    {H, M} = lists:last(ReportSpan),
    SendTime = lists:flatten(io_lib:format("~2..0B:~2..0B", [H, M])),
    send_remind_2(Users, SendTime).

send_remind_2([], _) -> ok;
send_remind_2([#email_list{email=Email}|RestUsers], SendTime) ->
    {ok, ReportUrl} = neg_hydra:get_env(report_address),

    User = binary_to_list(Email),
    Body = "<!DOCTYPE HTML>
            <html>
            <head>
            <title>Daily Report Remind</title>
            </head>
            <body style=\"color:#0099CC\">
                <p>
                   Dear " ++ string:substr(User, 1, string:str(User,"@")-1) ++",<br/><br/>
                   <Strong>Please <a href=\"" ++ ReportUrl ++ "\">submit</a> daily reports in a timely manner(Before "++
                    SendTime ++ ").</strong>
                </p>
                <p style=\"font-style: italic;\">
                   Cheers,<br/>
                   DDRT
                </p
            </body>
            </html>",

    Subject = "(Info)Submit Daily Report Remind " ++ ddrt_utils:get_str_today(),
    Cc = "",
    Mail = #mail{to=User, cc=Cc, subject=Subject, body=Body},
    spawn(ddrt_mail, send_mail, [Mail]),
    send_remind_2(RestUsers, SendTime).

send_mail(#groups{id=ID, group_name=Name}) ->
    %email, content, date, group_name, template, receive_type, domain_name

    StrName = binary_to_list(Name),
    Now = calendar:local_time(),
    Reports = ddrt_db:get_all_reports(Now, ?REPORTDAYS, ID),
    SendUsers = ddrt_db:get_send_users(ID),

    To = string:join([ binary_to_list(Email) || #send_user{email=Email, receive_type=Type} <- SendUsers,
        string:to_lower(binary_to_list(Type)) == "to"], ";"),
    Cc = string:join([ binary_to_list(Email) || #send_user{email=Email, receive_type=Type} <- SendUsers,
        string:to_lower(binary_to_list(Type)) == "cc"], ";"),
    
    Subject = "(Report)" ++ StrName ++ " Daily Report " ++ ddrt_utils:get_str_today(),
    Today = ddrt_utils:get_today_days(),
    Body = "<!DOCTYPE HTML><html><head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
<title>DFIS Daily Report</title>
    <style type=\"text/css\">
       *{ margin:0; padding: 0; }
       body { font-family: Calibri}
       h1 { font-size: 14pt; font-weight: bold; margin: 10px; text-align: center; color:#0099CC;}
       .data { margin: 0px 10px 10px; }
       table, th, td { border: 1px solid #D4E0EE; border-collapse: collapse; color: #333; 10pt;}
       th { background: #33B5E5; color:#FFF; }
       td, th { padding: 3px 7px 3px 7px; }
       tbody tr { background: #FCFDFE; }
       td { word-break:break-all; word-wrap:break-word;}
       td.center { text-align: center; width:10%}
       td.left {text-align:left; background: #CCFFFF; }
    </style>
</head>
<body>
    <h1>"++ StrName ++" Daily Report</h1>
    <div class=\"data\">
        <table style=\"width: 100%\">
            <thead>
                <tr>
                    <th>Team Member</th>
                    <th>Today<br/>" ++ ddrt_utils:days_to_str_date(Today) ++ "</th>
                    <th>Yesterday<br/>" ++ ddrt_utils:days_to_str_date(Today - 1)++"</th>
                    <th>D3<br/>" ++ ddrt_utils:days_to_str_date(Today - 2) ++ "</th>
                    <th>D4<br/>" ++ ddrt_utils:days_to_str_date(Today - 3) ++ "</th>
                    <th>D5<br/>" ++ ddrt_utils:days_to_str_date(Today - 4) ++ "</th>
                    <th>D6<br/>" ++ ddrt_utils:days_to_str_date(Today - 5) ++ "</th>
                    <th>D7<br/>" ++ ddrt_utils:days_to_str_date(Today - 6) ++ "</th>
                </tr>
            </thead>
            <tbody>" ++ get_body(Reports) ++ "</tbody></table></div></body></html>",
    Mail = #mail{to=To, cc=Cc, subject=Subject, body=Body},
    %Mail = #mail{to="Hardy.D.Huang@newegg.com", cc="", subject=Subject, body=Body},
    spawn(ddrt_mail, send_mail, [Mail]).


get_body(Reports) ->
    DefaultKey = "no_domain",
    Domians = parse_reports(Reports, DefaultKey),
    {ok, No_Domain} = dict:find(DefaultKey, Domians),

    dict:fold(fun(K, V, Acc) -> 
                    Acc ++ ("<tr><td class=\"left\" colspan=\"8\"><b>" ++ K ++ "</b></td></tr>") ++ build_body(parse_users(V))
                end, build_body(parse_users(No_Domain)), dict:erase(DefaultKey,Domians)).



build_body(UserReports) ->
    Today = ddrt_utils:get_today_days(),
    dict:fold(fun (K, V, Acc) ->
                    "<tr><td class=\"center\">" ++ string:substr(K, 1, string:str(K, "@")-1) ++ "</td>
                         <td width=\"15%;\">" ++ proplists:get_value(Today, V, "N/A") ++ "</td>
                         <td width=\"14%\">" ++ proplists:get_value(Today - 1, V, "N/A") ++ "</td>
                         <td width=\"13%\">" ++ proplists:get_value(Today - 2, V, "N/A") ++ "</td>
                         <td width=\"12%\">" ++ proplists:get_value(Today - 3, V, "N/A") ++ "</td>
                         <td width=\"12%\">" ++ proplists:get_value(Today - 4, V, "N/A") ++ "</td>
                         <td width=\"12%\">" ++ proplists:get_value(Today - 5, V, "N/A") ++ "</td>
                         <td width=\"12%\">" ++ proplists:get_value(Today - 6, V, "N/A") ++ "</td>
                         </tr>" ++ Acc
               end, "", UserReports).



parse_reports(Reports, DefaultKey)  ->
    parse_reports(Reports, DefaultKey, dict:from_list([{DefaultKey, []}])).

parse_reports([], _DefaultKey, Acct) -> Acct;
parse_reports([#report_mode{email=Email, content=Content, date=Date,domain_name=DomainName} | R], DefaultKey, Acct) ->
    NewDay  =   case Date of
                    {datetime, {{Y, M, D}, _}} -> ddrt_utils:get_days(Y, M, D);
                    undefined -> 0
                end,

    NewCon  =   case Content of
                    undefined -> <<"N/A">>;
                    Any -> Any
                end,

    Item = [{email, binary_to_list(Email)}, {NewDay, binary_to_list(NewCon)}],

    NewName =   case DomainName of
                    Other when is_binary(Other) ->
                        binary_to_list(Other);
                    _ ->
                        DefaultKey
                end,

    NewDict =   case dict:is_key(NewName, Acct) of
                    true ->
                        dict:append(NewName, Item, Acct);
                    false -> 
                        dict:store(NewName, [Item], Acct)
                end,

    parse_reports(R, DefaultKey, NewDict).


parse_users(Items) ->
    lists:foldl(fun(Item, Acct) -> 
                    ItemKey = proplists:get_value(email,Item),
                    [NewItem] = proplists:delete(email, Item),
                    case dict:is_key(ItemKey, Acct) of
                        true ->
                            dict:append(ItemKey, NewItem, Acct);
                        false -> 
                            dict:store(ItemKey, [NewItem], Acct)
                     end
                end, dict:new(), Items).
