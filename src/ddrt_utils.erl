-module (ddrt_utils).
-export([build_report_body/1, datetime_to_string/1,
         datetime_format/1, user_format/1, format_data_line/1,
         get_today_days/0, get_days/3, get_days/1,
         days_to_date/1, days_to_str_date/1, get_str_today/0,
         string_to_binary/1, binary_to_string/1,
         time_to_utc_string/1, get_value/2, get_value/3,
         get_orignal_value/2, get_orignal_value/3, get_jira_url/0, get_crl_comment/2, 
         to_float/1, to_sql_wvarchar/1, get_mssql_day_string/0, send_http/4, send_http/5, to_integer/1]).

-export([local_time/0]).
-include ("include/ddrt.hrl").

build_report_body(Reports) ->
    [{obj,
      [{"userid", Email}, {"content", Content},
       {"date", list_to_binary(datetime_to_string(Date))}, {"issue", Issue}, {"worklogId", Id}, {"timeSpent", TimeSpent}]}
     || #report_mode{worklog_id=Id,email = Email, content = Content,
                     date = {datetime, Date}, time_spent = TimeSpent , issue = Issue }
            <- Reports].


datetime_format(Date) when is_list(Date) ->
    list_to_binary(Date);
datetime_format(Date) when is_binary(Date) -> Date;
datetime_format(Date) ->
    {{Year, Month, Day}, _} = Date,
    list_to_binary(string:join([integer_to_list(Year),  integer_to_list(Month), integer_to_list(Day)],"-")).

days_to_date(DayNums) ->
    calendar:gregorian_days_to_date(DayNums).

days_to_str_date(DayNums) ->
    {Y, M, D} = days_to_date(DayNums),
    string:join([integer_to_list(Y), integer_to_list(M),
                 integer_to_list(D)],
                "-").

get_days({Y, M, D}) -> get_days(Y, M, D);
get_days({{Y, M, D}, _}) -> get_days(Y, M, D);
get_days({datetime, {{Y, M, D}, _}}) ->
    get_days(Y, M, D).

get_days(Y, M, D) ->
    calendar:date_to_gregorian_days(Y, M, D).

get_today_days() ->
    {{Y, M, D}, _} = calendar:local_time(),
    calendar:date_to_gregorian_days(Y, M, D).

get_str_today() ->
    {{Y, M, D}, _} = calendar:local_time(),
    string:join([integer_to_list(Y), integer_to_list(M),
                 integer_to_list(D)],
                "-").

format_data_line(Data) when is_binary(Data) ->
    re:replace(binary_to_list(Data), "\r*\n", "<br/>",
               [global, {return, list}]);
format_data_line(Data) when is_list(Data) ->
    re:replace(Data, "\r*\n", "<br/>",
               [global, {return, list}]).

user_format(Result) ->
    lists:map(fun (#userentity{id = Id, domain_name = Dname,
                               email = Email, type = Type,
                               receive_type = Receivetype, group_name = GroupName,
                               template = Template}) ->
                      {obj,
                       [{id, Id}, {email, Email}, {groupname, GroupName},
                        {domainname, Dname}, {type, Type},
                        {receivetype, Receivetype}, {template, Template}]}
              end,
              Result).

string_to_binary(L) when is_list(L) ->
    list_to_binary(L);
string_to_binary(L) when is_binary(L) -> L.

binary_to_string(B) when is_binary(B) ->
    binary_to_list(B);
binary_to_string(B) when is_list(B) -> B.


time_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
    calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    lists:flatten( io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ", [Year, Month, Day, Hour, Minute, Second, MicroSecs])).


datetime_to_string(DateTime) ->
 {{Y,Mo,D},{H,Mi, S}} = DateTime, 
 lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, Mo, D, H, Mi, S])).

get_mssql_day_string() ->
 {{Y,Mo,D},_} = erlang:localtime(), 
 lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, Mo, D, 0, 0, 0])).


get_value(Key,Lists) ->
    case proplists:get_value(Key,Lists,undefined) of
        undefined ->
            throw({termination, 400, [], <<>>});
        Value when is_binary(Value) ->
            erlang:binary_to_list(Value);
        Value ->
            Value
        end.

get_value(Key,Lists,Default) ->
    case proplists:get_value(Key,Lists,undefined) of
        undefined ->
            Default;
        Value when is_binary(Value) ->
            erlang:binary_to_list(Value);
        Value ->
            Value
        end.

get_orignal_value(Key,Lists) ->
    case proplists:get_value(Key,Lists,undefined) of
    undefined -> throw({termination, 400, [], <<>>});
    Value -> Value
    end.

get_orignal_value(Key,Lists,Default) ->
    case proplists:get_value(Key,Lists,undefined) of
        undefined ->
            Default;
        Value ->
            Value
        end.


get_jira_url()->
    {ok, JiraUrl} = neg_hydra:get_env(jira_address, "http://jira"),
    JiraUrl.

get_crl_comment(Comment, WorklogId) ->
    << <<"[AUTO#">>/binary, WorklogId/binary, <<"]">>/binary, Comment/binary>>.


to_float(Int) when is_integer(Int) ->
    float(Int);
to_float(Bin) when is_binary(Bin) ->
    to_float(binary_to_list(Bin));
to_float(Str) ->
    case string:to_float(Str) of
        {error,no_float} -> float(list_to_integer(Str));
        {F,_Rest} -> F
    end.



send_http(Method, Url, HttpHeaders, Body) ->
    send_http(Method, Url, HttpHeaders, Body, 10000).

send_http(Method, Url, HttpHeaders, Body, Timeout) ->
    Content_type = proplists:get_value("content-type", HttpHeaders, "text/plain"),
    SafeHttpHeaders = proplists:delete("content-type", HttpHeaders),
    Request = case Method of
        Req when Req =:= post; Req =:= put  -> {Url, SafeHttpHeaders, Content_type, Body};
        _ -> {Url, SafeHttpHeaders}
    end,
    %%httpc:set_options([{cookies, verify }]),
    Result = httpc:request(Method, Request, [{ssl,[{verify,0}]}, {timeout, Timeout}], []),
    case Result of
        {ok, {{_HttpVersion, StatusCode, _Description}, Headers, Content}} -> {ok, StatusCode, Headers, Content};
        {error, _Message} -> throw({error, 500, list_to_binary(io_lib:format("connect ~p failed.", [Url]))})
    end.


to_sql_wvarchar(Content) ->
    unicode:characters_to_binary(Content, utf8, {utf16, little}).

-spec local_time() -> {Hour :: pos_integer(), Minute :: pos_integer()}.
local_time() ->
    {_, {Hour, Minute, _}} = calendar:local_time(),
    {Hour, Minute}.

to_integer(Bin) when is_binary(Bin) ->
    to_integer(binary_to_list(Bin));
to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Num) -> Num.


%%% ===================================================================
%%% Tests  
%%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

datetime_to_string_test() ->
    DateTime = {{2015,8,21},{11, 5, 1}},
    ?assertEqual("2015-08-21 11:05:01",  datetime_to_string(DateTime)).

datetime_format_test() ->
    ?assertEqual(<<"2015-8-21 14:28:31">>, datetime_format("2015-8-21 14:28:31")),
    ?assertEqual(<<"2015-8-21 14:28:31">>, datetime_format(<<"2015-8-21 14:28:31">>)),
    ?assertEqual(<<"2015-8-21">>, datetime_format({{2015,8,21}, any})).

format_data_line_test() ->
    ?assertEqual("<br/>", format_data_line("\n")),
    ?assertEqual("<br/>", format_data_line("\r\n")),
    ?assertEqual("<br/>1234", format_data_line("\r\n1234")),
    ?assertEqual("<br/>1234", format_data_line(<<"\r\n1234">>)),
    ?assertEqual("abc<br/>1234", format_data_line(<<"abc\n1234">>)).

get_today_days_test() ->
    ?assert(is_number(get_today_days())),
    ?assert(get_today_days() > 736195). %2015-8-20


get_days_test() ->
    ?assertEqual(736196, get_days({2015,8,21})),
    ?assertEqual(736196, get_days(2015,8,21)),
    ?assertEqual(736196, get_days({{2015,8,21},{14,28,31}})),
    ?assertEqual(736196, get_days({datetime, {{2015,8,21},{14,28,31}}})).

days_to_date_test() ->
    ?assertEqual({2015, 8, 21}, days_to_date(736196)).

days_to_str_date_test() ->
    ?assertEqual("2015-8-21", days_to_str_date(736196)).

get_str_today_test() ->
    ?assert(is_list(get_str_today())).

string_to_binary_test() ->
    ?assertEqual(<<>>, string_to_binary([])),
    ?assertEqual(<<"hardy_test">>, string_to_binary("hardy_test")),
    ?assertEqual(<<"hardy_test">>, string_to_binary(<<"hardy_test">>)).

binary_to_string_test() ->
    ?assertEqual([], binary_to_string(<<>>)),
    ?assertEqual("hardy_test", binary_to_string(<<"hardy_test">>)),
    ?assertEqual("hardy_test", binary_to_string("hardy_test")).

time_to_utc_string_test() ->
    ?assertEqual("1970-01-01T00:00:00.000000Z", time_to_utc_string({0,0,0})).


get_value_test() ->
    Lists = [{"a", 1}, {"b", 2}, {"c", <<"3">>}, {"d", "3"}],
    ?assertEqual(1, get_value("a", Lists)),
    ?assertEqual(2, get_value("b", Lists)),
    ?assertEqual("3", get_value("c", Lists)),
    ?assertEqual("3", get_value("d", Lists)),
    ?assertThrow({termination, 400, [], <<>>}, get_value("e", Lists)),

    ?assertEqual(1, get_value("a", Lists, 3)),
    ?assertEqual(2, get_value("b", Lists, 3)),
    ?assertEqual("3", get_value("c", Lists, 3)),
    ?assertEqual("3", get_value("d", Lists, 3)),
    ?assertEqual(3, get_value("e", Lists, 3)).
   

get_orignal_value_test() ->
    Lists = [{"a", 1}, {"b", 2}, {"c", <<"3">>}, {"d", "3"}],
    ?assertEqual(1, get_orignal_value("a", Lists)),
    ?assertEqual(2, get_orignal_value("b", Lists)),
    ?assertEqual(<<"3">>, get_orignal_value("c", Lists)),
    ?assertEqual("3", get_orignal_value("d", Lists)),
    ?assertThrow({termination, 400, [], <<>>}, get_orignal_value("e", Lists)),

    ?assertEqual(1, get_orignal_value("a", Lists, 3)),
    ?assertEqual(2, get_orignal_value("b", Lists, 3)),
    ?assertEqual(<<"3">>, get_orignal_value("c", Lists, 3)),
    ?assertEqual("3", get_orignal_value("d", Lists, 3)),
    ?assertEqual(3, get_orignal_value("e", Lists, 3)).

get_jira_url_test() ->
   
    ok = meck:new(neg_hydra, [non_strict]),
    ok = meck:expect(neg_hydra, get_env, fun
        (jira_address, _)  -> {ok, "http://jira"}
    end),
    ?assertEqual("http://jira", get_jira_url()),
    true = meck:validate(neg_hydra),
    ok = meck:unload(neg_hydra).

get_crl_comment_test() ->
    ?assertEqual(<<"[AUTO#123456]test comment">>, get_crl_comment(<<"test comment">>, <<"123456">>)).

to_float_test() ->
    ?assertEqual(2.0, to_float("2")),
    ?assertEqual(2.0, to_float(2)),
    ?assertEqual(2.0, to_float("2.0")),
    ?assertEqual(2.0, to_float("2")),
    ?assertEqual(2.0, to_float(<<"2">>)).

to_sql_wvarchar_test() ->
    ?assert(is_binary(to_sql_wvarchar(<<"a">>))),
    ?assert(is_binary(to_sql_wvarchar("a"))),
    ?assertEqual(<<97,0>>,  to_sql_wvarchar(<<"a">>)).


get_mssql_day_string_test() ->
    ?assert(is_list(get_mssql_day_string())).

build_report_body_test() ->
    ?assertEqual([], build_report_body([])),
    TestReport = [#report_mode{worklog_id=1,email = "test@email", content = "test", date = {datetime, {{2015,8,21},{14,28,31}}},
     time_spent = 8 , issue = "test" }],
    ?assertMatch([{obj, _}], build_report_body(TestReport)).

user_format_test() ->
    ?assertEqual([], user_format([])),
    TestUser = [#userentity{id = 1, domain_name = "test", email = "test@email", type = "Type", receive_type = "cc",
        group_name = "GroupName",template = "Template"}],
    ?assertMatch([{obj,_}], user_format(TestUser)).

send_http_test() ->
    ok = meck:new(httpc, [non_strict]), 
    ok = meck:expect(httpc, request, fun
        (error, _, _, _) -> {error, "error"};
        (_, _, _, _)  -> {ok, {{"http/1.0",200, ""}, [], <<>>}}
    end),

    ?assertEqual({ok, 200, [], <<>>}, send_http(get, "http://test.com", [{"content-type", "application/json"}], <<>>)),
    ?assertEqual({ok, 200, [], <<>>}, send_http(post, "http://test.com", [{"content-type", "application/json"}], <<>>)),
    ?assertEqual({ok, 200, [], <<>>}, send_http(put, "http://test.com", [{"content-type", "application/json"}], <<>>)),
    ?assertEqual({ok, 200, [], <<>>}, send_http(head, "http://test.com", [{"content-type", "application/json"}], <<>>)),
    ?assertEqual({ok, 200, [], <<>>}, send_http(delete, "http://test.com", [{"content-type", "application/json"}], <<>>)),
    ?assertThrow({error, 500, _}, send_http(error, "http://test.com", [{"content-type", "application/json"}], <<>>)),

    true = meck:validate(httpc),
    ok = meck:unload(httpc).


-endif.