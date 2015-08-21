-module (ddrt_utils).
-export([build_report_body/1, datetime_to_string/1,
         datetime_format/1, user_format/1, format_data_line/1,
         get_today_days/0, get_days/3, get_days/1,
         days_to_date/1, days_to_str_date/1, get_str_today/0,
         string_to_binary/1, binary_to_string/1,
         time_to_utc_string/1, get_value/2, get_value/3,
         get_orignal_value/2, get_orignal_value/3, get_jira_url/0, get_crl_comment/2, 
         to_float/1, to_sql_wvarchar/1, get_mssql_day_string/0]).
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
    list_to_binary(string:join([integer_to_list(Year),
                                integer_to_list(Month), integer_to_list(Day)],
                               "-")).

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

get_crl_comment(Comment, WorklogId) when Comment =:= [] ->
    << <<"[AUTO#">>/binary, WorklogId/binary, <<"]work in JIRA">>/binary>>;
get_crl_comment(Comment, WorklogId) ->
    << <<"[AUTO#">>/binary, WorklogId/binary, Comment/binary>>.

to_float(Bin) when is_binary(Bin) ->
    to_float(binary_to_list(Bin));
to_float(Str) ->
    case string:to_float(Str) of
        {error,no_float} -> float(list_to_integer(Str));
        {F,_Rest} -> F
    end.


to_sql_wvarchar(Content) ->
    unicode:characters_to_binary(Content, utf8, {utf16, little}).


%%% ===================================================================
%%% Tests  
%%% ===================================================================

% [build_report_body/1, datetime_to_string/1,
%          datetime_format/1, user_format/1, format_data_line/1,
%          get_today_days/0, get_days/3, get_days/1,
%          days_to_date/1, days_to_str_date/1, get_str_today/0,
%          string_to_binary/1, binary_to_string/1,
%          time_to_utc_string/1, get_value/2, get_value/3,
%          get_orignal_value/2, get_orignal_value/3, get_jira_url/0, get_crl_comment/2, 
%          to_float/1, to_sql_wvarchar/1, get_mssql_day_string/0]
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-include ("include/ddrt.hrl").
datetime_to_string_test() ->
    % ok = meck:new(erlang, [non_strict]),
    % meck:expect(erlang, localtime, fun() -> {{2015,8,21},{11, 5, 1}} end),
    DateTime = {{2015,8,21},{11, 5, 1}},
    ?assertEqual("2015-08-21 11:05:01",  datetime_to_string(DateTime)).
    %true = meck:validate(erlang),
    %meck:unload(erlang).
% get_mssql_day_string_test() ->
%     ok = meck:new(erlang, [non_strict]),
%     meck:expect(erlang, localtime, fun() -> {{2015,8,21},{11, 5, 1}} end),
%     ?assertEqual("2015-08-21 00:00:00",  get_mssql_day_string()),
%     true = meck:validate(erlang),
%     meck:unload(erlang).
-endif.