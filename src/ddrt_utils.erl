-module (ddrt_utils).
-export ([build_report_body/1, datetime_to_string/1, datetime_format/1,user_format/1,format_data_line/1,
         get_today_days/0, get_days/3, days_to_date/1, days_to_str_date/1, get_str_today/0]).
-include ("include/ddrt.hrl").

build_report_body(Reports) ->
	[{obj,[{"userid", Email},{"content", Content},{"date", list_to_binary(datetime_to_string(Date))}]}
	 || #report_mode{email=Email, content=Content, date={datetime, Date}} <- Reports].


datetime_to_string(DateTime) ->
	{{Y, M1, D}, {H, M2, S}} = DateTime,
		string:join([integer_to_list(Y), integer_to_list(M1), integer_to_list(D)],"-") 
		++ " " 
		++ string:join([integer_to_list(H), integer_to_list(M2), integer_to_list(S)], ":").


datetime_format(Date) when is_list(Date) ->
    list_to_binary(Date);
datetime_format(Date) when is_binary(Date) ->
    Date;
datetime_format(Date) ->
    {{Year, Month, Day}, _} = Date,
    list_to_binary(string:join([integer_to_list(Year),integer_to_list(Month),integer_to_list(Day)],"-")).


days_to_date(DayNums) ->
    calendar:gregorian_days_to_date(DayNums).

days_to_str_date(DayNums) ->
    {Y, M, D} = days_to_date(DayNums), 
    string:join([integer_to_list(Y),integer_to_list(M),integer_to_list(D)],"-").

get_days(Y, M, D) ->
    calendar:date_to_gregorian_days(Y, M, D).  

get_today_days()->
    {{Y, M ,D}, _} = calendar:local_time(),
    calendar:date_to_gregorian_days(Y, M, D).

get_str_today() ->
    {{Y,M,D},_} = calendar:local_time(),
    string:join([integer_to_list(Y),integer_to_list(M),integer_to_list(D)],"-").


format_data_line(Data) when is_binary(Data) ->
    re:replace(binary_to_list(Data), "\r*\n", "<br/>", [global, {return, list}]);
format_data_line(Data) when is_list(Data) ->
    re:replace(Data, "\r*\n", "<br/>", [global, {return, list}]).

user_format(Result)->
    lists:map(fun(#userentity{id=Id, dname=Dname, email = Email,type=Type,receive_type=Receivetype,gname=GroupName,template=Template}) -> 
                {obj, [{id,Id},{email,Email},{groupname,GroupName},{domainname, Dname},
                {type,Type},{receivetype,Receivetype},{template,Template}]}
            end, Result).