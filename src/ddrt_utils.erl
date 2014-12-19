-module (ddrt_utils).
-export ([build_report_body/1, datetime_to_string/1, datetime_format/1]).

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
    string:join([integer_to_list(Year),integer_to_list(Month),integer_to_list(Day)],"-").