-module(ddrt_timer).
-behavior(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include ("include/ddrt.hrl").
-define (REPORTDAYS, 7).

%%%================================================
%%% gen_server callbacks
%%%================================================

init([]) ->
	{ok, T} = neg_hydra:get_env(ddrt_time),
	RemindTime = to_minutes(proplists:get_value(remind, T)),
	SendTime = to_minutes(proplists:get_value(send, T)),
	Timespan = proplists:get_value(span, T)*3600,
	{ok, {RemindTime, SendTime, Timespan}, 0}.

terminate(_Reason, _State) ->
	ok.

handle_cast(stop, State) ->
	{stop, normal, State}.


handle_call(_Message, _From, State) ->
	{_, _, Sp} = State,
	{reply, ok, State, Sp}.

handle_info(timeout, State) ->
	io:format("timeout calling ...~n"),
	{Re, Se, Sp} = State,
	send(Re, Se, Sp/3600),
	{noreply, State, Sp}.

code_change(_OldVsn, State, _Extra) ->
	{_, _, Sp} = State,
	{ok, State, Sp}.

%%%================================================
%%% API
%%%================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).


%%%==============================================
%%% Internal functions
%%%==============================================

to_minutes(TimeStr) ->
	[H, M] = [ list_to_integer(X) || X <- string:tokens(TimeStr,":")],
	H*60 + M.


send(RemindTime, SendTime, TimeSpan) ->
	{_, {H, M, _}} = calendar:local_time(),
	Minutes	= H*60 + M,
	if
		Minutes	>= RemindTime andalso Minutes < SendTime -> send_remind();
		Minutes	>= SendTime	andalso Minutes < (SendTime + TimeSpan) -> send_mail();
		true -> ok
	end.




get_remind_user()->
	ddrt_db:get_not_report_emails(calendar:local_time(), ?REPORTDAYS).


send_remind() ->
	Users = get_remind_user(),
	io:format("~n~p~n", [Users]),
	send_remind(Users).

send_remind([]) -> ok;
send_remind([#email_list{email=Email}|RestUsers]) ->
	
	User = binary_to_list(Email),
	Body = "<!DOCTYPE HTML>
<html>
<head>
<title>Daily Report Remind</title>
</head>
<body style=\"color:#0099CC\">
	<p>
       Dear " ++ string:substr(User, 1, string:str(User,"@")-1) ++",<br/><br/>
       <Strong>Please <a href=\"http://10.16.76.245:8080\">submit</a> daily reports in a timely manner(Before 18:00).</strong>
    </p>
    <p style=\"font-style: italic;\">
       Cheers,<br/>
       DDRT
	</p
</body>
</html>",
	Subject = "(Info)Submit Daily Report Remind---test",
	Cc = "",
	Mail = #mail{to=User, cc=Cc, subject=Subject, body=Body},
	spawn(ddrt_mail, send_mail, [Mail]),
	send_remind(RestUsers).

send_mail() ->
	{ok, Groups} = gen_server:call(?MODULE,groups),
	send_mail(Groups).

send_mail([]) -> ok;
send_mail([#groups{id=ID, group_name=_Name} | RestGroups]) ->
	%email, content, date, group_name, template, receive_type, domain_name
	Now = calendar:local_time(),
	Reports = ddrt_db:get_report(Now, ?REPORTDAYS, ID),
	To = string:join([ Email || #report_mode{email=Email, receive_type=Type} <- Reports, Type == "To"], ";"),
	Cc = string:join([ Email || #report_mode{email=Email, receive_type=Type} <- Reports, Type == "Cc"], ";"),
	Subject = "(Report)DFIS Daily Report" ++ getNowTime(),
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
	   td.center { text-align: center; }
	   td.large { width: 40%; }
       td.small { width: 20%; }
	</style>
</head>
<body>
    <h1>DFIS Daily Report</h1>
    <div class=\"data\">
        <table style=\"width: 100%\">
            <thead>
                <tr>
                    <th>Team Member</th>
                    <th>Today</th>
                    <th>Issue</th>
                    <th>Next</th>
                </tr>
            </thead>
            <tbody>" ++ string:join(getBody(Reports),"") ++ "</tbody></table></div></body></html>",
	Mail = #mail{to=To, cc=Cc, subject=Subject, body=Body},
	spawn(drt_mail, send_mail, [Mail]),
	send_mail(RestGroups).


getBody(Reports) ->
	getBody(Reports, "").

getBody([],Body) -> Body;
getBody([#report_mode{} | RestReport], Body) ->
	Body = "<tr><td class=\"center\">" ++ string:substr(#report_mode.email, 1, string:len(#report_mode.email)-11) ++
	"</td><td class=\"large\">N/A</td><td class=\"small\">N/A</td><td class=\"small\">N/A</td></tr>" ++ Body,
	getBody(RestReport, Body).


% Get now time and format
getNowTime() ->
	{{Y,M,D},_} = calendar:local_time(),
	string:join([integer_to_list(Y),integer_to_list(M),integer_to_list(D)],"-").

