-module(ddrt_timer).
-behavior(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include ("include/ddrt.hrl").
-define (REPORTDAYS, 7).
-define (TIMESPAN, 1000*60*5).

%%%================================================
%%% gen_server callbacks
%%%================================================

init([]) ->
	% {ok, null}.
	Groups = ddrt_db:get_groups(),
	%io:format("~n~p~n", [Groups]),
	{ok, Groups, 0}.

terminate(_Reason, _State) ->
	ok.

handle_cast(stop, State) ->
	{stop, normal, State}.


handle_call(groups, _From, State) ->
	if
		length(State) > 0 ->
			{reply, State, State};

		true ->
			Groups = ddrt_db:get_groups(),
			{reply, Groups, Groups}
	end;
handle_call(_Message, _From, State) ->
	{reply, ok, State}.

handle_info(timeout, State) ->
	loop(?TIMESPAN),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

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

get_remind_user()->
	ddrt_db:get_not_report_emails(calendar:local_time(), ?REPORTDAYS).


send_remind() ->
	Users = get_remind_user(),
	send_remind(Users).

send_remind([]) -> ok;
send_remind([User|RestUsers]) ->
	Body = "<!DOCTYPE HTML>
<html>
<head>
<title>Daily Report Remind</title>
</head>
<body style=\"color:#0099CC\">
	<p>
       Dear " ++ string:substr(User, 1, string:str(User,".")-1) ++",<br/><br/>
       <Strong>Please <a href=\"http://10.16.76.245:8080\">submit</a> daily reports in a timely manner(Before 18:00).</strong>
    </p>
    <p style=\"font-style: italic;\">
       Cheers,<br/>
       DDRT
	</p
</body>
</html>",
	Subject = "(Info)Submit Daily Report Remind",
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


-spec loop(Timespan :: integer()) -> ok.
loop(Timespan) ->
	%io:format("Enter looper ... ~n"),
	{_,{H,M,_}} = calendar:local_time(),
	if
		(H == 16) andalso ((M >= 30) andalso (M < 35)) -> send_remind(), timer:sleep(Timespan), loop(Timespan);
        (H == 17) andalso (M < 5) -> send_remind(), timer:sleep(Timespan), loop(Timespan);
        (H == 17) andalso ((M >= 25) andalso (M < 30)) -> send_remind(), timer:sleep(Timespan), loop(Timespan);
        (H == 18) andalso (M < 5) -> send_mail(), timer:sleep(Timespan), loop(Timespan);
        (H == 0) andalso (M < 5)-> timer:sleep(Timespan), loop(Timespan);
        true -> timer:sleep(Timespan), loop(Timespan)
    end.