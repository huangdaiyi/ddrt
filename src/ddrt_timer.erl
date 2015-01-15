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
	inets:start(),
	{ok, T} = neg_hydra:get_env(ddrt_time),
	RemindTime = to_minutes(proplists:get_value(remind, T)),
	SendTime = to_minutes(proplists:get_value(send, T)),
	Timespan = round(proplists:get_value(span, T)*60000),
	{ok, {RemindTime, SendTime, Timespan}, 0}.

terminate(_Reason, _State) ->
	ok.

handle_cast(stop, State) ->
	{stop, normal, State}.


handle_call(_Message, _From, State) ->
	{_, _, Sp} = State,
	{reply, ok, State, Sp}.

handle_info(timeout, State) ->
	{Re, Se, Sp} = State,
	{ok, NewSp} = send(Re, Se, Sp div 60000),
	{noreply, State, NewSp*60000}.

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


send(RemindTime, SendTime, Timespan) ->
	{_, {H, M, _}} = calendar:local_time(),
	Minutes	= H*60 + M,
	%send_mail().
	if

		Minutes	>= RemindTime andalso Minutes < SendTime ->

			if 
				Minutes	>= 1050	andalso Minutes < (1050 + Timespan) ->
					send_mail([#groups{id = 2, group_name = <<"PO Team">>}]);
				true -> pass
			end,

			RestTime = SendTime - Minutes,
			send_remind(),
			if 
				RestTime < Timespan ->  
					{ok, RestTime};
				true -> {ok, Timespan}
			end;

		Minutes	>= SendTime	andalso Minutes < (SendTime + Timespan) ->
			send_mail(),
			{ok, Timespan};

		true -> 
			RestRemindTime =  RemindTime - Minutes,
			if 
				RestRemindTime < Timespan ->  
					{ok, RestRemindTime};
				true -> {ok, Timespan}
			end
	end.


get_remind_user() ->
	ddrt_db:get_not_report_emails(calendar:local_time(), 1).


send_remind() ->
	Users = get_remind_user(),
	send_remind(Users).

send_remind([]) -> ok;
send_remind([#email_list{email=Email}|RestUsers]) ->
    {ok, ReportUrl} = neg_hydra:get_env(report_address),
    {ok, T} = neg_hydra:get_env(ddrt_time),

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
			        proplists:get_value(send, T) ++ ").</strong>
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
	send_remind(RestUsers).

send_mail() ->
	Groups = ddrt_db:get_groups(),
	%remove po ---> temp
	RestGroups = lists:delete(#groups{id = 2, group_name = <<"PO Team">>}, Groups),
	send_mail(RestGroups).

send_mail([]) -> ok;
send_mail([#groups{id=ID, group_name=Name} | RestGroups]) ->
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
	spawn(ddrt_mail, send_mail, [Mail]),
	send_mail(RestGroups).


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
	NewDay	= 	case Date of
					{datetime, {{Y, M, D}, _}} -> ddrt_utils:get_days(Y, M, D);
					undefined -> 0
			 	end,

	NewCon  =	case Content of
					undefined -> <<"N/A">>;
					Any -> Any
				end,

	Item = [{email, binary_to_list(Email)}, {NewDay, binary_to_list(NewCon)}],

	NewName = 	case DomainName of
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
