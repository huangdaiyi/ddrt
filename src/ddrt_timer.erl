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
	Timespan = proplists:get_value(span, T)*60000,
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
	send(Re, Se, Sp/60000),
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
	%send_mail().
	if
		Minutes	>= RemindTime andalso Minutes < SendTime ->
		send_remind();
		Minutes	>= SendTime	andalso Minutes < (SendTime + TimeSpan) ->
		send_mail();
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
	% {ok,IP} = neg_hydra:get_env(ip,{0,0,0,0}),
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
			       <Strong>Please <a href=\""++ ReportUrl ++"\">submit</a> daily reports in a timely manner(Before 18:00).</strong>
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
	Groups = ddrt_db:get_groups(),
	send_mail(Groups).

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
	
	Subject = "(Report)" ++ StrName ++ " Daily Report" ++ getNowTime(),
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
	   td.large { width: 13%; }
       td.small { width: 9%; }
       td.left {text-align:left; background: #33B5E5; }
	</style>
</head>
<body>
    <h1>"++ StrName ++" Daily Report</h1>
    <div class=\"data\">
        <table style=\"width: 100%\">
            <thead>
                <tr>
                    <th>Team Member</th>
                    <th>Today</th>
                    <th>Yesterday</th>
                    <th>D3</th>
                    <th>D4</th>
                    <th>D5</th>
                    <th>D6</th>
                    <th>D7</th>
                </tr>
            </thead>
            <tbody>" ++ get_body(Reports) ++ "</tbody></table></div></body></html>",
    %io:format(Body),
	Mail = #mail{to=To, cc=Cc, subject=Subject, body=Body},
	spawn(ddrt_mail, send_mail, [Mail]),
	send_mail(RestGroups).


get_body(Reports) ->
	DefaultKey = "no_domain",
	Domians = parse_reports(Reports, DefaultKey),
	dict:fold(fun(K, V, Acc) -> 
					if
						K =:= DefaultKey ->
							Acc ++ build_body(parse_users(V));
						true ->
							Acc ++ ("<tr><td class=\"left\">" ++ K ++ "</td></tr>") ++ build_body(parse_users(V))
					end
	 			end, "", Domians).



build_body(UserReports) ->

 % [<0.39.0>,"~p~n",
 %                       [{"Hardy.D.Huang@newegg.com",
 %                         [[{email,"Hardy.D.Huang@newegg.com"},{15,"math"}]]},
 %                        {"test@email.com",
 %                         [[{email,"test@email.com"},{14,"ddrt"}],
 %                          [{email,"test@email.com"},{19,"ddrt coding"}],
 %                          [{email,"test@email.com"},{18,"ddrt coding"}]]},
 %                        {"Tristan@email.com",
 %                         [[{email,"Tristan@email.com"},{16,"asdfasdfasd"}],
 %                          [{email,"Tristan@email.com"},{13,"what"}]]},
 %                        {"Tristan1@email.com",
 %                         [[{email,"Tristan1@email.com"},{0,"N/A"}]]}]],
 %                      []
	Today = get_today(),
	%io:format(dict:to_list(UserReports)),
	dict:fold(fun (K, V, Acc) ->
					"<tr><td class=\"center\">" ++string:substr(K, 1, string:str(K, "@")-1) ++ "</td>
					     <td class=\"large\">"++ proplists:get_value(Today, V, "N/A") ++ "</td>
					     <td class=\"large\">"++ proplists:get_value(Today - 1, V, "N/A") ++ "</td>
					     <td class=\"large\">"++ proplists:get_value(Today - 2, V, "N/A") ++ "</td>
					     <td class=\"large\">"++ proplists:get_value(Today - 3, V, "N/A") ++ "</td>
					     <td class=\"large\">"++ proplists:get_value(Today - 4, V, "N/A") ++ "</td>
					     <td class=\"large\">"++ proplists:get_value(Today - 5, V, "N/A") ++ "</td>
					     <td class=\"large\">"++ proplists:get_value(Today - 6, V, "N/A") ++ "</td>
					     </tr>" ++ Acc
			   end, "", UserReports).



parse_reports(Reports, DefaultKey)  ->
	parse_reports(Reports, DefaultKey, dict:from_list([{DefaultKey, []}])).

parse_reports([], _DefaultKey, Acct) -> Acct;
parse_reports([#report_mode{email=Email, content=Content, date=Date,domain_name=DomainName} | R], DefaultKey, Acct) ->
	NewDay	= 	case Date of
					{datetime, {{_, _, D}, _}} -> D;
					undefined -> 0
			 	end,
	NewCon  =	case Content of
					undefined -> <<"N/A">>;
					Any -> Any
				end,
	Item = [{email, binary_to_list(Email)}, {NewDay, binary_to_list(NewCon)}],
	NewName = binary_to_list(DomainName),

	NewDict  =  if
					length(NewName) =:= 0 -> dict:append(DefaultKey, Item, Acct);
					true ->
						case dict:is_key(NewName, Acct) of
						  	true ->
						  		dict:append(NewName, Item, Acct);
						  	false -> 
						  		dict:store(NewName, [Item], Acct)
						end
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



% Get now time and format
getNowTime() ->
	{{Y,M,D},_} = calendar:local_time(),
	string:join([integer_to_list(Y),integer_to_list(M),integer_to_list(D)],"-").


get_today() ->
	{{_,_,D},_} = calendar:local_time(),
	D.

