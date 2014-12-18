-module(ddrt_mail).
-export([send_mail/1]).

-include ("include/ddrt.hrl").
-define(MAIL, mail).

send_mail(#?MAIL{to = To, cc = Cc, subject = Subject,body = Body} = Mail) when is_record(Mail,?MAIL) ->
    Content = [{"From",<<"info@newegg.com">>},
                {"To", list_to_binary(To)},
                {"Cc", list_to_binary(Cc)},
                {"Subject", list_to_binary(Subject)},
                {"Body", list_to_binary(Body)},
                {"IsNeedLog",<<"false">>},
                {"Priority",<<"Normal">>},
                {"ContentType",<<"Html">>},
                {"MailType",<<"Smtp">>},
                {"SmtpSetting",{obj,[{"SubjectEncoding",<<"UTF8">>},{"BodyEncoding",<<"UTF8">>}]}}],

    BodyTemplate = rfc4627:encode({obj, Content}),

    inets:start(),
    Request = {getEmailHost(),[{"accept","application/json"}], "application/json", BodyTemplate},
 	case httpc:request(post, Request, [], []) of
    	{ok,{{_,200,_},_Headers,_Content}} -> true;
    	_ -> false
  	end.

getEmailHost() ->
    DefaultHost = "http://10.1.50.233/framework/v1/mail",
    case application:get_env(drt, email_server, DefaultHost) of
        [] -> DefaultHost;
        Host -> Host
    end.