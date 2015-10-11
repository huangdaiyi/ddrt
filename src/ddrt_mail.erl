-module(ddrt_mail).
-export([send_mail/1, send_mail_sync/1, error_remind/2]).

-include ("include/ddrt.hrl").
-define(MAIL, mail).

send_mail(#mail{to = To, cc = Cc, subject = Subject, body = Body} = Mail) when is_record(Mail, mail) ->
    Content = [{"From", <<"info@newegg.com">>},
               {"To", ddrt_utils:string_to_binary(To)},
               {"Cc", ddrt_utils:string_to_binary(Cc)},
               {"Subject", ddrt_utils:string_to_binary(Subject)},
               {"Body", ddrt_utils:string_to_binary(Body)},
               {"IsNeedLog", <<"false">>}, {"Priority", <<"Normal">>},
               {"ContentType", <<"Html">>}, {"MailType", <<"Smtp">>},
               {"SmtpSetting",
               {obj,[{"SubjectEncoding", <<"UTF8">>}, {"BodyEncoding", <<"UTF8">>}]}}],
    BodyTemplate = rfc4627:encode({obj, Content}),
    Request = {getEmailHost(),
               [{"accept", "application/json"}], "application/json",
               BodyTemplate},
    case httpc:request(post, Request, [], []) of
      {ok, {{_, 200, _}, _Headers, _Content}} -> true;
      _ -> false
    end.


send_mail_sync(Mail) ->
    spawn(ddrt_mail, send_mail, [Mail]).

error_remind(Reason, UserId) ->
  [#user_email{user_name=Name, email=Email}] = ddrt_db:get_user_email(UserId),
  Body = lists:flatten(io_lib:format("Dear ~s:<p> &nbsp;&nbsp;&nbsp;&nbsp;
    I'm sorry to tell you that Your worklog has failed. We'll deal with it as soon as possible.<br/> <b>Reason</b>: ~w</p>",
    [Name, Reason])),
  Mail = #mail{to = binary_to_list(Email), cc = "Hardy.D.Huang@newegg.com", subject = "(error) worklog error", body = Body},
  send_mail_sync(Mail).

getEmailHost() ->
    DefaultHost = "http://10.1.50.233/framework/v1/mail",
    case neg_hydra:get_env(email_server, DefaultHost) of
      {ok, Value} -> Value;
      Host -> Host
    end.
