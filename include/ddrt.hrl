
-record (userentity,{email,type,receive_type,gname,template,dname}).

-record (users, {id}).

-record (groups,{id, group_name}).

-record (report_mode, {email, content, date, group_name, template, receive_type, domain_name}).

-record (email_list, {email}).

-record (mail, {to, cc="", subject, body}).

