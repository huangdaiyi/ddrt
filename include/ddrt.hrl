
-record (userentity,{id, email,type,receive_type,gname,template,dname}).

-record (users, {id}).

-record (groups,{id, group_name}).

-record (report_mode, {user_id, email, content, date, group_name, template, receive_type, domain_name, domain_id}).

-record (email_list, {email}).

-record (id, {id}).

-record (send_user, {id, email, receive_type}).

-record (mail, {to, cc="", subject, body}).

