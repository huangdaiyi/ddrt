
-record (userentity,{id, email,type,receive_type,gname,template,dname}).

-record (users, {id}).

-record (groups,{id, group_name, scheduling_name="basic"}).

-record (report_mode, {user_id, email, content, date, group_name, template, receive_type, domain_name, domain_id}).

-record (email_list, {email}).

-record (id, {id}).

-record (send_user, {id, email, receive_type}).

-record (mail, {to, cc="", subject, body}).

-record(member,{id,email}).%can be a domain member,or a member that in a group but not in a domain 

-record(scheduling, {id, name, scheduling_time, type}).