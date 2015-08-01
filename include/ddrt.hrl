
-record (userentity,{id, email,type,receive_type, group_name, template, domain_name}).

-record (users, {id}).

-record (groups,{id, group_name, scheduling_name="basic"}).

-record (report_mode, {worklog_id, user_id, user_name, email, content, date, group_name, template, receive_type, domain_name, domain_id, time_spent ,issue}).

-record (email_list, {email, user_name}).

-record (id, {id}).

-record (group_user, {user_id, email, user_name, domain_name, report_type, receive_type}).

-record (group_report_user, {email, user_name, user_id}).

-record (mail, {to, cc="", subject, body}).

-record(member,{id,email}).%can be a domain member,or a member that in a group but not in a domain 

-record(scheduling, {id, name, scheduling_time, type}).

-record(template_opts, {
          module = template_parse,
          source,
          renderer = render,
          compile_opts = [report, return, {auto_escape, false}, force_recompile, {out_dir, false},{debug_info , true}],
          compile_vars = [],
          render_opts = [],
          render_vars = []
         }).