
-record (userentity,{id, email,type,receive_type, group_name, template, domain_name}).

-record (users, {id}).

-record (groups,{id, group_name, scheduling_name="basic"}).

-record (report_mode, {worklog_id, user_id, user_name, email, content, date, group_name, template, receive_type, domain_name, domain_id, time_spent ,issue}).

-record (email_list, {email, user_name}).

-record (id, {id}).

-record (group_user, {user_id, email, user_name, domain_id, domain_name, report_type, receive_type}).

-record (group_report_user, {email, user_name, user_id}).

-record (user_email, {user_name, email}).

-record (mail, {to, cc="", subject, body}).

-record(member,{id,email}).%can be a domain member,or a member that in a group but not in a domain 

-record(scheduling, {id, name, scheduling_time, type}).

-record(history_issue, {use_id, issue}).

-record(mssql_pool, {pool_id, conn, state}).

%-record (dailyhours, {task_execute_id, date, spend_time, activity, created, created_user_id, created_user_name, process_model_id, task_id, task_name, common_process_id, phase, task_type}).

-record(template_opts, {
          module = template_parse,
          source,
          render = render,
          compile_opts = [report, return, {auto_escape, false}, force_recompile, {out_dir, false},{debug_info , true}],
          compile_vars = [],
          render_opts = [],
          render_vars = []
         }).