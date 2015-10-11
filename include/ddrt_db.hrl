-define(DB_SCRIPT,[
%%% ===================================================================
%%% User
%%% ===================================================================

%if id is null,then will checkout all info about user
{get_user, <<"SELECT u.id, u.email,u.`type`,gu.receive_type,g.`name` AS group_name,g.template,d.`name` AS domain_name FROM users AS u
INNER JOIN groups_users AS gu ON u.id=gu.user_id INNER JOIN groups AS g ON gu.group_id=g.id LEFT JOIN
domains AS d ON gu.domain_id=d.id WHERE u.id = ?">>},

{get_user_email, <<"SELECT `email` , `user_name` FROM `users` WHERE `id` = ?">>},

{get_user_by_email, <<"SELECT u.id, u.email,u.`type`,gu.receive_type,g.`name` AS group_name,g.template,d.`name` AS domain_name FROM users AS u
INNER JOIN groups_users AS gu ON u.id=gu.user_id INNER JOIN groups AS g ON gu.group_id=g.id LEFT JOIN
domains AS d ON gu.domain_id=d.id WHERE u.email = ?">>},

{get_users, <<"SELECT u.id, u.email,u.`type`,gu.receive_type,g.`name` AS group_name,g.template,d.`name` AS domain_name FROM users AS u
INNER JOIN groups_users AS gu ON u.id=gu.user_id INNER JOIN groups AS g ON gu.group_id=g.id INNER JOIN
domains AS d ON gu.domain_id=d.id ">>},

{add_user, <<"INSERT IGNORE  INTO users(email,type) VALUES(?,?);">>},

{add_group, <<"INSERT IGNORE INTO groups(`name`, template) VALUES (?,?);">>},

{get_dommeber,<<"select u.id,u.email from users as u inner join groups_users as gu on u.id=gu.user_id  inner join domains as d
on d.group_id=gu.group_id where gu.group_id=? and d.name = ?">> },

{get_notdommeber,<<"select u.id ,u.email from users as u where u.id not in 
(select  gu.user_id from  groups_users as gu inner join domains as d on gu.group_id=d.group_id where  d.group_id=?)">> },

{add_report, <<"INSERT INTO reports(`user_id`, `content`, `date`, `time_spent`, `issue_name`, `worklog_id`) VALUES (?,?,?,?,?,?)">>},

{update_report, <<"UPDATE reports SET `content` = ?, `date` = ?, `time_spent` = ? WHERE `worklog_id` = ? AND `user_id` = ?">>},

{delete_report, <<"DELETE FROM reports WHERE `worklog_id` = ? AND `user_id` = ?">>},

{delete_reports, <<"DELETE FROM reports WHERE `worklog_id` IN (?) AND `user_id` = ?">>},

{get_report, <<"SELECT u.id AS user_id, u.email, r.`worklog_id`,  r.content, r.`date`, r.`time_spent`, r.`issue_name` AS issue,
 g.`name` AS group_name, g.template, gu.receive_type, d.`name` AS domain_name, d.id AS domain_id
				FROM reports AS r INNER JOIN users AS u ON u.id = r.user_id 
				INNER JOIN groups_users AS gu ON gu.user_id = u.id 
				INNER JOIN groups AS g ON g.id = gu.group_id 
				INNER JOIN domains AS d ON gu.domain_id = d.id 
				WHERE r.`date` BETWEEN DATE_SUB(?,INTERVAL ? DAY) AND DATE_ADD(?,INTERVAL 1 DAY) AND g.id = ? AND u.type = 'R'">>},

{create_history, <<"REPLACE INTO `history_issues` (`issue_key`,`user_id`,`day_num`,`create_at`) VALUES(?, ?, ?, now()); ">>},

{get_prev_issues, <<"SELECT `issue_key` AS issue,`user_id` FROM `history_issues`  WHERE `user_id` = ? AND `day_num` = (SELECT MAX(`day_num`) FROM `history_issues` WHERE `user_id`= ?);">>},

{get_user_report, <<"SELECT u.id AS user_id, u.email, r.`worklog_id`, r.`content`, r.`date`, r.`time_spent`, r.`issue_name` AS issue,
 g.`name` AS group_name, g.template, gu.receive_type, d.`name` AS domain_name, d.id AS domain_id
				FROM reports AS r INNER JOIN users AS u ON u.id = r.user_id 
				INNER JOIN groups_users AS gu ON gu.user_id = u.id 
				INNER JOIN groups AS g ON g.id = gu.group_id 
				INNER JOIN domains AS d ON gu.domain_id = d.id 
				WHERE r.`date` BETWEEN ? AND DATE_ADD(?,INTERVAL ? DAY) AND u.id = ? AND u.type = 'R'">>},

{get_group_user, <<"SELECT gu.`user_id`, u.`email`, u.`user_name`, u.`type` AS report_type, d.`id` AS domain_id, d.`name` AS domain_name, gu.`receive_type` FROM `users` AS u 
						INNER JOIN  `groups_users` AS gu ON gu.`user_id` = u.`id` LEFT JOIN `domains` AS d ON  gu.`domain_id` = d.`id`
						WHERE gu.`group_id` = ? ORDER BY domain_id;">>},


{check_today_report, <<"SELECT r.Id FROM reports AS r  
					WHERE r.user_id =? AND r.`date` BETWEEN ? AND DATE_ADD(?,INTERVAL 1 DAY);">>},

{check_today_report_by_email, <<"SELECT r.Id FROM reports AS r  INNER JOIN  users AS u 
								ON r.user_id = u.id WHERE u.email = ? AND r.`date` BETWEEN ? AND DATE_ADD(?,INTERVAL 1 DAY);">>},

{get_all_reports, <<"SELECT u.id AS user_id, u.user_name, u.`email`, r.`worklog_id`, r.`content`, r.`date`, r.`time_spent`, r.`issue_name` AS issue, g.`name` AS group_name, g.template, gu.receive_type, 
d.`name` AS domain_name, d.id AS domain_id
					FROM  users AS u 
					INNER JOIN reports AS r ON u.id = r.user_id 
					INNER JOIN  groups_users as gu ON gu.user_id = u.id  
					INNER JOIN groups AS g ON gu.group_id = g.id 
					LEFT JOIN domains AS d ON gu.domain_id = d.id
					WHERE r.`date` BETWEEN DATE_SUB(?,INTERVAL ? DAY) 
					AND DATE_ADD(?,INTERVAL 1 DAY) AND g.id = ? AND u.`type` = 'R'">>},

{get_not_report_emails, <<"SELECT u.email, u.user_name FROM users AS u 
INNER JOIN groups_users AS gu
    ON u.id = gu.user_id
WHERE u.`type` = 'R'
    AND gu.group_id=?
    AND 
    NOT EXISTS(SELECT r.user_id FROM reports AS r 
        WHERE r.`date` BETWEEN ? AND DATE_ADD(?, INTERVAL ? DAY) 
        AND u.id = r.user_id);">>},

{get_groups, <<"SELECT id, `name` AS group_name, scheduling_name FROM groups">>},

{get_scheduling, <<"SELECT id, name, scheduling_time, type FROM `scheduling` WHERE name = ?;">>},

{get_group_report_user, <<"SELECT  u.`email`, u.`user_name`, gu.`user_id` 
							FROM `users`  AS u INNER JOIN `groups_users` AS gu 
							ON u.`id` = gu.`user_id` WHERE gu.`group_id` = ? AND u.`type` = 'R'">>}

]).
