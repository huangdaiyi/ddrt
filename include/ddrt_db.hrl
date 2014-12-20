-define(DB_SCRIPT,[
%%% ===================================================================
%%% User
%%% ===================================================================

%if id is null,then will checkout all info about user
{get_user, <<"SELECT u.email,u.`type`,gu.receive_type,g.`name` AS gname,g.template,d.`name` AS dname FROM users AS u
INNER JOIN groups_users AS gu ON u.id=gu.user_id INNER JOIN groups AS g ON gu.group_id=g.id INNER JOIN
domains AS d ON gu.domain_id=d.id WHERE u.id = ?">>},

{get_users, <<"SELECT u.id, u.email,u.`type`,gu.receive_type,g.`name` AS gname,g.template,d.`name` AS dname FROM users AS u
INNER JOIN groups_users AS gu ON u.id=gu.user_id INNER JOIN groups AS g ON gu.group_id=g.id INNER JOIN
domains AS d ON gu.domain_id=d.id ">>},

{add_user, <<"insert into users(email,type) values(?,?);">>},

{add_group, <<"INSERT INTO ddrt.groups(`name`, template) VALUES (?,?);">>},

{add_report, <<"INSERT INTO ddrt.reports(user_id, content, `date`) VALUES (?,?,?)">>},

{get_report, <<"SELECT u.id AS user_id, u.email, r.content, r.`date`, g.`name` AS group_name, g.template, gu.receive_type, 
d.`name` AS domain_name, d.id AS domain_id
				FROM ddrt.reports AS r INNER JOIN ddrt.users AS u ON u.id = r.user_id 
				INNER JOIN ddrt.groups_users AS gu ON gu.user_id = u.id 
				INNER JOIN ddrt.groups AS g ON g.id = gu.group_id 
				INNER JOIN ddrt.domains AS d ON gu.domain_id = d.id 
				WHERE r.`date` BETWEEN DATE_SUB(?,INTERVAL ? DAY) AND DATE_ADD(?,INTERVAL 1 DAY) AND g.id = ? AND u.type = 'R'">>},

{get_send_users, <<"SELECT u.id, u.email, gu.receive_type FROM users AS u 
						INNER JOIN  groups_users AS gu ON gu.user_id = u.id
						WHERE gu.group_id = ?;">>},


{check_today_report, <<"SELECT r.Id FROM reports AS r  
					WHERE r.user_id =? AND r.`date` BETWEEN  ? AND DATE_ADD(?,INTERVAL 1 DAY);">>},


{get_all_reports, <<"SELECT u.id AS user_id, u.email, r.content, r.`date`, g.`name` AS group_name, g.template, gu.receive_type, 
d.`name` AS domain_name, d.id AS domain_id 
					FROM  ddrt.users AS u 
					LEFT JOIN ddrt.reports AS r ON u.id = r.user_id 
					LEFT JOIN  ddrt.groups_users as gu ON gu.user_id = u.id  
					LEFT JOIN ddrt.groups AS g ON gu.group_id = g.id 
					LEFT JOIN ddrt.domains AS d ON gu.domain_id = d.id
					WHERE (r.`date` BETWEEN DATE_SUB(?,INTERVAL ? DAY) 
					AND DATE_ADD(?,INTERVAL 1 DAY) or r.`date` IS NULL) AND g.id = ? AND u.`type` = 'R'">>},

{get_not_report_emails, <<"SELECT u.email FROM ddrt.users AS u WHERE u.`type` = 'r'
 						 AND NOT EXISTS(SELECT r.user_id FROM ddrt.reports AS r 
 						 WHERE r.`date` BETWEEN DATE_SUB(?,INTERVAL ? DAY) AND ? AND u.id = r.user_id)">>},

{get_groups, <<"SELECT id, `name` AS group_name FROM ddrt.groups">>}

]).
