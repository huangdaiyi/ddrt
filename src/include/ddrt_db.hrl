-define(DB_SCRIPT,[
%%% ===================================================================
%%% User
%%% ===================================================================
{add_group, <<"INSERT INTO groups(group_name) VALUE(?);">>},
{get_users, <<"SELECT * FROM users;">>}
]).
