%% Settings (defaults in include/emysql.hrl):
%% default_timeout (TIMEOUT = 8000)
%% lock_timeout (LOCK_TIMEOUT = 5000)

{application, emysql, [
    {description, "Emysql - Erlang MySQL driver"},
    {vsn, "0.4.1"},
    {modules, [emysql, emysql_app, emysql_auth, emysql_conn, emysql_conn_mgr, emysql_conv, emysql_statements, emysql_sup, emysql_tcp, emysql_util, emysql_worker]}, 
    {mod, {emysql_app, ["Sat Jul 19 14:13:02 CST 2014"]}},
    {registered, [emysql_conn_mgr, emysql_sup]},
    {applications, [kernel, stdlib, crypto]},
    {env, [
      {default_timeout, 20000},
      {lock_timeout,infinity},
      {conn_test_period, 28000}]}
]}.
