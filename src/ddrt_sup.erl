-module(ddrt_sup).
-export ([start_link/0]).
-export ([init/1]).
-behaviour (supervisor).

start_link() ->
	% Here did not start a new process 
    inets:start(),
    ssl:start(),
    ddrt_db:start(),
    ok = odbc:start(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %_Server = {ddrt_timer, {ddrt_timer, start_link, []}, permanent, 2000, worker, [ddrt_timer]},
    Sup = {ddrt_group_sup, {ddrt_group_sup, start_link, []}, permanent, 2000, supervisor, [ddrt_group_sup, ddrt_sup]},
    Manager = {ddrt_group_manager, {ddrt_group_manager, start_link, []}, permanent, 2000, worker, [ddrt_group_manager]},
    MssqlSup =  {ddrt_mssql_sup, {ddrt_mssql_sup, start_link, []}, permanent, 2000, supervisor, [ddrt_mssql_sup]},
    MssqlMgr =  {ddrt_mssql_mgr, {ddrt_mssql_mgr, start_link, []}, permanent, 2000, worker, [ddrt_mssql_mgr]},
    Processes = [Sup, Manager, MssqlSup, MssqlMgr],
    {ok, {{one_for_all, 10, 60}, Processes}}.
