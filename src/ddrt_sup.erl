-module(ddrt_sup).
-export ([start_link/0]).
-export ([init/1]).
-behaviour (supervisor).

start_link() ->
	% Here did not start a new process 
    inets:start(),
    ddrt_db:start(),
    odbc:start(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %_Server = {ddrt_timer, {ddrt_timer, start_link, []}, permanent, 2000, worker, [ddrt_timer]},
    Sup = {ddrt_group_sup, {ddrt_group_sup, start_link, []}, permanent, 2000, supervisor, [ddrt_group_sup, ddrt_sup]},
    Manager = {ddrt_group_manager, {ddrt_group_manager, start_link, []}, permanent, 2000, worker, [ddrt_group_manager]},
    Processes = [Sup, Manager],
    {ok, {{one_for_all, 10, 60}, Processes}}.
