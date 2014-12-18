-module(ddrt_sup).
-export ([start_link/0]).
-export ([init/1]).
-behaviour (supervisor).

start_link() ->
    ddrt_db:start(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	Server = {ddrt_timer, {ddrt_timer, start_link, []}, permanent, 2000, worker, [ddrt_timer]},
	Processes = [Server],
    {ok, {{one_for_one, 3600, 60}, Processes}}.
