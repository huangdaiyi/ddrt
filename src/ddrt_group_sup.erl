-module(ddrt_group_sup).
-export ([start_link/0, start_child/1]).
-export ([init/1]).
-behaviour (supervisor).

%%%================================================
%%% supervisor callbacks
%%%================================================

init([]) ->
    Server = {ddrt_group, {ddrt_group, start_link, []}, temporary, 2000, worker, [ddrt_timer]},
    Processes = [Server],
    {ok, {{simple_one_for_one, 10, 60}, Processes}}.


%%%================================================
%%% public methods
%%%================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Group) ->
    supervisor:start_child(?MODULE, [Group]).




