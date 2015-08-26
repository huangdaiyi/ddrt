-module(ddrt_group_sup).
-export ([start_link/0, start_child/1]).
-export ([init/1]).
-behaviour (supervisor).

%%%================================================
%%% supervisor callbacks
%%%================================================

init([]) ->
    Server = {ddrt_group, {ddrt_group, start_link, []}, temporary, 2000, worker, [ddrt_group]},
    Processes = [Server],
    {ok, {{simple_one_for_one, 10, 60}, Processes}}.


%%%================================================
%%% public methods
%%%================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Group) ->
    supervisor:start_child(?MODULE, [Group]).


-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
init_test() ->
    Strategy = {{simple_one_for_one, 10, 60}, [{ddrt_group, {ddrt_group, start_link, []}, temporary, 2000, worker, [ddrt_group]}]},
    ?assertEqual({ok, Strategy}, init([])).
-endif.
