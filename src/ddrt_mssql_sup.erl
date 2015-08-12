-module (ddrt_mssql_sup).
-export ([start_link/0, start_child/1]).
-export ([init/1]).
-behaviour (supervisor).

%%%================================================
%%% supervisor callbacks
%%%================================================

init(_State) ->
    Server = {ddrt_mssql, {ddrt_mssql, start_link, []}, permanent, 2000, worker, [ddrt_mssql]},
    Processes = [Server],
    {ok, {{simple_one_for_one , 10, 60}, Processes}}.


%%%================================================
%%% public methods
%%%================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Conn) ->
    supervisor:start_child(?MODULE, [Conn]).





