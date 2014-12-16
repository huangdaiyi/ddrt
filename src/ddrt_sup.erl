-module(ddrt_sup).
-export ([start_link/0]).
-export ([init/1]).
-behaviour (supervisor).

start_link() ->
    ddrt_db:start(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3600, 60}, []}}.
