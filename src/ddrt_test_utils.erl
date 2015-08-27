-module(ddrt_test_utils).

-ifdef (TEST).
-export ([test_setup/1, test_teardown/1]).
-include_lib("eunit/include/eunit.hrl").

test_setup(Modules) when is_list(Modules) ->
    lists:foreach(fun(Mod) ->  catch meck:new(Mod) end, Modules);
test_setup(Module) ->
    test_setup([Module]).

test_teardown(Modules)  when is_list(Modules)->
    lists:foreach(fun(Mod) -> ok = meck:unload(Mod) end, Modules);
test_teardown(Module)->
    test_teardown([Module]).
-endif.