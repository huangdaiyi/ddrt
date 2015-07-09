
-module (erlydtl_demo).
-export ([test/1,testList/0]).

test(TemplatePath)->
    erlydtl:compile(TemplatePath, template_parser),
    template_parser:render([
        {name, hardy()},
        {friends, [<<"hardy">>, <<"tim">>]},
        {primes, [1, 2, "3", <<"5">>]}
    ]).

hardy() ->
    "hardy".

testList() ->
    Info = [{name, call("name")}, {"age", call(24)}, {"sex", call("man")}],
    io:format("~p~n", [Info]).

call(Name) ->
    Name.

% bindMethod(Module) ->
%     Module:say()