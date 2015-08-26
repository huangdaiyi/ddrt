-module(ddrt_group_manager).
-behaviour (gen_server).
-include ("include/ddrt.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,  terminate/2]).
-export([start_link/0]).
-define (DDRT_GROUP, ddrt_group_mapper).
%%%================================================
%%% gen_server callbacks
%%%================================================
init(Timeout) ->
    TableId = ddrt_ets_db:new(?DDRT_GROUP),
    {ok, TableId, Timeout}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    lists:foreach(fun(Group)-> 
        {ok, Pid} = ddrt_group_sup:start_child(Group),
        MonitorRef = erlang:monitor(process, Pid),
        true = ddrt_ets_db:insert(State, {MonitorRef, Pid, Group})
    end, ddrt_db:get_groups()),
    {noreply, State};
handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, State) ->
    [{_, _, Group}] = ddrt_ets_db:lookup(State, MonitorRef),
    {ok, NewPid} = ddrt_group_sup:start_child(Group),
    NewMonitorRef = erlang:monitor(process, NewPid),
    true = ddrt_ets_db:delete(State, MonitorRef),
    true = ddrt_ets_db:insert(State, {NewMonitorRef, NewPid, Group}),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("~p~n", [_Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%================================================
%%% public methods
%%%================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,  20 * 1000, []).


-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ddrt_test_utils:test_setup([ddrt_ets_db]),
    TableId = 1,
    Timeout = 5000,
    ok = meck:expect(ddrt_ets_db, new, fun(_) -> TableId end),
    ?assertEqual({ok, TableId, Timeout}, init(Timeout)),
    ddrt_test_utils:test_teardown([ddrt_ets_db]).

handle_call_test()->
    ?assertEqual({reply, ok, undefined}, handle_call(undefined, undefined, undefined)).

handle_cast_test()->
    ?assertEqual({noreply, undefined}, handle_cast(undefined, undefined)).

handle_info2_test() ->
    ddrt_test_utils:test_setup([ddrt_group_sup, ddrt_ets_db, ddrt_db]),
    ok = meck:expect(ddrt_group_sup, start_child, fun(_) -> {ok, self()} end),
    ok = meck:expect(ddrt_ets_db, insert, fun(_, _) -> true end),
    ok = meck:expect(ddrt_db, get_groups, fun() -> [undefined, undefined] end),
    ?assertEqual({noreply, undefined}, handle_info(timeout, undefined)),
    ddrt_test_utils:test_teardown([ddrt_group_sup, ddrt_ets_db, ddrt_db]).

handle_info3_test() ->
    ddrt_test_utils:test_setup([ddrt_group_sup, ddrt_ets_db]),
    ok = meck:expect(ddrt_group_sup, start_child, fun(_) -> {ok, self()} end),
    ok = meck:expect(ddrt_ets_db, insert, fun(_, _) -> true end),
    ok = meck:expect(ddrt_ets_db, delete, fun(_, _) -> true end),
    ok = meck:expect(ddrt_ets_db, lookup, fun(_, _) -> [{undefined, undefined, undefined}] end),
    ?assertEqual({noreply, undefined}, handle_info({'DOWN', undefined, process, undefined, undefined}, undefined)),
    ddrt_test_utils:test_teardown([ddrt_group_sup, ddrt_ets_db]).
    
handle_info_test() ->
    ?assertEqual({noreply, undefined}, handle_info(undefined, undefined)).

code_change_test()->
    ?assertEqual({ok, undefined}, code_change(undefined, undefined, undefined)).

terminate_test() ->
    ?assertEqual(ok, terminate(undefined, undefined)).
-endif.