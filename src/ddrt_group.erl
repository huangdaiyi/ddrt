-module(ddrt_group).
-behaviour (gen_server).
-include ("include/ddrt.hrl").

-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).
-export([start_link/1]).
-define (TIMEOUT, 60 * 1000).
-record(status, {group, remind_span=[], report_span}).

%%%================================================
%%% gen_server callbacks
%%%================================================

init(#groups{scheduling_name=Name}=Group) ->
    {RemindSpan, ReportSpan} = ddrt_db:get_scheduling(Name),
    {ok, #status{group=Group, remind_span=RemindSpan, report_span=ReportSpan} , 10}.

terminate(_Reason, _State) ->
    ok.

handle_cast(stop, State) ->
    {stop, normal, State}.


handle_call(_Message, _From, State) ->
    {reply, ok, State, ?TIMEOUT}.

handle_info(timeout, #status{remind_span=RemindSpan, report_span=ReportSpan, group=Group}=State) ->
    {Hour, Minute} = ddrt_utils:local_time(),
    Minutes = Hour * 60 + Minute,
    check_time(Minutes, RemindSpan, fun() -> ddrt_email:send_remind(Group) end),
    check_time(Minutes, ReportSpan, fun() -> ddrt_email:send_mail(Group) end),
    {noreply, State, ?TIMEOUT}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State, ?TIMEOUT}.    

%%%================================================
%%% public methods
%%%================================================

start_link(Group) ->
    gen_server:start_link(?MODULE, Group, []).


%%%================================================
%%% private methods
%%%================================================

check_time(Minutes, Span, Fn) ->
    Pred = fun({H, M}) -> 
        Timeout = ?TIMEOUT/120000,
        Ms = (H * 60 + M) ,
        (Ms - Timeout) =< Minutes andalso (Ms + Timeout) > Minutes 
    end,
    case lists:any(Pred, Span) of
        true -> Fn();
        false -> pass
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
handle_info_test()->
    ddrt_test_utils:test_setup([ddrt_email, ddrt_utils]),
    ok = meck:expect(ddrt_email, send_remind, fun(_) -> true end),
    ok = meck:expect(ddrt_utils, local_time, fun() -> {0, 0} end),
    State = #status{remind_span=[{0, 0}], report_span=[{1, 0}]},
    ?assertEqual({noreply, State, ?TIMEOUT}, handle_info(timeout, State)),

    ddrt_test_utils:test_teardown([ddrt_email, ddrt_utils]).

init_test() ->
    ddrt_test_utils:test_setup([ddrt_db]),
    ok = meck:expect(ddrt_db, get_scheduling, fun(_) -> {[{0,0}], [{1,0}]} end),
    Group = #groups{},
    State = #status{group=Group, remind_span=[{0, 0}], report_span=[{1, 0}]},
    ?assertEqual({ok, State, 10}, init(Group)),

    ddrt_test_utils:test_teardown([ddrt_db]).

terminate_test() ->
    ?assertEqual(ok, terminate(undefined, undefined)).

handle_cast_test() ->
    ?assertEqual({stop, normal, undefined}, handle_cast(stop, undefined)).

handle_call_test() ->
    ?assertEqual({reply, ok, undefined, ?TIMEOUT}, handle_call(undefined, undefined, undefined)).

code_change_test() ->
    ?assertEqual({ok, undefined, ?TIMEOUT}, code_change(undefined, undefined, undefined)).
-endif.