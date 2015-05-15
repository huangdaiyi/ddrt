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
    TableId = ets:new(?DDRT_GROUP, []),
    {ok, TableId, Timeout}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    lists:foreach(fun(Group)-> 
        {ok, Pid} = ddrt_group_sup:start_child(Group),
        MonitorRef = erlang:monitor(process, Pid),
        true = ets:insert(State, {MonitorRef, Pid, Group})
    end, ddrt_db:get_groups()),
    {noreply, State};
handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, State) ->
    [{_, _, Group}] = ets:lookup(State, MonitorRef),
    {ok, NewPid} = ddrt_group_sup:start_child(Group),
    NewMonitorRef = erlang:monitor(process, NewPid),
    true = ets:delete(State, MonitorRef),
    true = ets:insert(State, {NewMonitorRef, NewPid, Group}),
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