-module (ddrt_mspool).
-behaviour (gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,  terminate/2]).
-export ([function/arity]).

-record (mssql_pools, {current_index = 0, pools = dict:new() :: dict()}).

%%%================================================
%%% gen_server callbacks
%%%================================================
init([Count]) ->
    {ok, Count, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    %State = #mssql_pools{ pools = initialize_pools(dict:new(), State)},
    {noreply, #mssql_pools{ pools = initialize_pools(dict:new(), State)}};
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


initialize_pools(Pools, Count) when Count < 1 -> Pools; 
initialize_pools(Pools, Count) ->
	initialize_pools( dict:store(Count, getRef(), Pools), Count - 1).


getRef() ->
	{ok, ConnectStr} = neg_hydra:get_env(crl_connect),
	connect(ConnectStr).

connect(ConnectStr) ->
	{ok, ConnectRef} = odbc:connect(ConnectStr, [{sql}]),
	ConnectStr.
