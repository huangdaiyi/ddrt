-module (ddrt_mssql).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,  terminate/2]).
-export ([start_link/1, execute/3, execute_sync/3]).
-record (connect, {connect, handler}).

%%%================================================
%%% public api
%%%================================================
start_link(Conn) ->
	%io:format("~p",[Conn]),
    gen_server:start_link(?MODULE, [Conn] , []).

execute(Worker, Sql, Params) ->
	gen_server:call(Worker, {exetuce, Sql, Params}).

execute_sync(Worker, Sql, Params) ->
	gen_server:cast(Worker, {sync_exetuce, Sql, Params}).

%%%================================================
%%% gen_server callbacks
%%%================================================
init([Conn]) ->
	%io:format("~p",[Conn]),
    {ok, #connect{connect=Conn}, 0}.

handle_call({exetuce, Sql, Params}, _From, State) ->
	{NewConn, Result} = exetuce_odbc(State, Sql, Params),
	{reply, Result, NewConn};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({sync_exetuce, Sql, Params}, State) ->
	{NewConn, _} = exetuce_odbc(State, Sql, Params),
	{noreply, NewConn};
	
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(timeout,  #connect{connect=Conn} = State) ->

    {noreply, State#connect{ handler = get_connect(Conn)}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
	 ok.


exetuce_odbc(#connect{handler=ConnectRef, connect=Conn} = Connect, Sql, Params) ->
	case odbc:param_query(ConnectRef, Sql, Params) of
		{error, connection_closed} ->
			exetuce_odbc(Connect#connect{handler=get_connect(Conn)}, Sql, Params);
		% {error, Reason} -> error;
		Result  -> {Connect, Result}
	end.

get_connect(Conn) ->
	{ok, ConnectRef} = odbc:connect(Conn, []),
	ConnectRef.