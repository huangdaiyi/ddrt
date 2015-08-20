-module (ddrt_mssql_mgr).
-behaviour (gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,  terminate/2]).

-export ([start_link/0, get_worker/0, execute/2, execute_sync/2]).
-define (SERVER, ?MODULE).
-record (mssql_worker, {size=5, index=0, workers = []}).

%%%================================================
%%% public api
%%%================================================
start_link() ->
	%%io:format("~w", [Size]),
    gen_server:start_link({local, ?MODULE}, ?SERVER, [] , []).

get_worker() -> gen_server:call(?SERVER, get).

execute(Sql, Params) ->
	Worker = get_worker(),
	ddrt_mssql:execute(Worker, Sql, Params).
	

execute_sync(Sql, Params) ->
	Worker = get_worker(),
	ddrt_mssql:execute_sync(Worker, Sql, Params).

% update(Pid, ConnectStr) ->
% 	gen_server:cast(?SERVER, {remove, Pid, ConnectStr}).



%%%================================================
%%% gen_server callbacks
%%%================================================
init(_Args) ->
    {ok, undefined, 0}.

handle_call(get, _From, State) ->
	{Worker, NewState} = get_worker(State),
	{reply, Worker, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
	
handle_cast({update, Pid, ConnectStr}, #mssql_worker{workers = Workers} = State) ->
	NewWorkers = update_work(Workers, Pid, ConnectStr),
	{noreply, State#mssql_worker{workers=NewWorkers}};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(timeout, undefined) ->
	{Size, ConnectStr} = parse_config(),
    {noreply, #mssql_worker{size = Size, workers=initialize_mssql([], ConnectStr, Size)}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
	 ok.


get_worker(#mssql_worker{index = Index, size=Size, workers = Workers} = State) ->
	NewIndex = Index + 1,
	case NewIndex > Size of
		true ->
			CurrentIndex = 1;
		false ->
			CurrentIndex = NewIndex
	end,
	{lists:nth(CurrentIndex, Workers), State#mssql_worker{index = CurrentIndex}}.


update_work(Workers, Pid, ConnectStr) ->
	RestWorkers = lists:delete(Pid, Workers),
	{ok, NewWork} = ddrt_mssql_sup:start_child(ConnectStr),
	[NewWork | RestWorkers].


initialize_mssql(Workers, _, Size) when Size < 1 -> Workers; 
initialize_mssql(Workers, ConnectStr, Size) ->
	WorkerName = generate_name(Size),
	ddrt_mssql_sup:start_child(WorkerName, ConnectStr),
	initialize_mssql([ WorkerName | Workers], ConnectStr, Size - 1).

parse_config() ->
	{ok, Config} = neg_hydra:get_env(mssql_pool),

	% ConnectStr = lists:flatten(io_lib:format("DRIVER={SQL Server};SERVER=~s;DATABASE=~s;UID=~s;PWD=~s",
	% 					[proplists:get_value(host, Config),
	% 					 proplists:get_value(database, Config),
	% 					 proplists:get_value(user, Config),
	% 					 proplists:get_value(password, Config)
	% 					])),

	{proplists:get_value(size, Config), proplists:get_value(connection, Config)}.

generate_name(Num) ->
	list_to_atom(string:join(["ddrt_mssql", integer_to_list(Num)], "_")).
