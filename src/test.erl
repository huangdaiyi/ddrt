-module (test).

-define (INSERT_DAILYHOUR, "INSERT INTO [DailyHours] ([TaskExecuteId],[Date],[SpendTime],[Activity],[Created],[CreatedUserId],
	[CreatedUserName],[ProcessModelId],[TaskId],[TaskName],[CommonProcessId],[Phase],[TaskType]) 
VALUES(0, ?, ?,?,GETDATE(),?,?,?,?,?,?,?,?)").

-define (INSERT_MAP, "DECLARE @Created DATETIME 
				SET @Created = GETDATE() 
				DECLARE @CreatedUserId CHAR(20) 
				SET @CreatedUserId = 'hh49' 
				DECLARE @CreatedUserName VARCHAR(50) 
				SELECT TOP 1 @CreatedUserName = FullName 
				FROM dbo.USERS WITH (NOLOCK) 
				WHERE UserAccount = @CreatedUserId 
				INSERT INTO [CommonProcess] ([InternalProcessId]
				,[ProcessModelId],[BizFormType],[Subject]
				,[Created],[Submitted],[CreatedUserId]
				,[CreatedUserName],[Closed],[ClosedUserId]
				,[ClosedUserName],[Priority],[Status]
				,[LastComment],[LaunchDate]) VALUES (0,5733, ''
				,'Project For Jira',@Created,@Created,@CreatedUserId
				,@CreatedUserName,@Created,@CreatedUserId
				,@CreatedUserName,0,'InComplete' ,N'Automatic Create By Jira System',@Created);
				INSERT INTO [dbo].[CrlJiraFixVerionMapping]([CommonProcessId],[FixVersionName])VALUES(@@IDENTITY, 'test')").

-export ([start/0, insert/0, insert_crl/0]).


start() ->
	odbc:start().

insert_crl() ->
	Conn = "DRIVER={SQL Server};SERVER=s7kms01;DATABASE=CRL2006;UID=CrlReadOnly;PWD=Git123!@#;CHARSET=UFT-8",
	ConnectRef =  get_connect(Conn),
	Params = [ {{sql_char,20},["hh49"]}],
    odbc:param_query(ConnectRef, ?INSERT_MAP, Params).



insert() ->
	 
	Conn = "DRIVER={SQL Server};SERVER=s7kms01;DATABASE=CRL2006;UID=CrlReadOnly;PWD=Git123!@#;CharacterSet=UTF-8",
	%Conn = "DSN=worklog;DATABASE=CRL2006;UID=CrlReadOnly;PWD=Git123!@#;Client_CSet=UTF-8",
	ConnectRef =  get_connect(Conn),

	Params = [ {{sql_varchar, 23},[list_to_binary(ddrt_utils:get_mssql_day_string())]}, {{sql_decimal,15,2},[ddrt_utils:to_float("8")]},
                                     {{sql_wvarchar,200},  [list_to_binary(ddrt_utils:to_sql_wvarchar("[AUTO#419290]wefsdafsdfsdfds中文测试"))]},
                                     {{sql_char,20},[<<"hh49">>]},
                                     {{sql_wvarchar,50},  [<<"中国"/utf8>>]},
                                     {sql_integer,[2]},
                                     {sql_integer,[105]},
                                     {{sql_varchar,50},[<<"Developers Coding">>]},
                                     {sql_integer,[1009292]},
                                     {sql_integer,[24]},
                                     {sql_integer,[31]}],

    odbc:param_query(ConnectRef, ?INSERT_DAILYHOUR, Params).
   
    

 get_connect(Conn) ->
	{ok, ConnectRef} = odbc:connect(Conn, [{binary_strings, on}]),
	ConnectRef.