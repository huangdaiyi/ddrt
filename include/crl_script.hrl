-define (INSERT_DAILYHOUR, "INSERT INTO [DailyHours] ([TaskExecuteId],[Date],[SpendTime],[Activity],[Created],[CreatedUserId],[CreatedUserName],[ProcessModelId],[TaskId],[TaskName],[CommonProcessId],[Phase],[TaskType]) VALUES(0,?,?,?,GETDATE(),?,?,?,?,?,?,?,?)").
-define (UPDATE_DAILYHOUR, "UPDATE [DailyHours] SET [TaskExecuteId] = 0, [Date] = ? ,[SpendTime] = ? ,[Activity] = ?,[CreatedUserId] = ?,[CreatedUserName] = ?,[Phase] = ? WHERE [Activity] LIKE '%[AUTO#?]%'' ESCAPE '['").
-define (DELETE_DAILYHOUR, "DELETE FROM [DailyHours] WHERE [Activity] like '[AUTO#?]%' ESCAPE '['").
-define (GET_COMMON_PROCESSID, "SELECT [CommonProcessId] FROM [CrlJiraFixVerionMapping] WHERE [FixVersionName] = ?").
%-define (GET_COMMON_PROCESSID, "SELECT CommonProcessID FROM dbo.CommonProcess WITH(NOLOCK) WHERE InternalProcessID = ?").

-define (ADD_COMMONPROCESS,   "DECLARE @Created DATETIME 
				SET @Created = GETDATE() 
				DECLARE @CreatedUserId CHAR(20) 
				SET @CreatedUserId = ? 
				DECLARE @CreatedUserName VARCHAR(50) 
				SELECT TOP 1 @CreatedUserName = FullName 
				FROM dbo.USERS WITH (NOLOCK) 
				WHERE UserAccount = @CreatedUserId 
				INSERT INTO [CommonProcess] ([InternalProcessId]
				,[ProcessModelId],[BizFormType],[Subject]
				,[Created],[Submitted],[CreatedUserId]
				,[CreatedUserName],[Closed],[ClosedUserId]
				,[ClosedUserName],[Priority],[Status]
				,[LastComment],[LaunchDate]) SELECT 0,5733, ''
				,?,@Created,@Created,@CreatedUserId
				,@CreatedUserName,@Created,@CreatedUserId
				,@CreatedUserName,0,'InComplete'
				,N'Automatic Create By Jira System',@Created 
				SELECT SCOPE_IDENTITY() AS CommonProcessId;").


-define (GET_TEAM_MEMBERS, "SELECT Members FROM [TeamMembers] WITH(NOLOCK) WHERE GroupName = ?"). %% index of



% -define (macro (param), body).