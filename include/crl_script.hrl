-define (INSERT_DAILYHOUR, "INSERT INTO [DailyHours] 
								([TaskExecuteId],[Date],[SpendTime],[Activity],[Created],[CreatedUserId],
								[CreatedUserName],[ProcessModelId],[TaskId],[TaskName],[CommonProcessId],[Phase],[TaskType])
							VALUES(0,?,?,?,GETDATE(),?,?,?,?,?,?,?,?)").

-define (UPDATE_DAILYHOUR(WorklogId), "UPDATE [DailyHours] SET [Date] = ?, [SpendTime] = ? ,[Activity] = ? WHERE [Activity] LIKE '%[AUTO#"++ WorklogId ++"]%' ESCAPE '['").

-define (DELETE_DAILYHOUR(WorklogId), "DELETE FROM [DailyHours] WHERE [Activity] LIKE '%[AUTO#"++WorklogId++"]%' ESCAPE '['").

-define (GET_COMMON_PROCESSID, "SELECT [CommonProcessId] FROM [CrlJiraFixVerionMapping] WHERE [FixVersionName] = ?").
-define (GET_PROCESSID_BY_INTERNAL, "SELECT [CommonProcessId] FROM [CommonProcess] WITH(NOLOCK) WHERE [InternalProcessId] = ?").
-define (EXIST_COMMONPROCESS, "SELECT TOP 1 [CommonProcessId] FROM [CommonProcess] WITH(NOLOCK) WHERE [CommonProcessId] = ?").

-define (ADD_COMMONPROCESS_MAP,   "DECLARE @Created DATETIME 
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
				,[LastComment],[LaunchDate]) VALUES (0,5733, ''
				, ? ,@Created,@Created,@CreatedUserId
				,@CreatedUserName,@Created,@CreatedUserId
				,@CreatedUserName,0,'InComplete' ,N'Automatic Create By Jira System',@Created);
				INSERT INTO [dbo].[CrlJiraFixVerionMapping]([CommonProcessId],[FixVersionName])VALUES(@@IDENTITY, ?)").


-define (GET_TEAM_BY_MEMBER(Member), "SELECT [GroupName] FROM [TeamMembers] WITH(NOLOCK) WHERE CHARINDEX('" ++ Member ++ "', [Members]) <> 0"). %% index of

-define (GET_CRLNO_BY_GROUP, "SELECT [CrlNumber] FROM [TeamCrlMapping] WITH(NOLOCK) where [GroupName] = ? AND [Activity] = ?").

% -define (macro (param), body).