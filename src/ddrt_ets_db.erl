-module(ddrt_ets_db).
-export([new/1, insert/2, lookup/2, delete/2]).

new(TableName) ->
    ets:new(TableName, []).

insert(TableName, Data) ->
    ets:insert(TableName, Data).

lookup(TableName, Key) ->
    ets:lookup(TableName, Key).

delete(TableName, Key) ->
    ets:delete(TableName, Key).