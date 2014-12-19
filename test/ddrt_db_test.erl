-include_lib(“eunit/include/eunit.hrl”).
-include ("include/ddrt.hrl").
-include ("include/ddrt_db.hrl").
-export ([select_test()/0]).

select_test()->
	?assertEqual(ok,ddrt_db:update(add_user,["enunit@newegg.com","R"])).



