-include_lib("eunit/include/eunit.hrl").
-module (ddrt_db_test).
-include ("include/ddrt.hrl").
-include ("include/ddrt_db.hrl").
-export ([select_test/0]).

select_test()->
	% ?assertEqual(ok,ddrt_db:update(add_user,["enunit@newegg.com","R"])).
		?assertEqual(ok,ddrt_db:t()).

