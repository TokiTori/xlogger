-module(xlogger_test).
-export([test/0]).

test()->
	io:format("start testing~n"),
	lists:foreach(fun(X)->
		timer:sleep(2500),
		
		xlogger:log(error, "Test message"),
		xlogger:log(error, "Test Кириллица"),
		xlogger:log(error, "Test message"),
		xlogger:log(info, "Test info message")
		
	end, lists:seq(1, 34)),
	io:format("end testing~n"),
	ok.
    