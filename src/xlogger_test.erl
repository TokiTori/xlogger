-module(xlogger_test).
-export([test/0]).

test()->
	io:format("start testing~n"),
	lists:foreach(fun(X)->
		timer:sleep(2500),
		
		try
			A = 10/0
		catch
			Type:What->
				xlogger:info(handler1, "That is ~p message ~p",["formatted", [{type, Type}, {what, What}, {trace, erlang:get_stacktrace()}]])
		end,

		xlogger:error(handler1, "Format numbers ~p, strings ~p, lists ~p, binaries ~p and other structures ~p",
			[6, "some string as arg", [1,a,2,3,b,c], <<"that is binary">>, [1,{2,3,[4,{a,b},c],d}]])
		
		% xlogger:log(error, "Test message"),
		% xlogger:log(error, "Test Кириллица"),
		% xlogger:log(error, "Test message"),
		% xlogger:log(info, "Test info message")

		
	end, lists:seq(1, 34)),
	io:format("end testing~n"),
	ok.
    