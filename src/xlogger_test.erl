-module(xlogger_test).
-export([test/0]).

test()->
	test1().
	
test1()->
	% handler_sup:dispatch(handler1, error, lists:concat(["Test message"])).

	lists:foreach(fun(X)->
		timer:sleep(2500),
		% H = application:get_env(handlers),
		% io:format("Handlers: ~p~n",[H]),
		xlogger:log(handler1, error, "Test message"),
		xlogger:log(handler2, error, "Test message")
		% io:format("ASd")
	end, lists:seq(1, 34)),
	% timer:sleep(75*1000),
	% lists:foreach(fun(X)->
	% 	timer:sleep(1500),
	% 	logger:log(handler1, error, "Test message"),
	% 	logger:log(handler2, error, "Test message")
	% 	% handler_sup:dispatch(handler1, error, lists:concat(["Test message #",X]))
	% 	% handler_sup:dispatch(handler1, info, lists:concat(["Test message #",X]))
	% end, lists:seq(1, 34)),
	ok.
    