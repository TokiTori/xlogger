-module(xlogger_app).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	A = xlogger_sup:start_link(),
	spawn(xlogger_test, test, []),
	A.


stop(_State) ->
	ok.