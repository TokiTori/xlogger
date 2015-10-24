-module(xlogger_app).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    xlogger_sup:start_link().

stop(_State) ->
    ok.