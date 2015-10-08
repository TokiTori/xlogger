-module(xlogger_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, config_change/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    A = xlogger_sup:start_link(),
    io:format("logging...~n"),
    
    % handler_sup:add_handler([{id, "handler1"}, {filename, "log_file_xyz.log"}]),
    % logger:error(handler1, "Shit"),
    io:format("logged~n"),
    
    spawn(xlogger_test, test, []),
    A.

stop(_State) ->
    ok.

config_change(Changed, New, Removed)->
	io:format("Changed: ~p~n",[Changed]),
	io:format("New: ~p~n",[New]),
	io:format("Removed: ~p~n",[Removed]),
	ok.