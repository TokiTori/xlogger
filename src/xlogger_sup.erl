-module(xlogger_sup).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behaviour(supervisor).
-export([start_link/0, init/1]).


%% ===================================================================
%% External API
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Specs = [
		#{id => xlogger_file_backend, start => {xlogger_file_backend, start_link, []}},
		#{id => xlogger_handler_sup, start => {xlogger_handler_sup, start_link, []}},
		#{id => xlogger_configurator, start => {xlogger_configurator, start_link, []}},
		#{id => xlogger, start => {xlogger, start_link, []}}
    ],
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    {ok, {SupFlags, Specs}}.

