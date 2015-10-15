-module(xlogger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Specs = [
		#{id => xlogger_front, start => {xlogger_front, start_link, []}},
		#{id => xlogger_handler_sup, start => {xlogger_handler_sup, start_link, []}},
		#{id => xlogger_file_backend, start => {xlogger_file_backend, start_link, []}}
    ],
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    {ok, {SupFlags, Specs}}.

