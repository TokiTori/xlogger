-module(xlogger_handler_sup).

-behaviour(supervisor).
-export([start_link/0, init/1, add_handler/2]).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_handler(HandlerName, Config)->
	ChildSpec = #{id => HandlerName, start => {xlogger_handler, start_link, [Config]}},
	supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	%% Starting supervisor without children
	SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
	{ok, {SupFlags, []}}.