-module(xlogger_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, dispatch/1]).

%% Supervisor callbacks
-export([init/1]).

-define(DEFAULT_HANDLER_CONFIG, 
	[
		{dest, [
			{console, []},
			{file, [{name, "logs/default.%level.log"}]}
		]}
	]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dispatch(Params)->
	HandlerName = proplists:get_value(handler, Params),
	{Name, Config} = check_handler(HandlerName),
	HandlerPid = get_handler(Name, Config),
	gen_server:cast(HandlerPid, {log, Params}),
	ok.

check_handler(HandlerName)->
	case application:get_env(handlers) of 
		{ok, Handlers} when is_list(Handlers)->
			case proplists:get_value(HandlerName, Handlers) of 
				undefined->
					{default, ?DEFAULT_HANDLER_CONFIG};
				Config->
					{HandlerName, Config}
			end;
		_->	
			{default, ?DEFAULT_HANDLER_CONFIG}
	end.

get_handler(HandlerName, Config)->
	Children = supervisor:which_children(?MODULE),
	HandlerPid = case lists:keysearch(HandlerName, 1, Children) of
		{value, {_, Pid, _, _}}->
			Pid;
		_->
			case add_handler(HandlerName, Config) of
				{ok, H} when is_pid(H)->
					H;
				_->
					io:format("can't start handler~n")
			end
	end.

add_handler(HandlerName, Config)->
	ChildSpec = #{id => HandlerName, start => {xlogger_handler, start_link, [HandlerName, Config]}},
	supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	%% Starting supervisor without children
	SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
	{ok, {SupFlags, []}}.