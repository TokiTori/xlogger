-module(xlogger_handler_sup).

-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([apply_configuration/1, add_handler/2, dispatch/1]).
-define(DEFAULT_HANDLER_CONFIG,
	[
		{dest, [
			{console, []},
			{file, [{name, "logs/default.%level.log"}, {rotate, 6}, {size, 5242880}]}
		]},
		{msg_pattern, "[%HH:%mm:%ss] %uptime: %msg"}
	]).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dispatch(Params)->
	HandlerName = proplists:get_value(handler, Params),
	Handler = get_handler(HandlerName),
	gen_server:cast(Handler, {log, Params}).

apply_configuration(Configuration)->
	Handlers = proplists:get_value(handlers, Configuration),
	update_handlers(Handlers).

%% ===================================================================
%% Internal functions
%% ===================================================================

update_handlers([])->
	ok;
update_handlers([Handler | Rest])->
	try
		{HandlerName, HandlerConfig} = Handler,
		Children = supervisor:which_children(?MODULE),
		case lists:keyfind(HandlerName, 1, Children) of
			false->
				add_handler(HandlerName, HandlerConfig);
			ChildTuple->
				HandlerPid = element(2, ChildTuple),
				gen_server:cast(HandlerPid, {update_config, HandlerConfig}),
				ok
		end
	catch
		Type:What->
			io:format("~p:~p~n\t~p~n",[Type,What, erlang:get_stacktrace()])
	end,
	update_handlers(Rest).

get_handler(HandlerName)->
	Children = supervisor:which_children(?MODULE),
	case lists:keyfind(HandlerName, 1, Children) of
		false->
			ensure_default_handler(Children);
		ChildTuple->
			element(2, ChildTuple)
	end.

add_handler(HandlerName, Config)->
	ChildSpec = #{id => HandlerName, start => {xlogger_handler, start_link, [Config]}},
	supervisor:start_child(?MODULE, ChildSpec).

ensure_default_handler(Children)->
	case lists:keyfind(default, 1, Children) of
		false->
			add_handler(default, ?DEFAULT_HANDLER_CONFIG);
		ChildTuple->
			element(2, ChildTuple)
	end.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	%% Starting supervisor without children
	SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
	{ok, {SupFlags, []}}.