-module(xlogger_handler_sup).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([apply_configuration/1, add_handler/2, dispatch/1]).
-define(DEFAULT_HANDLER_CONFIG,
	[
		{dest, [
			{console, []},
			{file, [{name, "logs/xlogger.%level.log"}, {rotate, 6}, {size, 5242880}]}
		]},
		{msg_pattern, "[%HH:%mm:%ss] %uptime: %msg"}
	]).


%% ===================================================================
%% External API
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

dispatch(Params)->
	HandlerName = proplists:get_value(handler, Params),
	Handler = get_handler(HandlerName),
	gen_server:cast(Handler, {log, Params}),
	ok.

apply_configuration(Configuration)->
	try
		Handlers = proplists:get_value(handlers, Configuration),
		Children = supervisor:which_children(?MODULE),
		update_handlers(Handlers, Children),
		remove_old_handlers(Handlers, Children)
	catch
		What:Why->
			io:format("Can't update configuration. ~p:~p~n\t~p~n",[What, Why, erlang:get_stacktrace()])
	end.

remove_old_handlers(Handlers, Children)->
	lists:foreach(fun(Child)->
		ExistedHandlerName = element(1, Child),
		case ExistedHandlerName of
			default->
				ok;
			_->
				case proplists:get_value(ExistedHandlerName, Handlers) of
					undefined->
						supervisor:terminate_child(?MODULE, ExistedHandlerName),
						supervisor:delete_child(?MODULE, ExistedHandlerName);
					_->
						ok
				end
		end
	end, Children).


%% ===================================================================
%% Internal API
%% ===================================================================

update_handlers([Handler | Rest], Children)->
	{HandlerName, HandlerConfig} = Handler,
	case lists:keysearch(HandlerName, 1, Children) of
		{value, ChildTuple}->
			HandlerPid = element(2, ChildTuple),
			gen_server:cast(HandlerPid, {update_config, HandlerConfig});
		_->
			add_handler(HandlerName, HandlerConfig)
	end,
	update_handlers(Rest, Children);

update_handlers(R, _)->
	ok.

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
			{ok, Pid} = add_handler(default, ?DEFAULT_HANDLER_CONFIG),
			Pid;
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