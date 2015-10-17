-module(xlogger_front).

-behavior(gen_server).
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-export([dispatch/1]).
-define(DEFAULT_HANDLER_CONFIG, 
	[
		{dest, [
			{console, []},
			{file, [{name, "logs/default.%level.log"}, {rotate, 6}, {size, 5242880}]}
		]}, 
		{msg_pattern, "[%HH:%mm:%ss] %uptime: %msg"}
	]).


start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([])->
	{ok, dict:new()}.

dispatch(Params)->
	gen_server:cast(?MODULE, {log, Params}).

get_config(HandlerName)->
	case xlogger_configurator:get_config() of 
		Config when is_list(Config), length(Config)>0->
			HandlersConfig = proplists:get_value(handlers, Config, []),
			proplists:get_value(HandlerName, HandlersConfig, ?DEFAULT_HANDLER_CONFIG);
		_->
			?DEFAULT_HANDLER_CONFIG
	end.

handle_info(_, _State)->
	{noreply, _State}.

handle_call(_, _, _State)->
	{reply, ok, _State}.

handle_cast({log, Params}, State)->
	ResultState = try
		HandlerName = proplists:get_value(handler, Params),
		HandlerConfig = get_config(HandlerName),
		{Handler, NewState} = case dict:find(HandlerName, State) of 
			{ok, HandlerPid} when is_pid(HandlerPid)->
				IsAlive = is_process_alive(HandlerPid),
				if
					IsAlive ->
						gen_server:cast(HandlerPid, {update_config, HandlerConfig}),
						{HandlerPid, State};
					true->
						case xlogger_handler_sup:add_handler(HandlerName, HandlerConfig) of
							{ok, NewHandlerPid} ->
								{NewHandlerPid, dict:store(HandlerName, NewHandlerPid, State)};
							{error, {already_started, NewHandlerPid}}->
								{NewHandlerPid, dict:store(HandlerName, NewHandlerPid, State)}
						end
				end;
			_->
				{ok, HandlerPid} = xlogger_handler_sup:add_handler(HandlerName, HandlerConfig),
				{HandlerPid, dict:store(HandlerName, HandlerPid, State)}
		end,
		gen_server:cast(Handler, {log, Params}),
		NewState
	catch
		Type:What->
			io:format("~p:~p~n\t~p",[Type, What, erlang:get_stacktrace()])
	end,
	{noreply, ResultState};

handle_cast(_, _State)->
	{noreply, _State}.

code_change(_OldVsn, _State, _Extra)->
	{ok, _State}.

terminate(_Reason, _State)->
	ok.