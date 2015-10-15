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

get_config()->
	case get_config_from_module() of
		Cfg when is_list(Cfg), length(Cfg)>0->
			Cfg;
		undefined->
			get_config_from_env()
	end.

get_config_from_module()->
	case code:ensure_loaded(xlogger_config) of 
		{module, Module}->
			case erlang:function_exported(Module, get_config, 0) of
				true->
					try
						erlang:apply(Module, get_config, [])
					catch
						Type:What->
							io:format("Can't fetch config from module. ~p:~p~n\t~p~n",[Type, What, erlang:get_stacktrace()]),
							undefined
					end;
				_->
					undefined
			end;
		_->
			undefined
	end.

get_config_from_env()->
	case application:get_all_env(xlogger) of 
		Env when is_list(Env), length(Env)>0->
			Env;
		_->
			undefined
	end.

get_config(HandlerName)->
	case get_config() of 
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
		{Handler, NewState} = case dict:find(HandlerName, State) of 
			{ok, HandlerPid} when is_pid(HandlerPid)->
				IsAlive = is_process_alive(HandlerPid),
				if
					IsAlive ->
						{HandlerPid, State};
					true->
						{ok, NewHandlerPid} = xlogger_handler_sup:add_handler(HandlerName, get_config(HandlerName)),
						{NewHandlerPid, dict:store(HandlerName, NewHandlerPid, State)}
				end;
			_->
				{ok, HandlerPid} = xlogger_handler_sup:add_handler(HandlerName, get_config(HandlerName)),
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