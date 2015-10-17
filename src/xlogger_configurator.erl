-module(xlogger_configurator).
-behavior(gen_server).
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-export([get_config/0]).
-define(CONFIG_MODULE, xlogger_config).
-define(CONFIG_FUNCTION, get_config).


start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([])->
	Config = reload(?CONFIG_MODULE),
	{ok, Config}.

handle_info(reload, _OldConfig)->
	case reload(?CONFIG_MODULE) of 
		_OldConfig->
			{noreply, _OldConfig};
		NewConfig->
			io:format("Configuration changed!~n"),
			{noreply, NewConfig}
	end.

handle_call(get_config, _From, _State)->
	{reply, _State, _State};

handle_call(_, _From, _State)->
	{reply, ok, _State}.

handle_cast(_, _State)->
	{noreply, _State}.


get_config()->
	case gen_server:call(?MODULE, get_config) of
		Cfg when is_list(Cfg), length(Cfg)>0->
			Cfg;
		undefined->
			get_config_from_env()
	end.

get_config_from_env()->
	case application:get_all_env(xlogger) of 
		Env when is_list(Env), length(Env)>0->
			Env;
		_->
			undefined
	end.
	

reload(Module)->
	code:purge(Module),
	Config = case code:load_file(Module) of 
		{module, Module}->
			apply_config(Module, ?CONFIG_FUNCTION);
		{error, What}->
			undefined
	end,
	erlang:send_after(2000, self(), reload),
	Config.

apply_config(Module, Function)->
	try
		case erlang:function_exported(Module, Function, 0) of
			true->
				case erlang:apply(Module, Function, []) of
					Config when is_list(Config)->
						Config;
					_->
						undefined
				end;
			false->
				io:format("Function ~p/0 has not exported in module ~p~n",[Function, Module]),
				undefined
		end
	catch
		Type:What->
			io:format("Can't fetch config ~p:~p~n\t~p~n",[Type, What, erlang:get_stacktrace()]),
			undefined
	end.

code_change(_OldVsn, _State, _Extra)->
	{ok, _State}.

terminate(_Reason, _State)->
	io:format("Terminated with reason: ~p~n",[_Reason]),
	ok.