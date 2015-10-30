-module(xlogger_configurator).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behavior(gen_server).
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-define(CONFIG_MODULE, xlogger_config).
-define(CONFIG_FUNCTION, get_config).


%% ===================================================================
%% External API
%% ===================================================================

start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ===================================================================
%% Internal API
%% ===================================================================

get_config_from_env()->
	case application:get_all_env(xlogger) of 
		Env when is_list(Env), length(Env)>0->
			Env;
		_->
			undefined
	end.

reload(Module)->
	code:purge(Module),
	code:load_file(Module),
	%% TODO check module's modification time
	Config = case code:ensure_loaded(Module) of 
		{module, Module}->
			get_config(Module, ?CONFIG_FUNCTION);
		{error, What}->
			xlogger:log(error, "can't load module ~p",[What]),
			undefined
	end,
	erlang:send_after(2000, self(), reload),
	Config.

get_config(Module, Function)->
	try
		case erlang:function_exported(Module, Function, 0) of
			true->
				case erlang:apply(Module, Function, []) of
					Config when is_list(Config)->
						validate_config(Config);
					_->
						undefined
				end;
			false->
				xlogger:log(error, "function ~p/0 has not exported in module ~p",[Function, Module]),
				undefined
		end
	catch
		Type:What->
			xlogger:log(error, "can't get config. ~p:~p~n\t~p",[Type, What, erlang:get_stacktrace()]),
			undefined
	end.

validate_config(Config)->
	ValidatedHandlers = case proplists:get_value(handlers, Config) of
		Handlers when is_list(Handlers)->
			lists:map(fun(Handler)->
				validate_handler(Handler)
			end, Handlers);
		_->
			error("Handlers not found")
	end,
	lists:flatten([proplists:delete(handlers, Config), {handlers, ValidatedHandlers}]).

validate_handler({HandlerName, HandlerProps})->
	NewDests = case proplists:get_value(dest, HandlerProps) of
		Dests when is_list(Dests), length(Dests)>0 ->
			lists:map(fun(Dest)->
				validate_dest(Dest, HandlerProps)
			end, Dests);
		_->
			error("Handler's destinations not found")
	end,
	{HandlerName, lists:flatten([proplists:delete(dest, HandlerProps), {dest, NewDests}])}.

validate_dest({DestName, Props}, Handler)->
	%% using handler's msg_pattern if it not exists in dest
	MsgPattern = proplists:get_value(msg_pattern, Props, proplists:get_value(msg_pattern, Handler)),
	PropsWithMsgPattern = replace(msg_pattern, MsgPattern, Props, undefined),

	%% merging handler's filters and dest's filters
	Filters = lists:flatten([proplists:get_value(filters, Props, []) | proplists:get_value(filters, Handler, [])]),
	PropsWithFilters = replace(filters, Filters, PropsWithMsgPattern, []),
	{DestName, PropsWithFilters};

validate_dest(_,_)->
	[].

replace(Key, Value, Props, IgnoreValue)->
	PropsWithoutKey = proplists:delete(Key, Props),
	case Value of
		IgnoreValue->
			PropsWithoutKey;
		_->
			lists:flatten([PropsWithoutKey, {Key, Value}])
	end.


%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init([])->
	Config = reload(?CONFIG_MODULE),
	xlogger_handler_sup:apply_configuration(Config),
	{ok, Config}.

handle_info(reload, _OldConfig)->
	case reload(?CONFIG_MODULE) of
		_OldConfig->
			{noreply, _OldConfig};
		NewConfig->
			xlogger:log(info, "xlogger configuration changed"),
			xlogger_handler_sup:apply_configuration(NewConfig),
			{noreply, NewConfig}
	end.

handle_call(get_config, _From, _State)->
	{reply, _State, _State};

handle_call(_, _From, _State)->
	{reply, ok, _State}.

handle_cast(_, _State)->
	{noreply, _State}.

code_change(_OldVsn, _State, _Extra)->
	{ok, _State}.

terminate(Reason, _State)->
	xlogger:log(error, "~p terminated with reason: ~p",[?MODULE, Reason]),
	ok.