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
	Result = try
		case code:which(Module) of
			ModuleWhich when is_list(ModuleWhich)->
				LastModified = filelib:last_modified(ModuleWhich),
				OldLastModified = get(last_modified),
				case LastModified of
					OldLastModified->
						not_changed;
					_->
						put(last_modified, LastModified),
						case code:ensure_loaded(Module) of
							{module, Module}->
								get_config(Module, ?CONFIG_FUNCTION);
							{error, Msg}->
								xlogger:error(default, "Can't load module ~p", [Msg]),
								undefined
						end
				end;
			E->
				throw(E)
		end
	catch
		Type:What->
			io:format("Can't find configuration module. ~p:~p~n\t~p~n",[Type, What, erlang:get_stacktrace()]),
			undefined
	end,
	erlang:send_after(2000, self(), reload),
	Result.

get_config(Module, Function)->
	try
		case erlang:function_exported(Module, Function, 0) of
			true->
				case erlang:apply(Module, Function, []) of
					Config when is_list(Config)->
						{new_config, validate_config(Config)};
					_->
						undefined
				end;
			false->
				xlogger:error(default, "function ~p/0 has not exported in module ~p",[Function, Module]),
				undefined
		end
	catch
		{function_not_exported, M, F}->
			xlogger:error(default, "Filter function ~p:~p is not exported", [M, F]);
		{module_or_function_bad_name, M, F}->
			xlogger:error(default, "Module name or Function name of filter is not atom ~p:~p", [M, F]);
		handlers_not_found->
			xlogger:error(default, "Handlers not found");
		handler_destinations_not_found->
			xlogger:error(default, "Handler's destinations not found");
		Type:What->
			xlogger:error(default, "can't get config. ~p: ~p~n\t~p",[Type, What, erlang:get_stacktrace()]),
			undefined
	end.

validate_config(Config)->
	ValidatedHandlers = case proplists:get_value(handlers, Config) of
		Handlers when is_list(Handlers)->
			HandlersWithoutDefault = proplists:delete(default, Handlers),
			lists:map(fun(Handler)->
				validate_handler(Handler)
			end, HandlersWithoutDefault);
		_->
			throw(handlers_not_found)
	end,
	lists:flatten([proplists:delete(handlers, Config), {handlers, ValidatedHandlers}]).

validate_handler({HandlerName, HandlerProps})->
	NewDests = case proplists:get_value(dest, HandlerProps) of
		Dests when is_list(Dests), length(Dests)>0 ->
			lists:map(fun(Dest)->
				validate_dest(Dest, HandlerProps)
			end, Dests);
		_->
			throw(handler_destinations_not_found)
	end,
	{HandlerName, lists:flatten([proplists:delete(dest, HandlerProps), {dest, NewDests}])}.

validate_dest({DestName, Props}, Handler)->
	%% using handler's msg_pattern if it not exists in dest
	MsgPattern = proplists:get_value(msg_pattern, Props, proplists:get_value(msg_pattern, Handler)),
	PropsWithMsgPattern = replace(msg_pattern, MsgPattern, Props, undefined),

	%% using handler's msg_size_limit if it not exists in dest
	MsgSizeLimit = proplists:get_value(msg_size_limit, Props, proplists:get_value(msg_size_limit, Handler)),
	PropsWithMsgSizeLimit = replace(msg_size_limit, MsgSizeLimit, PropsWithMsgPattern, undefined),


	%% merging handler's filters and dest's filters
	Filters = lists:flatten([proplists:get_value(filters, Props, []) | proplists:get_value(filters, Handler, [])]),
	validate_filters(Filters),
	PropsWithFilters = replace(filters, Filters, PropsWithMsgSizeLimit, []),
	{DestName, PropsWithFilters};

validate_dest(_,_)->
	[].

validate_filters(Filters)->
	lists:foreach(fun({Module, Function} = X)->
		case is_atom(Module) andalso is_atom(Function) of
			true->
				ExportedFunctions = Module:module_info(exports),
				case lists:keyfind(Function, 1, ExportedFunctions) of 
					{Function, 1}->
						true;
					_->
						throw({function_not_exported, Module, Function})
				end;
			_->
				throw({module_or_function_bad_name, Module, Function})
		end
	end, Filters).


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
	Cfg = case reload(?CONFIG_MODULE) of
		{new_config, Config}->
			xlogger_handler_sup:apply_configuration(Config);
		_->
			[]
	end,
	{ok, Cfg}.

handle_info(reload, _OldConfig)->
	case reload(?CONFIG_MODULE) of
		{new_config, NewConfig}->
			xlogger:info(default, "xlogger configuration changed"),
			xlogger_handler_sup:apply_configuration(NewConfig),
			{noreply, NewConfig};
		_->
			{noreply, _OldConfig}
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
	xlogger:error(default, "~p terminated with reason: ~p",[?MODULE, Reason]),
	ok.