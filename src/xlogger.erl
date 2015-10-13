-module(xlogger).

-export([log/1, log/2, log/3, log/4, 
	info/2, info/3,
	debug/2, debug/3,
	warning/2, warning/3,
	error/2, error/3]).

log(Msg)->
	log(default, info, Msg).

log(Level, Msg)->
	log(default, Level, Msg).

log(Handler, Level, Format, Args)->
	IOF = lists:flatten(io_lib:format(Format, Args)),
	log(Handler, Level, IOF).

log(Handler, Level, Msg)->
	{UserModule, ExecutedModule} = get_module_name(),
	xlogger_handler_sup:dispatch([
		{handler, Handler}, 
		{level, Level}, 
		{msg, Msg}, 
		{pid, self()},
		{user_module, UserModule},
		{module, ExecutedModule}
	]).

info(Handler, Msg)->
	log(Handler, info, Msg).

info(Handler, Format, Args)->
	log(Handler, info, Format, Args).

debug(Handler, Msg)->
	log(Handler, debug, Msg).

debug(Handler, Format, Args)->
	log(Handler, debug, Format, Args).

warning(Handler, Msg)->
	log(Handler, warning, Msg).

warning(Handler, Format, Args)->
	log(Handler, warning, Format, Args).

error(Handler, Msg)->
	log(Handler, error, Msg).

error(Handler, Format, Args)->
	log(Handler, error, Format, Args).

get_module_name()->
	try 
		erlang:error("that is not error"),
		{undefined, undefined}
	catch
		_:_->
			LD = code:lib_dir(),
			ExcludeModules = lists:map(fun({ModuleName, Path})->
				case Path of 
					_ when is_list(Path)->
						case lists:prefix(LD, Path) of 
							true->
								ModuleName;
							_->
								[]
						end;
					_->
						ModuleName
				end
			end, code:all_loaded()),
			UserModule = get_module_name(erlang:get_stacktrace(), lists:flatten([ExcludeModules, ?MODULE])),
			ExecutedModule = get_module_name(erlang:get_stacktrace(), [?MODULE]),
			{UserModule, ExecutedModule}
	end.

get_module_name([], _)->
	undefined_module;

get_module_name([Head | Tail] = Stacktrace, ExcludedModules)->
	ModuleName = erlang:element(1, Head),
	case lists:member(ModuleName, ExcludedModules) of
		true->
			get_module_name(Tail, ExcludedModules);
		_->
			ModuleName
	end.