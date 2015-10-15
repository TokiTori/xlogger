-module(xlogger).

-export([
		log/1, log/2, log/3, info/2, debug/2, warning/2, error/2, 
		log_format/2, log_format/3, log_format/4, info_format/3, debug_format/3, warning_format/3, error_format/3
	]).

%% ===================================================================
%% API for prepared messages
%% ===================================================================

log(Msg)->
	log(info, Msg).

log(Level, Msg)->
	log(default, Level, Msg).

log(Handler, Level, Msg)->
	{UserModule, ExecutedModule} = get_module_name(),
	xlogger_front:dispatch([
		{handler, Handler}, 
		{level, Level}, 
		{msg, Msg}, 
		{pid, self()},
		{user_module, UserModule},
		{module, ExecutedModule},
		{time, calendar:local_time()}
	]).

info(Handler, Msg)->
	log(Handler, info, Msg).

debug(Handler, Msg)->
	log(Handler, debug, Msg).

warning(Handler, Msg)->
	log(Handler, warning, Msg).

error(Handler, Msg)->
	log(Handler, error, Msg).

%% ===================================================================
%% API for format message with arguments
%% ===================================================================

log_format(Format, Args)->
	log_format(info, Format, Args).

log_format(Level, Format, Args)->
	log_format(default, Level, Format, Args).

log_format(Handler, Level, Format, Args)->
	FormattedMessage = lists:flatten(io_lib:format(Format, Args)),
	log(Handler, Level, FormattedMessage).

info_format(Handler, Format, Args)->
	log_format(Handler, info, Format, Args).
	
debug_format(Handler, Format, Args)->
	log_format(Handler, debug, Format, Args).
	
warning_format(Handler, Format, Args)->
	log_format(Handler, warning, Format, Args).

error_format(Handler, Format, Args)->
	log_format(Handler, error, Format, Args).

%% @doc Trying to determine module which executes xlogger
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
			Stacktrace = erlang:get_stacktrace(),
			UserModule = get_module_name(Stacktrace, lists:flatten([ExcludeModules, ?MODULE])),
			ExecutedModule = get_module_name(Stacktrace, [?MODULE]),
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