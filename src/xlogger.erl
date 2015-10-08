-module(xlogger).

-export([info/2, debug/2, warning/2, error/2, log/3]).

log(Handler, Level, Msg)->
	% ModuleName = get_module_name(),
	xlogger_handler_sup:dispatch([{handler, Handler}, {level, Level}, {msg, Msg}, {pid, self()}]).

info(Handler, Msg)->
	log(Handler, info, Msg).

debug(Handler, Msg)->
	log(Handler, debug, Msg).

warning(Handler, Msg)->
	log(Handler, warning, Msg).

error(Handler, Msg)->
	log(Handler, error, Msg).

% get_module_name()->
% 	try 
% 		erlang:error("that is not error"),
% 		undefined
% 	catch
% 		_:_->
% 			% io:format("Backtrace: ~s~n", [element(2, process_info(self(), backtrace))]),
% 			% io:format("Trace: ~s~n", [process_info(self(), {trace, 2})]),
% 			% io:format("root_dir: ~p~n",[code:root_dir()]),
% 			io:format("lib_dir: ~p~n",[code:lib_dir()]),
% 			AllLoaded = code:all_loaded(),
% 			io:format("AllLoaded: ~p~n",[AllLoaded]),
% 			% Loaded = erlang:loaded(),
% 			% io:format("Loaded: ~p~n",[Loaded]),
% 			% io:format("AllLoaded: ~p~n",[AllLoaded]),
% 			LD = code:lib_dir(),
% 			ExcludeModules = lists:map(fun({ModuleName, Path})->
% 				case Path of 
% 					_ when is_list(Path)->
% 						case lists:prefix(LD, Path) of 
% 							true->
% 								ModuleName;
% 							_->
% 								[]
% 						end;
% 					_->
% 						ModuleName
% 				end
% 			end, code:all_loaded()),
% 			get_module_name(erlang:get_stacktrace(), lists:flatten([ExcludeModules, ?MODULE]))
% 	end.

% get_module_name([], _)->
% 	undefined;

% get_module_name([Head | Tail] = Stacktrace, ExcludedModules)->
% 	ModuleName = erlang:element(1, Head),
% 	case lists:member(ModuleName, ExcludedModules) of
% 		true->
% 			get_module_name(Tail, ExcludedModules);
% 		_->
% 			ModuleName
% 	end.