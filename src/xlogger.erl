%% @author Mikhail Yashkov <mike25@ya.ru>
%% @doc That is the interface to log messages via xlogger. 
%% You can use strings or formatted messages with arguments to save your data to log files
%% with specified log level and handler.

-module(xlogger).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behavior(gen_server).
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-export([
		log/1, log/2, log/3, info/2, debug/2, warning/2, error/2, 
		log/4, info/3, debug/3, warning/3, error/3
	]).


%% ===================================================================
%% External API
%% ===================================================================

start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ===================================================================
%% External API for prepared messages
%% ===================================================================

%% @doc Send message to default handler with 'info' log level
%% @spec log(Msg::string()) -> ok
log(Msg)->
	log(info, Msg).

%% @doc Send message to default handler
%% @spec log(Level::atom(), Msg::string()) -> ok
log(Level, Msg)->
	log(default, Level, Msg).

%% @doc Send message
%% @spec log(HandlerName::atom(), Level::atom(), Msg::string()) -> ok
log(HandlerName, Level, Msg)->
	{UserModule, ExecutedModule} = get_module_name(),
	Params = [
		{handler, HandlerName}, 
		{level, Level}, 
		{msg, Msg}, 
		{pid, self()},
		{user_module, UserModule},
		{module, ExecutedModule},
		{time, calendar:local_time()}
	],
	gen_server:cast(?MODULE, {log, Params}).

%% @doc Send info message
%% @spec info(HandlerName::atom(), Msg::string()) -> ok
info(HandlerName, Msg)->
	log(HandlerName, info, Msg).

%% @doc Send debug message
%% @spec debug(HandlerName::atom(), Msg::string()) -> ok
debug(HandlerName, Msg)->
	log(HandlerName, debug, Msg).

%% @doc Send warning message
%% @spec warning(HandlerName::atom(), Msg::string()) -> ok
warning(HandlerName, Msg)->
	log(HandlerName, warning, Msg).

%% @doc Send error message
%% @spec error(HandlerName::atom(), Msg::string()) -> ok
error(HandlerName, Msg)->
	log(HandlerName, error, Msg).

%% ===================================================================
%% External API for format message with arguments
%% ===================================================================

%% @doc Send formatted message with arguments
%% @spec log(HandlerName::atom(), Level::atom(), Format::string(), Args::list()) -> ok
log(HandlerName, Level, Format, Args)->
	FormattedMessage = lists:flatten(io_lib:format(Format, Args)),
	log(HandlerName, Level, FormattedMessage).

%% @doc Send formatted info message with arguments
%% @spec info(HandlerName::atom(), Format::string(), Args::list()) -> ok
info(HandlerName, Format, Args)->
	log(HandlerName, info, Format, Args).

%% @doc Send formatted debug message with arguments
%% @spec debug(HandlerName::atom(), Format::string(), Args::list()) -> ok
debug(HandlerName, Format, Args)->
	log(HandlerName, debug, Format, Args).

%% @doc Send formatted warning message with arguments
%% @spec warning(HandlerName::atom(), Format::string(), Args::list()) -> ok
warning(HandlerName, Format, Args)->
	log(HandlerName, warning, Format, Args).

%% @doc Send formatted error message with arguments
%% @spec error(HandlerName::atom(), Format::string(), Args::list()) -> ok
error(HandlerName, Format, Args)->
	log(HandlerName, error, Format, Args).

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

get_module_name([Head | Tail], ExcludedModules)->
	ModuleName = erlang:element(1, Head),
	case lists:member(ModuleName, ExcludedModules) of
		true->
			get_module_name(Tail, ExcludedModules);
		_->
			ModuleName
	end.


%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init([])->
	{ok, dict:new()}.

handle_info(_, _State)->
	{noreply, _State}.

handle_call(_, _, _State)->
	{reply, ok, _State}.

handle_cast({log, Params}, State)->
	xlogger_handler_sup:dispatch(Params),
	{noreply, State};

handle_cast(_, _State)->
	{noreply, _State}.

code_change(_OldVsn, _State, _Extra)->
	{ok, _State}.

terminate(_Reason, _State)->
	ok.
