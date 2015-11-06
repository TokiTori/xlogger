-module(xlogger_handler).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behavior(gen_server).
-export([start_link/1, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-define(DEFAULT_FILE_NAME, "%YYYY-%MM-%DD/%level.log").


%% ===================================================================
%% External API
%% ===================================================================

start_link(Config)->
	gen_server:start_link(?MODULE, [Config], []).


%% ===================================================================
%% Internal API
%% ===================================================================

check_filters(Params, [{Module, Function} | RestFilters] = Filters) when is_list(Filters), length(Filters)>0->
	case catch erlang:apply(Module, Function, [Params]) of
		true->
			check_filters(Params, RestFilters);
		{'EXIT', ErrorMsg}->
			xlogger:error(default, "Can't check filter ~p:~p~n\tError: ~p", [Module, Function, ErrorMsg]);
		_->
			false
	end;

check_filters(_, _)->
	true.

write(Config, Params, CompiledMsgPattern)->
	M1 = xlogger_formatter:format(CompiledMsgPattern, Params),
	Msg = unicode:characters_to_list(M1),
	case Config of
		undefined->
			ok;
		{console, ConsoleProp}->
			TruncatedMsg = truncate_msg(Msg, ConsoleProp),
			io:format(io_lib:format("~ts~n",[TruncatedMsg]));
		{file, FileProp}->
			FilenamePattern = proplists:get_value(name, FileProp, ?DEFAULT_FILE_NAME),
			File = xlogger_formatter:format(xlogger_formatter:compile(FilenamePattern), Params),
			TruncatedMsg = truncate_msg(Msg, FileProp),
			Bin = unicode:characters_to_binary(io_lib:format("~ts~n",[TruncatedMsg])),
			xlogger_file_backend:write(File, Bin, FileProp),
			ok
	end.

get_compiled_patterns(Args)->
	try
		case proplists:get_value(dest, Args) of
			Dest when is_list(Dest), length(Dest)>0 ->
				lists:map(fun(Handler)->
					case Handler of
						{H, HParams}->
							{H, xlogger_formatter:compile(proplists:get_value(msg_pattern, HParams))};
						_->
							[]
					end
				end, Dest);
			_->
				[]
		end
	catch
		Type:What->
			io:format("Error: ~p, ~p, ~p~n",[Type, What, erlang:get_stacktrace()]),
			[]
	end.

truncate_msg(Msg, Props)->
	case proplists:get_value(msg_size_limit, Props) of
		Limit when is_integer(Limit), Limit>0->
			string:substr(Msg, 1, Limit);
		_->
			Msg
	end.


%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

init([Config])->
	process_flag(trap_exit, true),
	CompiledMsgPatterns = get_compiled_patterns(Config),
	{ok, dict:from_list([{config, Config}, {compiled_patterns, CompiledMsgPatterns}])}.

handle_info(_, State)->
	{noreply, State}.

handle_call(_, _, State)->
	{reply, ok, State}.

handle_cast({update_config, NewConfig}, State)->
	NewState = try
		OldConfig = dict:fetch(config, State),
		if
			OldConfig == NewConfig ->
				State;
			true->
				CompiledMsgPatterns = get_compiled_patterns(NewConfig),
				dict:from_list([{config, NewConfig}, {compiled_patterns, CompiledMsgPatterns}])
		end
	catch
		Type:What->
			io:format("~p:~p~n\t~p",[Type, What, erlang:get_stacktrace()])
	end,
	{noreply, NewState};
	

handle_cast({log, Params}, State)->
	try
		Config = dict:fetch(config, State),
		CompiledMsgPatterns = dict:fetch(compiled_patterns, State),
		lists:foreach(fun(X)->
			case X of
				{Dest, DestProps}->
					case check_filters(Params, proplists:get_value(filters, DestProps)) of 
						true->
							CompiledMsgPattern = proplists:get_value(Dest, CompiledMsgPatterns),
							write(X, Params, CompiledMsgPattern);
						_->
							ok
					end;
				_->
					ok
			end
		end, proplists:get_value(dest, Config))
	catch
		Type:What->
			io:format("~p:~p~n\t~p",[Type, What, erlang:get_stacktrace()])
	end,
	{noreply, State};

handle_cast(C, State)->
	{noreply, State}.

code_change(_OldVsn, _State, _Extra)->
	{ok, _State}.

terminate(_Reason, _State)->
	io:format("handler terminated with reason: ~p~n",[_Reason]),
	ok.