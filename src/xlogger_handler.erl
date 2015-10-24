-module(xlogger_handler).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behavior(gen_server).
-export([start_link/2, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-define(DEFAULT_FILE_NAME, "%YYYY-%MM-%DD/%level.log").


start_link(Id, Config)->
	gen_server:start_link(?MODULE, [Id, Config], []).

init([Id, Config])->
	CompiledMsgPatterns = get_compiled_patterns(Config),
	{ok, dict:from_list([{config, Config}, {compiled_patterns, CompiledMsgPatterns}])}.

handle_info(_, State)->
	{noreply, State}.

handle_call(_, _, State)->
	{reply, ok, State}.

handle_cast({update_config, NewConfig}, State)->
	OldConfig = dict:fetch(config, State),
	NewState = if 
		OldConfig == NewConfig ->
			State;
		true->
			CompiledMsgPatterns = get_compiled_patterns(NewConfig),
			dict:from_list([{config, NewConfig}, {compiled_patterns, CompiledMsgPatterns}])
	end,
	{noreply, NewState};
	

handle_cast({log, Params}, State)->
	Config = dict:fetch(config, State),
	CompiledMsgPatterns = dict:fetch(compiled_patterns, State),
	lists:foreach(fun(X)->
		case X of 
			{Dest, _}->
				CompiledMsgPattern = proplists:get_value(Dest, CompiledMsgPatterns),
				write(X, Params, CompiledMsgPattern);
			_->
				ok
		end
	end, proplists:get_value(dest, Config)),
	{noreply, State}.

write(Config, Params, CompiledMsgPattern)->

	M1 = xlogger_formatter:format(CompiledMsgPattern, Params),
	Msg = unicode:characters_to_list(M1),
	MsgFormatted = io_lib:format("~ts~n",[Msg]),
	case Config of
		undefined->
			ok;
		{console, _ConsoleProp}->
			io:format(MsgFormatted);
		{file, FileProp}->
			FilenamePattern = proplists:get_value(name, FileProp, ?DEFAULT_FILE_NAME),
			File = xlogger_formatter:format(xlogger_formatter:compile(FilenamePattern), Params),
			Bin = unicode:characters_to_binary(MsgFormatted),
			xlogger_file_backend:write(File, Bin, FileProp),

			ok
	end.

get_compiled_patterns(Args)->
	try
		case proplists:get_value(dest, Args) of
			Dest when is_list(Dest), length(Dest)>0 ->
				CommonMsgPattern = proplists:get_value(msg_pattern, Args),
				lists:map(fun(Handler)->
					case Handler of
						{H, HParams}->
							P = case proplists:get_value(msg_pattern, HParams) of
								undefined->
									xlogger_formatter:compile(CommonMsgPattern);
								MsgPattern->
									xlogger_formatter:compile(MsgPattern)
							end,
							{H, P};
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

code_change(_OldVsn, _State, _Extra)->
	{ok, _State}.

terminate(_Reason, _State)->
	io:format("handler terminated with reason: ~p~n",[_Reason]),
	ok.
