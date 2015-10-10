-module(xlogger_handler).
-behavior(gen_server).
-export([start_link/2, init/1, handle_cast/2]).

-include("const.hrl").

start_link(Id, Config)->
	io:format("Config:~p~n",[Config]),
	gen_server:start_link({local, Id}, ?MODULE, [Id | Config], []).

init([Id | Config])->
	io:format("handler ~p started! with Config: ~p~n", [Id, Config]),
	CompiledMsgPatterns = get_compiled_patterns(Config),
	put(handler_id, Id),
	{ok, dict:from_list([{config, Config}, {compiled_patterns, CompiledMsgPatterns}])}.

handle_cast({log, Params}, State)->
	% io:format("handle_cast dict: ~p~n",[State]),
	% io:format("handler ~p: ~p~n",[get(handler_id), self()]),
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
	Msg = unicode:characters_to_list(xlogger_formatter:format(CompiledMsgPattern, Params)),
	MsgFormatted = io_lib:format("~ts~n",[Msg]),
	case Config of
		undefined->
			io:format("file undefined~n"),
			undefined;
		{console, ConsoleProp}->
			% Msg = unicode:characters_to_list(xlogger_formatter:format(CompiledMsgPattern, Params)),
			io:format(MsgFormatted),
			ok;
		{file, FileProp}->
			FilenamePattern = proplists:get_value(name, FileProp, ?DEFAULT_FILE_NAME),
			File = xlogger_formatter:format(xlogger_formatter:compile(FilenamePattern), Params),
			filelib:ensure_dir(File),

			case proplists:get_value(rotate, FileProp) of
				RotateCount when is_integer(RotateCount)->
					FileSizeLimit = proplists:get_value(size, FileProp, ?DEFAULT_FILE_SIZE_LIMIT),
					CurrentFileLength = get_current_file_length(File),
					if
						CurrentFileLength >= FileSizeLimit->
							rotate_files(File, RotateCount),
							set_current_file_length(0);
						true->
							ok
					end;
				_->
					ok
			end,
			Bin = unicode:characters_to_binary(MsgFormatted),
			WriteDelay = proplists:get_value(write_delay, FileProp, ?DEFAULT_WRITE_DELAY),
			xlogger_file_backend:write(File, Bin, WriteDelay),

			increase_current_file_length(File, size(Bin)),
			File
	end.

rotate_files(File, RotateCount)->
	RotatedFileBase = lists:concat([File, "."]),
	FullSequence = lists:map(fun(X)->
		lists:concat([RotatedFileBase, X])
	end, lists:seq(1, RotateCount)),

	RotateIndexSize = length(integer_to_list(RotateCount)),
	WC = lists:concat([RotatedFileBase, lists:duplicate(RotateIndexSize, "?")]),
	ExistedFiles = filelib:wildcard(WC),

	TrueSequence = lists:takewhile(fun(X)->
		lists:member(X, ExistedFiles)
	end, FullSequence),

	OrderedFiles = lists:reverse(lists:sort(TrueSequence)),
	BaseSize = length(File),
	lists:foreach(fun(X)->
		Index = string:substr(X, BaseSize + 2, length(X) - BaseSize),
		IndexInt = list_to_integer(Index),
		if
			IndexInt < RotateCount->
				NewFilename = lists:concat([RotatedFileBase, IndexInt + 1]),
				file:rename(X, NewFilename);
			true->
				file:delete(X)
		end
	end, OrderedFiles),
	NewMainFilename = lists:concat([RotatedFileBase, "1"]),
	file:rename(File, NewMainFilename).


get_current_file_length(File)->
	case get(current_file_length) of
		undefined->
			Size = filelib:file_size(File),
			put(current_file_length, Size),
			Size;
		L->
			L
	end.

set_current_file_length(Value)->
	put(current_file_length, Value).


increase_current_file_length(File, Count)->
	put(current_file_length, get_current_file_length(File) + Count).


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
									CommonMsgPattern;
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