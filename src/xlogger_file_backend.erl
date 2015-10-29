-module(xlogger_file_backend).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behavior(gen_server).
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-export([write/3]).
-define(FD_TIMEOUT, 30*1000).
-define(FD_EXPIRATION_CHECK_TIMEOUT, 10*1000).
-define(DEFAULT_FILE_SIZE_LIMIT, 1024*1024*6).
-define(DEFAULT_WRITE_BLOCK_SIZE, 1024*4).
-define(DEFAULT_WRITE_DELAY, 500).


start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([])->
	{ok, dict:new()}.

write(Filename, Data, FileOptions) when is_binary(Data), is_list(FileOptions)->
	gen_server:cast(?MODULE, {write, Filename, Data, FileOptions}).

handle_info(fd_check, State)->
	CurrentTime = erlang:system_time(milli_seconds),
	NotExpiredFD = dict:filter(fun(_Key, Value)->
		{IoDevice, LastActiveTime, _} = Value,
		if
			LastActiveTime + ?FD_TIMEOUT < CurrentTime ->
				file:datasync(IoDevice),
				file:close(IoDevice),
				false;
			true->
				true
		end
	end, State),
	case dict:size(NotExpiredFD) of
		0->
			ok;
		_->
			ensure_fd_timer()
	end,
	{noreply, NotExpiredFD}.

handle_call(_, _, State)->
	{reply, ok, State}.

handle_cast({write, Filename, Data, FileOptions}, State)->
	NewState = write_data(Filename, Data, FileOptions, State),
	{noreply, NewState}.

write_data(Filename, Data, FileOptions, State)->
	case get_fd(Filename, FileOptions, State) of
		{IoDevice, FileSize} when is_pid(IoDevice)->
			ensure_fd_timer(),
			file:write(IoDevice, Data),
			dict:store(Filename, {IoDevice, erlang:system_time(milli_seconds), FileSize + size(Data)}, State);
		_->
			io:format("Can't open file~n"),
			State
	end.

get_fd(Filename, FileOptions, State)->
	case dict:find(Filename, State) of
		{ok, {FD, _, CurrentSize}} when is_pid(FD)->
			case check_file_rotation(Filename, FileOptions, CurrentSize) of
				true->
					%% closing previous file descriptor and open new
					file:datasync(FD),
					file:close(FD),
					open_fd(Filename, FileOptions);
				_->
					%% check whether file exists
					case filelib:is_regular(Filename) of
						true->
							{FD, CurrentSize};
						_->
							open_fd(Filename, FileOptions)
					end
			end;
		_->
			open_fd(Filename, FileOptions)
	end.

check_file_rotation(File, FileOptions, CurrentSize)->
	case proplists:get_value(rotate, FileOptions) of
		RotateCount when is_integer(RotateCount)->
		FileSizeLimit = proplists:get_value(size, FileOptions, ?DEFAULT_FILE_SIZE_LIMIT),
			if
				CurrentSize >= FileSizeLimit->
					rotate_files(File, RotateCount),
					true;
				true->
					false
			end;
		_->
			false
	end.

rotate_files(Filename, RotateCount) when RotateCount<1->
	file:delete(Filename);

rotate_files(Filename, RotateCount)->
	RotatedFileBase = lists:concat([Filename, "."]),
	ExistedFiles = filelib:wildcard(lists:concat([RotatedFileBase, "*"])),

	ExistedContinuousIndexesSequence = lists:takewhile(fun(X)->
		SeqFilename = lists:concat([RotatedFileBase, X]),
		lists:member(SeqFilename, ExistedFiles)
	end, lists:seq(1, RotateCount)),

	OrderedExistedIndexes = lists:reverse(lists:sort(lists:flatten(ExistedContinuousIndexesSequence))),
	lists:foreach(fun(Index)->
		IndexedFilename = lists:concat([RotatedFileBase, Index]),
		if
			Index < RotateCount->
				NewFilename = lists:concat([RotatedFileBase, Index + 1]),
				file:rename(IndexedFilename, NewFilename);
			true->
				file:delete(IndexedFilename)
		end
	end, OrderedExistedIndexes),
	NewMainFilename = lists:concat([RotatedFileBase, "1"]),
	file:rename(Filename, NewMainFilename).

open_fd(Filename, FileOptions)->
	filelib:ensure_dir(Filename),
	FileSize = filelib:file_size(Filename),
	WriteBlockSize = proplists:get_value('write_block_size', FileOptions, ?DEFAULT_WRITE_BLOCK_SIZE),
	WriteDelay = proplists:get_value('write_delay', FileOptions, ?DEFAULT_WRITE_DELAY),
	IoDevice = case file:open(Filename, [write, append, {delayed_write, WriteBlockSize, WriteDelay}]) of
		{ok, FD}->
			FD;
		_->
			undefined
	end,
	{IoDevice, FileSize}.

ensure_fd_timer()->
	case get(fd_check_timer) of 
		undefined->
			create_fd_check_timer();
		TimerRef->
			case erlang:read_timer(TimerRef) of 
				false->
					create_fd_check_timer();
				_->
					ok
			end,
			ok
	end.

create_fd_check_timer()->
	TimerRef = erlang:send_after(?FD_EXPIRATION_CHECK_TIMEOUT, self(), fd_check),
	put(fd_check_timer, TimerRef).

code_change(_OldVsn, _State, _Extra)->
	{ok, _State}.

terminate(_Reason, State)->
	lists:foreach(fun(X)->
		{IoDevice, _} = X,
		file:datasync(IoDevice),
		file:close(IoDevice)
	end, dict:to_list(State)),
	ok.