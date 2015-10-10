-module(xlogger_file_backend).
-behavior(gen_server).
-export([start_link/0, init/1, handle_cast/2, handle_info/2, terminate/2]).
-export([write/2, write/3]).

-include("const.hrl").

start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([])->
	{ok, dict:new()}.

write(File, Data)->
	write(File, Data, []).

write(File, Data, WriteDelay) when is_list(File), is_binary(Data), is_integer(WriteDelay)->
	write(File, Data, [{write_delay, WriteDelay}]);

write(File, Data, Options) when is_list(File), is_binary(Data), is_list(Options)->
	gen_server:cast(?MODULE, {write, File, Data, Options}).

handle_info(fd_check, State)->
	CurrentTime = erlang:system_time(milli_seconds),
	NotExpiredFD = dict:filter(fun(Key, Value)->
		{IoDevice, LastActiveTime} = Value,
		if
			LastActiveTime + ?FD_TIMEOUT < CurrentTime ->
				file:datasync(IoDevice),
				file:close(IoDevice),
				io:format("Closing ~p~n",[Key]),
				false;
			true->
				true
		end
	end, State),
	case dict:size(NotExpiredFD) of
		0->
			io:format("timer not needed~n"),
			ok;
		_->
			io:format("upgrade timer~n"),
			ensure_fd_timer()
	end,
	{noreply, NotExpiredFD}.

handle_cast({write, File, Data, Options}, State)->
	NewState = write_data(File, Data, Options, State),
	ensure_fd_timer(),
	{noreply, NewState}.

write_data(File, Data, Options, State)->
	{NewState, IoDevice} = get_fd(File, Options, State),
	WriteResult = file:write(IoDevice, Data),
	NewState.

get_fd(File, Options, State)->
	IoDevice = case dict:find(File, State) of
		{ok, {FD, _}}->
			FD;
		_->
			WriteBlockSize = proplists:get_value('write_block_size', Options, ?DEFAULT_WRITE_BLOCK_SIZE),
			WriteDelay = proplists:get_value('write_delay', Options, ?DEFAULT_WRITE_DELAY),
			{ok, FD} = file:open(File, [write, append, {delayed_write, WriteBlockSize, WriteDelay}]),
			FD
	end,
	ensure_fd_timer(),
	NewState = dict:store(File, {IoDevice, erlang:system_time(milli_seconds)}, State),
	{NewState, IoDevice}.

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

terminate(Reason, State)->
	io:format("Terminate: ~p~n",[Reason]),
	ok.