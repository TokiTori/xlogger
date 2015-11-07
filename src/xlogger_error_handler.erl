-module(xlogger_error_handler).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behavior(gen_event).
-export([start_link/0, init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3, format_status/2]).

start_link()->
	S = gen_event:start_link({local, ?MODULE}),
	error_logger:add_report_handler(xlogger_error_handler),
	S.

init([])->
	{ok, []}.

handle_event(Event, State)->
	case Event of 
		{error, _, {_, Format, Data}}->
			xlogger:error(default, Format, Data);
		{error_report, _, {_, _, Report}}->
			xlogger:error(default, Report);
		{warning_msg, _, {_, Format, Data}}->
			xlogger:warning(default, Format, Data);
		{warning_report, _, {_, _, Report}}->
			xlogger:warning(default, Report);
		{info_msg, _, {_, Format, Data}}->
			xlogger:info(default, Format, Data);
		{info_report, _, {_, _, Report}}->
			xlogger:info(default, Report)
	end,
	{ok, State}.

handle_call(Request, State)->
	{ok, reply_call, State}.

handle_info(Info, State)->
	{ok, State}.

terminate(Args, State)->
	ok.

code_change(OldVsn, State, Extra)->
	{ok, State}.

format_status(Opt, [PDict, State])->
	ok.