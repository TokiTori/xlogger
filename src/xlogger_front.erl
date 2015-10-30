-module(xlogger_front).
-author("Mikhail Yashkov <mike25@ya.ru>").

-behavior(gen_server).
-export([start_link/0, init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).
-export([dispatch/1]).

%% ===================================================================
%% API
%% ===================================================================

dispatch(Params)->
	gen_server:cast(?MODULE, {log, Params}).

%% ===================================================================
%% Gen Server callbacks
%% ===================================================================

start_link()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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