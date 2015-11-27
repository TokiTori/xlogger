-module(xlogger_formatter).
-author("Mikhail Yashkov <mike25@ya.ru>").

-export([compile/1, format/2]).
-define(AVAILABLE_MSG_PARAMS, lists:reverse(lists:sort(["%YYYY", "%YY", "%MM", "%M", "%DD", "%D", "%H", "%HH", 
	"%m","%mm", "%s", "%ss", "%level", "%msg", "%pid", "%uptime", "%unixtime", "%handler", "%user_module", "%module"]))).
-define(DEFAULT_MSG_PATTERN, ["[", '%HH', ":", '%mm', ":", '%ss', "] ", '%uptime', " ", '%level', ": ", '%msg']).


%% ===================================================================
%% External API
%% ===================================================================

compile(Pattern) when is_list(Pattern)->
	Pattern1 = binary_to_list(unicode:characters_to_binary(Pattern, utf8)),
	{struct, CompiledPattern} = f(Pattern1),
	CompiledPattern;

compile(_)->
	?DEFAULT_MSG_PATTERN.

format(CompiledPattern, Params) when is_list(Params), length(Params)>0, is_list(CompiledPattern), length(CompiledPattern)>0->
	DateTime = {{Year, Month, Day}, {Hour, Minute, Second}} = proplists:get_value(time, Params),
	FormattedString = lists:concat(lists:map(fun(X)->
		case X of
			'%YYYY'->
				Year;
			'%YY'->
				Year - 2000;
			'%MM'->
				pad2(Month);
			'%M'->
				Month;
			'%DD'->
				pad2(Day);
			'%D'->
				Day;
			'%HH'->
				pad2(Hour);
			'%H'->
				Hour;
			'%mm'->
				pad2(Minute);
			'%m'->
				Minute;
			'%ss'->
				pad2(Second);
			'%s'->
				Second;
			'%unixtime'->
				calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200;
			'%uptime'->
				{UptimeInMS, _} = erlang:statistics(wall_clock),
				UptimeInMS;
			'%level'->
				proplists:get_value(level, Params);
			'%msg'->
				Message = proplists:get_value(msg, Params),
				case is_string(Message) of
					true->
						Message;
					_->
						io_lib:format("~p",[Message])
				end;
			'%pid'->
				case proplists:get_value(pid, Params) of
					Pid when is_pid(Pid)->
						pid_to_list(Pid);
					_->
						[]
				end;
			'%handler'->
				proplists:get_value(handler, Params);
			'%user_module'->
				proplists:get_value(user_module, Params);
			'%module'->
				proplists:get_value(module, Params);
			_->
				X
		end
	end, CompiledPattern)),
	FormattedString;

format(_, Params)->
	format(?DEFAULT_MSG_PATTERN, Params).


%% ===================================================================
%% Internal API
%% ===================================================================

f(P)->
	Res = lists:foldl(fun(X, SourcePattern)->
		SourcePatternData = case SourcePattern of
			{struct, Structured}->
				Structured;
			_->
				SourcePattern
		end,
		D = case string:str(SourcePatternData, X) of
			0->
				SourcePattern;
			_->
				Tokens = re:split(SourcePatternData, X, [{return, list}]),
				case length(Tokens) of
					1->
						SourcePatternData;
					_->
						B1 = lists:foldl(fun(Token, Acc)->
							Acc1 = case f(Token) of
								{struct, ParsedPatternValue}->
									lists:append(Acc, ParsedPatternValue);
								PlainTextValue when is_list(PlainTextValue), length(PlainTextValue)>0->
									lists:append(Acc, [unicode:characters_to_list(list_to_binary(PlainTextValue))]);
								_->
									Acc
							end,
							lists:append(Acc1, [list_to_atom(X)])
						end, [], Tokens),
						B = lists:droplast(B1),
						{struct, B}
				end
		end,
		D
	end, P, ?AVAILABLE_MSG_PARAMS),
	Res.

pad2(Value)->
	lists:flatten(io_lib:format("~2..0w",[Value])).

is_string([])->
	true;
is_string([H | T]) when is_integer(H)->
	is_string(T);

is_string(_)->
	false.