-module(xlogger_formatter).
-export([compile/1, format/2]).
-define(AVAILABLE_MSG_PARAMS, lists:reverse(lists:sort(["%YYYY", "%YY", "%MM", "%M", "%DD", "%D", "%H", "%HH", 
	"%m","%mm", "%s", "%ss", "%level", "%msg", "%uptime", "%handler"]))).
-define(DEFAULT_MSG_PATTERN, ["[", '%HH', ":", '%mm', ":", '%ss', "] ", '%uptime', " ", '%level', ": ", '%msg']).

compile(Pattern)->
	{struct, CompiledPattern} = f(Pattern),
	CompiledPattern.

format(WrongPattern, Params) when WrongPattern == undefined; WrongPattern == []->
	format(?DEFAULT_MSG_PATTERN, Params);

format(CompiledPattern, Params)->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
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
			'%uptime'->
				{UptimeInMS, _} = erlang:statistics(wall_clock),
				UptimeInMS;
			'%level'->
				proplists:get_value(level, Params);
			'%msg'->
				proplists:get_value(msg, Params);
			'%handler'->
				proplists:get_value(handler, Params);
			'%module'->
				proplists:get_value(module, Params);
			_->
				X
		end
	end, CompiledPattern)),
	FormattedString.

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
								ParsedPatternValue->
									lists:append(Acc, [ParsedPatternValue])
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