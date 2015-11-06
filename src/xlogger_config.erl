-module(xlogger_config).
-export([get_config/0, filter1/1, filter2/1]).


get_config()->
	[		
		{handlers, [
			{handler1, [
				{dest, [
					{file, [
						{name, "logs/%YYYY-%MM-%DD/%user_module_new1.%level.log"}, 
						{size, 5242880}, 
						{rotate, 3}, 
						{level, error}, 
						{msg_pattern, "[%HH:%mm:%ss] %uptime %level - %msg1"}

					]},
					{console, [
						
					]}
				]},
				{msg_pattern, "[%HH:%mm:%ss] %uptime %level - %msg2"},
				{msg_size_limit, 500},
				{filters, [
					{xlogger_config, filter1}, {xlogger_config, filter2}
				]}
			]},
			{handler2, [
				{dest, [
					{file, [
						{name, "logs/%YYYY-%MM-%DD/%user_module_new.%level.log"}, 
						{size, 2097152}, 
						{rotate, 3}, 
						{level, error}, 
						{msg_pattern, "[%HH:%mm:%ss] %uptime %level - %msg"}, 
						{write_delay, 3000}
					]},
					{console, []}
				]},
				{msg_pattern, "[%HH:%mm:%ss] %uptime %level CommonPattern - %msg"}
			]}
		]}
	].

filter1(Params)->
	{{Year, Month, Day}, {Hour, Minute, Second}} = proplists:get_value(time, Params),
	Second > 2.

filter2(Params)->
	{{Year, Month, Day}, {Hour, Minute, Second}} = proplists:get_value(time, Params),
	Second rem 2 == 1.