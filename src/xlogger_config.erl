-module(xlogger_config).
-export([get_config/0]).


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
						{msg_pattern, "[%HH:%mm:%ss] %uptime %level - %msg"}
					]},
					{console, []}
				]},
				{msg_pattern, "[%HH:%mm:%ss] %uptime %level - %msg"}
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
					]}
				]},
				{msg_pattern, "[%HH:%mm:%ss] %uptime %level CommonPattern - %msg"}
			]}
		]},
		{config_module, logger_cfg},
		{enable_default_logger, true}
	].