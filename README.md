# xlogger 
xlogger is an erlang logging application with features such as files rotation with size limit, filename formatting, message formatting. This logger has pre-defined log levels: info, debug, warning, error. You can specify your own log level.
# Usage

```
xlogger:log(Message)
xlogger:log(Level, Message)
xlogger:log(HandlerName, Level, Message)
xlogger:info(HandlerName, Message)
xlogger:debug(HandlerName, Message)
xlogger:warning(HandlerName, Message)
xlogger:error(HandlerName, Message)

xlogger:log_format(Format, Args)
xlogger:log_format(Level, Format, Args)
xlogger:log_format(HandlerName, Level, Format, Args)
xlogger:info_format(HandlerName, Format, Args)
xlogger:debug_format(HandlerName, Format, Args)
xlogger:warning_format(HandlerName, Format, Args)
xlogger:error_format(HandlerName, Format, Args)
```
First function with one parameter uses default handler and info level.
Second function with two parameters uses default handler.
In other functions HandlerName is a name of handler from configuration. If the handler with this name not found will be used default handler with default configuration. Also default configuraion will be used by using function <code>xlogger:log/2</code>.
Functions has two variants: prepared Message or Format string with Arguments.

# Confiuration
Configuration is a proplist. The root of this proplist has only property 'handlers', which defines handlers for logger. Handler can contain 'dest' - destinations of log and 'msg_pattern' - common message pattern for all destinations in current handler. Destinations can be two types (at this moment): 'console', 'file'. Console type has no properties yet. File type has properties 'name', 'size', 'rotate', 'msg_pattern', 'write_delay'. Properties 'name' and 'msg_pattern' can be formatted by next params:
Year:			%YYYY, %YY
Month:			%MM, %M
Day:			%DD, %D
Hour:			%H, %HH
Minute:			%m, %mm
Second:			%s, %ss
Level:			%level			Curent log level of message
Message:		%msg			Just a message
VM Uptime:		%uptime			Uptime of virtual machine in ms
Handler:		%handler
User module:	%user_module	This is a module, invoked current message and not in ``code:lib_dir()`` directory
Module:			%module			This is a module, invoked current message
