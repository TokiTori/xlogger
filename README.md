# xlogger 
xlogger is an Erlang logging application with features:

1. Time-based and size-based log files rotation
2. Name of file/directory and message can be formatted using date, time, log level, handler name, current module name...
3. Pre-defined log levels: info, debug, warning, error. You can specify your own log level
4. Many handlers that can contain many destinations (console, files...)
5. Helpful functions: filters, message size limitation

# Starting
1. Make shure that you have a configuration in module <code>xlogger_config.erl</code> or in application configuration file
2. Run xlogger as <code>application:start(xlogger)</code>
3. Integrate functions of <code>xlogger.erl</code> module into your modules

# Usage

```
xlogger:log(Message)
xlogger:log(Level, Message)
xlogger:log(HandlerName, Level, Message)
xlogger:info(HandlerName, Message)
xlogger:debug(HandlerName, Message)
xlogger:warning(HandlerName, Message)
xlogger:error(HandlerName, Message)

xlogger:log(HandlerName, Level, Format, Args)
xlogger:info(HandlerName, Format, Args)
xlogger:debug(HandlerName, Format, Args)
xlogger:warning(HandlerName, Format, Args)
xlogger:error(HandlerName, Format, Args)
```
First function with one parameter uses default handler and info level. This function used by internal modules of xlogger. Thus you can see xlogger's messages in dests of default handler.
Second function with two parameters uses default handler.
In other functions HandlerName is a name of handler from configuration. If the handler with this name not found will be used default handler with default configuration. Also default configuraion will be used by using function <code>xlogger:log/2</code>.
Functions has two variants: prepared Message or Format string with Arguments.

# Configuration
Configuration is a proplist. The root of this proplist has only property 'handlers', which defines handlers for logger. Handler can contain 'dest' - destinations of log and 'msg_pattern' - common message pattern for all destinations in current handler. Destinations can be two types (at this moment): 'console', 'file'. Console type has no properties yet. File type has properties 'name', 'size', 'rotate', 'msg_pattern', 'write_delay'. Properties 'name' and 'msg_pattern' can be formatted by next params:
```
%YYYY, %YY		Year in four-digit format and two-digit format
%M, %MM			Months and months with leading zero
%D, %DD			Days and days with leading zero
%H, %HH			Hours and hours with leading zero
%m, %mm			Minutes and minutes with leading zero
%s, %ss			Seconds and seconds with leading zero
%level			Curent log level of message
%msg			Just a message
%unixtime       Current time in seconds since 1970-01-01
%uptime			Uptime of virtual machine in ms
%handler		Handler
%user_module	This is a module, invoked current message and not in 'code:lib_dir()' directory
%module			This is a module, invoked current message
```

All releases of xlogger tested by <a href="http://true-converter.com">true-converter.com</a> and <a href="http://http-master.com">http-master.com</a>
