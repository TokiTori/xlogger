# xlogger 
xlogger is an erlang logging application with features such as files rotation with size limit, filename formatting, message formatting. This logger has pre-defined log levels: info, debug, warning, error. You can specify your own log level.
# Usage

```
xlogger:log(Message)
xlogger:log(Level, Message)
xlogger:log(HandlerName, Level, Message)
xlogger:log(HandlerName, Level, Format, Args)
xlogger:info(HandlerName, Message)
xlogger:info(HandlerName, Format, Args)
xlogger:debug(HandlerName, Message)
xlogger:debug(HandlerName, Format, Args)
xlogger:warning(HandlerName, Message)
xlogger:warning(HandlerName, Format, Args)
xlogger:error(HandlerName, Message)
xlogger:error(HandlerName, Format, Args)
```
First function with one parameter uses default handler and info level.
Second function with two parameters users default handler.
In other functions HandlerName is a name of handler from configuration. If the handler with this name not found will be used default handler with default configuration. Also default configuraion will be used by using function <code>xlogger:log/2</code>.
Functions has two variants: prepared Message or Format string with Arguments.
