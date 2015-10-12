# xlogger 
xlogger is an erlang logging application with features such as files rotation with size limit, filename formatting, message formatting. This logger has pre-defined log levels: info, debug, warning, error. You can specify your own log level.
# Usage

```
xlogger:log(Level, Message)
xlogger:log(HandlerName, Level, Message)
xlogger:info(HandlerName, Message)
xlogger:debug(HandlerName, Message)
xlogger:warning(HandlerName, Message)
xlogger:error(HandlerName, Message)
```
HandlerName is a name of handler from configuration. If the handler with this name not found will be used default handler with default configuration. Also default configuraion will be used by using function <code>xlogger:log/2</code>.
