# xlogger 
xlogger is an erlang logging application with rich features
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
