#!/bin/sh
exec erl -pa ebin -eval "application:start(xlogger)" -config app.config
