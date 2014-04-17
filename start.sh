#!/bin/sh

#export DISPLAY=localhost:10.0

export DISPLAY=:0.0

ulimit -n 4096

erl +pc unicode -pa ebin/ deps/*/ebin/ -config app.config -sname for_work -eval 'lager:start()'
