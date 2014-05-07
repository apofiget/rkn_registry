#!/bin/sh

erl +pc unicode -pa ebin/ deps/*/ebin/ -config app.config -sname for_work -eval 'lager:start()'
