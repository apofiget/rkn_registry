#!/bin/sh

erl +pc unicode -pa ebin/ deps/*/ebin/ -config app.config -sname some_for_work -eval 'application:start(rkn_registry)'
