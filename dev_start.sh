#!/bin/sh

erl +pc unicode -pa ebin/ deps/*/ebin/ -config app.config -sname rkn_registry -eval 'application:start(rkn_registry)'
