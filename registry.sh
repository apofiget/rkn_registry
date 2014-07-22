#!/bin/bash

LOG_DIR=./

function usage {
	echo "Usage: ./registry.sh {start|stop|status}"
}

case $1 in
	start)
		run_erl -daemon $LOG_DIR $LOG_DIR "erl +pc unicode -pa ebin/ deps/*/ebin/ -config app.config -sname rkn_registry -eval 'application:start(rkn_registry)'"
	;;
	stop)
		./api.sh stop
	;;
	restart)
		$0 stop
		sleep 2
		$0 start
	;;
	status)
		./api.sh status
	;;
	*)
		usage
esac
