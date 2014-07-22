#!/usr/bin/env escript
%% -*- erlang -*-

main([String]) ->
    {ok, Host} = inet:gethostname(),
    Node = list_to_atom("rkn_registry@" ++ Host),
    Cmd = list_to_atom(String),
    case net_adm:ping(Node) of
	pong -> 
	    case Cmd of
		status -> io:format("Application running~n");
		stop -> stop(Node);
		_  -> usage()
	    end;
	pang -> io:format("Probably application is not running~n"),
		ok
    end;
main(_) -> 
    usage().


stop(Node) ->
    rpc:call(Node, application, stop, [rkn_registry], 2500),
    timer:sleep(2000),
    rpc:call(Node, init, stop, [], 2500).

usage() ->
    io:format("~nUsage: ./api.sh {stop|status}~n").
