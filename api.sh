#!/usr/bin/env escript
%% -*- erlang -*-

main([String]) ->
    {ok, Host} = inet:gethostname(),
    {ok, _Pid} = net_kernel:start([rkn_api_shell, shortnames]),
    Node = list_to_atom("rkn_registry@" ++ Host),
    Cmd = list_to_atom(String),
    case net_adm:ping(Node) of
	pong ->
	    case Cmd of
		status -> io:format("Application running~n");
		stop -> stop(Node);
		state -> server_state(Node);
		_  -> usage()
	    end;
	pang -> io:format("Probably application is not running~n"),
		ok
    end,
    net_kernel:stop();
main(_) ->
    usage().


stop(Node) ->
    rpc:call(Node, application, stop, [rkn_registry], 2500),
    timer:sleep(2000),
    rpc:call(Node, init, stop, [], 2500).

usage() ->
    io:format("~nUsage: ./api.sh {stop|status|state}~n").

server_state(Node) ->
    io:format("~p~n",[rpc:call(Node, registry, status, [])]).
