%% -*- coding: utf-8 -*-
-module(y_embed).

-export([start/0,run/0]).

-include("include/registry.hrl").

start() -> {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "yaws_emb",
    GconfList = 
        [{id, Id},
        {server_signature, "Yaws"},
        {log_wrap_size, 1048576},
        {copy_error_log, false},
        {logdir, tools:get_option(www_log_path)}],
    Docroot = tools:get_option(www_root), 
    SconfList = 
        [{port, tools:get_option(port)}, 
        {access_log, true},
        {dir_listings, false},
        {servername, tools:get_option(hostname)},
        {listen, proplists:get_value(ok, [inet:parse_address(tools:get_option(listen_on))], {0,0,0,0})},
        {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
    yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(y_embed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    io:format("~n***~p start...~w~n", [?MODULE, self()]),
    {ok, self()}.
