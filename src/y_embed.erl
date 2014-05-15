%% -*- coding: utf-8 -*-
-module(y_embed).

-export([start/0,run/0]).

-include("include/registry.hrl").

start() -> {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "yaws_emb",
    Dir = get_option(doc_root),
    GconfList = 
        [{id, Id},
        {server_signature, "Yaws"},
        {log_wrap_size, 1048576},
        {copy_error_log, false},
	    {logdir, Dir ++ "/priv/logs/www"},
	    {ebin_dir, [Dir ++ "/ebin"]}],
    Docroot = Dir ++ "/www",
    SconfList = 
        [{port, get_option(port)}, 
         {access_log, true},
		 {dir_listings, false},
		 {servername, get_option(hostname)},
		 {listen, {0, 0, 0, 0}}, 
         {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
	yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(y_embed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
