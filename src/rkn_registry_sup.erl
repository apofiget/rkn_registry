%% -*- coding: utf-8 -*-
-module(rkn_registry_sup).

-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).

-export([init/1, start/1]).

-include("include/registry.hrl").

start(Mod) -> supervisor:start_link({local, list_to_atom(atom_to_list(Mod) ++ "_sup")},?MODULE, [Mod]).

init([y_embed]) ->
    YBed = {y_embed, {y_embed, start, []}, permanent, 2000, worker, [y_embed]},
    {ok, {{one_for_all, 0, 1}, [YBed]}};

init([Mod]) ->
    XMLReq = tools:get_option(xml),
    SignXMLReq = tools:get_option(sign),
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 30,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    Child = {Mod, {Mod, start, [XMLReq,SignXMLReq]}, Restart, Shutdown, Type, [Mod]},
    {ok, {SupFlags, [Child]}}.
