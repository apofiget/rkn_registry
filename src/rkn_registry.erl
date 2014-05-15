-module(rkn_registry).

-behaviour(application).

-export([start/2, stop/1]).

-include("include/registry.hrl").

start(_StartType, _StartArgs) ->
	[rkn_registry_sup:start(Mod) || Mod <- [registry, y_embed]],
	{ok, self()}.

stop(_State) ->
    ok.
