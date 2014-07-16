-module(rkn_registry).

-behaviour(application).

-export([start/2, stop/1]).

-include("include/registry.hrl").

start(_StartType, _StartArgs) ->
	lager:start(),
	application:ensure_started(inets),
	[rkn_registry_sup:start(Mod) || Mod <- [y_embed, registry]],
	{ok, self()}.

stop(_State) ->
    ok.
