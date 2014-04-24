%% -*- coding: utf-8 -*-
-module(register).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
	Tid = ets:new(?MODULE, []),
    {ok, #{table => Tid, lastDumpDate => 0, codestring => 0}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
