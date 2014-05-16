%% -*- coding: utf-8 -*-
-module(registry).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start/2, send_req/0, get_reply/1, 
		status/0, list/0, filter/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("include/registry.hrl").

-define(MONTH,[{1,"January"}, {2,"February"}, {3,"March"}, {4,"April"}, 
			   {5,"May"}, {6,"June"}, {7,"Jule"}, {8,"August"}, {9,"September"}, 
			   {10,"October"}, {11,"November"}, {12,"December"}]).

send_req() -> gen_server:cast(?MODULE, {send_req, ?REG_SRV_URL}). 
get_reply(Id) -> gen_server:cast(?MODULE, {get_reply, ?REG_SRV_URL, Id}).
status() -> gen_server:call(?MODULE, {status}).
list() -> gen_server:call(?MODULE, {list}).
filter(Crt) -> gen_server:call(?MODULE, {filter, Crt}).

start(Xml, Sign) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Xml, Sign], []).

init([Xml, Sign]) ->
	{ok, Trace} = lager:trace_file(get_option(trace), [{module, ?MODULE}], debug),
	[application:ensure_started(App) || App <- [lager,inets]],
	Tid = ets:new(?MODULE, []),
	Tref = timer:apply_after(100, ?MODULE, send_req, []), 
    {ok, #{xml => Xml, sign => Sign, table => Tid, lastDumpDate => 0, codestring => "", 
    	   update_count => 0, trycount => 1, timer => Tref, 
    	   last_error => "", fin_state => send_req, trace => Trace, lastArch => ""}}.

handle_call({status}, _From, #{xml := Xml, sign := Sign, codestring := Code, lastDumpDate := LastDump, update_count := Update, fin_state := FState, last_error := LastErr, trycount := Try, lastArch := Arch } = State) ->
	R = [
			{"XMLRequest", Xml}, {"XMLRequestSign", Sign},
			{"lastDumpDate",ts2date(LastDump)}, {"NextAction", atom_to_list(FState)},
			{"UpdateCounter", Update}, {"lastArchive", filename:basename(Arch) }, {"LastError", LastErr},
			{"CodeString", Code}, {"LastTryCount", Try}
		],
	{reply, R, State};

handle_call({list}, _From, #{ table := Tid } = State) -> 
	List = case ets:tab2list(Tid) of
				[] -> [];
				L  -> [E || {_,E} <- L]
			end, 
	{reply,List,State};

handle_call({Filter, Crt}, _From, #{ table := Tid } = State) -> 
	R = case ets:tab2list(Tid) of
				[] -> [];
				L  -> lists:foldl(fun(E, Acc) -> 
						case lists:member(E, Acc) of
							true -> Acc;
							false -> Acc ++ [E]
						end 
						end, [], [proplists:get_value(Crt, E) || {_,E} <- L])
		end, 
	{reply, R, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({send_req, Url}, #{xml := Xml, sign := Sign, lastDumpDate := LastDump, trycount := Try, timer := Tref } = State) ->
	timer:cancel(Tref),
	Ts = unix_ts(),
	case blacklist:last_update(Url) of
		{ok, Last, LastUrg} when LastDump < LastUrg; Ts - LastDump > 43200 ->
				lager:debug("LastUpdate: ~ts, LastRegDump: ~ts, LastRegUrgDump: ~ts~n",[ts2date(LastDump),ts2date(Last),ts2date(LastUrg)]),
				case blacklist:send_req(Url, Xml, Sign) of
					{ok, Code} ->
						lager:debug("Code: ~p Wait for registry.~n",[Code]),
						Timer = timer:apply_after(180000, ?MODULE, get_reply, [Code]), 
						{noreply, State#{lastDumpDate := unix_ts(), codestring := Code, fin_state := get_reply, timer := Timer}};
					{error, E} ->  
						lager:debug("Not success reply. Try later. Reply: ~p~n",[E]),
						Timer = timer:apply_after(Try * 5000, ?MODULE, send_req, []),
						{noreply, State#{trycount := Try + 1, time := Timer, last_error := E}}
				end;
		{ok, _Last, _LastUrg} -> 
						lager:debug("Nothing to update"),
						Timer = timer:apply_after(1200000, ?MODULE, send_req, []),
						{noreply, State#{timer := Timer}};
		{error,E} -> lager:debug("Unexpected reply: ~p~n",[E]),
			   Timer = timer:apply_after(Try * 5000, ?MODULE, send_req, []),
			   {noreply, State#{trycount := Try + 1, timer := Timer, last_error := E}}
	end;

handle_cast({get_reply, Url, Id},#{table := Tid, update_count := Update, trycount := Try, timer := Tref } = State) ->
	timer:cancel(Tref), 
	case blacklist:get_reply(Url, Id) of
		{ok, File, Arch} ->
			lager:debug("Code: ~p, Load registry to file: ~p~n",[Id,File]),
			case blacklist:load_xml(File) of 
				{ok, List} ->
					lists:map(fun([{url, U}, {decision, _D}, {org, _Org}, {date, _Date}, {domain, _Dom}, {ip, IPs}] = E) -> 
									ets:insert(Tid, {erlang:phash2(U ++ IPs), E})
							end, List);
				{error, E} -> 
					lager:error("Parse XML error: ~p~n",[term_to_binary(E)])
			end,
			Timer = timer:apply_after(1200000, ?MODULE, send_req, []),
			{noreply, State#{fin_state := send_req, update_count := Update + 1, trycount := 1, timer := Timer, codestring := "", lastArch := Arch}};
		{error,{ok, _,[{'p:getResultResponse',[], _, Error, _}]}} when Try > 3 ->
			lager:debug("Reply load error: ~ts~n",[unicode:characters_to_list(list_to_binary(Error))]),
			Timer = timer:apply_after(1200000, ?MODULE, send_req, []), 
			{noreply, State#{fin_state := send_req, trycount := 1, timer := Timer, codestring := "", last_error := list_to_binary(Error)}};
		{error,{ok, _,[{'p:getResultResponse',[], _, Error, _}]}} -> 
			lager:debug("Code: ~p, File load error: ~ts, try later...~n",[Id,unicode:characters_to_list(list_to_binary(Error))]),
			Timer = timer:apply_after(Try * 120000, ?MODULE, get_reply, [Id]), 
			{noreply, State#{trycount := Try + 1, timer := Timer}};
		{error, E} ->
			lager:debug("Unknown error: ~p~n",[E]),
			Timer = timer:apply_after(1200000, ?MODULE, send_req, []), 
			{noreply, State#{fin_state := send_req, trycount := 1, timer := Timer, codestring := "", last_error := term_to_binary(E)}}
	end; 

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
	lager:stop_trace(maps:get(trace, State)), 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internals

ts2date(Ts) ->
    {{Y, M, D}, {H, Min, _S}} =
		calendar:now_to_local_time({Ts div 1000000, Ts rem 1000000, 0}),
    lists:flatten(io_lib:format('~2..0b-~3s-~4..0b, ~2..0b:~2..0b',
				[D, proplists:get_value(M, ?MONTH), Y, H, Min])).
unix_ts() ->
    {Mega, Seconds, _} = erlang:now(),
    Mega * 1000000 + Seconds.
