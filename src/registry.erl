%% -*- coding: utf-8 -*-
-module(registry).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start/2, set_last_update/1, get_reply/1, 
		get_reply/2, process_reply/1, status/0, list/0, 
		filter/1, get_last_update/0, get_last_update/1, 
		get_codestring/2, get_codestring/3, set_codestring/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("include/registry.hrl").

-define(MONTH,[{1,"January"}, {2,"February"}, {3,"March"}, {4,"April"}, 
			   {5,"May"}, {6,"June"}, {7,"Jule"}, {8,"August"}, {9,"September"}, 
			   {10,"October"}, {11,"November"}, {12,"December"}]).

get_last_update() -> gen_server:cast(?MODULE, {get_last_update, ?REG_SRV_URL}).
set_last_update(Param) when is_tuple(Param)  -> gen_server:cast(?MODULE, {set_last_update, Param}).
get_codestring(Xml, Sign) -> gen_server:cast(?MODULE, {get_codestring, ?REG_SRV_URL, Xml, Sign}).
set_codestring(Param) when is_tuple(Param)  -> gen_server:cast(?MODULE, {set_codestring, Param}).

get_reply(Code) -> gen_server:cast(?MODULE, {get_reply, ?REG_SRV_URL, Code}).
process_reply(Reply) -> gen_server:cast(?MODULE, {process_reply, Reply}).

status() -> gen_server:call(?MODULE, {status}).
list() -> gen_server:call(?MODULE, {list}).
filter(Crt) -> gen_server:call(?MODULE, {filter, Crt}).

start(Xml, Sign) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Xml, Sign], []).

init([Xml, Sign]) ->
	{ok, Trace} = lager:trace_file(get_option(trace), [{module, ?MODULE}], debug),
	[application:ensure_started(App) || App <- [lager,inets]],
	Tid = ets:new(?MODULE, []),
	Tref = timer:apply_after(100, ?MODULE, get_last_update, []), 
    {ok, #{xml => Xml, sign => Sign, table => Tid, lastDumpDate => 0, codestring => "", 
    	   update_count => 0, trycount => 1, timer => Tref, 
    	   last_error => "", fin_state => send_req, trace => Trace, lastArch => ""}}.

handle_call({status}, _From, #{xml := Xml, sign := Sign, codestring := Code, lastDumpDate := LastDump, update_count := Update, fin_state := FState, last_error := LastErr, trycount := Try, lastArch := Arch } = State) ->
	R = [
			{"XMLRequest", Xml}, {"XMLRequestSign", Sign},
			{"lastDumpDate",ts2date(LastDump)}, {"NextAction", atom_to_list(FState)},
			{"UpdateCounter", Update}, {"lastArchive", Arch}, {"LastError", LastErr},
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

handle_cast({get_last_update, Url}, #{timer := Tref } = State) -> 
	timer:cancel(Tref), 
	spawn_link(?MODULE, get_last_update, [Url]),
	{noreply, State#{fin_state := wait_last_update}}; 

handle_cast({set_last_update, {error, E}}, #{trycount := Try} = State) ->
	lager:debug("Unexpected reply: ~p~n",[E]),
  Timer = timer:apply_after(Try * 30000, ?MODULE, get_last_update, []),
  {noreply, State#{fin_state := get_last_update, trycount := Try + 1, timer := Timer, last_error := E}};

handle_cast({set_last_update, {Last, LastUrg}}, #{xml := Xml, sign := Sign, lastDumpDate := LastDump, timer := Tref } = State) ->
	timer:cancel(Tref),
	Ts = unix_ts(),
	if LastDump < LastUrg; Ts - LastDump > 43200 ->
			lager:debug("LastUpdate: ~ts, LastRegDump: ~ts, LastRegUrgDump: ~ts~n",[ts2date(LastDump),ts2date(Last),ts2date(LastUrg)]),
			get_codestring(Xml, Sign),
			{noreply, State#{fin_state := get_codestring}};
		true -> 
			lager:debug("Nothing to update"),
			Timer = timer:apply_after(1200000, ?MODULE, get_last_update, []),
			{noreply, State#{timer := Timer, fin_state := get_last_update}}
	end;

handle_cast({get_codestring, Url, Xml, Sign}, State) ->
	spawn_link(?MODULE, get_codestring, [Url, Xml, Sign]),
	{noreply, State#{fin_state := wait_codestring}};

handle_cast({set_codestring,{error, E}}, #{trycount := 10} = State) -> 
	lager:debug("GetCodestring. MaxTry reached. Last reply: ~p~n",[E]),
	Timer = timer:apply_after(1000, ?MODULE, get_last_update, []),
	{noreply, State#{trycount := 1, time := Timer, last_error := E, fin_state := get_last_update}};

handle_cast({set_codestring,{error, E}}, #{xml := Xml, sign := Sign, trycount := Try} = State) -> 
	lager:debug("Not success reply. Trycount: ~p , try later. Reply: ~p~n",[Try, E]),
	Timer = timer:apply_after(Try * 5000, ?MODULE, get_codestring, [Xml, Sign]),
	{noreply, State#{trycount := Try + 1, time := Timer, last_error := E}};

handle_cast({set_codestring,{ok, Code}}, State) -> 
	lager:debug("Code: ~p Wait for registry.~n",[Code]),
	Timer = timer:apply_after(180000, ?MODULE, get_reply, [Code]), 
	{noreply, State#{lastDumpDate := unix_ts(), trycount := 0, codestring := Code, fin_state := get_reply, timer := Timer}};

handle_cast({get_reply, Url, Id}, State) ->
	spawn_link(?MODULE, get_reply, [Url, Id]),
	{noreply, State#{fin_state := wait_for_reply}}; 

handle_cast({process_reply, {error,{ok, _,[{'p:getResultResponse',[], _, Error, _}]}}},#{trycount := 10, codestring := Code} = State) ->
	lager:debug("GetReply. Codestring: ~p, MaxTry reached. Last reply: ~p~n",[Code, unicode:characters_to_list(list_to_binary(Error))]),
	Timer = timer:apply_after(1000, ?MODULE, get_last_update, []),
	{noreply, State#{fin_state := get_last_update, trycount := 1, codestring := "", time := Timer, last_error := unicode:characters_to_list(list_to_binary(Error))}};

handle_cast({process_reply, {error,{ok, _,[{'p:getResultResponse',[], _, Error, _}]}}},#{trycount := Try, codestring := Code} = State) ->
	lager:debug("GetReply. Codestring: ~p, Trycount: ~p Last reply: ~p~n",[Code, Try, unicode:characters_to_list(list_to_binary(Error))]),
	Timer = timer:apply_after(Try * 5000, ?MODULE, get_reply, [Code]),
	{noreply, State#{trycount := Try + 1, codestring := "", time := Timer, last_error := unicode:characters_to_list(list_to_binary(Error))}};

handle_cast({process_reply, {error,E}},#{trycount := Try, codestring := Code} = State) ->
	lager:debug("GetReply. Codestring: ~p, Trycount: ~p Unknown error: ~p~n",[Code, Try, term_to_binary(E)]),
	Timer = timer:apply_after(1000, ?MODULE, get_last_update, []),
	{noreply, State#{trycount := 1, codestring := "", time := Timer, last_error := term_to_binary(E), fin_state := get_last_update}};

handle_cast({process_reply, {ok, File, Arch}},#{codestring := Code, table := Tid, update_count := Update} = State) ->
	lager:debug("Code: ~p, Load registry to file: ~p~n",[Code,File]),
	case blacklist:load_xml(File) of 
		{ok, List} ->
			IsDump = get_option(dump_csv),
			if IsDump -> 
				DFile = get_option(csv_file),
				Separator = get_option(csv_separator),
				FList = get_option(csv_fields),
					case blacklist:export_to_csv(DFile, List, Separator, FList) of
						ok -> lager:debug("Dump registry to CSV file: ~p~n",[get_option(csv_file)]);
						{error, R} -> lager:debug("Error ~p while dump registry to CSV file: ~p~n",[get_option(csv_file), R])
					end;
				true -> ok
			end,
			lists:map(fun([{url, U}, {decision, _D}, {org, _Org}, {date, _Date}, {domain, _Dom}, {ip, IPs}] = E) -> 
							ets:insert(Tid, {erlang:phash2(U ++ IPs), E})
						end, List);
		{error, E} -> 
			lager:error("Parse XML error: ~p~n",[term_to_binary(E)])
	end,
	Timer = timer:apply_after(1200000, ?MODULE, get_last_update, []),
	{noreply, State#{fin_state := get_last_update, update_count := Update + 1, trycount := 1, timer := Timer, codestring := "", lastArch := Arch}};

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

get_last_update(Url) ->
	case blacklist:last_update(Url) of
		{ok, Last, LastUrg} -> set_last_update({Last, LastUrg});
		{error,E} -> set_last_update({error,E})
	end.

get_codestring(Url, Xml, Sign) ->
	Reply = blacklist:send_req(Url, Xml, Sign),
	set_codestring(Reply).

get_reply(Url, Id) ->
  Reply = blacklist:get_reply(Url, Id),
  process_reply(Reply).

ts2date(Ts) ->
    {{Y, M, D}, {H, Min, _S}} =
		calendar:now_to_local_time({Ts div 1000000, Ts rem 1000000, 0}),
    lists:flatten(io_lib:format('~2..0b-~3s-~4..0b, ~2..0b:~2..0b',
				[D, proplists:get_value(M, ?MONTH), Y, H, Min])).
unix_ts() ->
    {Mega, Seconds, _} = erlang:now(),
    Mega * 1000000 + Seconds.
