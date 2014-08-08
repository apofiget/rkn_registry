%% -*- coding: utf-8 -*-
-module(registry).

-author("Andrey Andruschenko <apofiget@gmail.com>").

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start/2, set_last_update/1, get_reply/1, 
		get_reply/2, process_reply/1, status/0, list/0, 
		list_only/1, get_last_update/0, get_last_update/1, 
		get_codestring/2, get_codestring/4, set_codestring/1, 
		clean_old/1, search/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("include/registry.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

get_last_update() -> gen_server:cast(?MODULE, {get_last_update, ?REG_SRV_URL}).
set_last_update(Param) when is_tuple(Param)  -> gen_server:cast(?MODULE, {set_last_update, Param}).
get_codestring(Xml, Sign) -> gen_server:cast(?MODULE, {get_codestring, ?REG_SRV_URL, Xml, Sign}).
set_codestring(Param) when is_tuple(Param)  -> gen_server:cast(?MODULE, {set_codestring, Param}).

get_reply(Code) -> gen_server:cast(?MODULE, {get_reply, ?REG_SRV_URL, Code}).
process_reply(Reply) -> gen_server:cast(?MODULE, {process_reply, Reply}).

status() -> gen_server:call(?MODULE, {status}).
list() -> gen_server:call(?MODULE, {list}).
list_only(Crt) ->  gen_server:call(?MODULE, {list_only, Crt}).

clean_old(Ts) -> gen_server:call(?MODULE, {clean_old, Ts}). 
search(In, Value) -> gen_server:call(?MODULE, {search, In, Value}). 

start(Xml, Sign) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Xml, Sign], []).

init([Xml, Sign]) ->
	{ok, Trace} = lager:trace_file(tools:get_option(trace), [{module, ?MODULE}], debug),
	Tid = ets:new(?MODULE, []),
	{ok, _} = timer:apply_after(100, ?MODULE, get_last_update, []),
	io:format("~n***~p start...~p~n", [?MODULE, self()]),
    {ok, #{xml => Xml, sign => Sign, table => Tid, lastDumpDate => 0, codestring => "", 
    	   update_count => 0, trycount => 1, last_error => "", fin_state => send_req, trace => Trace, lastArch => "", dumpVersion => tools:get_option(dump_format_ver)}}.

handle_call({status}, _From, #{xml := Xml, sign := Sign, codestring := Code, lastDumpDate := LastDump, update_count := Update, fin_state := FState, last_error := LastErr, trycount := Try, lastArch := Arch, dumpVersion := DumpVer } = State) ->
	R = [
			{"XMLRequest", Xml}, {"XMLRequestSign", Sign}, {"dumpFormatVersion", DumpVer},
			{"lastDumpDate",tools:ts2date(LastDump)}, {"NextAction", atom_to_list(FState)},
			{"UpdateCounter", Update}, {"lastArchive", Arch}, {"LastError", tools:to_list(LastErr)},
			{"CodeString", Code}, {"LastTryCount", Try}
		],
	{reply, R, State};

handle_call({list}, _From, #{ table := Tid } = State) -> 
	List = case ets:tab2list(Tid) of
				[] -> [];
				L  -> [E || {_,{_,E}} <- L]
			end, 
	{reply,List,State};

handle_call({list_only, Crt}, _From, #{ table := Tid } = State) -> 
	R = case ets:tab2list(Tid) of
				[] -> [];
				L  -> 
					lists:foldl(fun(Elm, Acc) -> 
						case lists:member(Elm, Acc) of
							true -> Acc;
							false -> 
								Acc ++ if Crt =:= ip -> Elm; 
									true -> [Elm]
								end
						end 
					end, [], [proplists:get_value(Crt, E) || {_,{_,E}} <- L])
		end, 
	{reply, R, State};

handle_call({clean_old, Ts}, _From, #{ table := Tab } = State) -> 
	Ms = ets:fun2ms(fun({H,{T,L}}) when T < Ts -> H end),
	case ets:match_spec_run(ets:tab2list(Tab),ets:match_spec_compile(Ms)) of
		[] -> ok;
		List -> [ ets:delete(Tab, K)  || K <- List],
			lager:debug("Clean ~p oldest records.~n",[length(List)])
	end,
	{reply, ok, State};

handle_call({search, Field, Value}, _From, #{ table := Tab} = State) ->
	R = case ets:tab2list(Tab) of
		[] -> [];
		L -> lists:foldl(fun(E, Acc) ->
			Prop = case Field of
				ip -> proplists:get_value(ip, E);
				_ -> [proplists:get_value(Field, E)]
			end,
			IsMem = lists:member(Value, Prop),
			if IsMem =:= true -> Acc ++ [E];
				true -> Acc
			end 
		end, [], [Elm || {_,{_,Elm}}<- L]) 
	end, 
	{reply, R, State}; 

handle_call(Request, _From, State) -> 
	lager:debug("Unexpected call: ~p~n",[Request]),
	{reply, ok, State}.

handle_cast({get_last_update, Url}, State) -> 
	spawn_link(?MODULE, get_last_update, [Url]),
	{noreply, State#{fin_state := wait_last_update}}; 

handle_cast({set_last_update, {error, E}}, #{trycount := Try} = State) ->
	lager:debug("GetLastUpdate. Unexpected reply: ~p~n",[E]),
  {ok, _} = timer:apply_after(Try * 30000, ?MODULE, get_last_update, [tools:get_option(get_last_update_period)]),
  {noreply, State#{fin_state := get_last_update, trycount := Try + 1, last_error := E}};

handle_cast({set_last_update, {Last, LastUrg}}, #{xml := Xml, sign := Sign, lastDumpDate := LastDump} = State) ->
	Ts = tools:unix_ts(),
	if LastDump < LastUrg; Ts - LastDump > 43200 ->
			lager:debug("LastUpdate: ~ts, LastRegDump: ~ts, LastRegUrgDump: ~ts~n",[tools:ts2date(LastDump),tools:ts2date(Last),tools:ts2date(LastUrg)]),
			get_codestring(Xml, Sign),
			{noreply, State#{fin_state := get_codestring}};
		true -> 
			lager:debug("Nothing to update"),
			get_last_update(tools:get_option(get_last_update_period)),
			{noreply, State#{fin_state := get_last_update}}
	end;

handle_cast({get_codestring, Url, Xml, Sign}, #{dumpVersion := Ver} = State) ->
	spawn_link(?MODULE, get_codestring, [Url, Xml, Sign, Ver]),
	{noreply, State#{fin_state := wait_codestring}};

handle_cast({set_codestring,{error, E}}, #{trycount := 10} = State) -> 
	lager:debug("GetCodestring. MaxTry reached. Last reply: ~tp~n",[E]),
	get_last_update(tools:get_option(get_last_update_period)),
	{noreply, State#{trycount := 1, last_error := E, fin_state := get_last_update}};

handle_cast({set_codestring,{error, E}}, #{xml := Xml, sign := Sign, trycount := Try} = State) -> 
	lager:debug("Not success reply. Trycount: ~p , try later. Reply: ~tp~n",[Try, E]),
	{ok, _} = timer:apply_after(Try * 5000, ?MODULE, get_codestring, [Xml, Sign]),
	{noreply, State#{trycount := Try + 1, last_error := E}};

handle_cast({set_codestring,{ok, Code}}, State) -> 
	lager:debug("Code: ~p Wait for registry.~n",[Code]),
	{ok, _} = timer:apply_after(180000, ?MODULE, get_reply, [Code]), 
	{noreply, State#{lastDumpDate := tools:unix_ts(), trycount := 1, codestring := Code, fin_state := get_reply}};

handle_cast({get_reply, Url, Id}, State) ->
	spawn_link(?MODULE, get_reply, [Url, Id]),
	{noreply, State#{fin_state := wait_for_reply}}; 

handle_cast({process_reply, {error,ErrCode}},#{trycount := 10, codestring := Code} = State) when is_integer(ErrCode) ->
	lager:debug("GetReply. Codestring: ~p, MaxTry reached. Last reply: ~tp~n",[Code, unicode:characters_to_list(tools:get_result_comment(ErrCode))]),
	get_last_update(tools:get_option(get_last_update_period)),
	{noreply, State#{fin_state := get_last_update, trycount := 1, codestring := "", last_error := tools:get_result_comment(ErrCode)}};

handle_cast({process_reply, {error,ErrCode}},#{trycount := Try, codestring := Code} = State) when is_integer(ErrCode) ->
	lager:debug("GetReply. Codestring: ~p, Trycount: ~p Last reply: ~tp~n",[Code, Try, unicode:characters_to_list(tools:get_result_comment(ErrCode))]),
	{ok, _} = timer:apply_after(Try * 5000, ?MODULE, get_reply, [Code]),
	{noreply, State#{trycount := Try + 1, codestring := Code, last_error := tools:get_result_comment(ErrCode)}};

handle_cast({process_reply, {error,E}},#{trycount := Try, codestring := Code} = State) ->
	lager:debug("GetReply. Codestring: ~p, Trycount: ~p Unknown error: ~tp~n",[Code, Try, term_to_binary(E)]),
	get_last_update(tools:get_option(get_last_update_period)),
	{noreply, State#{trycount := 1, codestring := "", last_error := term_to_binary(E), fin_state := get_last_update}};

handle_cast({process_reply, {ok, File, Arch, _Ver}},#{codestring := Code, table := Tid, update_count := Update} = State) ->
	lager:debug("Code: ~p, Load registry to file: ~p~n",[Code,File]),
	case blacklist:load_xml(File) of 
		{ok, List} ->
			IsDump = tools:get_option(dump_csv),
			if IsDump -> 
				DFile = tools:get_option(csv_file),
				Separator = tools:get_option(csv_separator),
				FList = tools:get_option(csv_fields),
					case blacklist:export_to_csv(DFile, List, Separator, FList) of
						ok -> lager:debug("Dump registry to CSV file: ~p~n",[tools:get_option(csv_file)]);
						{error, R} -> lager:debug("Error ~p while dump registry to CSV file: ~p~n",[tools:get_option(csv_file), R])
					end;
				true -> ok
			end,
			Ts = tools:unix_ts(),
			lists:map(fun(E) -> 
							ets:insert(Tid, {erlang:phash2(tools:to_list(proplists:get_value(url, E)) ++ tools:to_list(proplists:get_value(ip, E))), {Ts, E}})
						end, List),
			timer:apply_after(5000, ?MODULE, clean_old, [Ts]);
		{error, E} -> 
			lager:error("Parse XML error: ~tp~n",[term_to_binary(E)])
	end,
	get_last_update(tools:get_option(get_last_update_period)),
	{noreply, State#{fin_state := get_last_update, update_count := Update + 1, trycount := 1, codestring := "", lastArch := Arch}};

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

get_last_update(Time) when is_integer(Time) -> 
	timer:apply_after(Time*1000, ?MODULE, get_last_update, []);

get_last_update(Url) ->
	case blacklist:last_update(Url) of
		{ok, Last, LastUrg} -> set_last_update({Last, LastUrg});
		{error,E} -> set_last_update({error,E})
	end.

get_codestring(Url, Xml, Sign, Ver) ->
	Reply = blacklist:send_req(Url, Xml, Sign, Ver),
	set_codestring(Reply).

get_reply(Url, Id) ->
  Reply = blacklist:get_reply(Url, Id),
  process_reply(Reply).
