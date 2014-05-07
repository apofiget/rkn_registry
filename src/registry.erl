%% -*- coding: utf-8 -*-
-module(registry).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start/2, send_req/0, get_reply/1, status/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(REG_SRV_URL,"http://vigruzki.rkn.gov.ru/services/OperatorRequest/?wsdl").

-define(MONTH,[{1,"January"}, {2,"February"}, {3,"March"}, {4,"April"}, 
			   {5,"May"}, {6,"June"}, {7,"Jule"}, {8,"August"}, {9,"September"}, 
			   {10,"October"}, {11,"November"}, {12,"December"}]).

send_req() -> gen_server:cast(?MODULE, {send_req, ?REG_SRV_URL}). 
get_reply(Id) -> gen_server:cast(?MODULE, {get_reply, ?REG_SRV_URL, Id}).
status() -> gen_server:call(?MODULE, {status}).

start(Xml, Sign) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Xml, Sign], []).

init([Xml, Sign]) ->
	{ok, Trace} = lager:trace_file("priv/logs/updates.log", [{module, ?MODULE}], debug),
	[application:ensure_started(App) || App <- [lager,inets]],
	Tid = ets:new(?MODULE, []),
	Tref = timer:apply_after(100, ?MODULE, send_req, []), 
    {ok, #{xml => Xml, sign => Sign, table => Tid, lastDumpDate => 0, codestring => "", 
    	   update_count => 0, trycount => 1, timer => Tref, 
    	   last_error => "", fin_state => send_req, trace => Trace}}.

handle_call({status}, _From, #{xml := Xml, sign := Sign, codestring := Code, lastDumpDate := LastDump, update_count := Update, fin_state := FState, last_error := LastErr } = State) ->
	io:format("~nCurrent state: ~nXMLRequest: ~p~nXMLRequestSign: ~p~nLastDumpDate: ~p~nNextAction: ~p~nCodeString: ~p~nUpdateCounter: ~p~nLastError: ~p~n", [Xml, Sign, ts2date(LastDump), FState, Code, Update, LastErr]),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
					Any ->  
						lager:debug("Not success reply. Try later. Reply: ~p~n",[Any]),
						Timer = timer:apply_after(Try * 5000, ?MODULE, send_req, []),
						{noreply, State#{trycount := Try + 1, time := Timer, last_error := Any}}
				end;
		{ok, _Last, _LastUrg} -> 
						lager:debug("Nothing to update"),
						Timer = timer:apply_after(1200000, ?MODULE, send_req, []),
						{noreply, State#{timer := Timer}};
		Any -> lager:debug("Unexpected reply: ~p~n",[Any]),
			   Timer = timer:apply_after(Try * 5000, ?MODULE, send_req, []),
			   {noreply, State#{trycount := Try + 1, timer := Timer, last_error := Any}}
	end;

handle_cast({get_reply, Url, Id},#{table := Tid, update_count := Update, trycount := Try, timer := Tref } = State) ->
	timer:cancel(Tref), 
	case blacklist:get_reply(Url, Id) of
		{ok, File} ->
			lager:debug("Code: ~p, Load register to file: ~p~n",[Id,File]),
			case blacklist:load_xml(File) of 
				{ok, List} ->
					lists:map(fun([{url, U}, {decision, _D}, {domain, _Dom}, {ip, IPs}] = E) -> 
									ets:insert(Tid, {erlang:phash2(U ++ IPs), E})
							end, List);
				Any -> 
					lager:error("Parse XML error: ~p~n",[term_to_binary(Any)])
			end,
			Timer = timer:apply_after(1200000, ?MODULE, send_req, []),
			{noreply, State#{fin_state := send_req, update_count := Update + 1, trycount := 1, timer := Timer, codestring := ""}};
		{error,{ok, _,[{'p:getResultResponse',[], _, Error, _}]}} when Try > 3 ->
			lager:debug("Reply load error: ~ts~n",[unicode:characters_to_list(list_to_binary(Error))]),
			Timer = timer:apply_after(1200000, ?MODULE, send_req, []), 
			{noreply, State#{fin_state := send_req, trycount := 1, timer := Timer, codestring := "", last_error := list_to_binary(Error)}};
		{error,{ok, _,[{'p:getResultResponse',[], _, Error, _}]}} -> 
			lager:debug("Code: ~p, File load error: ~ts, try later...~n",[Id,unicode:characters_to_list(list_to_binary(Error))]),
			Timer = timer:apply_after(Try * 120000, ?MODULE, get_reply, [Id]), 
			{noreply, State#{trycount := Try + 1, timer := Timer}};
		Any ->
			lager:debug("Unknown error: ~p~n",[Any]),
			Timer = timer:apply_after(1200000, ?MODULE, send_req, []), 
			{noreply, State#{fin_state := send_req, trycount := 1, timer := Timer, codestring := "", last_error := term_to_binary(Any)}}
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
