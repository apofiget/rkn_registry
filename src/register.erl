%% -*- coding: utf-8 -*-
-module(register).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start/0, send_req/3, get_reply/2, status/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(REG_SRV_URL,"http://vigruzki.rkn.gov.ru/services/OperatorRequest/?wsdl").

-define(MONTH,[{1,"January"}, {2,"February"}, {3,"March"}, {4,"April"}, 
			   {5,"May"}, {6,"June"}, {7,"Jule"}, {8,"August"}, {9,"September"}, 
			   {10,"October"}, {11,"November"}, {12,"December"}]).

send_req(Url, Xml, Sign) -> gen_server:cast(?MODULE, {send_req, Url, Xml, Sign}). 
get_reply(Url, Id) -> gen_server:cast(?MODULE, {get_reply, Url, Id}).
status() -> gen_server:call(?MODULE, {status}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	[application:ensure_started(App) || App <- [lager,inets]],
	Tid = ets:new(?MODULE, []),
	Tref = timer:apply_after(100, ?MODULE, send_req, [?REG_SRV_URL, "priv/request.xml", "priv/request.bin"]), 
    {ok, #{table => Tid, lastDumpDate => 0, codestring => "", update_count => 0, trycount => 1, timer => Tref, last_error => "", fin_state => send_req}}.

handle_call({status}, _From, #{codestring := Code, lastDumpDate := LastDump, update_count := Update, timer := Tref, fin_state := FState, last_error := LastErr } = State) ->
	io:format("~nCurrent state: ~nLastDumpDate: ~p~nNextAction: ~p~nCodeString: ~p~nUpdateCounter: ~p~nLastError: ~p~n", [ts2date(LastDump), FState, Code, Update, LastErr]),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_req, Url, Xml, Sign}, #{lastDumpDate := LastDump, trycount := Try, timer := Tref } = State) ->
	timer:cancel(Tref),
	case blacklist:last_update(Url) of
		{ok, Last, LastUrg} when LastDump < Last; LastDump < LastUrg ->
				case blacklist:send_req(Url, Xml, Sign) of
					{ok, Code} -> 
							Timer = timer:apply_after(180000, ?MODULE, get_reply, [Url, Code]), 
							{noreply, State#{lastDumpDate := lists:max([LastDump, LastUrg]), codestring := Code, fin_state := get_reply, timer := Timer}};
					Any -> Timer = timer:apply_after(Try * 5000, ?MODULE, send_req, [?REG_SRV_URL, "priv/request.xml", "priv/request.bin"]),
							{noreply, State#{trycount := Try + 1, time := Timer, last_error := Any}}
				end;
		{ok, Last, LastUrg} -> Timer = timer:apply_after(1200000, ?MODULE, send_req, [?REG_SRV_URL, "priv/request.xml", "priv/request.bin"]),
							{noreply, State#{timer := Timer}};
		Any -> Timer = timer:apply_after(Try * 5000, ?MODULE, send_req, [?REG_SRV_URL, "priv/request.xml", "priv/request.bin"]),
			   {noreply, State#{trycount := Try + 1, timer := Timer, last_error := Any}}
	end;

handle_cast({get_reply, Url, Id},#{table := Tid, update_count := Update, trycount := Try, timer := Tref } = State) ->
	timer:cancel(Tref), 
	case blacklist:get_reply(Url, Id) of
		{ok, File} ->
			case blacklist:load_xml(File) of 
				{ok, List} ->
					lists:map(fun([{url, U}, {decision, D}, {domain, Dom}, {ip, IPs}] = E) -> 
									Hash = erlang:phash2(U ++ IPs),
									ets:insert(Tid, {Hash, E})
							end, List);
				Any -> 
					lager:error("Parse XML error: ~p~n",[Any])
			end,
			Timer = timer:apply_after(1200000, ?MODULE, send_req, [?REG_SRV_URL, "priv/request.xml", "priv/request.bin"]),
			{noreply, State#{fin_state := send_req, update_count := Update + 1, trycount := 1, timer := Timer, codestring := ""}};
		Any when Try > 3 -> 
			Timer = timer:apply_after(1200000, ?MODULE, send_req, [?REG_SRV_URL, "priv/request.xml", "priv/request.bin"]), 
			{noreply, State#{fin_state := send_req, trycount := 1, timer := Timer, codestring := "", last_error := Any}};
		Any -> 
			Timer = timer:apply_after(Try * 120000, ?MODULE, get_reply, [Url, Id]), 
			{noreply, State#{trycount := Try + 1, timer := Timer}}
	end; 

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internals

ts2date(Ts) ->
    {{Y, M, D}, {H, Min, _S}} =
		calendar:now_to_local_time({Ts div 1000000, Ts rem 1000000, 0}),
    lists:flatten(io_lib:format('~2..0b-~3s-~4..0b, ~2..0b:~2..0b',
				[D, proplists:get_value(M, ?MONTH), Y, H, Min])).