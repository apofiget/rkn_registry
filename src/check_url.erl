%% -*- coding: utf-8 -*-
-module(check_url).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([start_link/0, start_test/2, start_test/3, test/1, result/2, web_client/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

result(Url, Result) -> gen_server:cast(?MODULE, {result, Url, Result}). 

test(List) -> 
	gen_server:cast(?MODULE, {start, List}).

init([]) ->
	[application:ensure_started(App) || App <- [lager,ssl,inets,ibrowse] ],
	{ok, ets:new(?MODULE, [])}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, [{decision, _Dec}, {url, Url}, {domain, _Domain}, {ip, IP}]}, State) ->
	case ets:lookup(State, Url) of
		[] -> ok;
		[Obj] -> ets:delete_object(State, Obj)
	end,
	ets:insert(State, {Url, [{ip,IP}]}),
	spawn(?MODULE, web_client, [Url]),
	{noreply, State};

handle_cast({result, Url, Result}, State) ->
	case ets:lookup(State, Url) of
		[] -> ok;
		[{Url,List}] -> ets:insert(State, {Url, List ++ Result}) 
	end, 
    {noreply, State};

handle_cast(_Msg,State) ->
	{noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%

web_client(Url) ->
	try ibrowse:send_req(string:strip(Url) , [], head,[],[{connect_timeout, 10000},{socket_options, [{active,true}]}],20000) of
		{error, Error} -> result(Url,[{error, Error}]);
		{ok, Code, Resp, _} -> result(Url,[{code, Code},{size, proplists:get_value("Content-Length", Resp, 0)}])
	catch _:E -> result(Url,[{error, E}])
	end.

start_test(L,Num) -> spawn_link(?MODULE, start_test, [L,Num,Num]).
start_test([],_Num,_Opt) -> ok;
start_test(L,0,Opt) ->
	timer:sleep(2000),
	start_test(L,Opt,Opt);
start_test([H|T],Num,Opt) ->
	test(H),
	timer:sleep(100), 
	start_test(T,Num-1,Opt).
	
