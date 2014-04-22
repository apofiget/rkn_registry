%% -*- coding: utf-8 -*-

-module(check_url).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-export([start_link/0, test/1, result/2, web_client/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

result(Url, Result) -> gen_server:cast(?MODULE, {result, Url, Result}). 

test(List) -> 
	io:format("Start test~n"),
	gen_server:cast(?MODULE, {start, List}).

init([]) ->
	{ok, ets:new(?MODULE, [])}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start, List}, State) ->
	[ begin 
			ets:insert(State, {Url, [{decision, Dec}, {ip, IP}, {domain, Domain}]}),
			spawn(?MODULE, web_client, [Url]) 
		  end
	|| [{decision, Dec}, {url, Url}, {domain, Domain}, {ip, IP}] <- List],
	{noreply, State};

handle_cast({result, Url, Result}, State) ->
	case ets:lookup(State, Url) of
		[] -> ok;
		[List] -> ets:insert(State, {Url, [List] ++ Result}) 
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
	try ibrowse:send_req(Url, [], head) of
		{error, Error} -> result(Url,[{error, Error}]);
		{ok, Code, Resp, _} -> result(Url,[{code, Code},{size, proplists:get_value("Content-Length", Resp)}])
	catch _:E -> result(Url,[{error, E}])
	end.