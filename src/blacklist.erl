%% -*- coding: utf-8 -*-
-module(blacklist).

-include_lib("xmerl/include/xmerl.hrl").

-compile([{parse_transform, lager_transform}]).

-export([load_xml/1, send_req/3, get_reply/2, last_update/1,
		 export_to_csv/4, jnx_set_route/2, jnx_del_route/2]).

%% @spec load_xml(F :: list()) -> {ok, DeepList} | {error, Reason}
%%		DeepList = [Proplist]
%%		Proplist = [{decision,list()} | {url, list()} | {domain, list()} | {ip, list()}]
%% @doc Open XML file file F and load list from it. File encoding must be UTF-8
%% @end
load_xml(File) when is_list(File) ->
    try xmerl_scan:file(File,[{encoding, 'latin1'}]) of
    	{Doc, _Any} -> {ok, parse_xml(Doc)}
    	catch _:E -> {error, E}
    end.

%% @spec export_to_csv(F :: list(), L :: DeepList, S :: Separator, Fields) -> ok | {error, Reason}
%%		DeepList = [Proplist]
%%		Proplist = [{decision,list()} | {url, list()} | {domain, list()} | {ip, list()}]
%%		Separator = character()
%%		Fields = decision | domain | url | ip
%% @doc Write blacklist to file F as CSV, with separator S.
%% @end
export_to_csv(F,L,S,E) ->
	case file:open(F, [raw,write]) of
		{ok, Io} -> write_csv(Io,L,S,E);
		{error,Reason} -> {error, Reason}
	end. 

%% @spec jnx_set_route(F :: list(), L :: list()) -> {ok, List} | {error, reason}
%% @doc Make Juniper config to file F with set route commands.
%% @end
jnx_set_route(F, L) -> jnx_route("set", F, L).

%% @spec jnx_del_route(F :: list(), L :: list()) -> {ok, List} | {error, reason}
%% @doc Make Juniper config to file F with delete route commands.
%% @end
jnx_del_route(F, L) -> jnx_route("delete", F, L).

%% @spec send_req(Rf :: list(), Sf :: list()) -> {ok, ReqId} | {error, Error}
%%		ReqId = list()
%% @doc Send SOAP request where Rf - XML request file, Sf - request sign file.
%% @end
send_req(Url, Rf, Sf) ->
	{ok, Rfl} = file:read_file(Rf), 
	{ok, Sfl} = file:read_file(Sf), 
	try yaws_soap_lib:call(Url, "sendRequest", [base64:encode(Rfl),base64:encode(Sfl)]) of
		{ok,_,
			[{'p:sendRequestResponse',_,true,_,Id}]} -> {ok, Id};
		E -> {error, E}
	catch _:X -> {error, X}
	end.	

%% @spec get_reply(Id :: list()) -> {ok, XMLFileName, ArchiveFileName} | {error, Reason}
%% @doc Fetch reply from register service, store archive and extract XML file.
%% @end
get_reply(Url,Id) ->
	File = "priv/arch/" ++ arch_name(),
	try yaws_soap_lib:call(Url, "getResult",[Id]) of
		{ok,_,
			[{'p:getResultResponse',_,true,_,Reply}]} -> Data = base64:decode(Reply),
					   ok = file:write_file(File, Data),
					   {ok, [XML]} = zip:extract(File,[{cwd,"priv"},{file_list,["dump.xml"]}]),
					   {ok, XML, File};
		E -> {error, E}
	catch _:X -> {error, X}
	end.

%% @spec last_update() -> {ok, TimeStamp, UrgentTimeStamp} | {error, error}
%% @doc Get register last update timestamp.
%% @end
last_update(Url) ->
	try yaws_soap_lib:call(Url, "getLastDumpDateEx",[]) of
		{ok,_,
			[{'p:getLastDumpDateExResponse',_,LastDumpMs,LastDumpUrgMs}]} ->
				{ok, list_to_integer(LastDumpMs) div 1000, list_to_integer(LastDumpUrgMs) div 1000};
		E -> {error, E}
	catch _:X -> {error, X}
	end.

%% @hidden
write_csv(Io,[],_S,_E) -> file:close(Io); 
write_csv(Io,[H|T],S,E) ->
	Str = string:join([proplists:get_all_values(El, H) || El <- E], S),
	file:write(Io, unicode:characters_to_binary(Str ++ "\n")),
	write_csv(Io,T,S,E). 

%% @hidden
parse_xml(D) ->
	PList = [ [ case El1#xmlElement.name of
					decision ->
						Dl = [ extract(A) || A <- El1#xmlElement.attributes ],
						[{decision, proplists:get_value(number, Dl)}, 
						 {org, proplists:get_value(org, Dl)}, {date, proplists:get_value(date, Dl)}];
					_ ->
						[ extract(A) || A <- El1#xmlElement.content ]
				end
			|| El1 <- El] || El <- [ El#xmlElement.content || El <- D#xmlElement.content]],
			List = [lists:flatten(E) || E <- PList],
			lists:foldl(fun(E, Acc) ->
					case proplists:get_all_values(url, E)  of
						[Val] when is_list(Val) -> Acc ++ [[{url, proplists:get_value(url, E)},
									 				 {decision, proplists:get_value(decision, E)},
									 				 {org, proplists:get_value(org, E)},
									 				 {date, proplists:get_value(date, E)},
						                             {domain, proplists:get_value(domain, E)},
						                             {ip, string:join(proplists:get_all_values(ip, E) , " ")}]];
						Val when is_list(Val) ->  FList = [ [{url, El},
									 {decision, proplists:get_value(decision, E)},
									 {org, proplists:get_value(org, E)},
									 {date, proplists:get_value(date, E)},
						             {domain, proplists:get_value(domain, E)},
						             {ip, string:join(proplists:get_all_values(ip, E) , " ")}] || El <- Val],
						             lists:foldl(fun(E1, Acc0) -> Acc0 ++ [E1] end, Acc, FList)
					end
			end, [], List).

%% @hidden
extract(#xmlAttribute{name = date, value = Value}) -> {date, Value};
extract(#xmlAttribute{name = number, value = Value}) -> {number, unicode:characters_to_list(win_to_utf(Value))};
extract(#xmlAttribute{name = org, value = Value}) -> {org, unicode:characters_to_list(win_to_utf(Value))};
extract(#xmlText{parents=[{Name,_},_,_], value = Value}) -> {Name, unicode:characters_to_list(win_to_utf(Value))};
extract(_) -> ok.


%% @hidden
%% CP1251 -> UTF-8
%% recipe from http://stm.rest.ru/blog/?p=75
conv2(Char) ->
	if
		Char == 136 -> _Char = Char + 889; %% Ё
		Char == 168 -> _Char = Char + 969; %% ё
		Char >= 191 -> _Char = Char + 848; %% А..Яа..я
		true -> Char
	end.

%% @hidden 
win_to_utf(Str) ->
	[conv2(Char) || Char <- Str].

%% @hidden
arch_name() ->
    {{Y, M, D}, {H, Mm, _S}} = calendar:local_time(),
    lists:flatten(io_lib:format('~4..0b-~2..0b-~2..0b-~2..0b-~2..0b.zip',[Y, M, D, H, Mm])).

%% @hidden
jnx_route(Prefix, File, L) ->
	case file:open(File, [write]) of
		{ok, Dev} -> make_jnx_route_list(Prefix, Dev, L);
		{error, Reason} -> {error, Reason}
	end. 
%% @hidden
make_jnx_route_list(_Prefix, Dev, []) -> file:close(Dev);

make_jnx_route_list(Prefix, Dev, [H|T]) ->
	Str = Prefix ++ " static route " ++ binary_to_list(H) ++ "/32 discard\n",
	case file:write(Dev, list_to_binary(Str)) of
		ok -> make_jnx_route_list(Prefix, Dev, T);
		{error, Reason} -> {error, Reason}
	end.
