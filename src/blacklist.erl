%% -*- coding: utf-8 -*-
-module(blacklist).

-author("Andrey Andruschenko <apofiget@gmail.com>").

-include_lib("xmerl/include/xmerl.hrl").

-compile([{parse_transform, lager_transform}]).

-export([load_xml/1, send_req/3, send_req/4, get_reply/3, last_update/1,
		 export_to_csv/4, jnx_set_route/1, jnx_del_route/1, csc_set_route/1, 
		 csc_set_route/2, csc_del_route/1, csc_del_route/2]).

%% @spec load_xml(F :: list()) -> {ok, DeepList} | {error, Reason}
%%		DeepList = [Proplist]
%%		Proplist = [{decision,list()} | {url, list()} | {domain, list()} | {ip, list()} | {id, list()} | {subnet , list()} | {entryType, integer} | {includeTime, list()}]
%% @doc Open XML file file F and load list from it. File encoding must be UTF-8
%% @end
load_xml(File) when is_list(File) ->
    try xmerl_scan:file(File,[{encoding, 'latin1'}]) of
    	{Doc, _Any} -> {ok, xml_to_proplist(Doc)}
    	catch _:E -> {error, E}
    end.

%% @spec export_to_csv(F :: list(), L :: DeepList, S :: Separator, Fields) -> ok | {error, Reason}
%%		DeepList = [Proplist]
%%		Proplist = [{decision,list()} | {url, list()} | {domain, list()} | {ip, list()} | {id, list()} | {subnet , list()} | {entryType, integer} | {includeTime, list()}]
%%		Separator = character()
%%		Fields = decision | domain | url | ip
%% @doc Write blacklist to file F as CSV, with separator S.
%% @end
export_to_csv(F,L,S,E) ->
	case file:open(F, [raw,write]) of
		{ok, Io} -> write_csv(Io,L,S,E);
		{error,Reason} -> {error, Reason}
	end. 

%% @spec jnx_set_route(L :: list()) -> List
%% @doc Make Juniper config with set route commands.
%% @end
jnx_set_route(L) -> make_jnx_route_list("set", L).

%% @spec jnx_del_route(L :: list()) -> List
%% @doc Make Juniper config with delete route commands.
%% @end
jnx_del_route(L) -> make_jnx_route_list("delete", L).

%% @spec csc_set_route(L :: list()) -> List
%% @doc Make Cisco config with ip route commands in global route table.
%% @end
csc_set_route(L) -> make_csc_route_list("", "", L).

%% @spec csc_del_route(L :: list()) -> List
%% @doc Make Cisco config with no ip route commands in global route table.
%% @end
csc_del_route(L) -> make_csc_route_list("no ", "", L).

%% @spec csc_set_route(L :: list(), V :: list()) -> List
%% @doc Make Cisco config with ip route commands in V routing table.
%% @end
csc_set_route(L, V) -> make_csc_route_list("", " vrf " ++ V, L).

%% @spec csc_del_route(L :: list()) -> List
%% @doc Make Cisco config with no ip route commands in V routing table.
%% @end
csc_del_route(L, V) -> make_csc_route_list("no ", " vrf " ++ V, L).

%% @spec send_req(Rf :: list(), Sf :: list(), Ver :: list()) -> {ok, ReqId} | {error, Error}
%%		ReqId = list()
%% @doc Send SOAP request where Rf - XML request file, Sf - request sign file, Ver - registry version ["1.0" | "2.0"]
%% @end
send_req(Url, Rf, Sf) -> send_req(Url, Rf, Sf, "1.0").

send_req(Url, Rf, Sf, Ver) ->
	{ok, Rfl} = file:read_file(Rf), 
	{ok, Sfl} = file:read_file(Sf), 
	try yaws_soap_lib:call(Url, "sendRequest", [base64:encode(Rfl),base64:encode(Sfl), Ver]) of
		{ok,_,[{'p:sendRequestResponse',_,true,_,Id}]} -> {ok, Id};
		{ok,_,[{'p:sendRequestResponse',_,false,Comment,_}]} -> {error, win_to_utf(Comment)};
		E -> {error, E}
	catch _:X -> {error, X}
	end.	

%% @spec get_reply(Url :: list(), Id :: list(), SavePath :: list()) -> {ok, ArchiveFileName, Version} | {error, Reason}
%% @doc Fetch reply from register service and store archive.
%% @end
get_reply(Url,Id, SavePath) ->
	File = SavePath ++ arch_name(),
	%%% use try because yaws_soap_lib have error with
	%%% pattern-matching when HTTP error while process RPC
	try yaws_soap_lib:call(Url, "getResult",[Id]) of
		{ok,_,
			[{'p:getResultResponse', _, true, _, Reply}]} -> 
			 save_reply(Reply, File, "1.0");	%%% Only for 1.0 version !!!
		{ok, _,
			[{'p:getResultResponse', _, true, _RComment, Reply, _RCode, DocVer}]} ->
				save_reply(Reply, File, DocVer);
		{ok, _,
			[{'p:getResultResponse', _, false, _RComment, _Reply, RCode, _DocVer}]} ->
				{error, RCode};
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
		{ok,_,
			[{'p:getLastDumpDateExResponse',_,LastDumpMs,LastDumpUrgMs, _WebServVer, _DumpFormatVer, _DocVer}]} ->
				{ok, list_to_integer(LastDumpMs) div 1000, list_to_integer(LastDumpUrgMs) div 1000};
		E -> {error, E}
	catch _:X -> {error, X}
	end.

%% @hidden
write_csv(Io,[],_S,_E) -> file:close(Io); 
write_csv(Io,[H|T],S,E) ->
	Str = string:join([ 
		if El =:= ip -> 
			string:join(proplists:get_value(El, H), " "); 
			true -> proplists:get_all_values(El, H)
		end
	 || El <- E], S),
	file:write(Io, unicode:characters_to_binary(Str ++ "\n")),
	write_csv(Io,T,S,E). 

%% @hidden
xml_to_proplist(D) ->
	List = [ parse_element_attrs(El) ++ [parse_element_content(El1) 
		|| El1 <- El#xmlElement.content, is_record(El1, xmlElement)] 
		|| El <- D#xmlElement.content, is_record(El, xmlElement)],
		lists:foldl(fun(E, Acc) ->
			case proplists:get_all_values(url, E)  of
				[] -> 
					Acc ++ [[{url, []}] ++ normalize_proplist(E)];
				Val when is_list(Val) ->  
					FList = [ [{url, El}]  ++ normalize_proplist(E) || El <- Val],
				  lists:foldl(fun(E1, Acc0) -> Acc0 ++ [E1] end, Acc, FList)
			end
		end, [], [lists:flatten(E) || E <- List]).

%% @hidden
parse_element_attrs(El) ->
	[  extract(A) || A <- El#xmlElement.attributes ].

%% @hidden
parse_element_content(El) ->
case El#xmlElement.name of
	decision ->
		Dl = [ extract(A) || A <- El#xmlElement.attributes ],
		[{decision, proplists:get_value(number, Dl)}, 
		 {org, proplists:get_value(org, Dl)}, {date, proplists:get_value(date, Dl)}];
	_ ->
		[ extract(A) || A <- El#xmlElement.content ]
end.

%% @hidden
normalize_proplist(E) ->
	[{id,proplists:get_value(id, E)},
	{entryType,list_to_integer(proplists:get_value(entryType, E))},
	{includeTime,proplists:get_value(includeTime, E)},
	{decision, proplists:get_value(decision, E)},
	{org, proplists:get_value(org, E)},
	{date, proplists:get_value(date, E)},
	{domain, proplists:get_value(domain, E)},
	{subnet, proplists:get_value(ipSubnet, E)},
	{ip, proplists:get_all_values(ip, E)}].

%% @hidden
extract(#xmlAttribute{name = date, value = Value}) -> {date, Value};
extract(#xmlAttribute{name = number, value = Value}) -> {number, unicode:characters_to_list(win_to_utf(Value))};
extract(#xmlAttribute{name = org, value = Value}) -> {org, unicode:characters_to_list(win_to_utf(Value))};
extract(#xmlAttribute{name = Name, value = Value}) -> {Name, Value};
extract(#xmlText{parents=[{Name,_},_,_], value = Value}) -> {Name, unicode:characters_to_list(win_to_utf(Value))};
extract(_) -> ok.

%% @hidden
save_reply(Reply, File, Ver) ->
	Data = base64:decode(Reply),
	try file:write_file(File, Data) of
		ok -> {ok, File, Ver}
	catch _:X -> {error, X}
	end.

%% @hidden
%% CP1251 -> UTF-8
%% recipe from http://stm.rest.ru/blog/?p=75
conv2(Char) ->
	if
		Char == 136 -> Char + 889; %% Ё
		Char == 168 -> Char + 969; %% ё
		Char >= 191 -> Char + 848; %% А..Яа..я
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
make_jnx_route_list(Prefix, L) ->
	lists:foldl(fun(E, Acc) -> lists:append(Acc, Prefix ++ " static route " ++ E ++ "/32 discard\n") end, [], L).

%% @hidden
make_csc_route_list(Prefix, Table, L) -> 
	lists:foldl(fun(E,Acc) -> lists:append(Acc, Prefix ++ "ip route" ++ Table ++ " " ++ E ++ " 255.255.255.255 Null 0\n") end, [], L).
