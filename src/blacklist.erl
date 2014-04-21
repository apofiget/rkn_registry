%% -*- coding: utf-8 -*-

-module(blacklist).

-include_lib("xmerl/include/xmerl.hrl").

-compile([{parse_transform, lager_transform}]).

-export([load_csv/1, load_xml/1, 
		 export_to_csv/4,
		 jnx_set_route/2, jnx_del_route/2]).

%% @spec load_csv(F :: list()) -> {ok, List} | {error, Reason}
%% @doc Open file file F and load list from it. String separator is ';'
%% @end
load_csv(File) when is_list(File) ->
    case file:open(File, [read]) of
    	{ok, Dev} -> parse_csv(Dev,[]);
    	{error, Reason} -> {error, Reason}
    end.

%% @spec load_xml(F :: list()) -> {ok, DeepList} | {error, Reason}
%%		DeepList = [Proplist]
%%		Proplist = [{decision,list()} | {url, list()} | {domain, list()} | {ip, list()}]
%% @doc Open XML file file F and load list from it. File encoding must be UTF-8
%% @end
load_xml(File) when is_list(File) ->
    try xmerl_scan:file(File) of
    	{Doc, _Any} -> {ok, parse_xml(Doc)}
    	catch _:E -> {error, E}
    end.

%% @spec export_to_csv(F :: list(), L :: DeepList, S :: Separator, Fields) -> ok | {error, Reason}
%%		DeepList = [Proplist]
%%		Proplist = [{decision,list()} | {url, list()} | {domain, list()} | {ip, list()}]
%%      Separator = character()
%%		Fields = decision | domain | url | ip
%% @doc Write blacklist to file F as CSV, with separator S. Format is:<\br>
%% decision:url:ip
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

%% @hidden
write_csv(Io,[],_S,_E) -> file:close(Io); 
write_csv(Io,[H|T],S,E) ->
	Str = string:join([proplists:get_value(El, H, "error_field_undefined") || El <- E], S),
	file:write(Io, unicode:characters_to_binary(Str ++ "\n")),
	write_csv(Io,T,S,E). 

%% @hidden
parse_csv(Dev, Acc) ->
	case file:read_line(Dev) of 
		eof -> file:close(Dev), {ok, Acc};
		{error, Reason} -> file:close(Dev), {error, Reason};
		{ok, Data} ->	
			case string:tokens(Data, ";") of
				[H|T] when T =/= [] -> 
					El = list_to_binary(H),
					Is_member = lists:member(El , Acc),
					if Is_member ->  
							parse_csv(Dev, Acc);
					 true -> parse_csv(Dev, Acc ++ [El]) end;
				_->
					lager:warning("Can't parse string: ~p~n",[Data]),
					parse_csv(Dev, Acc) 
			end
	end.

%% @hidden
parse_xml(D) ->
	PList = [ [ case El1#xmlElement.name of
					decision ->
						Dl = [ extract(A) || A <- El1#xmlElement.attributes ],
						[{decision, "Решение: " ++ proplists:get_value(number, Dl) ++ ", от: " ++ 
						  proplists:get_value(date, Dl) ++ 
						  ". Организация: " ++ proplists:get_value(org, Dl)}];
					_ ->
						[ extract(A) || A <- El1#xmlElement.content ]
				end
			|| El1 <- El] || El <- [ El#xmlElement.content || El <- D#xmlElement.content]],
	[begin 
		K = proplists:get_keys(E), 
		[  
			case proplists:get_all_values(Key, E) of
				[Value] when is_list(Value) -> {K,Value};
				Value when is_list(Value) -> {K,string:join(Value, ",")}  
			end
		|| Key <- K] 
	 end 
		|| E <- [lists:flatten(E) || E <- PList]
	].

post_proc(K,L) ->
	case proplists:get_all_values(K, L) of
		[Value] when is_list(Value) -> {K,Value};
		Value when is_list(Value) -> {K,string:join(Value, ",")}  
	end. 

%% @hidden
extract(#xmlAttribute{name = date, value = Value}) -> {date, Value};
extract(#xmlAttribute{name = number, value = Value}) -> {number, unicode:characters_to_list(Value)};
extract(#xmlAttribute{name = org, value = Value}) -> {org, unicode:characters_to_list(Value)};
extract(#xmlText{parents=[{Name,_},_,_], value = Value}) -> {Name, unicode:characters_to_list(Value)};
extract(_) -> ok.

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
