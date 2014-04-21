-module(blacklist).

-include_lib("xmerl/include/xmerl.hrl").

-compile([{parse_transform, lager_transform}]).

-export([load_csv/1, load_xml/1, 
		 export_to_csv/2,
		 jnx_set_route/2, jnx_del_route/2]).

%% @spec load_csv(F :: list()) -> {ok, List} | {error, Reason}
%% @doc Open file file F and load list from it. String separator is ';'
%% @end
load_csv(File) when is_list(File) ->
    case file:open(File, [read]) of
    	{ok, Dev} -> parse_csv(Dev,[]);
    	{error, Reason} -> {error, Reason}
    end.

%% @spec load_xml(F :: list()) -> {ok, List} | {error, Reason}
%% @doc Open XML file file F and load list from it. File encoding must be UTF-8
%% @end
load_xml(File) when is_list(File) ->
    try xmerl_scan:file(File) of
    	{Doc, _Any} -> {ok, parse_xml(Doc)}
    	catch _:E -> {error, E}
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
	L = [ El#xmlElement.content || El <- D#xmlElement.content],
	List = [ [ 
				case El1#xmlElement.name of
					decision ->
						[ extract(A) || A <- El1#xmlElement.attributes ];
					ip ->
						{ip, [IP || {ip, IP} <- [ extract(A) || A <- El1#xmlElement.content]]};
					_ ->
						[ extract(A) || A <- El1#xmlElement.content ]
				end
			|| El1 <- El] || El <- L],
	[lists:flatten(E) || E <- List].

%% @hidden

extract(#xmlAttribute{name = date, value = Val}) -> {date, Val};
extract(#xmlAttribute{name = number, value = Val}) -> {number, unicode:characters_to_list(Val)};
extract(#xmlAttribute{name = org, value = Val}) -> {org, unicode:characters_to_list(Val)};
extract(#xmlText{parents=[{Name,_},{content,_},{'reg:register',_}], value = Value}) -> {Name, unicode:characters_to_list(Value)};
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
