-module(blacklist).

-include_lib("xmerl/include/xmerl.hrl").

-compile([{parse_transform, lager_transform}]).

-export([load_csv/1, load_xml/1, jnx_set_route/2, jnx_del_route/2]).

%% @spec load_csv(F :: list()) -> {ok, List} | {error, Reason}
%% @doc Open file file F and load list from it. String separator is ';'
%% @end
load_csv(File) when is_list(File) ->
    case file:open(File, [read]) of
    	{ok, Dev} -> parse_list(Dev,[]);
    	{error, Reason} -> {error, Reason}
    end.

%% @spec load_xml(F :: list()) -> {ok, List} | {error, Reason}
%% @doc Open XML file file F and load list from it. File encoding must be UTF-8
%% @end
load_xml(File) when is_list(File) ->
    try xmerl_scan:file("priv/dump1.xml") of
    	{Doc, _Any} -> parse_xml(Doc,[])
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
parse_list(Dev, Acc) ->
	case file:read_line(Dev) of 
		eof -> file:close(Dev), {ok, Acc};
		{error, Reason} -> file:close(Dev), {error, Reason};
		{ok, Data} ->	
			case string:tokens(Data, ";") of
				[H|T] when T =/= [] -> 
					El = list_to_binary(H),
					Is_member = lists:member(El , Acc),
					if Is_member ->  
							parse_list(Dev, Acc);
					 true -> parse_list(Dev, Acc ++ [El]) end;
				_->
					lager:warning("Can't parse string: ~p~n",[Data]),
					parse_list(Dev, Acc) 
			end
	end.

%% @hidden
parse_xml(D,Acc) ->
	case lager:trace_file("priv/xml_trace.txt", [{module, ?MODULE}], debug) of
		Trace when is_tuple(Trace) ->  
				L = [ El#xmlElement.content || El <- D#xmlElement.content],
				List = [ [ 
							case El1#xmlElement.name of
								decision ->
									[ extract(A) || A <- El1#xmlElement.attributes ];
								url ->
									%[  lager:debug("~p~n",[lager:pr(A, ?MODULE)]) || A <- El1#xmlElement.content ];
									[ extract(A) || A <- El1#xmlElement.content ];
								ip ->
									[ extract(A) || A <- El1#xmlElement.content ];
								domain ->
									[ extract(A) || A <- El1#xmlElement.content ];
								_-> ok
							end
							|| El1 <- El] || El <- L],
				%[E || E <- List];
				[lager:debug("~p~n",[E]) || E <- List];
		_-> lager:error("Trace error.")
	end.

%% @hidden

extract(#xmlAttribute{name = date, value = Val}) -> {date, Val};
extract(#xmlAttribute{name = number, value = Val}) -> {number, unicode:characters_to_list(Val)};
extract(#xmlAttribute{name = org, value = Val}) -> {org, unicode:characters_to_list(Val)};
extract(#xmlText{parents=[{url,_},{content,_},{'reg:register',_}], value = URL}) -> {url, unicode:characters_to_list(URL)};
extract(#xmlText{parents=[{ip,_},{content,_},{'reg:register',_}], value = IP}) -> {ip, unicode:characters_to_list(IP)};
extract(#xmlText{parents=[{domain,_},{content,_},{'reg:register',_}], value = Domain}) -> {domain, unicode:characters_to_list(Domain)};
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
