-module(blacklist).

-export([load_list/1, jnx_set_route/2, jnx_del_route/2]).

load_list(File) when is_list(File) ->
    case file:open(File, [read]) of
    	{ok, Dev} -> parse_list(Dev,[]);
    	{error, Reason} -> {error, Reason}
    end.

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

jnx_set_route(F, L) -> jnx_route("set", F, L).
jnx_del_route(F, L) -> jnx_route("delete", F, L).

jnx_route(Prefix, File, L) ->
	case file:open(File, [write]) of
		{ok, Dev} -> make_jnx_route_list(Prefix, Dev, L);
		{error, Reason} -> {error, Reason}
	end. 

make_jnx_route_list(_Prefix, Dev, []) -> file:close(Dev);

make_jnx_route_list(Prefix, Dev, [H|T]) ->
	Str = Prefix ++ " static route " ++ binary_to_list(H) ++ "/32 discard;\n",
	io:format("Str: ~p~n", [Str]),
	case file:write(Dev, list_to_binary(Str)) of
		ok -> make_jnx_route_list(Prefix, Dev, T);
		{error, Reason} -> {error, Reason}
	end.
