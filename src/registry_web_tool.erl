-module(registry_web_tool).

-author("Andrey Andruschenko <apofiget@gmail.com>").

-compile([{parse_transform, lager_transform}]).

-export([act/2]).

-include("../deps/yaws/include/yaws_api.hrl").
-include("include/registry.hrl").

act(Act, Args) -> 
	Fn = case Act of
		list_only -> fun() -> handle_act(Act, list_to_atom(proplists:get_value("field", Args, "undefined"))) end;
		list -> 
			case [ proplists:get_value(P,yaws_api:parse_query(Args), undefined) || P <- ["field", "value"]] of
				[undefined, undefined] -> 
					fun() -> handle_act(Act, fun() -> registry:list() end) end;
				[F, V] when F =/= undefined, V =/= undefined -> 
					fun() -> handle_act(Act, fun() -> registry:search(list_to_atom(F) , V) end ) end;
				[_,_] -> 
					fun() -> prep("error", "Отсутствуют необходимые аргументы") end
			end;
		_-> fun() -> handle_act(Act) end
	end,
	try Fn()	
		catch _:_ -> 
			lager:error("Runtime error. Stacktrace: ~p~n",[erlang:get_stacktrace()]),
			prep("error","Ошибка исполнения") end.

%%% Internals
handle_act(status) -> prep("ok",json2:obj_from_list(registry:status()));

handle_act(_) -> prep("error","Unknown function").

handle_act(list, Fn) ->
	case Fn() of
		[] -> prep("ok",[]);
		List -> prep("ok",[json2:obj_from_list(El) || El <- 
			[ lists:foldl(fun(El, Acc) -> 
				R = case El of 
					{entryType, I} -> 
						{entryType, tools:get_reg_type(I)}; 
					{K,V} when V =:= undefined; V =:= [] -> 
						{K,<<"Значение атрибута не определено"/utf8>>}; 
					_-> El
			end, Acc ++ [R]
			end, [], E) || E <- List] ])
	end;

handle_act(list_only, Crt) ->
	case registry:list_only(Crt) of
		[] -> prep("ok",[]);
		List -> prep("ok",[json2:encode(El) || El <- List ])
	end.

prep(Status, Data) -> json2:encode(json2:obj_from_list([{"status", Status}, {"data",  Data}])).
