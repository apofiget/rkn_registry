-module(registry_web_tool).

-author("Andrey Andruschenko <apofiget@gmail.com>").

-compile([{parse_transform, lager_transform}]).

-export([act/2]).

-include("../deps/yaws/include/yaws_api.hrl").
-include("include/registry.hrl").

act(Act, Args) ->
    Fn = case Act of
             list_only -> fun() ->
                                  handle_act(Act, list_to_atom(proplists:get_value("field", Args, "undefined"))) end;
             list ->
                 case [ proplists:get_value(P,yaws_api:parse_query(Args), undefined) || P <- ["field", "value"]] of
                     [undefined, undefined] ->
                         fun() ->
                                 handle_act(Act, fun() -> registry:list() end) end;
                     [F, V] when F =/= undefined, V =/= undefined ->
                         fun() ->
                                 handle_act(Act, fun() -> registry:search(list_to_atom(F) , V) end ) end;
                     [_,_] ->
                         fun() ->
                                 prep("error", "Отсутствуют необходимые аргументы") end
                 end;
             _-> fun() -> handle_act(Act, Args) end
         end,
    try Fn()
    catch _:_ ->
            lager:error("Runtime error. Stacktrace: ~p~n",[erlang:get_stacktrace()]),
            prep("error","Ошибка исполнения") end.

%%% Internals
handle_act(status, _) -> prep("ok",json2:obj_from_list(registry:status()));

handle_act(list, Fn) ->
    case Fn() of
        [] -> prep("ok",[]);
        List -> prep("ok",[json2:obj_from_list(El) || El <-
                                                          [ lists:reverse(lists:foldl(fun({entryType, I}, Acc) ->
                                                                                              [{entryType, tools:get_reg_type(I)} | Acc];
                                                                                         ({K,V}, Acc) when V =:= undefined; V =:= [] ->
                                                                                              [{K,<<"Значение атрибута не определено"/utf8>>} | Acc];
                                                                                         (El, Acc) ->
                                                                                              [El | Acc]
                                                                                      end, [], E)) || E <- List]])
    end;

handle_act(list_only, Crt) ->
    case registry:list_only(Crt) of
        [] -> prep("ok",[]);
        List -> prep("ok",[json2:encode(El) || El <- List ])
    end;

handle_act(dump_route, Args) ->
    lists:concat(case proplists:get_value("spec", Args) of
                     undefined -> "";
                     F -> case  proplists:get_value("table", Args) of
                              undefined -> apply(blacklist, list_to_atom(F), [registry:list_only(ip)]);
                              T -> apply(blacklist, list_to_atom(F), [registry:list_only(ip), T])
                          end
                 end);

handle_act(_,_) -> prep("error","Unknown function").

prep(Status, Data) -> json2:encode(json2:obj_from_list([{"status", Status}, {"version", tools:vsn()}, {"data",  Data}])).
