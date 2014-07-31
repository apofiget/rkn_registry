-module(registry_web_tool).

-author("Andrey Andruschenko <apofiget@gmail.com>").

-compile([{parse_transform, lager_transform}]).

-export([act/2]).

-include("../deps/yaws/include/yaws_api.hrl").
-include("include/registry.hrl").

act(Act, Args) -> 
	case Act of
		filter -> handle_act(Act, list_to_atom(proplists:get_value("match", Args, "undefined")));
		_-> handle_act(Act)
	end.

%%% Internals
handle_act(list) ->
	case registry:list() of
		[] -> prep("error","No data");
		List -> prep("ok",[json2:obj_from_list(El) || El <- 
			[ case E of 
				{type, I} -> {type, tools:get_reg_type(I)}; 
				{K,undefined} -> 
				{K,<<"Значение атрибута не определено"/utf8>>}; _-> E 
			end || E <- List] ])
	end;

handle_act(status) -> prep("ok",json2:obj_from_list(registry:status()));

handle_act(_) -> prep("error","Unknown function").

handle_act(filter, Crt) ->
	case registry:filter(Crt) of
		[] -> prep("error","No data");
		List -> prep("ok",[json2:encode(El) || El <- List ])
	end.

prep(Status, Data) -> json2:encode(json2:obj_from_list([{"status", Status}, {"data",  Data}])).
