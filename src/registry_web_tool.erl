-module(registry_web_tool).

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
		[] -> prep_error("No data");
		List -> prep_data([json2:obj_from_list(El) || El <- List ])
	end;

handle_act(status) -> prep_data(json2:obj_from_list(registry:status()));

handle_act(_) -> prep_error("Unknown function").

handle_act(filter, Crt) ->
	case registry:filter(Crt) of
		[] -> prep_error("No data");
		List -> prep_data([List])
	end.

prep_data(Data) -> json2:encode(json2:obj_from_list([{"status", "ok"}, {"data",  Data}])).
prep_error(Msg) -> json2:encode(json2:obj_from_list([{"status", "error"}, {"error",  Msg}])).