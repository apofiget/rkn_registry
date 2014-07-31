%% -*- coding: utf-8 -*-
-module(tools).

-author("Andrey Andruschenko <apofiget@gmail.com>").

-export([get_option/1, get_reg_type/1,
	ts2date/1, unix_ts/0, to_list/1, get_result_comment/1]).

-include("include/registry.hrl").

get_reg_type(Id) -> proplists:get_value(Id, ?REG_TYPE, <<"Неизвестный тип реестра"/utf8>>).
get_result_comment(Id) -> proplists:get_value(Id, ?RESULT_COMMENT, <<"Неизвестный код ответа"/utf8>>).

get_option(Key) ->
    case application:get_env(Key) of
      undefined -> proplists:get_value(Key, ?DEFCONF);
      {ok, Val} -> Val
    end.

ts2date(Ts) ->
    {{Y, M, D}, {H, Min, _S}} =
		calendar:now_to_local_time({Ts div 1000000, Ts rem 1000000, 0}),
    lists:flatten(io_lib:format('~2..0b-~3s-~4..0b, ~2..0b:~2..0b',
				[D, proplists:get_value(M, ?MONTH), Y, H, Min])).
unix_ts() ->
    {Mega, Seconds, _} = erlang:now(),
    Mega * 1000000 + Seconds.

to_list(Term) when is_list(Term) -> Term;
to_list(Term) when is_atom(Term) -> atom_to_list(Term); 
to_list(Term) when is_tuple(Term) -> tuple_to_list(Term).
