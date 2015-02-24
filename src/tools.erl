%% -*- coding: utf-8 -*-
-module(tools).

-author("Andrey Andruschenko <apofiget@gmail.com>").

-export([get_option/1, get_reg_type/1, format/1,
         ts2date/1, unix_ts/0, to_list/1,
         get_result_comment/1, make_envelope/3, vsn/0]).

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

format(Term) when is_binary(Term) ->  unicode:characters_to_list(io_lib:format("~ts", [Term]));
format(Term) when is_tuple(Term) -> format(tuple_to_list(Term));
format(Term) when is_atom(Term) -> format(atom_to_list(Term));
format(Term) when is_list(Term)  -> io_lib:format("~tp", [Term]);
format(Term) -> Term.

to_list(Term) when is_list(Term) -> Term;
to_list(Term) when is_atom(Term) -> atom_to_list(Term);
to_list(Term) when is_tuple(Term) -> io_lib:format("~p",[tuple_to_list(Term)]);
to_list(Term) when is_binary(Term) ->
    case unicode:characters_to_binary(Term) of
        {error, _L, _R} -> Term;
        {incomplete, L, _B} -> L;
        L -> L
    end.

make_envelope(From, ListT, Body) ->
    lists:foldl(fun(E, Acc) ->
                        [[{email, proplists:get_value(email, E, "none@localhost")},
                          {message, lists:concat(["Subject: ", proplists:get_value(subject, E, "undefined"), "\r\n",
                                                  "From: ", From, "\r\n", "To: ", proplists:get_value(email, E, "undefined"), "\r\n\r\n",
                                                  proplists:get_value(body_prefix, E, ""), Body, proplists:get_value(body_suffix, E, "")])}] | Acc]
                end, [], ListT).

vsn() ->
    case application:get_all_key(rkn_registry) of
        undefined ->
            "undefined";
        {ok, L} ->
            proplists:get_value(vsn, L, "undefined")
    end.
