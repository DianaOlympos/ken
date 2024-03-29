#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "ken_ryu_table_f32").

-define(TABLE_SIZE, 48).
-define(INV_TABLE_SIZE, 55).

-define(POW5_BITCOUNT, 61).
-define(POW5_INV_BITCOUNT, 59).

main(_) ->
    Values = [values(X) || X <- lists:seq(0, ?TABLE_SIZE - 1)],
    InvValues = [inv_values(X) || X <- lists:seq(0, ?INV_TABLE_SIZE - 1)],

    %% Make module
    {ok, Out} = file:open("../src/" ++ ?MOD ++ ".erl", [write]),
    gen_file(Out, Values, InvValues),
    ok = file:close(Out),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inv_values(X) ->
    Pow = pow5(X),
    Pow5len = log2floor(Pow),
    J = Pow5len + ?POW5_INV_BITCOUNT - 1,
    Inv = ((1 bsl J) div Pow) + 1,
    {X, Inv}.

values(X) ->
    Pow = pow5(X),
    Pow5len = log2floor(Pow),
    Pow5 = Pow bsr (Pow5len - ?POW5_BITCOUNT),
    {X, Pow5}.

pow5(0) ->
    1;
pow5(1) ->
    5;
pow5(X) ->
    5 * pow5(X - 1).

log2floor(Int) when is_integer(Int), Int > 0 ->
    log2floor(Int, 0).

log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_file(Fd, Values, InvValues) ->
    gen_header(Fd),
    gen_pow5_static(Fd),
    gen_table(Fd, Values),
    gen_inv_table(Fd, InvValues),
    ok.

gen_header(Fd) ->
    io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
    io:put_chars(Fd, "%% see ../script/generate_ryu_table_f32.escript\n\n"),
    io:put_chars(Fd, "-module(" ++ ?MOD ++ ").\n"),
    io:put_chars(Fd, "-export([pow5_bitcount/0, pow5_inv_bitcount/0, value/1, inv_value/1]).\n\n"),
    ok.

gen_pow5_static(Fd) ->
    io:put_chars(Fd, "-spec pow5_bitcount() -> integer().\n"),
    io:format(Fd, "pow5_bitcount() -> ~p.~n~n", [?POW5_BITCOUNT]),
    io:put_chars(Fd, "-spec pow5_inv_bitcount() -> integer().\n"),
    io:format(Fd, "pow5_inv_bitcount() -> ~p.~n~n", [?POW5_INV_BITCOUNT]),
    ok.

gen_table(Fd, Values) ->
    io:put_chars(Fd, "-spec value(integer()) -> integer().\n"),
    [io:format(Fd, "value(~p) -> ~p;~n", [Key, Val]) || {Key, Val} <- Values],
    io:put_chars(Fd, "value(_) -> error(function_clause).\n\n"),
    ok.

gen_inv_table(Fd, Values) ->
    io:put_chars(Fd, "-spec inv_value(integer()) -> integer().\n"),
    [io:format(Fd, "inv_value(~p) -> ~p;~n", [Key, Val]) || {Key, Val} <- Values],
    io:put_chars(Fd, "inv_value(_) -> error(function_clause).\n"),
    ok.
