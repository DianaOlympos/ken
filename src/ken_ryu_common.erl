-module(ken_ryu_common).

-export([log10pow2/1, log10pow5/1, pow5bits/1, cmp_float_32/2]).

log10pow2(I) ->
    ((I * 78913) bsr 18) - 1.

log10pow5(I) ->
    ((-I * 732923) bsr 20) - 1.

pow5bits(E) ->
    ((E * 1217359) bsr 19) + 1.

cmp_float_32(A, A) ->
    eq;
cmp_float_32(<<_:1, 0:31>>, <<_:1, 0:31>>) ->
    eq;
cmp_float_32(<<0:1, _:31>>, <<1:1, _:31>>) ->
    sup;
cmp_float_32(<<1:1, _:31>>, <<0:1, _:31>>) ->
    inf;
cmp_float_32(<<0:1, E1:8, _:23>>, <<0:1, E2:8, _:23>>) when E1 > E2 ->
    sup;
cmp_float_32(<<0:1, E1:8, _:23>>, <<0:1, E2:8, _:23>>) when E1 < E2 ->
    inf;
cmp_float_32(<<0:1, E:8, M1:23>>, <<0:1, E:8, M2:23>>) when M1 < M2 ->
    inf;
cmp_float_32(<<0:1, E:8, M1:23>>, <<0:1, E:8, M2:23>>) when M1 > M2 ->
    sup;
cmp_float_32(<<1:1, E1:8, _:23>>, <<1:1, E2:8, _:23>>) when E1 > E2 ->
    inf;
cmp_float_32(<<1:1, E1:8, _:23>>, <<1:1, E2:8, _:23>>) when E1 < E2 ->
    sup;
cmp_float_32(<<1:1, E:8, M1:23>>, <<1:1, E:8, M2:23>>) when M1 < M2 ->
    sup;
cmp_float_32(<<1:1, E:8, M1:23>>, <<1:1, E:8, M2:23>>) when M1 > M2 ->
    inf.
