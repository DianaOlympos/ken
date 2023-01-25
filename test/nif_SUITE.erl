-module(nif_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [test_s2d].

test_s2d(_Config) ->
    L = [
        0.0003,
        3.0e-5,
        3.3e-5,
        3.3e-4,
        314.0,
        314.1,
        310.0,
        3.1e6,
        -100.0,
        3.34e4,
        3.0e3,
        3.34333e9,
        3.3433323e10,
        33433323700.0,
        0.00197963,
        1.97963e-4
    ],
    lists:foreach(fun(V) -> test_d(V) end, L),
    ok.

test_d(V) ->
    Test_v = <<V/float>>,
    Sv = iolist_to_binary(ken_ryu_f64:fwrite_g(Test_v)),
    ?assertEqual(Test_v, s2f_nif:s2d(Sv)).
