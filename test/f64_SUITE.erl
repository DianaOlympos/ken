-module(f64_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        format_neg_zero,
        g_big_pos_float,
        g_small_neg_float,
        g_close_to_zero,
        g_denormalized,
        g_normalized,
        g_choice,
        g_ryu,
        g_anomalous,
        g_misc
    ].

format_neg_zero(_Config) ->
    <<NegZero/float>> = <<16#8000000000000000:64>>,
    ?assertEqual("-0.0", ken_ryu_f64:fwrite_g(NegZero)).

-define(ONE(N), 1 bsl N - 1).
-define(ALL_ONES, ((1 bsl 52) - 1)).

g_warm_up(_Config) ->
    g_t(0.5),
    g_t(-0.5),
    g_t((1 bsl 55) * 0.5),
    g_t(-(1 bsl 55) * 0.5),
    g_t(1.6799127650033296e+308),
    g_t(pack(1, 0, 2#1010101010001100101000010111100101000000101100110001)),
    g_t(pack(1, 0, 2#0001010000001001110100000101010101001110010001010110)),
    g_t(234324324.23432432432432),
    ok.

g_big_pos_float(_Config) ->
    %% The greatest positive float:
    ft({{0, 2046, ?ONE(52)}, 100, 0}),
    ok.

g_small_neg_float(_Config) ->
    %% The least negative float:
    ft({{1, 2046, ?ONE(52)}, 0, 100}),
    ok.

g_close_to_zero(_Config) ->
    %% A few denormalized floats close to zero:
    ft({{0, 0, 0}, 100, 100}),
    % -0.0
    g_t(pack(1, 0, 0)),
    ok.

g_denormalized(_Config) ->
    %% Denormalized floats (mantissa carry):
    %%    D = 5,
    %% Faster:
    D = 1,
    [ft({{S, 0, ?ONE(N)}, D, D}) || S <- [0, 1], N <- lists:seq(0, 52)],
    ok.

g_normalized(_Config) ->
    %% Normalized floats (exponent carry):
    %%    D = 5,
    %% Faster:
    D = 1,
    [ft({{S, E, ?ONE(52)}, D, D}) || S <- [0, 1], E <- lists:seq(0, 2045)],
    ok.

g_choice(_Config) ->
    %% Exponent should be used when and only when the string is shorter.
    %% (g_misc/0 checks this too, and probably more throughly).
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
    lists:foreach(fun(V) -> g_t(V) end, L),
    ok.

g_anomalous(_Config) ->
    %% These test cases come from https://github.com/microsoft/STL/blob/f1515e04fd00876137e762c08b90d9aa450859e0/tests/std/tests/P0067R5_charconv/double_to_chars_test_cases.hpp
    %% https://www.exploringbinary.com/the-shortest-decimal-string-that-round-trips-may-not-be-the-nearest/
    %% This is an exhaustive list of anomalous values
    %% Because math, these values have shortest-round-trip decimal representations containing 16 significant digits,
    %% but those decimal digits aren't what would be printed by "%.15e". For ordinary values, shortest-round-trip
    %% behaves as if it can magically pick a precision for "%.*e", finding the smallest precision that round-trips.
    %% (That is, start with the exact decimal representation, and then round it as much as possible.) These anomalous
    %% values demonstrate an exception to that mental model. They aren't being "incorrectly rounded"; instead, having
    %% the shortest output that round-trips is being prioritized. (This differs by 1 in the least significant decimal
    %% digit printed, so it's a very small difference.)
    L_anom =
        [
            6.386688990511104e+293,
            5.282945311356653e+269,
            6.150157786156811e+259,
            5.334411546303884e+241,
            5.386379163185535e+213,
            6.483618076376552e+178,
            6.183260036827614e+172,
            5.896816288783659e+166,
            5.758609657015292e+163,
            5.623642243178996e+160,
            6.243497100631985e+144,
            8.263199609878108e+121,
            6.455624695217272e+119,
            6.156563468186638e+113,
            7.167183174968974e+103,
            6.518515124270356e+91,
            6.070840288205404e+82,
            6.129982163463556e+54,
            5.986310706507379e+51,
            5.444517870735016e+39,
            5.316911983139664e+36,
            6.189700196426902e+26,
            5.960464477539063e-08,
            5.684341886080802e-14,
            6.617444900424222e-24,
            6.310887241768095e-30,
            7.174648137343064e-43,
            7.854549544476363e-90,
            6.653062250012736e-111,
            5.075883674631299e-116,
            6.256509672447191e-148,
            4.887898181599368e-150,
            5.966672584960166e-154,
            5.426657103235053e-166,
            5.351097043477547e-197,
            5.225680706521042e-200,
            6.083493012144512e-210,
            5.940911144672375e-213,
            6.290184345309701e-235,
            6.142758149716505e-238,
            7.678447687145631e-239,
            5.858190679279809e-244,
            5.641232424577593e-278,
            8.209073602596753e-289,
            7.291122019556398e-304,
            7.120236347223045e-307
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_anom),

    %% This is an exhaustive list of almost-but-not-quite-anomalous values.
    L_quasi_anom =
        [
            6.237000967296e+290,
            6.090821257125e+287,
            8.25460204899477e+267,
            5.78358058743443e+222,
            7.1362384635298e+44,
            6.10987272699921e-151,
            5.17526350329881e-172,
            6.84940421565126e-195
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_quasi_anom),

    ok.

g_ryu(_Config) ->
    %% specific white box tests that should trigger specific edge cases
    %% to the ryu algorithm see:
    %% https://github.com/ulfjack/ryu/blob/master/ryu/tests/d2s_test.cc
    %% this list is regression tests from the ryu C ref implementation
    L_regression =
        [
            -2.109808898695963e16,
            4.940656e-318,
            1.18575755E-316,
            2.989102097996e-312,
            9.0608011534336e15,
            4.708356024711512e18,
            9.409340012568248e18,
            1.2345678
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_regression),

    %% These numbers have a mantissa that is a multiple of the largest power of 5 that fits,
    %% and an exponent that causes the computation for q to result in 22, which is a corner
    %% case for Ryu.
    L_pow5 = [16#4830F0CF064DD592, 16#4840F0CF064DD592, 16#4850F0CF064DD592],
    lists:foreach(fun(V) -> g_t(i_2_d(V)) end, L_pow5),

    %% Test 32-bit chunking 2^32 +- 1/2
    L_32bits = [4.294967294, 4.294967295, 4.294967296, 4.294967297, 4.294967298],
    lists:foreach(fun(V) -> g_t(V) end, L_32bits),

    %% Test 32-bit chunking 2^32 +- 1/2
    L_32bits = [4.294967294, 4.294967295, 4.294967296, 4.294967297, 4.294967298],
    lists:foreach(fun(V) -> g_t(V) end, L_32bits),

    L = [
        1.2e+1,
        1.23e+2,
        1.234e+3,
        1.2345e+4,
        1.23456e+5,
        1.234567e+6,
        1.2345678e+7,
        1.23456789e+8,
        1.23456789e+9,
        1.234567895e+9,
        1.2345678901e+10,
        1.23456789012e+11,
        1.234567890123e+12,
        1.2345678901234e+13,
        1.23456789012345e+14,
        1.234567890123456e+15
    ],
    lists:foreach(fun(V) -> g_t(V) end, L),

    %% power of 2
    L_pow2 =
        [
            8.0,
            64.0,
            512.0,
            8192.0,
            65536.0,
            524288.0,
            8388608.0,
            67108864.0,
            536870912.0,
            8589934592.0,
            68719476736.0,
            549755813888.0,
            8796093022208.0,
            70368744177664.0,
            562949953421312.0,
            9007199254740992.0
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_pow2),

    %% 1000 * power of 2
    L_pow2_1000 =
        [
            8.0e+3,
            64.0e+3,
            512.0e+3,
            8192.0e+3,
            65536.0e+3,
            524288.0e+3,
            8388608.0e+3,
            67108864.0e+3,
            536870912.0e+3,
            8589934592.0e+3,
            68719476736.0e+3,
            549755813888.0e+3,
            8796093022208.0e+3
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_pow2_1000),

    %% 10^15 + 10^i
    L_pow10_plus =
        [
            1.0e+15 + 1.0e+0,
            1.0e+15 + 1.0e+1,
            1.0e+15 + 1.0e+2,
            1.0e+15 + 1.0e+3,
            1.0e+15 + 1.0e+4,
            1.0e+15 + 1.0e+5,
            1.0e+15 + 1.0e+6,
            1.0e+15 + 1.0e+7,
            1.0e+15 + 1.0e+8,
            1.0e+15 + 1.0e+9,
            1.0e+15 + 1.0e+10,
            1.0e+15 + 1.0e+11,
            1.0e+15 + 1.0e+12,
            1.0e+15 + 1.0e+13,
            1.0e+15 + 1.0e+14
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_pow10_plus),

    %% min and max
    g_t(i_2_d(1)),
    g_t(i_2_d(16#7fefffffffffffff)),

    %% lots of trailing zeroes
    g_t(2.98023223876953125e-8),

    %% Switch to Subnormal
    g_t(2.2250738585072014e-308),

    %% special case to check for the shift to the right by 128
    L_shift =
        [
            parts_2_f(0, 4, 0),
            parts_2_f(0, 6, 1 bsl 53 - 1),
            parts_2_f(0, 41, 0),
            parts_2_f(0, 40, 1 bsl 53 - 1),
            parts_2_f(0, 1077, 0),
            parts_2_f(0, 1076, 1 bsl 53 - 1),
            parts_2_f(0, 307, 0),
            parts_2_f(0, 306, 1 bsl 53 - 1),
            parts_2_f(0, 934, 16#000FA7161A4D6E0C)
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_shift),

    %% following test cases come from https://github.com/microsoft/STL/blob/f1515e04fd00876137e762c08b90d9aa450859e0/tests/std/tests/P0067R5_charconv/double_to_chars_test_cases.hpp
    %% These numbers have odd mantissas (unaffected by shifting)
    %% that are barely within the "max shifted mantissa" limit.
    L_mantissas_within_limit =
        [
            1801439850948197.0e1,
            360287970189639.0e2,
            72057594037927.0e3,
            14411518807585.0e4,
            2882303761517.0e5,
            576460752303.0e6,
            115292150459.0e7,
            23058430091.0e8,
            4611686017.0e9,
            922337203.0e10,
            184467439.0e11,
            36893487.0e12,
            7378697.0e13,
            1475739.0e14,
            295147.0e15,
            59029.0e16,
            11805.0e17,
            2361.0e18,
            471.0e19,
            93.0e20,
            17.0e21,
            3.0e22
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_mantissas_within_limit),

    %% These numbers have odd mantissas (unaffected by shifting)
    %% that are barely above the "max shifted mantissa" limit.
    L_mantissas_above_limit =
        [
            1801439850948199.0e1,
            360287970189641.0e2,
            72057594037929.0e3,
            14411518807587.0e4,
            2882303761519.0e5,
            576460752305.0e6,
            115292150461.0e7,
            23058430093.0e8,
            4611686019.0e9,
            922337205.0e10,
            184467441.0e11,
            36893489.0e12,
            7378699.0e13,
            1475741.0e14,
            295149.0e15,
            59031.0e16,
            11807.0e17,
            2363.0e18,
            473.0e19,
            95.0e20,
            19.0e21,
            5.0e22
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_mantissas_above_limit),

    L_switch =
        [
            1801439850948197.0e1,
            360287970189639.0e2,
            72057594037927.0e3,
            14411518807585.0e4,
            2882303761517.0e5,
            576460752303.0e6,
            115292150459.0e7,
            23058430091.0e8,
            4611686017.0e9,
            922337203.0e10,
            184467439.0e11,
            36893487.0e12,
            7378697.0e13,
            1475739.0e14,
            295147.0e15,
            59029.0e16,
            11805.0e17,
            2361.0e18,
            471.0e19,
            93.0e20,
            17.0e21,
            3.0e22,
            1801439850948199.0e1,
            360287970189641.0e2,
            72057594037929.0e3,
            14411518807587.0e4,
            2882303761519.0e5,
            576460752305.0e6,
            115292150461.0e7,
            23058430093.0e8,
            4611686019.0e9,
            922337205.0e10,
            184467441.0e11,
            36893489.0e12,
            7378699.0e13,
            1475741.0e14,
            295149.0e15,
            59031.0e16,
            11807.0e17,
            2363.0e18,
            473.0e19,
            95.0e20,
            19.0e21,
            5.0e22,
            302230528.0e15,
            302232576.0e15,
            81123342286848.0e18,
            81192061763584.0e18
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_switch),

    L_edge =
        [
            123456789012345683968.0,
            1.9156918820264798e-56,
            6.6564021122018745e+264,
            4.91e-6,
            5.547e-6
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_edge),

    ok.

g_misc(_Config) ->
    L_0_308 = lists:seq(0, 308),
    L_0_307 = lists:seq(0, 307),

    %% Faster:
    L_1_9 = [1, 5, 9],
    L_0_9 = [0, 1, 5, 9],

    %% 1.0,10.0, ... 2.0,20.0, ... 9.0,90.0, ... -1,-10, ... -2.0,-20.0...
    [g_t(S * T * pow10(N)) || S <- [1.0, -1.0], T <- L_1_9, N <- L_0_307],

    %% 1.0,1.0/10,1.0/100,... 2.0,2.0/10,2.0/100, ... 9.0,9.0/10,9.0/100,
    %% -1.0,-1.0/10,... -9.0,-9.0/10...
    [g_t(S * T / pow10(N)) || S <- [1.0, -1.0], T <- L_1_9, N <- L_0_308],

    %% 0.0,1.0,2.0,...,9.0, 0.0,10.0,20.0,...,90.0,...
    %% 0.0,-1.0,-2.0,...,-9.0, 0.0,-10.0,-20.0,...,-90.0,...
    [
        g_t(S * list_to_float([D + $0] ++ lists:duplicate(N, $0) ++ ".0"))
     || S <- [1.0, -1.0], N <- lists:seq(0, 300), D <- L_0_9
    ],

    %% 0.0,0.1,0.2,...0,9, 0.0,0.01,0.02,...,0.09,
    %% 0.0,-0.1,-0.2,...-0,9, 0.0,-0.01,-0.02,...,-0.09,
    [
        g_t(S * list_to_float("0." ++ lists:duplicate(N, $0) ++ [D + $0]))
     || S <- [1.0, -1.0], N <- lists:seq(0, 300), D <- L_0_9
    ],
    ok.

ft({{S, E, M}, L, G}) ->
    ft({pack(S, E, M), L, G});
ft({V, Less, Greater}) when is_float(V) ->
    _ = g_t(V),
    ft(V, fun inc/1, Greater),
    ft(V, fun dec/1, Less).

ft(V0, F, I) when I > 0, is_float(V0) ->
    V = F(V0),
    _ = g_t(V),
    ft(V, F, I - 1);
ft(V, _F, 0) when is_float(V) ->
    ok.

g_t(V) when is_float(V) ->
    %% io:format("Testing ~.17g~n", [V]),
    Io = ken_ryu_f64:fwrite_g(V),
    Sv = binary_to_list(iolist_to_binary(Io)),
    ok = g_t(V, Sv),
    Sv.

%% -> ok | THROW

%% Checks that Sv is the shortest, correctly rounded string that
%% converts to V when read back with list_to_float/1.
%% Note: in a few cases the least significant digit has been
%% incremented by one, namely when the correctly rounded string
%% converts to another floating point number.
g_t(V, Sv) when V > 0.0; V < 0.0 ->
    try
        g_t_1(V, Sv)
    catch
        Reason ->
            throw({Reason, V, Sv})
    end;
g_t(Zero, Format) ->
    case <<Zero/float>> of
        <<1:1, _:63>> ->
            "-0.0" = Format,
            ok;
        <<0:1, _:63>> ->
            "0.0" = Format,
            ok
    end.

g_t_1(V, Sv) ->
    %% Check that the least significant digit is correct.
    %% If Sv is "3.14" then Sv- is "3.13" and Sv+ is "3.15".
    %% Check that |V - Sv| =< (V - Sv-) and
    %%       that |V - Sv| =< (Sv+ - V)
    Times = least_significant_digit(Sv),
    ?assertNotEqual(0, Times),
    S =
        if
            V < 0 ->
                -1;
            true ->
                1
        end,
    SvMinus = incr_lsd(Sv, -S),
    SvPlus = incr_lsd(Sv, S),
    Svr = s2r(Sv),
    Svminusr = s2r(SvMinus),
    Svplusr = s2r(SvPlus),
    Vr = f2r(V),

    Abs_Sv_Vr = rat_abs(rat_minus(Svr, Vr)),
    Svminus_Vr = rat_minus(Vr, Svminusr),
    Svplus_Vr = rat_minus(Svplusr, Vr),
    %% The are 45 (negative) floats where SvMinus (SvPlus) is closer
    %% to V than Sv, but such that when reading SvMinus (SvPlus) wrong
    %% float would be returned.
    case rat_lte(Abs_Sv_Vr, Svminus_Vr) of
        true ->
            ?assertEqual(ok, ok);
        false ->
            try list_to_float(SvMinus) of
                VMinus -> ?assertNotEqual(V, VMinus)
            catch
                error:badarg ->
                    ok
            end
    end,
    case rat_lte(Abs_Sv_Vr, Svplus_Vr) of
        true ->
            ?assertEqual(ok, ok);
        false ->
            try list_to_float(SvPlus) of
                VPlus -> ?assertNotEqual(V, VPlus)
            catch
                error:badarg ->
                    ok
            end
    end,

    %% Check that Sv is closer to V than to V- and V+.
    %% Check that |V - Sv| =< (V - V-) and
    %%       that |V - Sv| =< (V+ - V)
    %% (An alternative is  V- + V =< 2*Sv =< V + V+.)
    case inc(V) of
        inf ->
            ok;
        Vplus ->
            Vplusr = f2r(Vplus),
            V_Vplusr = rat_minus(Vplusr, Vr),
            ?assert(rat_lte(Abs_Sv_Vr, V_Vplusr))
    end,
    case dec(V) of
        '-inf' ->
            ok;
        Vminus ->
            Vminusr = f2r(Vminus),
            V_Vminusr = rat_minus(Vr, Vminusr),
            ?assert(rat_lte(Abs_Sv_Vr, V_Vminusr))
    end,

    %% Check that no prefix of Sv yields V.
    %% If Sv is "3.14" then Svlow is "3.1" and Svhigh is "3.2".
    %%
    %% This is just one way of getting Svlow and Svhigh:
    if
        V < 0 ->
            SvHigh = step_lsd(Sv, -Times),
            SvLow = step_lsd(Sv, 10 - Times);
        true ->
            SvHigh = step_lsd(Sv, 10 - Times),
            SvLow = step_lsd(Sv, -Times)
    end,
    try list_to_float(SvHigh) of
        VHigh -> ?assertNotEqual(V, VHigh)
    catch
        error:badarg ->
            ok
    end,
    try list_to_float(SvLow) of
        VLow -> ?assertNotEqual(V, VLow)
    catch
        error:badarg ->
            ok
    end,

    %% Check that Sv has enough digits.
    ?assertEqual(V, list_to_float(Sv)),

    g_choice_1(Sv),

    ok.

i_2_d(Int) ->
    <<F:64/float>> = <<Int:64/unsigned-integer>>,
    F.

parts_2_f(S, E, M) ->
    <<F:64/float>> = <<S:1, E:11, M:52>>,
    F.

pack(Sign, Exp, Frac) ->
    <<Float:64/float>> = <<Sign:1, Exp:11, Frac:52>>,
    Float.

pow10(X) ->
    int_pow(10, X).

int_pow(X, 0) when is_integer(X) ->
    1;
int_pow(X, N) when is_integer(X), is_integer(N), N > 0 ->
    int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(
        X * X,
        N bsr 1,
        case N band 1 of
            1 -> R * X;
            0 -> R
        end
    ).

dec(F) when is_float(F) ->
    <<S:1, BE:11, M:52>> = <<F:64/float>>,
    dec({S, BE, M});
dec({1, 2046, ?ALL_ONES}) ->
    '-inf';
dec({S, BE, M}) when
    0 =< S,
    S =< 1,
    0 =< BE,
    BE =< 2046,
    0 =< M,
    M =< ?ALL_ONES
->
    {S1, BE1, M1} = dec1(S, BE, M),
    <<F:64/float>> = <<S:1, BE:11, M:52>>,
    <<F1:64/float>> = <<S1:1, BE1:11, M1:52>>,
    true = F1 < F,
    F1.

dec1(0, 0, 0) ->
    dec1(1, 0, 0);
dec1(0, BE, 0) ->
    {0, BE - 1, ?ALL_ONES};
dec1(0, BE, M) ->
    {0, BE, M - 1};
dec1(1, BE, ?ALL_ONES) ->
    {1, BE + 1, 0};
dec1(1, BE, M) ->
    {1, BE, M + 1}.

inc(F) when is_float(F) ->
    <<S:1, BE:11, M:52>> = <<F:64/float>>,
    inc({S, BE, M});
inc({0, 2046, ?ALL_ONES}) ->
    inf;
inc({S, BE, M}) when
    0 =< S,
    S =< 1,
    0 =< BE,
    BE =< 2046,
    0 =< M,
    M =< ?ALL_ONES
->
    {S1, BE1, M1} = inc1(S, BE, M),
    <<F:64/float>> = <<S:1, BE:11, M:52>>,
    <<F1:64/float>> = <<S1:1, BE1:11, M1:52>>,
    true = F1 > F,
    F1.

inc1(0, BE, ?ALL_ONES) ->
    {0, BE + 1, 0};
inc1(0, BE, M) ->
    {0, BE, M + 1};
inc1(1, 0, 0) ->
    inc1(0, 0, 0);
inc1(1, BE, 0) ->
    {1, BE - 1, ?ALL_ONES};
inc1(1, BE, M) ->
    {1, BE, M - 1}.

least_significant_digit("-" ++ Ds) ->
    least_significant_digit(Ds);
least_significant_digit("+" ++ Ds) ->
    least_significant_digit(Ds);
least_significant_digit(Ds) ->
    [MS | _E] = string:tokens(Ds, "eE"),
    lsd0(lists:reverse(MS)) - $0.

lsd0("0." ++ Ds) ->
    lsd1(Ds);
lsd0([D | _Ds]) ->
    D.

lsd1("0" ++ Ds) ->
    lsd1(Ds);
lsd1([D | _Ds]) ->
    D.

%% Assumes Ds represents some other number than zero.
%% Increments or decrements the least significant digit.
incr_lsd("-" ++ Ds, I) ->
    "-" ++ incr_lsd(Ds, I);
incr_lsd(Ds, I) when I =:= 1; I =:= -1 ->
    [MS | E] = string:tokens(Ds, "eE"),
    X = ["e" || true <- [E =/= []]],
    lists:flatten([incr_lsd0(lists:reverse(MS), I, []), X, E]).

incr_lsd0("0." ++ Ds, C, L) ->
    incr_lsd1(Ds, C, [$., $0 | L]);
incr_lsd0(Ds, C, L) ->
    incr_lsd2(Ds, C, L).

incr_lsd1("0" ++ Ds, C, L) ->
    incr_lsd1(Ds, C, [$0 | L]);
incr_lsd1(Ds, C, L) ->
    incr_lsd2(Ds, C, L).

incr_lsd2([], C, L) ->
    [C + $0 | L];
incr_lsd2("." ++ Ds, C, L) ->
    incr_lsd2(Ds, C, [$. | L]);
incr_lsd2("9" ++ Ds, 1 = C, L) ->
    incr_lsd2(Ds, C, [$0 | L]);
incr_lsd2("0" ++ Ds, -1 = C, L) ->
    incr_lsd2(Ds, C, [$9 | L]);
incr_lsd2([D | Ds], C, L) ->
    lists:reverse(Ds, [D + C | L]).

s2r(S) when is_list(S) ->
    case string:tokens(S, "eE") of
        [MS] ->
            s10(MS);
        [MS, ES] ->
            Mr = s10(MS),
            E = list_to_integer(ES),
            if
                E < 0 ->
                    rat_multiply(Mr, {1, pow10(-E)});
                true ->
                    rat_multiply(Mr, {pow10(E), 1})
            end
    end.

%%% Rational numbers (very scetchy).

rat_abs({A, B}) when A < 0 ->
    {-A, B};
rat_abs({A, B}) ->
    {A, B}.

rat_lte({A, B}, {C, D}) when B =/= 0, D =/= 0 ->
    A * D =< C * B.

rat_minus({A, B}, {C, D}) ->
    rat_plus({A, B}, {-C, D}).

rat_plus({A, B}, {C, D}) when B =/= 0, D =/= 0 ->
    rat_normalize({A * D + B * C, B * D}).

rat_multiply({A, B}, {C, D}) when B =/= 0, D =/= 0 ->
    rat_normalize({A * C, B * D}).

rat_normalize({T, N}) when N =/= 0 ->
    G = gcd(T, N),
    T2 = T div G,
    N2 = N div G,
    if
        T2 < 0 ->
            if
                N2 < 0 -> {-T2, -N2};
                true -> {T2, N2}
            end;
        true ->
            if
                N2 < 0 -> {-T2, -N2};
                true -> {T2, N2}
            end
    end.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%% Check that there is an exponent if and only if characters are saved
%% when abs(list_to_float(S)) < float(1 bsl 53) and that there is an
%% exponent when abs(list_to_float(S)) >= float(1 bsl 53).
g_choice_1(S) when is_list(S) ->
    ShouldAlwaysHaveExponent = abs(list_to_float(S)) >= float(1 bsl 53),
    HasExponent = lists:member($e, S) orelse lists:member($E, S),
    case ShouldAlwaysHaveExponent of
        true ->
            ?assert(HasExponent);
        false ->
            g_choice_small(S)
    end.

%% Check that there is an exponent if and only if characters are
%% saved. Note: this assumes floating point numbers "Erlang style"
%% (with a single zero before and after the dot, and no extra leading
%% zero in the exponent).
g_choice_small(S) when is_list(S) ->
    [MS | ES0] = string:tokens(S, "eE"),
    [IS, FS] = string:tokens(MS, "."),
    Il = length(IS),
    Fl = length(FS),
    Pre = z(MS),
    Post = z(lists:reverse(MS)),
    ES = lists:append(ES0),
    El = length(ES),
    I = list_to_integer(IS),

    ?assertNot((El =/= 0) and ((I > 9) or (I < -9))),
    ?assertNot((El =/= 0) and (I =:= 0)),
    % DDDD0000.0
    if
        Pre =:= 0, Post > 0, El =:= 0 ->
            Saving =
                if
                    I < 0, Il =:= Post + 2 ->
                        Post;
                    I > 0, Il =:= Post + 1 ->
                        Post;
                    I =/= 0, true ->
                        Post + 1
                end,
            Cost = 1 + length(integer_to_list(Il - 1)),
            ?assertNot(Cost < Saving);
        % 0.000DDDD
        Pre > 0, Post =:= 0, El =:= 0 ->
            Saving =
                if
                    Fl =:= Pre + 1 ->
                        Pre;
                    true ->
                        Pre + 1
                end,
            Cost = 2 + length(integer_to_list(Pre + 1)),
            ?assertNot(Cost < Saving);
        % D.DDDeDD
        Pre =:= 0, Post =:= 0, El > 0 ->
            E = list_to_integer(ES),
            if
                E >= 0 ->
                    Cost = E - (Fl - 1);
                E < 0 ->
                    Cost = -E
            end,
            Saving = length(ES) + 1,
            ?assertNotEqual(Cost, Saving),
            ?assertNot(Cost < Saving);
        % DDD.DDD
        Pre =:= 0, Post =:= 0, El =:= 0 ->
            ?assert(true);
        true ->
            throw(badly_formed_floating_point_string)
    end.

z("0." ++ Ds) ->
    length(lists:takewhile(fun(D) -> D =:= $0 end, Ds));
z(_Ds) ->
    0.

f2r(F) when is_float(F) ->
    <<S:1, BE:11, M:52>> = <<F:64/float>>,
    f2r({S, BE, M});
f2r({S, BE, M}) when
    0 =< S,
    S =< 1,
    0 =< BE,
    BE =< 2046,
    0 =< M,
    M =< ?ALL_ONES
->
    Vr = {T, N} = f2r1(S, BE, M),
    <<F:64/float>> = <<S:1, BE:11, M:52>>,
    case catch T / N of
        {'EXIT', _} -> ok;
        TN -> true = F =:= TN
    end,
    Vr.

f2r1(S, 0, M) ->
    rat_multiply({sign(S), 1}, {M, 1 bsl 1074});
f2r1(S, BE, M) when BE - 1075 >= 0 ->
    rat_multiply({sign(S), 1}, {((1 bsl 52) + M) * (1 bsl (BE - 1075)), 1});
f2r1(S, BE, M) ->
    rat_multiply({sign(S), 1}, {(1 bsl 52) + M, 1 bsl (1075 - BE)}).

sign(0) ->
    1;
sign(1) ->
    -1.

step_lsd(Ds, 0) ->
    Ds;
step_lsd(Ds, N) when N > 0 ->
    NDs = incr_lsd(Ds, 1),
    step_lsd(NDs, N - 1);
step_lsd(Ds, N) when N < 0 ->
    NDs = incr_lsd(Ds, -1),
    step_lsd(NDs, N + 1).

s10("-" ++ S) ->
    rat_multiply({-1, 1}, s10(S));
s10(S) ->
    [AS, BS] = string:tokens(S, "."),
    Sc = length(BS),
    A = list_to_integer(AS),
    B = list_to_integer(BS),
    F = pow10(Sc),
    rat_multiply({1, 1}, {A * F + B, F}).
