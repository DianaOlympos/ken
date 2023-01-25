-module(f32_SUITE).

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
        g_anomalous
        % g_misc
    ].

format_neg_zero(_Config) ->
    <<NegZero:32>> = <<16#80_00_00_00:32>>,
    ?assertEqual(
        "-0.0", binary_to_list(iolist_to_binary(ken_ryu_f32:fwrite_g(<<NegZero:32>>)))
    ).

-define(ONE(N), 1 bsl N - 1).
-define(ALL_ONES, ((1 bsl 23) - 1)).

g_warm_up(_Config) ->
    g_t(<<16#3F000000:32>>),
    g_t(<<16#BF000000:32>>),
    g_t(<<16#4C000000:32>>),
    g_t(<<16#CC000000:32>>),
    g_t(<<16#FEFCC3E3:32>>),
    g_t(pack(1, 0, 2#101010101000110010100001)),
    g_t(pack(1, 0, 2#000101000000100111010000)),
    g_t(<<16#4D5F7816:32>>),
    ok.

g_big_pos_float(_Config) ->
    %% The greatest positive float:
    ft({{0, 254, ?ONE(23)}, 100, 0}),
    ok.

g_small_neg_float(_Config) ->
    %% The least negative float:
    ft({{1, 254, ?ONE(23)}, 0, 100}),
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
    [ft({{S, 0, ?ONE(N)}, D, D}) || S <- [0, 1], N <- lists:seq(0, 23)],
    ok.

g_normalized(_Config) ->
    %% Normalized floats (exponent carry):
    %%    D = 5,
    %% Faster:
    D = 1,
    [ft({{S, E, ?ONE(23)}, D, D}) || S <- [0, 1], E <- lists:seq(0, 253)],
    ok.

g_choice(_Config) ->
    %% Exponent should be used when and only when the string is shorter.
    %% (g_misc/0 checks this too, and probably more throughly).
    L = [
        %0.003
        <<16#399d4952:32>>,
        %3.0e-5
        <<16#37fba882:32>>,
        %3.3e-5
        <<16#380a697b:32>>,
        %3.3e-4
        <<16#39ad03da:32>>,
        %314.0
        <<16#439d0000:32>>,
        %314.1
        <<16#439d0ccd:32>>,
        %310.0
        <<16#439b0000:32>>,
        %3.1e6
        <<16#4a3d3580:32>>,
        %-100.0
        <<16#c2c80000:32>>,
        %3.34e4
        <<16#47027800:32>>,
        %3.0e3
        <<16#453b8000:32>>,
        %3.34333e9
        <<16#4f47472b:32>>,
        %3.3433323e10
        <<16#50f91901:32>>,
        %33433323700.0
        <<16#50f91901:32>>,
        %0.00197963
        <<16#3b01bcae:32>>,
        %1.97963e-4
        <<16#394f944a:32>>
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
            <<16#51e5f4c9:32>>,
            <<16#6c800000:32>>,
            <<16#6b000000:32>>,
            <<16#0f800000:32>>
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_anom),
    ok.

g_ryu(_Config) ->
    %% specific white box tests that should trigger specific edge cases
    %% to the ryu algorithm see:
    %% https://github.com/ulfjack/ryu/blob/master/ryu/tests/d2s_test.cc
    %% this list is regression tests from the ryu C ref implementation
    L_regression =
        [
            <<16#63800000:32>>,
            <<16#4b000000:32>>,
            <<16#4b800000:32>>,
            <<16#4c000001:32>>,
            <<16#4c800b0d:32>>,
            <<16#00d24584:32>>,
            <<16#800000b0:32>>,
            <<16#00d90b88:32>>,
            <<16#45803f34:32>>,
            <<16#4dfea18b:32>>,
            <<16#00424fe2:32>>,
            <<16#3a8722c3:32>>,
            <<16#5c800041:32>>,
            <<16#15ae43fd:32>>,
            <<16#5d4cccfb:32>>,
            <<16#4c800001:32>>,
            <<16#00000007:32>>,
            <<16#57800ed8:32>>,
            <<16#5f000000:32>>,
            <<16#700000f0:32>>,
            <<16#5f23e9ac:32>>,
            <<16#5e9502f9:32>>,
            <<16#5e8012b1:32>>,
            <<16#3c000028:32>>,
            <<16#00000001:32>>,
            <<16#03aa2a50:32>>,
            <<16#60cde861:32>>,
            <<16#43480000:32>>,
            <<16#4c000000:32>>,
            <<16#3f9e0651:32>>
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_regression),

    %% These numbers have a mantissa that is a multiple of the largest power of 5 that fits,
    %% and an exponent that causes the computation for q to result in 22, which is a corner
    %% case for Ryu.
    L_pow5 = [16#5D1502F9, 16#5D9502F9, 16#5E1502F9],
    lists:foreach(fun(V) -> g_t(i_2_f(V)) end, L_pow5),

    %% Test rounding
    L_roundbits = [<<16#48951f84:32>>, <<16#45fd1840:32>>],
    lists:foreach(fun(V) -> g_t(V) end, L_roundbits),
    L = [
        % 1.2e+1,
        <<16#41400000:32>>,
        % 1.23e+2,
        <<16#42f60000:32>>,
        % 1.234e+3,
        <<16#449a4000:32>>,
        % 1.2345e+4,
        <<16#4640e400:32>>,
        % 1.23456e+5,
        <<16#47f12000:32>>,
        % 1.234567e+6,
        <<16#4996b438:32>>,
        % 1.2345678e+7,
        <<16#4b3c614e:32>>,
        % 1.23456789e+8,
        <<16#4ceb79a3:32>>,
        % 1.23456789e+9,
        <<16#4e932c06:32>>,
        % 1.234567895e+9,
        <<16#4e932c06:32>>,
        % 1.2345678901e+10,
        <<16#5037f707:32>>,
        % 1.23456789012e+11,
        <<16#51e5f4c9:32>>,
        % 1.234567890123e+12,
        <<16#538fb8fe:32>>,
        % 1.2345678901234e+13,
        <<16#5533a73d:32>>,
        % 1.23456789012345e+14,
        <<16#56e0910c:32>>,
        % 1.234567890123456e+15
        <<16#588c5aa8:32>>
    ],
    lists:foreach(fun(V) -> g_t(V) end, L),

    %% power of 2
    L_pow2 =
        [
            % 8.0,
            <<16#41000000:32>>,
            % 64.0,
            <<16#42800000:32>>,
            % 512.0,
            <<16#44000000:32>>,
            % 8192.0,
            <<16#46000000:32>>,
            % 65536.0,
            <<16#47800000:32>>,
            % 524288.0,
            <<16#49000000:32>>,
            % 8388608.0,
            <<16#4b000000:32>>,
            % 67108864.0,
            <<16#4c800000:32>>,
            % 536870912.0,
            <<16#4e000000:32>>,
            % 8589934592.0,
            <<16#50000000:32>>,
            % 68719476736.0,
            <<16#51800000:32>>,
            % 549755813888.0,
            <<16#53000000:32>>,
            % 8796093022208.0,
            <<16#55000000:32>>,
            % 70368744177664.0,
            <<16#56800000:32>>,
            % 562949953421312.0,
            <<16#58000000:32>>,
            % 9007199254740992.0
            <<16#5a000000:32>>
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_pow2),

    %% 1000 * power of 2
    L_pow2_1000 =
        [
            % 8.0e+3
            <<16#45fa0000:32>>,
            % 64.0e+3
            <<16#477a0000:32>>,
            % 512.0e+3
            <<16#48fa0000:32>>,
            % 8192.0e+3
            <<16#4afa0000:32>>,
            % 65536.0e+3
            <<16#4c7a0000:32>>,
            % 524288.0e+3
            <<16#4dfa0000:32>>,
            % 8388608.0e+3
            <<16#4ffa0000:32>>,
            % 67108864.0e+3
            <<16#517a0000:32>>,
            % 536870912.0e+3
            <<16#52fa0000:32>>,
            % 8589934592.0e+3
            <<16#54fa0000:32>>,
            % 68719476736.0e+3
            <<16#567a0000:32>>,
            % 549755813888.0e+3
            <<16#57fa0000:32>>,
            % 8796093022208.0e+3
            <<16#59fa0000:32>>
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_pow2_1000),

    %% 10^15 + 10^i
    L_pow10_plus =
        [
            % 1.0e+15 + 1.0e+0,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+1,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+2,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+3,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+4,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+5,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+6,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+7,
            <<16#58635fa9:32>>,
            % 1.0e+15 + 1.0e+8,
            <<16#58635fab:32>>,
            % 1.0e+15 + 1.0e+9,
            <<16#58635fb8:32>>,
            % 1.0e+15 + 1.0e+10,
            <<16#5863603e:32>>,
            % 1.0e+15 + 1.0e+11,
            <<16#5863657b:32>>,
            % 1.0e+15 + 1.0e+12,
            <<16#586399de:32>>,
            % 1.0e+15 + 1.0e+13,
            <<16#5865a5bd:32>>,
            % 1.0e+15 + 1.0e+14
            <<16#587a1c6d:32>>
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_pow10_plus),

    %% min and max
    g_t(i_2_f(1)),
    g_t(i_2_f(16#7fefffffffffffff)),

    %% lots of trailing zeroes
    g_t(<<16#39800000:32>>),
    g_t(<<16#3b200000:32>>),
    g_t(<<16#3b900000:32>>),
    g_t(<<16#3bd00000:32>>),

    %% Switch to Subnormal
    g_t(<<16#00800000:32>>),

    %% following test cases come from https://github.com/microsoft/STL/blob/f1515e04fd00876137e762c08b90d9aa450859e0/tests/std/tests/P0067R5_charconv/double_to_chars_test_cases.hpp
    %% These numbers have odd mantissas (unaffected by shifting)
    %% that are barely within the "max shifted mantissa" limit.
    L_mantissas_within_limit =
        [
            % 3355443e1
            <<16#4bffffff:32>>,
            % 671087e2
            <<16#4c7fffd7:32>>,
            % 134217e3
            <<16#4cffffa5:32>>,
            % 26843e4
            <<16#4d7ffeab:32>>,
            % 5367e5
            <<16#4dffeb23:32>>,
            % 1073e6
            <<16#4e7fd2b9:32>>,
            % 213e7
            <<16#4efdea71:32>>,
            % 41e8
            <<16#4f746109:32>>,
            % 7e9
            <<16#4fd09dc3:32>>,
            % 1e10
            <<16#501502f9:32>>
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_mantissas_within_limit),

    %% These numbers have odd mantissas (unaffected by shifting)
    %% that are barely above the "max shifted mantissa" limit.
    L_mantissas_above_limit =
        [
            % 3355445e1
            <<16#4c000004:32>>,
            % 671089e2
            <<16#4c800004:32>>,
            % 134219e3
            <<16#4d000050:32>>,
            % 26845e4
            <<16#4d8001c6:32>>,
            % 5369e5
            <<16#4e0001c6:32>>,
            % 1075e6
            <<16#4e802666:32>>,
            % 215e7
            <<16#4f002666:32>>,
            % 43e8
            <<16#4f802666:32>>,
            % 9e9
            <<16#50061c46:32>>,
            % 3e10
            <<16#50df8476:32>>
        ],
    lists:foreach(fun(V) -> g_t(V) end, L_mantissas_above_limit),

    ok.

g_misc(_Config) ->
    L_0_38 = lists:seq(0, 38),
    L_0_37 = lists:seq(0, 37),

    %% Faster:
    L_1_9 = [1, 5, 9],
    L_0_9 = [0, 1, 5, 9],

    %% 1.0,10.0, ... 2.0,20.0, ... 9.0,90.0, ... -1,-10, ... -2.0,-20.0...
    [g_t(S * T * pow10(N)) || S <- [1.0, -1.0], T <- L_1_9, N <- L_0_37],

    %% 1.0,1.0/10,1.0/100,... 2.0,2.0/10,2.0/100, ... 9.0,9.0/10,9.0/100,
    %% -1.0,-1.0/10,... -9.0,-9.0/10...
    [g_t(S * T / pow10(N)) || S <- [1.0, -1.0], T <- L_1_9, N <- L_0_38],

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
ft({V, Less, Greater}) when is_binary(V) ->
    _ = g_t(V),
    ft1(V, fun inc/1, Greater),
    ft1(V, fun dec/1, Less).

ft1(V0, F, I) when I > 0, is_binary(V0) ->
    V = F(V0),
    _ = g_t(V),
    ft1(V, F, I - 1);
ft1(V, _F, 0) when is_binary(V) ->
    ok.

g_t(Vf) when is_float(Vf) ->
    g_t(double_to_single_precision(Vf));
g_t(V) when is_binary(V) ->
    Io = ken_ryu_f32:fwrite_g(V),
    Sv = binary_to_list(iolist_to_binary(Io)),
    % ct:print("test Sv: ~p", [Sv]),
    % ct:print("test V: ~p", [V]),
    ok = g_t(V, Sv),
    Sv.

%% -> ok | THROW

%% Checks that Sv is the shortest, correctly rounded string that
%% converts to V when read back with list_to_float/1.
%% Note: in a few cases the least significant digit has been
%% incremented by one, namely when the correctly rounded string
%% converts to another floating point number.
g_t(<<1:1, 0:31>>, Format) ->
    "-0.0" = Format,
    ok;
g_t(<<0:1, 0:31>>, Format) ->
    "0.0" = Format,
    ok;
g_t(V, Sv) ->
    try
        g_t_1(V, Sv)
    catch
        Reason ->
            throw({Reason, V, Sv})
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
    %% The are 3 (negative) floats where SvMinus (SvPlus) is closer
    %% to V than Sv, but such that when reading SvMinus (SvPlus) wrong
    %% float would be returned.
    ct:print("test Abs_Sv_Vr: ~p", [Abs_Sv_Vr]),
    ct:print("Svminus_Vr: ~p", [Svminus_Vr]),
    case rat_lte(Abs_Sv_Vr, Svminus_Vr) of
        true ->
            ?assertEqual(ok, ok);
        false ->
            try s2f_nif:s2f(iolist_to_binary(SvMinus)) of
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
            try s2f_nif:s2f(iolist_to_binary(SvPlus)) of
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
    try s2f_nif:s2f(iolist_to_binary(SvHigh)) of
        VHigh -> ?assertNotEqual(V, VHigh)
    catch
        error:badarg ->
            ok
    end,
    try s2f_nif:s2f(iolist_to_binary(SvLow)) of
        VLow -> ?assertNotEqual(V, VLow)
    catch
        error:badarg ->
            ok
    end,
    %% Check that Sv has enough digits.
    ?assertEqual(V, s2f_nif:s2f(iolist_to_binary(Sv))),

    g_choice_1(Sv),

    ok.

i_2_f(Int) ->
    <<Int:32/unsigned-integer>>.

pack(Sign, Exp, Frac) ->
    <<Sign:1, Exp:8, Frac:23>>.

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

dec(<<S:1, BE:8, M:23>>) ->
    dec({S, BE, M});
dec({1, 254, ?ALL_ONES}) ->
    '-inf';
dec({S, BE, M}) when
    0 =< S,
    S =< 1,
    0 =< BE,
    BE =< 254,
    0 =< M,
    M =< ?ALL_ONES
->
    {S1, BE1, M1} = dec1(S, BE, M),
    F = <<S:1, BE:8, M:23>>,
    F1 = <<S1:1, BE1:8, M1:23>>,
    ?assertEqual(inf, ken_ryu_common:cmp_float_32(F1, F)),
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

inc(<<S:1, BE:8, M:23>>) ->
    inc({S, BE, M});
inc({0, 254, ?ALL_ONES}) ->
    inf;
inc({S, BE, M}) when
    0 =< S,
    S =< 1,
    0 =< BE,
    BE =< 254,
    0 =< M,
    M =< ?ALL_ONES
->
    {S1, BE1, M1} = inc1(S, BE, M),
    F = <<S:1, BE:8, M:23>>,
    F1 = <<S1:1, BE1:8, M1:23>>,
    ?assertEqual(sup, ken_ryu_common:cmp_float_32(F1, F)),
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
    ShouldAlwaysHaveExponent = abs(list_to_float(S)) >= float(1 bsl 24),
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

f2r(<<S:1, BE:8, M:23>>) ->
    f2r({S, BE, M});
f2r({S, BE, M}) when
    0 =< S,
    S =< 1,
    0 =< BE,
    BE =< 254,
    0 =< M,
    M =< ?ALL_ONES
->
    Vr = {T, N} = f2r1(S, BE, M),
    F = <<S:1, BE:8, M:23>>,
    case catch T / N of
        {'EXIT', _} ->
            ok;
        TN ->
            % ct:print("test T: ~p", [T]),
            % ct:print("test N: ~p", [N]),
            % ct:print("test TN: ~p", [TN]),
            % ct:print("test TN_to_f: ~p", [double_to_single_precision(TN)]),
            % ct:print("test F: ~p", [F]),
            true = F =:= double_to_single_precision(TN)
    end,
    Vr.

f2r1(S, 0, M) ->
    rat_multiply({sign(S), 1}, {M, 1 bsl 151});
f2r1(S, BE, M) when BE - 150 >= 0 ->
    rat_multiply({sign(S), 1}, {((1 bsl 23) + M) * (1 bsl (BE - 150)), 1});
f2r1(S, BE, M) ->
    rat_multiply({sign(S), 1}, {(1 bsl 23) + M, 1 bsl (150 - BE)}).

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

double_to_single_precision(-0.0) ->
    <<1:1, 0:31>>;
double_to_single_precision(0.0) ->
    <<0:32>>;
double_to_single_precision(Float) ->
    do_d2f(<<Float/float>>).

do_d2f(<<S:1, 0:11, M:52>>) ->
    % denormalized,
    % it follows through the leading one algorithm stage to
    % calculate leading one position in the number and calculates the
    % exponent in the number and adds the single precision bias.
    % Since the mantissa of the double precision data input is 53-bit,
    % a rounding operation is performed to truncate the mantissa to
    % 23-bit.
    Exp = -1022 - leading_zero(M) + 127,
    Round =
        case <<M:52>> of
            <<_:22, 0:1, 1:1, _:28>> ->
                1;
            _Else ->
                0
        end,

    <<Mant1:23, _:29>> = <<M:52>>,
    Mant = Mant1 + Round,
    <<S:1, Exp:8, Mant:23>>;
do_d2f(<<S:1, E:11, M:52>>) ->
    % normalized
    % First it removes the double precision bias from
    % the single precision exponent by subtracting the 1023, and then
    % it adds the single precision bias value (127). After the exponent
    % addition, a 52-bit mantissa is truncated to 23-bit by using the
    % rounding mode specified as the input.
    Exp = E - 1023 + 127,
    Round =
        case <<M:52>> of
            <<_:22, 1:1, 1:1, _:28>> ->
                1;
            _Else ->
                0
        end,

    <<Mant1:23, _:29>> = <<M:52>>,
    Mant = Mant1 + Round,
    % ct:print("test M: ~p", [<<M:52>>]),
    % ct:print("test Mant: ~p", [<<Mant:23>>]),
    % ct:print("test Round: ~p", [Round]),
    % ct:print("test Mant1: ~p", [Mant1]),
    <<S:1, Exp:8, Mant:23>>.

leading_zero(M) ->
    ceil(math:log2(M)).
