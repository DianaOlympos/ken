-module(ken_ryu_f64).

-export([fwrite_g/1]).

%%  Returns a correctly rounded string that converts to Float when
%%  read back with list_to_float/1.
%%
%%  When abs(Float) < float(1 bsl 53) the shortest such string is
%%  returned, and otherwise the shortest such string using scientific
%%  notation is returned. That is, scientific notation is used if and
%%  only if scientific notation results in a shorter string than
%%  normal notation when abs(Float) < float(1 bsl 53), and scientific
%%  notation is used unconditionally if abs(Float) >= float(1 bsl
%%  53). See comment in insert_decimal/2 for an explanation for why
%%  float(1 bsl 53) is chosen as cutoff point.
%%
%%  The algorithm that is used to find the decimal number that is
%%  represented by the returned String is described in "Ryu: Fast
%%  Float-to-String Conversion" in Proceedings of 39th ACM SIGPLAN
%%  Conference on Programming Language Design and Implementation.
%%  https://dl.acm.org/doi/pdf/10.1145/3192366.3192369

-spec fwrite_g(binary()) -> string().
fwrite_g(Float) ->
    case sign_mantissa_exponent(Float) of
        {0, 0, 0} ->
            "0.0";
        {1, 0, 0} ->
            "-0.0";
        {S, M, E} when E < 2047 ->
            {Place, Digits} =
                case is_small_int(M, E) of
                    {int, M1, E1} ->
                        compute_shortest_int(M1, E1);
                    not_int ->
                        fwrite_g_1(M, E)
                end,
            DigitList = insert_decimal(Place, Digits, Float),
            insert_minus(S, DigitList)
    end.

-define(BIG_POW, (1 bsl 52)).
-define(DECODE_CORRECTION, 1075).

sign_mantissa_exponent(<<S:1, BE:11, M:52>>) ->
    % <<S:1, BE:11, M:52>> = <<F:64/float>>,
    {S, M, BE}.

is_small_int(M, E) ->
    M2 = ?BIG_POW bor M,
    E2 = E - ?DECODE_CORRECTION,
    case E2 > 0 orelse E2 < -52 of
        true ->
            %% f = m2 * 2^e2 >= 2^53 is an integer.
            %% Ignore this case for now.
            %% or f < 1
            not_int;
        _ ->
            %% Since 2^52 <= m2 < 2^53 and 0 <= -e2 <= 52: 1 <= f = m2 / 2^-e2 < 2^53.
            %% Test if the lower -e2 bits of the significand are 0, i.e. whether the fraction is 0.
            Mask = (1 bsl -E2) - 1,
            Fraction = M2 band Mask,
            case Fraction of
                0 ->
                    %% f is an integer in the range [1, 2^53).
                    %% Note: mantissa might contain trailing (decimal) 0's.
                    {int, M2 bsr -E2, 0};
                _ ->
                    not_int
            end
    end.

%%  For small integers in the range [1, 2^53), v.mantissa might contain trailing (decimal) zeros.
compute_shortest_int(M, E) when M rem 10 =:= 0 ->
    Q = M div 10,
    compute_shortest_int(Q, E + 1);
compute_shortest_int(M, E) ->
    {E, integer_to_list(M)}.

fwrite_g_1(M, E) ->
    {Mf, Ef} = decode(M, E),
    Shift = mmshift(M, E),
    Mv = 4 * Mf,
    {Q, Vm, Vr, Vp, E10} = convert_to_decimal(Ef, Mv, Shift),
    Accept = M rem 2 == 0,
    {VmIsTrailingZero, VrIsTrailingZero, Vp1} = bounds(Mv, Q, Vp, Accept, Ef, Shift),
    {D1, E1} = compute_shortest(Vm, Vr, Vp1, VmIsTrailingZero, VrIsTrailingZero, Accept),
    {E1 + E10, integer_to_list(D1)}.

decode(Mantissa, 0) ->
    {Mantissa, 1 - ?DECODE_CORRECTION - 2};
decode(Mantissa, Exponent) ->
    {Mantissa + ?BIG_POW, Exponent - ?DECODE_CORRECTION - 2}.

mmshift(0, E) when E > 1 ->
    0;
mmshift(_M, _E) ->
    1.

convert_to_decimal(E2, Mv, Shift) when E2 >= 0 ->
    Q = max(0, ((E2 * 78913) bsr 18) - 1),
    Mul = ken_ryu_table_f64:inv_value(Q),
    K = ken_ryu_table_f64:pow5_inv_bitcount() + pow5bits(Q) - 1,
    I = -E2 + Q + K,
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, I, Mul),
    {Q, Vm, Vr, Vp, Q};
convert_to_decimal(E2, Mv, Shift) when E2 < 0 ->
    Q = max(0, ((-E2 * 732923) bsr 20) - 1),
    I = -E2 - Q,
    K = pow5bits(I) - ken_ryu_table_f64:pow5_bitcount(),
    From_file = ken_ryu_table_f64:value(I),
    J = Q - K,
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, J, From_file),
    E10 = E2 + Q,
    {Q, Vm, Vr, Vp, E10}.

pow5bits(E) ->
    ((E * 1217359) bsr 19) + 1.

mulShiftAll(Mv, Shift, J, Mul) ->
    A = mulShift64(Mv - 1 - Shift, Mul, J),
    B = mulShift64(Mv, Mul, J),
    C = mulShift64(Mv + 2, Mul, J),
    {A, B, C}.

mulShift64(M, Mul, J) ->
    (M * Mul) bsr J.

bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q =< 21, Mv rem 5 =:= 0 ->
    {false, multipleOfPowerOf5(Mv, Q), Vp};
bounds(Mv, Q, Vp, true, E2, Shift) when E2 >= 0, Q =< 21 ->
    {multipleOfPowerOf5(Mv - 1 - Shift, Q), false, Vp};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q =< 21 ->
    {false, false, Vp - vpmodifier(multipleOfPowerOf5(Mv + 2, Q))};
bounds(_Mv, Q, Vp, true, E2, Shift) when E2 < 0, Q =< 1 ->
    {Shift =:= 1, true, Vp};
bounds(_Mv, Q, Vp, false, E2, _Shift) when E2 < 0, Q =< 1 ->
    {false, true, Vp - 1};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 < 0, Q < 63 ->
    {false, Mv band ((1 bsl Q) - 1) =:= 0, Vp};
bounds(_Mv, _Q, Vp, _Accept, _E2, _Shift) ->
    {false, false, Vp}.

multipleOfPowerOf5(Value, Q) ->
    pow5factor(Value) >= Q.

pow5factor(Val) ->
    pow5factor(Val div 5, 0).

pow5factor(Val, Count) when Val rem 5 /= 0 ->
    Count;
pow5factor(Val, Count) ->
    pow5factor(Val div 5, Count + 1).

vpmodifier(true) ->
    1;
vpmodifier(false) ->
    0.

compute_shortest(Vm, Vr, Vp, false, false, _Accept) ->
    {Vm1, Vr1, Removed, RoundUp} = general_case(Vm, Vr, Vp, 0, false),
    Output = Vr1 + handle_normal_output_mod(Vr1, Vm1, RoundUp),
    {Output, Removed};
compute_shortest(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, Accept) ->
    {Vm1, Vr1, Removed, LastRemovedDigit} =
        handle_trailing_zeros(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, 0, 0),
    Output =
        Vr1 + handle_zero_output_mod(Vr1, Vm1, Accept, VmIsTrailingZero, LastRemovedDigit),
    {Output, Removed}.

general_case(Vm, Vr, Vp, Removed, RoundUp) when Vp div 100 =< Vm div 100 ->
    general_case_10(Vm, Vr, Vp, Removed, RoundUp);
general_case(Vm, Vr, Vp, Removed, _RU) ->
    VmD100 = Vm div 100,
    VrD100 = Vr div 100,
    VpD100 = Vp div 100,
    RoundUp = Vr rem 100 >= 50,
    general_case_10(VmD100, VrD100, VpD100, 2 + Removed, RoundUp).

general_case_10(Vm, Vr, Vp, Removed, RoundUp) when Vp div 10 =< Vm div 10 ->
    {Vm, Vr, Removed, RoundUp};
general_case_10(Vm, Vr, Vp, Removed, _RU) ->
    VmD10 = Vm div 10,
    VrD10 = Vr div 10,
    VpD10 = Vp div 10,
    RoundUp = Vr rem 10 >= 5,
    general_case_10(VmD10, VrD10, VpD10, 1 + Removed, RoundUp).

handle_normal_output_mod(Vr, Vm, RoundUp) when (Vm =:= Vr) or RoundUp ->
    1;
handle_normal_output_mod(_Vr, _Vm, _RoundUp) ->
    0.

handle_trailing_zeros(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit) when
    (Vp div 10) =< (Vm div 10)
->
    vmIsTrailingZero(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit);
handle_trailing_zeros(
    Vm,
    Vr,
    Vp,
    VmIsTrailingZero,
    VrIsTrailingZero,
    Removed,
    LastRemovedDigit
) ->
    VmTZ = VmIsTrailingZero and ((Vm rem 10) =:= 0),
    VrTZ = VrIsTrailingZero and (LastRemovedDigit =:= 0),
    handle_trailing_zeros(
        Vm div 10,
        Vr div 10,
        Vp div 10,
        VmTZ,
        VrTZ,
        1 + Removed,
        Vr rem 10
    ).

vmIsTrailingZero(Vm, Vr, _Vp, false = _VmTZ, VrTZ, Removed, LastRemovedDigit) ->
    handle_50_dotdot_0(Vm, Vr, VrTZ, Removed, LastRemovedDigit);
vmIsTrailingZero(Vm, Vr, _Vp, _VmTZ, VrTZ, Removed, LastRemovedDigit) when
    (Vm rem 10) /= 0
->
    handle_50_dotdot_0(Vm, Vr, VrTZ, Removed, LastRemovedDigit);
vmIsTrailingZero(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit) ->
    vmIsTrailingZero(
        Vm div 10,
        Vr div 10,
        Vp div 10,
        VmTZ,
        LastRemovedDigit == 0 andalso VrTZ,
        1 + Removed,
        Vr rem 10
    ).

handle_50_dotdot_0(Vm, Vr, true, Removed, 5) when (Vr rem 2) =:= 0 ->
    {Vm, Vr, Removed, 4};
handle_50_dotdot_0(Vm, Vr, _VrTZ, Removed, LastRemovedDigit) ->
    {Vm, Vr, Removed, LastRemovedDigit}.

handle_zero_output_mod(_Vr, _Vm, _Accept, _VmTZ, LastRemovedDigit) when
    LastRemovedDigit >= 5
->
    1;
handle_zero_output_mod(Vr, Vm, Accept, VmTZ, _LastRemovedDigit) when
    Vr =:= Vm, ((not Accept) or not (VmTZ))
->
    1;
handle_zero_output_mod(_Vr, _Vm, _Accept, _VmTZ, _LastRemovedDigit) ->
    0.

insert_decimal(Place, S, F) ->
    <<Float/float>> = F,
    L = length(S),
    Exp = Place + L - 1,
    ExpL = integer_to_list(Exp),
    ExpCost = length(ExpL) + 2,
    if
        Place < 0 ->
            if
                Exp >= 0 ->
                    {S0, S1} = lists:split(L + Place, S),
                    S0 ++ "." ++ S1;
                2 - Place - L =< ExpCost ->
                    "0." ++ lists:duplicate(-Place - L, $0) ++ S;
                true ->
                    insert_exp(ExpL, S)
            end;
        true ->
            Dot =
                if
                    L =:= 1 ->
                        1;
                    true ->
                        0
                end,
            %% All integers in the range [-2^53, 2^53] can
            if
                %% be stored without loss of precision in an
                %% IEEE 754 64-bit double but 2^53+1 cannot be
                %% stored in an IEEE 754 64-bit double without
                %% loss of precision (float((1 bsl 53)+1) =:=
                %% float(1 bsl 53)). It thus makes sense to
                %% show floats that are >= 2^53 or <= -2^53 in
                %% scientific notation to indicate that the
                %% number is so large that there could be loss
                %% in precion when adding or subtracting 1.
                %%
                %% https://stackoverflow.com/questions/1848700/biggest-integer-that-can-be-stored-in-a-double?answertab=votes#tab-top
                ExpCost + Dot >= Place + 2 andalso abs(Float) < float(1 bsl 53) ->
                    S ++ lists:duplicate(Place, $0) ++ ".0";
                true ->
                    insert_exp(ExpL, S)
            end
    end.

insert_exp(ExpL, [C]) ->
    [C] ++ ".0e" ++ ExpL;
insert_exp(ExpL, [C | S]) ->
    [C] ++ "." ++ S ++ "e" ++ ExpL.

insert_minus(0, Digits) ->
    Digits;
insert_minus(1, Digits) ->
    [$-] ++ Digits.
