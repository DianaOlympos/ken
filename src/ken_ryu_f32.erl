%%%-------------------------------------------------------------------
%% @doc Implements the Ryu algorithm for f64 binaries
%%
%% This has been extracted from OTP 24, and is mostly present here as a reference implementation.
%% @end
%%%-------------------------------------------------------------------

-module(ken_ryu_f32).

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

-spec fwrite_g(binary()) -> iodata().
fwrite_g(Float) ->
    case sign_mantissa_exponent(Float) of
        {0, 0, 0} ->
            "0.0";
        {1, 0, 0} ->
            "-0.0";
        {S, M, E} when E < 255 ->
            {Place, Digits} = fwrite_g_1(M, E),
            DigitList = insert_decimal(Place, Digits, Float),
            insert_minus(S, DigitList)
    end.

-define(BIG_POW, (1 bsl 23)).
-define(DECODE_CORRECTION, 150).

sign_mantissa_exponent(<<S:1, BE:8, M:23>>) ->
    % <<S:1, BE:11, M:52>> = <<F:64/float>>,
    erlang:display([S, BE, M]),
    {S, M, BE}.

fwrite_g_1(M, E) ->
    {Mf, Ef} = decode(M, E),
    Shift = mmshift(M, E),
    Mv = 4 * Mf,
    {Q, Vm, Vr, Vp, E10} = convert_to_decimal(Ef, Mv, Shift),
    Accept = M rem 2 == 0,
    {VmIsTrailingZero, VrIsTrailingZero, Vp1} = bounds(Mv, Q, Vp, Accept, Ef, Shift),
    {D1, E1} = compute_shortest(Vm, Vr, Vp1, VmIsTrailingZero, VrIsTrailingZero, Accept),
    {E1 + E10, integer_to_binary(D1)}.

decode(Mantissa, 0) ->
    {Mantissa, 1 - ?DECODE_CORRECTION - 2};
decode(Mantissa, Exponent) ->
    {Mantissa + ?BIG_POW, Exponent - ?DECODE_CORRECTION - 2}.

mmshift(0, E) when E > 1 ->
    0;
mmshift(_M, _E) ->
    1.

convert_to_decimal(E2, Mv, Shift) when E2 >= 0 ->
    Q = max(0, ken_ryu_common:log10pow2(E2)),
    Mul = ken_ryu_table_f32:inv_value(Q),
    K = ken_ryu_table_f32:pow5_inv_bitcount() + ken_ryu_common:pow5bits(Q) - 1,
    I = -E2 + Q + K,
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, I, Mul),
    {Q, Vm, Vr, Vp, Q};
convert_to_decimal(E2, Mv, Shift) when E2 < 0 ->
    Q = max(0, ken_ryu_common:log10pow5(E2)),
    I = -E2 - Q,
    K = ken_ryu_common:pow5bits(I) - ken_ryu_table_f32:pow5_bitcount(),
    From_file = ken_ryu_table_f32:value(I),
    J = Q - K,
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, J, From_file),
    E10 = E2 + Q,
    {Q, Vm, Vr, Vp, E10}.

mulShiftAll(Mv, Shift, J, Mul) ->
    A = mulShift64(Mv - 1 - Shift, Mul, J),
    B = mulShift64(Mv, Mul, J),
    C = mulShift64(Mv + 2, Mul, J),
    {A, B, C}.

mulShift64(M, Mul, J) ->
    (M * Mul) bsr J.

bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q =< 9, Mv rem 5 =:= 0 ->
    {false, multipleOfPowerOf5(Mv, Q), Vp};
bounds(Mv, Q, Vp, true, E2, Shift) when E2 >= 0, Q =< 9 ->
    {multipleOfPowerOf5(Mv - 1 - Shift, Q), false, Vp};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q > 9 ->
    {false, false, Vp - vpmodifier(multipleOfPowerOf5(Mv + 2, Q))};
bounds(_Mv, Q, Vp, true, E2, Shift) when E2 < 0, Q =< 1 ->
    {Shift =:= 1, true, Vp};
bounds(_Mv, Q, Vp, false, E2, _Shift) when E2 < 0, Q =< 1 ->
    {false, true, Vp - 1};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 < 0, Q < 31 ->
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
    %TODO check if we need the last removed digits to start with q, see ryu
    {Vm1, Vr1, Removed, LastRemovedDigit} =
        handle_trailing_zeros(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, 0, 0),
    Output =
        Vr1 + handle_zero_output_mod(Vr1, Vm1, Accept, VmIsTrailingZero, LastRemovedDigit),
    {Output, Removed}.

general_case(Vm, Vr, Vp, Removed, RoundUp) when Vp div 10 =< Vm div 10 ->
    {Vm, Vr, Removed, RoundUp};
general_case(Vm, Vr, Vp, Removed, _RU) ->
    VmD10 = Vm div 10,
    VrD10 = Vr div 10,
    VpD10 = Vp div 10,
    RoundUp = Vr rem 10 >= 5,
    general_case(VmD10, VrD10, VpD10, 1 + Removed, RoundUp).

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
    L = byte_size(S),
    Exp = Place + L - 1,
    ExpL = integer_to_binary(Exp),
    ExpCost = byte_size(ExpL) + 2,
    if
        Place < 0 ->
            if
                Exp >= 0 ->
                    S0_size = L + Place,
                    <<S0:S0_size/binary, S1/binary>> = S,
                    [S0, $., S1];
                2 - Place - L =< ExpCost ->
                    ["0.", binary:copy(<<$0>>, -Place - L), S];
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
            <<_:1, AbsF:31>> = F,
            IsInt = (inf == ken_ryu_common:cmp_float_32(<<0:1, AbsF:31>>, <<0:1, 151:8, 0:23>>)),
            if
                %% All integers in the range [-2^24, 2^24] can
                %% be stored without loss of precision in an
                %% IEEE 754 32-bit double but 2^24+1 cannot be
                %% stored in an IEEE 754 32-bit double without
                %% loss of precision (float_32((1 bsl 24)+1) =:=
                %% float_32(1 bsl 24)). It thus makes sense to
                %% show floats that are >= 2^24 or <= -2^24 in
                %% scientific notation to indicate that the
                %% number is so large that there could be loss
                %% in precion when adding or subtracting 1.
                %%
                %% https://stackoverflow.com/questions/1848700/biggest-integer-that-can-be-stored-in-a-double?answertab=votes#tab-top
                ExpCost + Dot >= Place + 2 andalso IsInt ->
                    [S, binary:copy(<<$0>>, Place), ".0"];
                true ->
                    insert_exp(ExpL, S)
            end
    end.

insert_exp(ExpL, <<C:1/binary>>) ->
    [C, ".0e", ExpL];
insert_exp(ExpL, <<C:1/binary, S/binary>>) ->
    [C, $., S, $e, ExpL].

insert_minus(0, Digits) ->
    Digits;
insert_minus(1, Digits) ->
    [$-, Digits].
