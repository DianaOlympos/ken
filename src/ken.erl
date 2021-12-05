%%%-------------------------------------------------------------------
%% @doc `ken' allows you to get the shortest round-trip representation of a binary encoding an IEEE 754 floating point number.
%%
%% Ken use the Ryu algorithm from Ulf Adams in order to get a fast and precise output.
%% It has been modified to keep the erlang/OTP representation of floating point numbers as string.
%%
%% @end
%%
%% @reference See <a href="https://github.com/ulfjack/ryu">https://github.com/ulfjack/ryu</a>
%%
%% @end
%%%-------------------------------------------------------------------

-module(ken).

-export([binary_to_iodata/1]).

-type f64_binary_type() :: <<_:64 * 8>>.

%% @doc Take the binary of a 64 bits IEEE 754 floating point number and return
%% an Iodata representing the shortest round trip decimal string representation
%% of it by the erlang rules for such.
-spec binary_to_iodata(f64_binary_type()) -> iodata().
binary_to_iodata(<<_:64>> = Binary) ->
    ken_ryu_f64:fwrite_g(Binary).
