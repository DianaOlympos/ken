# Ken

"Binary float" to string conversion in erlang for f64, f32 and f16 using Ryu

## Build

    rebar3 compile

## Test

    rebar3 ct

## Table

This algorithm need precomputed tables, done using the script in `scripts`.

If you changed things that affect these, rerun the scripts before compiling.
Only do this if you know what you are doing.
