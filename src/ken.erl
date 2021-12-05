-module(ken).

-export([binary_to_iodata/1]).

binary_to_iodata(<<_:64>> = Binary) ->
    ken_ryu_f64:fwrite_g(Binary).
