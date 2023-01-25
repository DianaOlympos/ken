-module(s2f_nif).

-export([s2f/1, s2d/1]).

%% Native library support
-export([load/0]).
-on_load(load/0).

load() ->
    erlang:load_nif(filename:join(priv(), "libs2f"), none).
not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

s2f(_A_bin) ->
    not_loaded(?LINE).

s2d(_A_bin) ->
    not_loaded(?LINE).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
