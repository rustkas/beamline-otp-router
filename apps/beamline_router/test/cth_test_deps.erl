%% @doc CT Hook to ensure test dependencies are loaded
-module(cth_test_deps).

-export([id/1, init/2]).

id(_Opts) -> ?MODULE.

init(_Id, _Opts) ->
    %% Find and add test lib paths
    BaseDir = filename:dirname(code:which(?MODULE)),
    BuildTestDir = filename:join([BaseDir, "..", "..", "_build", "test", "lib"]),
    case filelib:is_dir(BuildTestDir) of
        true ->
            Libs = filelib:wildcard(filename:join([BuildTestDir, "*", "ebin"])),
            lists:foreach(fun(P) -> code:add_pathz(P) end, Libs),
            ct:pal("Loaded test deps from: ~p~n", [Libs]);
        false ->
            ct:pal("Test deps dir not found: ~p~n", [BuildTestDir])
    end,
    {ok, []}.

