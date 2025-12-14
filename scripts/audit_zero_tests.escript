#!/usr/bin/env escript
%%! -pa _build/test/lib/beamline_router/test

main(_) ->
    Path = filename:absname("_build/test/lib/beamline_router/test"),
    code:add_path(Path),
    
    {ok, Files} = file:list_dir(Path),
    Suites = [list_to_atom(filename:rootname(F)) || F <- Files, filename:extension(F) == ".beam", lists:suffix("_SUITE.beam", F)],
    
    io:format("Checking ~p suites in ~s...~n", [length(Suites), Path]),
    
    lists:foreach(fun(Suite) ->
        case catch Suite:groups_for_level(full) of
            {'EXIT', _} ->
                ok;
            [] ->
                io:format("ZERO: ~s returns [] for 'full'~n", [Suite]);
            _ ->
                ok
        end
    end, Suites).
