#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

%% @doc Migrate a test suite to groups_for_level pattern
%% Usage: ./scripts/migrate-suite.escript <suite_file> <tier>
%% Tier: fast | full | heavy

main([SuiteFile, Tier]) ->
    io:format("Migrating ~s to tier: ~s~n", [SuiteFile, Tier]),
    
    %% Read file
    {ok, Content} = file:read_file(SuiteFile),
    ContentStr = binary_to_list(Content),
    
    %% Check if already has groups_for_level
    case string:find(ContentStr, "groups_for_level") of
        nomatch ->
            %% Generate migration patch
            Patch = generate_patch(Tier),
            io:format("~nPatch to apply:~n~s~n", [Patch]),
            io:format("~nManual migration required - see patch above~n");
        _ ->
            io:format("Suite already has groups_for_level~n")
    end;

main(_) ->
    io:format("Usage: ./scripts/migrate-suite.escript <suite_file> <tier>~n"),
    io:format("  tier: fast | full | heavy~n"),
    halt(1).

generate_patch(Tier) ->
    case Tier of
        "fast" -> fast_patch();
        "full" -> full_patch();
        "heavy" -> heavy_patch();
        _ -> "Unknown tier"
    end.

fast_patch() ->
    "
all() ->
    Level = case os:getenv(\"ROUTER_TEST_LEVEL\") of
        \"heavy\" -> heavy;
        \"full\"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast, sanity
    [{group, unit_tests}].
".

full_patch() ->
    "
all() ->
    Level = case os:getenv(\"ROUTER_TEST_LEVEL\") of
        \"heavy\" -> heavy;
        \"full\"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, integration_tests}];
groups_for_level(full) ->
    [{group, integration_tests}];
groups_for_level(_) -> %% fast, sanity
    [].  %% Integration tests require full context
".

heavy_patch() ->
    "
all() ->
    Level = case os:getenv(\"ROUTER_TEST_LEVEL\") of
        \"heavy\" -> heavy;
        \"full\"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, heavy_tests}];
groups_for_level(_) -> %% fast, full, sanity
    [].  %% Heavy tests only run in heavy tier
".
