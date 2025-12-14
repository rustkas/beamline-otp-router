#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell

%% Auto-migrate a test suite to the groups_for_level/1 pattern
%% Usage: ./scripts/auto-migrate-suite.escript <suite_file> <tier>
%% Where tier is: unit | integration | heavy

main([SuiteFile, Tier]) ->
    {ok, Content} = file:read_file(SuiteFile),
    ContentStr = binary_to_list(Content),
    
    %% Check if already migrated
    case string:find(ContentStr, "groups_for_level") of
        nomatch ->
            %% Generate the migration patch
            Patch = generate_patch(Tier),
            io:format("~s~n", [Patch]),
            io:format("~n%% Add this after the exports section in ~s~n", [SuiteFile]);
        _ ->
            io:format("Suite already has groups_for_level/1~n")
    end;

main(_) ->
    io:format("Usage: auto-migrate-suite.escript <suite_file> <tier>~n"),
    io:format("  tier: unit | integration | heavy~n"),
    halt(1).

generate_patch("unit") ->
    "%% --- groups_for_level/1 pattern (unit/fast suite) ---
-export([groups_for_level/1]).

all() ->
    Level = case os:getenv(\"ROUTER_TEST_LEVEL\") of
        \"sanity\" -> sanity;
        \"heavy\" -> heavy;
        \"full\" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(sanity) -> [];
groups_for_level(fast) -> [{group, unit_tests}];
groups_for_level(full) -> [{group, unit_tests}];
groups_for_level(heavy) -> [{group, unit_tests}].

groups() ->
    [{unit_tests, [parallel], [
        %% Add test case names here
    ]}].
%% --- end pattern ---";

generate_patch("integration") ->
    "%% --- groups_for_level/1 pattern (integration suite) ---
-export([groups_for_level/1]).

all() ->
    Level = case os:getenv(\"ROUTER_TEST_LEVEL\") of
        \"sanity\" -> sanity;
        \"heavy\" -> heavy;
        \"full\" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];  %% Integration tests skip fast tier
groups_for_level(full) -> [{group, integration_tests}];
groups_for_level(heavy) -> [{group, integration_tests}].

groups() ->
    [{integration_tests, [sequence], [
        %% Add test case names here
    ]}].
%% --- end pattern ---";

generate_patch("heavy") ->
    "%% --- groups_for_level/1 pattern (heavy-only suite) ---
-export([groups_for_level/1]).

all() ->
    Level = case os:getenv(\"ROUTER_TEST_LEVEL\") of
        \"sanity\" -> sanity;
        \"heavy\" -> heavy;
        \"full\" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [];  %% Heavy tests only run in heavy tier
groups_for_level(heavy) -> [{group, heavy_tests}].

groups() ->
    [{heavy_tests, [sequence], [
        %% Add test case names here
    ]}].
%% --- end pattern ---";

generate_patch(_) ->
    "Unknown tier".
