#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell

%% Script to add missing -export directives to Common Test suites
%% Usage: escript scripts/fix_missing_exports.escript

main([]) ->
    TestDir = "test",
    {ok, Files} = file:list_dir(TestDir),
    Suites = [F || F <- Files, lists:suffix("_SUITE.erl", F)],
    
    io:format("Checking ~p test suites for missing exports...~n", [length(Suites)]),
    
    Fixed = lists:foldl(fun(Suite, Acc) ->
        Path = filename:join(TestDir, Suite),
        case fix_suite(Path) of
            {ok, fixed} ->
                io:format("  FIXED: ~s~n", [Suite]),
                Acc + 1;
            {ok, already_exported} ->
                Acc;
            {error, Reason} ->
                io:format("  ERROR: ~s - ~p~n", [Suite, Reason]),
                Acc
        end
    end, 0, Suites),
    
    io:format("~nDone. Fixed ~p suites.~n", [Fixed]).

fix_suite(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            ContentStr = binary_to_list(Content),
            %% Check if already has -export
            case re:run(ContentStr, "^-export\\s*\\(", [{newline, any}, multiline]) of
                {match, _} ->
                    {ok, already_exported};
                nomatch ->
                    %% Find test functions defined in all() or groups()
                    Tests = extract_test_functions(ContentStr),
                    case Tests of
                        [] ->
                            {error, no_tests_found};
                        _ ->
                            %% Add exports after -compile or -module
                            add_exports(Path, ContentStr, Tests)
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

extract_test_functions(Content) ->
    %% Look for test_* function definitions
    case re:run(Content, "(test_[a-z0-9_]+)\\s*\\(_Config\\)", 
                [global, {capture, [1], list}]) of
        {match, Matches} ->
            lists:usort([M || [M] <- Matches]);
        nomatch ->
            []
    end.

add_exports(Path, Content, Tests) ->
    %% Find position after -compile or after -module
    InsertPoint = find_insert_point(Content),
    
    %% Build export strings
    CTExports = "-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).",
    
    TestExports = case length(Tests) of
        N when N > 0 ->
            TestList = string:join([T ++ "/1" || T <- Tests], ",\n    "),
            io_lib:format("-export([~n    ~s~n]).", [TestList]);
        _ ->
            ""
    end,
    
    Exports = io_lib:format("~n%% Common Test exports (REQUIRED for CT to find tests)~n~s~n~n%% Test function exports~n~s~n", 
                            [CTExports, TestExports]),
    
    %% Insert exports
    {Before, After} = lists:split(InsertPoint, Content),
    NewContent = Before ++ lists:flatten(Exports) ++ After,
    
    case file:write_file(Path, NewContent) of
        ok -> {ok, fixed};
        {error, R} -> {error, R}
    end.

find_insert_point(Content) ->
    %% Find end of -compile or -module directive
    Patterns = [
        "-compile\\s*\\([^)]+\\)\\s*\\.",
        "-module\\s*\\([^)]+\\)\\s*\\."
    ],
    
    find_best_insert_point(Content, Patterns, 0).

find_best_insert_point(_Content, [], Best) ->
    Best;
find_best_insert_point(Content, [Pattern | Rest], Best) ->
    case re:run(Content, Pattern, [{capture, first, index}]) of
        {match, [{Start, Len}]} ->
            EndPos = Start + Len,
            NewBest = max(Best, EndPos),
            find_best_insert_point(Content, Rest, NewBest);
        nomatch ->
            find_best_insert_point(Content, Rest, Best)
    end.
