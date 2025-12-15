%%% @doc Common Test quarantine group utilities.
%%%
%%% Provides functions to work with quarantine groups in Common Test suites.
%%% This module helps manage test cases that should be excluded from normal
%%% test runs but included in heavy/nightly test runs.
%%%
%%% @end

-module(ct_quarantine).

-export([groups/0, is_quarantined/1]).

%% @doc Returns the quarantine group definition.
%% This group can be included in test suite group/0 functions.
-spec groups() -> [{quarantine, [], [atom()]}].
groups() ->
    [{quarantine, [], [quarantined_tests]}].

%% @doc Helper function to check if a test is quarantined.
%% Can be used in test case guards or conditionals.
-spec is_quarantined(TestCase :: atom()) -> boolean().
is_quarantined(TestCase) ->
    case erlang:get(test_server_group_properties) of
        undefined -> false;
        Props -> proplists:is_defined(quarantine, Props)
    end.
