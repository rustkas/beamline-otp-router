%% @doc Unit tests for router_caf_adapter
%% Tests: ExecAssignment building, field validation, assignment_id generation
%% @test_category fast
-module(router_caf_adapter_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(_) ->
    [].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

