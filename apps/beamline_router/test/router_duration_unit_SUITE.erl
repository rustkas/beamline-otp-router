%% @doc Unit Tests for router_duration module
%% @test_category unit, fast, coverage_hotspot
-module(router_duration_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_duration_ms_positive/1,
    test_duration_ms_zero/1,
    test_duration_ms_large_values/1,
    test_duration_us_positive/1,
    test_duration_us_zero/1,
    test_duration_us_negative/1,
    test_duration_seconds_positive/1,
    test_duration_seconds_zero/1,
    test_duration_seconds_fractional/1,
    test_duration_ms_real_time/1,
    test_duration_us_real_time/1,
    test_duration_seconds_real_time/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    test_duration_ms_positive/1,
    test_duration_ms_zero/1,
    test_duration_ms_large_values/1,
    test_duration_us_positive/1,
    test_duration_us_zero/1,
    test_duration_us_negative/1,
    test_duration_seconds_positive/1,
    test_duration_seconds_zero/1,
    test_duration_seconds_fractional/1,
    test_duration_ms_real_time/1,
    test_duration_us_real_time/1,
    test_duration_seconds_real_time/1
]}).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) ->
    [{group, unit_tests}].
groups() ->
    [
        {unit_tests, [sequence], [
            test_duration_ms_positive,
            test_duration_ms_zero,
            test_duration_ms_large_values,
            test_duration_us_positive,
            test_duration_us_zero,
            test_duration_us_negative,
            test_duration_seconds_positive,
            test_duration_seconds_zero,
            test_duration_seconds_fractional,
            test_duration_ms_real_time,
            test_duration_us_real_time,
            test_duration_seconds_real_time
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% Tests for duration_ms/2
%% ============================================================================

test_duration_ms_positive(_Config) ->
    StartTime = 1000000,
    EndTime = 2000000,
    
    Result = router_duration:duration_ms(StartTime, EndTime),
    
    ?assertEqual(true, is_float(Result)),
    ?assertEqual(1000.0, Result),
    
    ok.

test_duration_ms_zero(_Config) ->
    Time = 1000000,
    
    Result = router_duration:duration_ms(Time, Time),
    
    ?assertEqual(0.0, Result),
    
    ok.

test_duration_ms_large_values(_Config) ->
    StartTime = 0,
    EndTime = 1000000000,  %% 1 billion microseconds = 1000 seconds
    
    Result = router_duration:duration_ms(StartTime, EndTime),
    
    ?assertEqual(1000000.0, Result),  %% 1 million milliseconds
    
    ok.

%% ============================================================================
%% Tests for duration_us/2
%% ============================================================================

test_duration_us_positive(_Config) ->
    StartTime = 1000,
    EndTime = 5000,
    
    Result = router_duration:duration_us(StartTime, EndTime),
    
    ?assertEqual(true, is_integer(Result)),
    ?assertEqual(4000, Result),
    
    ok.

test_duration_us_zero(_Config) ->
    Time = 1000,
    
    Result = router_duration:duration_us(Time, Time),
    
    ?assertEqual(0, Result),
    
    ok.

test_duration_us_negative(_Config) ->
    StartTime = 5000,
    EndTime = 1000,
    
    Result = router_duration:duration_us(StartTime, EndTime),
    
    %% Negative duration when end < start
    ?assertEqual(-4000, Result),
    
    ok.

%% ============================================================================
%% Tests for duration_seconds/2
%% ============================================================================

test_duration_seconds_positive(_Config) ->
    StartTime = 0,
    EndTime = 1000000,  %% 1 million microseconds = 1 second
    
    Result = router_duration:duration_seconds(StartTime, EndTime),
    
    ?assertEqual(true, is_float(Result)),
    ?assertEqual(1.0, Result),
    
    ok.

test_duration_seconds_zero(_Config) ->
    Time = 1000000,
    
    Result = router_duration:duration_seconds(Time, Time),
    
    ?assertEqual(0.0, Result),
    
    ok.

test_duration_seconds_fractional(_Config) ->
    StartTime = 0,
    EndTime = 500000,  %% 500000 microseconds = 0.5 seconds
    
    Result = router_duration:duration_seconds(StartTime, EndTime),
    
    ?assertEqual(0.5, Result),
    
    ok.

%% ============================================================================
%% Tests with real monotonic time
%% ============================================================================

test_duration_ms_real_time(_Config) ->
    StartTime = erlang:monotonic_time(microsecond),
    router_test_utils:wait_for_metric(fun() -> erlang:monotonic_time(microsecond) - StartTime end, 10 * 1000, 1000),
    EndTime = erlang:monotonic_time(microsecond),
    
    Result = router_duration:duration_ms(StartTime, EndTime),
    
    ?assertEqual(true, is_float(Result)),
    ?assertEqual(true, Result >= 10.0),  %% At least 10ms
    ?assertEqual(true, Result < 100.0),  %% Less than 100ms (reasonable)
    
    ok.

test_duration_us_real_time(_Config) ->
    StartTime = erlang:monotonic_time(microsecond),
    router_test_utils:wait_for_metric(fun() -> erlang:monotonic_time(microsecond) - StartTime end, 5 * 1000, 1000),
    EndTime = erlang:monotonic_time(microsecond),
    
    Result = router_duration:duration_us(StartTime, EndTime),
    
    ?assertEqual(true, is_integer(Result)),
    ?assertEqual(true, Result >= 5000),   %% At least 5000us
    ?assertEqual(true, Result < 200000),  %% Less than 200ms (allows slower hosts)
    
    ok.

test_duration_seconds_real_time(_Config) ->
    StartTime = erlang:monotonic_time(microsecond),
    router_test_utils:wait_for_metric(fun() -> erlang:monotonic_time(microsecond) - StartTime end, 50 * 1000, 2000),
    EndTime = erlang:monotonic_time(microsecond),
    
    Result = router_duration:duration_seconds(StartTime, EndTime),
    
    ?assertEqual(true, is_float(Result)),
    ?assertEqual(true, Result >= 0.05),  %% At least 50ms
    ?assertEqual(true, Result < 0.5),    %% Less than 500ms
    
    ok.
