%% @doc Common Test suite for router_nats_publish_retry module
%% Tests: Retry logic, backoff strategies, jitter, deadline, max attempts, error classification
%% @test_category unit, retry_logic, r10
-module(router_nats_publish_retry_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    get_metric_value/2  %% Helper for metric assertions
]}).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Export test functions for Common Test
-export([
    test_retry_module_smoke/1,
    test_exponential_backoff_calculation/1,
    test_linear_backoff_calculation/1,
    test_backoff_with_jitter/1,
    test_max_attempts_enforced/1,
    test_deadline_exceeded_before_max_attempts/1,
    test_deadline_not_exceeded_with_fast_success/1,
    test_retryable_errors/1,
    test_non_retryable_errors/1
]).

%% No tier branching in suite

all() ->
    [
        {group, smoke_tests},
        {group, retry_tests},
        {group, quarantine}
    ].

meta_all() ->
    all().

%% @doc Retry logic tests -> Full
%% No groups_for_level branching

groups() ->
    Base = base_groups(),
    Quarantine = [{quarantine, [sequence], [
        test_retry_module_smoke,
        test_exponential_backoff_calculation
    ]}],
    router_ct_groups:groups_definitions(?MODULE, Base ++ Quarantine).

base_groups() ->
    [
        {smoke_tests, [parallel], [
            test_retry_module_smoke
        ]},
        {retry_tests, [parallel], [
            test_exponential_backoff_calculation,
            test_linear_backoff_calculation,
            test_backoff_with_jitter,
            test_max_attempts_enforced,
            test_deadline_exceeded_before_max_attempts,
            test_deadline_not_exceeded_with_fast_success,
            test_retryable_errors,
            test_non_retryable_errors
        ]}
    ].

init_per_suite(Config) ->
    %% Load application configuration
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    %% Ensure metrics table exists
    router_metrics:ensure(),
    %% Clear metrics using router_r10_metrics access layer
    router_r10_metrics:clear_metrics(),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clear metrics before each test using router_r10_metrics access layer
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ========================================================================
%% SMOKE TEST
%% ========================================================================

%% @doc Smoke test - verify module loads and exports expected functions
test_retry_module_smoke(_Config) ->
    ct:comment("Smoke test: verify router_nats_publish_retry module"),
    
    %% Verify module is loaded
    {module, router_nats_publish_retry} = code:ensure_loaded(router_nats_publish_retry),
    
    %% Verify expected functions exist
    true = erlang:function_exported(router_nats_publish_retry, calculate_backoff, 5),
    true = erlang:function_exported(router_nats_publish_retry, is_retryable_error, 1),
    true = erlang:function_exported(router_nats_publish_retry, get_default_config, 0),
    
    %% Quick sanity check on calculate_backoff
    Delay = router_nats_publish_retry:calculate_backoff(1, exponential, 100, 5000, 0),
    ?assertEqual(100, Delay),
    
    %% Quick sanity check on is_retryable_error
    true = router_nats_publish_retry:is_retryable_error(timeout),
    false = router_nats_publish_retry:is_retryable_error(invalid_payload),
    
    ok.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Get metric value using router_r10_metrics access layer
-spec get_metric_value(atom(), map()) -> integer() | float() | undefined.
get_metric_value(MetricName, Labels) ->
    router_r10_metrics:get_metric_value(MetricName, Labels).

%% ========================================================================
%% RETRY LOGIC TESTS
%% ========================================================================

%% @doc Test exponential backoff calculation
test_exponential_backoff_calculation(_Config) ->
    ct:comment("Testing exponential backoff calculation"),
    
    Base = 100,  % 100ms
    Max = 5000,  % 5000ms
    Jitter = 0,  % No jitter for deterministic test
    
    %% Test attempt 1: should be ~100ms
    Delay1 = router_nats_publish_retry:calculate_backoff(1, exponential, Base, Max, Jitter),
    ?assertEqual(100, Delay1),
    
    %% Test attempt 2: should be ~200ms (2^1 * 100)
    Delay2 = router_nats_publish_retry:calculate_backoff(2, exponential, Base, Max, Jitter),
    ?assertEqual(200, Delay2),
    
    %% Test attempt 3: should be ~400ms (2^2 * 100)
    Delay3 = router_nats_publish_retry:calculate_backoff(3, exponential, Base, Max, Jitter),
    ?assertEqual(400, Delay3),
    
    %% Test attempt 6: should be 3200ms (2^5 * 100 = 3200, not capped yet)
    Delay6 = router_nats_publish_retry:calculate_backoff(6, exponential, Base, Max, Jitter),
    ?assertEqual(3200, Delay6),
    
    %% Test attempt 10: should be capped at Max
    Delay10 = router_nats_publish_retry:calculate_backoff(10, exponential, Base, Max, Jitter),
    ?assert(Delay10 =< Max),
    ?assert(Delay10 >= 100),
    
    ok.

%% @doc Test linear backoff calculation
test_linear_backoff_calculation(_Config) ->
    ct:comment("Testing linear backoff calculation"),
    
    Base = 100,
    Max = 5000,
    Jitter = 0,
    
    %% Test attempt 1: should be 100ms
    Delay1 = router_nats_publish_retry:calculate_backoff(1, linear, Base, Max, Jitter),
    ?assertEqual(100, Delay1),
    
    %% Test attempt 2: should be 200ms (2 * 100)
    Delay2 = router_nats_publish_retry:calculate_backoff(2, linear, Base, Max, Jitter),
    ?assertEqual(200, Delay2),
    
    %% Test attempt 50: should be capped at Max (50 * 100 = 5000)
    Delay50 = router_nats_publish_retry:calculate_backoff(50, linear, Base, Max, Jitter),
    ?assertEqual(5000, Delay50),
    
    %% Test attempt 100: should be capped at Max
    Delay100 = router_nats_publish_retry:calculate_backoff(100, linear, Base, Max, Jitter),
    ?assertEqual(5000, Delay100),
    
    ok.

%% @doc Test backoff with jitter
test_backoff_with_jitter(_Config) ->
    ct:comment("Testing backoff with jitter"),
    
    Base = 100,
    Max = 5000,
    JitterPercent = 20,  % ±20%
    
    %% Run multiple times to verify jitter range
    Delays = [router_nats_publish_retry:calculate_backoff(2, exponential, Base, Max, JitterPercent) 
              || _ <- lists:seq(1, 100)],
    
    %% Base delay for attempt 2: 200ms
    BaseDelay = 200,
    JitterRange = trunc(BaseDelay * JitterPercent / 100),  % ±40ms
    
    %% All delays should be within [160, 240]ms
    lists:foreach(fun(Delay) ->
        ?assert(Delay >= BaseDelay - JitterRange),
        ?assert(Delay =< BaseDelay + JitterRange),
        ?assert(Delay >= 0),
        ?assert(Delay =< Max)
    end, Delays),
    
    %% Verify we have some variation (not all same value)
    UniqueDelays = sets:from_list(Delays),
    ?assert(sets:size(UniqueDelays) > 1),
    
    ok.

%% @doc Test max attempts enforced
test_max_attempts_enforced(_Config) ->
    ct:comment("Testing max attempts enforcement"),
    
    MaxAttempts = 3,
    
    %% Mock publish function that always fails
    AttemptCountRef = make_ref(),
    put({attempt_count, AttemptCountRef}, 0),
    
    PublishFun = fun(_Subject, _Payload) ->
        Count = get({attempt_count, AttemptCountRef}),
        put({attempt_count, AttemptCountRef}, Count + 1),
        {error, timeout}
    end,
    
    %% Mock metrics function
    MetricsFun = fun(_Attempt, _Max) -> ok end,
    
    Config = #{
        <<"max_attempts">> => MaxAttempts,
        <<"backoff_strategy">> => exponential,
        <<"backoff_base_ms">> => 10,  % Small for fast test
        <<"backoff_max_ms">> => 100,
        <<"jitter_percent">> => 0,
        <<"timeout_per_attempt_ms">> => 1000,
        <<"total_deadline_ms">> => 10000
    },
    
    Result = router_nats_publish_retry:publish_with_retry(
        <<"test.subject">>, <<"payload">>, PublishFun, MetricsFun, #{}, Config
    ),
    
    FinalCount = get({attempt_count, AttemptCountRef}),
    
    %% Should have attempted exactly MaxAttempts times
    ?assertEqual(MaxAttempts, FinalCount),
    ?assertMatch({error, max_attempts_exceeded}, Result),
    
    %% Cleanup
    erase({attempt_count, AttemptCountRef}),
    
    ok.

%% @doc Test deadline exceeded before max attempts
test_deadline_exceeded_before_max_attempts(_Config) ->
    ct:comment("Testing deadline exceeded before max attempts"),
    
    MaxAttempts = 10,  % High max attempts
    TotalDeadline = 500,  % 500ms deadline (short)
    BackoffBase = 200,  % 200ms base delay (will exceed deadline quickly)
    
    PublishFun = fun(_Subject, _Payload) ->
        timer:sleep(50),  % Simulate some processing time
        {error, timeout}
    end,
    
    MetricsFun = fun(_Attempt, _Max) -> ok end,
    
    Config = #{
        <<"max_attempts">> => MaxAttempts,
        <<"backoff_strategy">> => exponential,
        <<"backoff_base_ms">> => BackoffBase,
        <<"backoff_max_ms">> => 5000,
        <<"jitter_percent">> => 0,
        <<"timeout_per_attempt_ms">> => 1000,
        <<"total_deadline_ms">> => TotalDeadline
    },
    
    StartTime = erlang:system_time(millisecond),
    Result = router_nats_publish_retry:publish_with_retry(
        <<"test.subject">>, <<"payload">>, PublishFun, MetricsFun, #{}, Config
    ),
    Elapsed = erlang:system_time(millisecond) - StartTime,
    
    %% Should fail with deadline_exceeded
    ?assertMatch({error, deadline_exceeded}, Result),
    
    %% Should have taken approximately TotalDeadline (with some tolerance)
    %% Note: Backoff delays can cause deadline to be exceeded later than expected
    ?assert(Elapsed >= TotalDeadline),
    ?assert(Elapsed < TotalDeadline + 500),  % Allow 500ms tolerance for backoff delays
    
    ok.

%% @doc Test deadline not exceeded with fast success
test_deadline_not_exceeded_with_fast_success(_Config) ->
    ct:comment("Testing deadline not exceeded with fast success"),
    
    TotalDeadline = 10000,  % 10s deadline
    
    %% Mock publish that succeeds on 2nd attempt
    AttemptCountRef = make_ref(),
    put({attempt_count, AttemptCountRef}, 0),
    
    PublishFun = fun(_Subject, _Payload) ->
        Count = get({attempt_count, AttemptCountRef}),
        put({attempt_count, AttemptCountRef}, Count + 1),
        case Count of
            0 -> {error, timeout};  % First attempt fails
            _ -> ok  % Second attempt succeeds
        end
    end,
    
    MetricsFun = fun(_Attempt, _Max) -> ok end,
    
    Config = router_nats_publish_retry:get_default_config(),
    Config1 = Config#{<<"total_deadline_ms">> => TotalDeadline},
    
    StartTime = erlang:system_time(millisecond),
    Result = router_nats_publish_retry:publish_with_retry(
        <<"test.subject">>, <<"payload">>, PublishFun, MetricsFun, #{}, Config1
    ),
    Elapsed = erlang:system_time(millisecond) - StartTime,
    
    %% Should succeed
    ?assertMatch({ok, 2}, Result),
    
    %% Should complete well before deadline
    ?assert(Elapsed < TotalDeadline),
    
    %% Should have made 2 attempts
    ?assertEqual(2, get({attempt_count, AttemptCountRef})),
    
    %% Cleanup
    erase({attempt_count, AttemptCountRef}),
    
    ok.

%% @doc Test retryable errors
test_retryable_errors(_Config) ->
    ct:comment("Testing retryable error classification"),
    
    RetryableErrors = [
        timeout,
        connection_refused,
        connection_closed,
        nats_unavailable,
        not_connected,
        {error, timeout},
        {error, connection_refused}
    ],
    
    lists:foreach(fun(Error) ->
        IsRetryable = router_nats_publish_retry:is_retryable_error(Error),
        ?assert(IsRetryable)
    end, RetryableErrors),
    
    ok.

%% @doc Test non-retryable errors
test_non_retryable_errors(_Config) ->
    ct:comment("Testing non-retryable error classification"),
    
    NonRetryableErrors = [
        invalid_payload,
        authorization_failed,
        {error, invalid_payload},
        {error, authorization_failed}
    ],
    
    lists:foreach(fun(Error) ->
        IsRetryable = router_nats_publish_retry:is_retryable_error(Error),
        ?assertNot(IsRetryable)
    end, NonRetryableErrors),
    
    ok.
