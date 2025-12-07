%% @doc Common Test suite for R10 metrics validation
%% Tests: Label validation and value verification for R10 metrics
%% @test_category unit, metrics, r10
-module(router_metrics_r10_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Import test utilities
-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0
]).

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

%% Export test functions for Common Test
-export([
    test_publish_attempts_metrics_labels/1,
    test_publish_errors_metrics_labels/1,
    test_circuit_breaker_state_metrics_labels/1,
    test_circuit_breaker_transitions_metrics_labels/1,
    test_publish_latency_metrics_values/1,
    test_retry_delay_metrics_values/1
]).

all() ->
    [
        {group, metrics_tests}
    ].

groups() ->
    [
        {metrics_tests, [sequence], [
            test_publish_attempts_metrics_labels,
            test_publish_errors_metrics_labels,
            test_circuit_breaker_state_metrics_labels,
            test_circuit_breaker_transitions_metrics_labels,
            test_publish_latency_metrics_values,
            test_retry_delay_metrics_values
        ]}
    ].

init_per_suite(Config) ->
    %% Start application through supervisor (this starts router_circuit_breaker)
    ok = start_router_app(),
    %% Ensure circuit breaker is alive (fail immediately if not)
    ok = ensure_circuit_breaker_alive(),
    %% Ensure metrics table exists
    router_metrics:ensure(),
    %% Clear metrics (using router_r10_metrics to avoid direct ETS access)
    router_r10_metrics:clear_metrics(),
    Config.

end_per_suite(_Config) ->
    stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Idempotent start (safe for --case execution)
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    %% Clear metrics before each test (using router_r10_metrics to avoid direct ETS access)
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Get metric value using router_r10_metrics (no direct ETS access)
%% Delegates to router_r10_metrics:get_metric_value/2
-spec get_metric_value(atom(), map()) -> integer() | float() | undefined.
get_metric_value(MetricName, Labels) ->
    router_r10_metrics:get_metric_value(MetricName, Labels).

%% ========================================================================
%% METRICS TESTS
%% ========================================================================

%% @doc Test publish attempts metrics labels
test_publish_attempts_metrics_labels(_Config) ->
    ct:comment("Testing publish attempts metrics labels"),
    
    router_metrics:ensure(),
    
    %% Simulate publish attempts with different retry counts
    router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
        status => <<"success">>,
        retry_count => <<"0">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
        status => <<"success">>,
        retry_count => <<"1">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
        status => <<"error">>,
        retry_count => <<"2">>
    }),
    
    %% Verify metrics exist with correct labels
    Success0 = get_metric_value(router_nats_publish_attempts_total, #{
        status => <<"success">>,
        retry_count => <<"0">>
    }),
    ?assertEqual(1, Success0),
    
    Success1 = get_metric_value(router_nats_publish_attempts_total, #{
        status => <<"success">>,
        retry_count => <<"1">>
    }),
    ?assertEqual(1, Success1),
    
    Error2 = get_metric_value(router_nats_publish_attempts_total, #{
        status => <<"error">>,
        retry_count => <<"2">>
    }),
    ?assertEqual(1, Error2),
    
    ok.

%% @doc Test publish errors metrics labels
test_publish_errors_metrics_labels(_Config) ->
    ct:comment("Testing publish errors metrics labels"),
    
    router_metrics:ensure(),
    
    %% Emit errors with different types
    router_metrics:emit_metric(router_nats_publish_errors_total, #{count => 1}, #{
        error_type => <<"timeout">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_errors_total, #{count => 1}, #{
        error_type => <<"connection">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_errors_total, #{count => 1}, #{
        error_type => <<"nack">>
    }),
    
    %% Verify labels
    Timeout = get_metric_value(router_nats_publish_errors_total, #{error_type => <<"timeout">>}),
    ?assertEqual(1, Timeout),
    
    Connection = get_metric_value(router_nats_publish_errors_total, #{error_type => <<"connection">>}),
    ?assertEqual(1, Connection),
    
    Nack = get_metric_value(router_nats_publish_errors_total, #{error_type => <<"nack">>}),
    ?assertEqual(1, Nack),
    
    ok.

%% @doc Test circuit breaker state metrics labels
test_circuit_breaker_state_metrics_labels(_Config) ->
    ct:comment("Testing circuit breaker state metrics labels"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"latency_threshold_ms">> => 0
    }),
    
    %% Initialize closed state by recording a success (emits closed state metric)
    router_circuit_breaker:record_success(TenantId, ProviderId),
    timer:sleep(100),
    
    %% Verify closed state (emitted by record_success)
    ClosedMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"closed">>
    }),
    ?assertEqual(0.0, ClosedMetric),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    
    %% Verify open state
    OpenMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"open">>
    }),
    ?assertEqual(1.0, OpenMetric),
    
    %% Wait for timeout and transition to half-open
    %% Use should_allow to trigger timeout-based transition (open → half_open)
    timer:sleep(1100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),  %% Triggers timeout transition
    timer:sleep(100),
    
    %% Verify half-open state
    HalfOpenMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"half_open">>
    }),
    ?assertEqual(2.0, HalfOpenMetric),
    
    ok.

%% @doc Test circuit breaker transitions metrics labels
test_circuit_breaker_transitions_metrics_labels(_Config) ->
    ct:comment("Testing circuit breaker transitions metrics labels"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"latency_threshold_ms">> => 0
    }),
    
    %% Initialize closed state by recording a success (emits closed state metric and ensures state exists)
    router_circuit_breaker:record_success(TenantId, ProviderId),
    timer:sleep(50),
    
    %% Open circuit (closed → open)
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    
    ClosedToOpen = get_metric_value(router_circuit_breaker_state_transitions_total, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        from => <<"closed">>,
        to => <<"open">>
    }),
    ?assertEqual(1, ClosedToOpen),
    
    %% Wait for timeout (open → half_open)
    %% Use should_allow to trigger timeout-based transition (open → half_open)
    timer:sleep(1100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),  %% Triggers timeout transition
    timer:sleep(100),
    
    OpenToHalfOpen = get_metric_value(router_circuit_breaker_state_transitions_total, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        from => <<"open">>,
        to => <<"half_open">>
    }),
    ?assertEqual(1, OpenToHalfOpen),
    
    %% Close circuit (half_open → closed)
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, 2)),
    
    timer:sleep(100),
    
    HalfOpenToClosed = get_metric_value(router_circuit_breaker_state_transitions_total, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        from => <<"half_open">>,
        to => <<"closed">>
    }),
    ?assertEqual(1, HalfOpenToClosed),
    
    ok.

%% @doc Test publish latency metrics values
test_publish_latency_metrics_values(_Config) ->
    ct:comment("Testing publish latency metrics values"),
    
    router_metrics:ensure(),
    
    %% Emit latency values
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 0.1}, #{}),
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 0.5}, #{}),
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 2.0}, #{}),
    
    %% Verify values (should be last emitted value for gauge)
    Latency = get_metric_value(router_nats_publish_latency_seconds, #{}),
    ?assertEqual(2.0, Latency),
    
    ok.

%% @doc Test retry delay metrics values
test_retry_delay_metrics_values(_Config) ->
    ct:comment("Testing retry delay metrics values"),
    
    router_metrics:ensure(),
    
    %% Emit retry delays for different attempts
    router_metrics:emit_metric(router_nats_publish_retry_delay_seconds, #{value => 0.1}, #{
        attempt => <<"1">>
    }),
    router_metrics:emit_metric(router_nats_publish_retry_delay_seconds, #{value => 0.2}, #{
        attempt => <<"2">>
    }),
    router_metrics:emit_metric(router_nats_publish_retry_delay_seconds, #{value => 0.4}, #{
        attempt => <<"3">>
    }),
    
    %% Verify values
    Delay1 = get_metric_value(router_nats_publish_retry_delay_seconds, #{attempt => <<"1">>}),
    ?assertEqual(0.1, Delay1),
    
    Delay2 = get_metric_value(router_nats_publish_retry_delay_seconds, #{attempt => <<"2">>}),
    ?assertEqual(0.2, Delay2),
    
    Delay3 = get_metric_value(router_nats_publish_retry_delay_seconds, #{attempt => <<"3">>}),
    ?assertEqual(0.4, Delay3),
    
    ok.

