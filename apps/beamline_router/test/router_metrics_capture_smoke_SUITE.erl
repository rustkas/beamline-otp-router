%% @doc Smoke test for router_metrics_test_helper.
%%
%% This suite verifies that the metrics capture harness works correctly
%% in isolation before being used by the NATS publish test suites.
%%
%% @test_category sanity, metrics
-module(router_metrics_capture_smoke_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups_for_level/1]).
-export([
    test_emit_metric_capture/1,
    test_inc_capture/1,
    test_multiple_metrics_capture/1,
    test_wait_for_metric_timeout/1,
    test_clear_metrics/1
]).

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    %% Metrics capture tests are infrastructure/meta tests - run in full/heavy only
    %% They test the router_metrics_test_helper meck harness, not core router logic
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, harness_tests}];
groups_for_level(full) ->
    [{group, harness_tests}];
groups_for_level(_) -> %% fast - skip meta-infrastructure tests
    [].

groups() ->
    [{harness_tests, [sequence], [
        test_emit_metric_capture,
        test_inc_capture,
        test_multiple_metrics_capture,
        test_wait_for_metric_timeout,
        test_clear_metrics
    ]}].
init_per_suite(Config) ->
    _ = application:load(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    router_metrics_test_helper:setup([{debug, true}]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_metrics_test_helper:teardown(),
    ok.

%% @doc Test: emit_metric is captured and can be waited for.
test_emit_metric_capture(_Config) ->
    %% Call emit_metric directly
    ok = router_metrics:emit_metric(test_metric_1, #{count => 1}, #{label => value}),
    
    %% Wait for it
    ok = router_metrics_test_helper:wait_for_metric(test_metric_1, 1),
    
    %% Verify it's in the captured list
    Metrics = router_metrics_test_helper:get_metrics_by_name(test_metric_1),
    1 = length(Metrics),
    
    %% Verify structure using flexible assertions (Task 12 - reduce brittleness)
    %% Don't assert exact map contents - only check required fields exist
    [{emit_metric, MetricName, Measurements, Metadata, _Ts, _Ref}] = Metrics,
    test_metric_1 = MetricName,
    true = maps:is_key(count, Measurements),
    1 = maps:get(count, Measurements),
    true = maps:is_key(label, Metadata),
    
    ok.

%% @doc Test: inc is captured and can be waited for.
test_inc_capture(_Config) ->
    %% Call inc directly
    ok = router_metrics:inc(test_counter_1),
    
    %% Wait for it
    ok = router_metrics_test_helper:wait_for_inc(test_counter_1, 1),
    
    %% Verify it's in the captured list
    Metrics = router_metrics_test_helper:get_metrics_by_name(test_counter_1),
    1 = length(Metrics),
    
    ok.

%% @doc Test: Multiple metrics are captured correctly.
test_multiple_metrics_capture(_Config) ->
    %% Emit several metrics
    ok = router_metrics:emit_metric(multi_metric_a, #{count => 1}, #{}),
    ok = router_metrics:emit_metric(multi_metric_a, #{count => 2}, #{}),
    ok = router_metrics:emit_metric(multi_metric_b, #{count => 1}, #{}),
    ok = router_metrics:inc(multi_counter_a),
    ok = router_metrics:inc(multi_counter_a),
    
    %% Wait for each
    ok = router_metrics_test_helper:wait_for_metric(multi_metric_a, 2),
    ok = router_metrics_test_helper:wait_for_metric(multi_metric_b, 1),
    ok = router_metrics_test_helper:wait_for_inc(multi_counter_a, 2),
    
    %% Verify counts
    2 = length(router_metrics_test_helper:get_metrics_by_name(multi_metric_a)),
    1 = length(router_metrics_test_helper:get_metrics_by_name(multi_metric_b)),
    2 = length(router_metrics_test_helper:get_metrics_by_name(multi_counter_a)),
    
    %% Total should be 5
    5 = length(router_metrics_test_helper:get_all_metrics()),
    
    ok.

%% @doc Test: wait_for_metric times out correctly when metric not emitted.
test_wait_for_metric_timeout(_Config) ->
    %% Try to wait for a metric that won't be emitted
    %% This should timeout quickly (200ms)
    %% Note: ct:fail raises an 'exit' not 'error'
    try
        router_metrics_test_helper:wait_for_metric(nonexistent_metric, 1, 200),
        ct:fail(expected_timeout_but_got_ok)
    catch
        exit:{test_case_failed, {metric_not_reached, Expected, Actual, elapsed_ms, Elapsed}} ->
            %% This is the expected behavior - verify the values
            1 = Expected,
            0 = Actual,
            %% Elapsed should be around 200ms (allow some tolerance)
            true = (Elapsed >= 190 andalso Elapsed =< 500),
            ok
    end.

%% @doc Test: clear removes all captured metrics.
test_clear_metrics(_Config) ->
    %% Emit some metrics
    ok = router_metrics:emit_metric(clear_test_metric, #{count => 1}, #{}),
    ok = router_metrics:inc(clear_test_counter),
    
    %% Verify they're captured
    2 = length(router_metrics_test_helper:get_all_metrics()),
    
    %% Clear
    ok = router_metrics_test_helper:clear(),
    
    %% Verify empty
    0 = length(router_metrics_test_helper:get_all_metrics()),
    
    ok.
