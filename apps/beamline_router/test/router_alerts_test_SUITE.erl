%% # Alert Evaluation and Rules Tests
%%
%% Tests focused on alert rule evaluation and rule configuration paths.

-module(router_alerts_test_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").  % Needed for #alert_rule{} and #alert{} records

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_alert_rule_evaluation_basic/1,
    test_alert_rule_thresholds/1,
    test_alert_rule_time_windows/1,
    test_alert_rule_priority_and_dedup/1,
    test_alert_rule_storage_and_retrieval/1,
    test_alert_rule_integration_with_metrics/1,
    test_quarantined_alert_rule/1,
    test_circuit_breaker_alert_conditions/1
]).

all() ->
    TestLevel = os:getenv("ROUTER_TEST_LEVEL", "full"),
    case TestLevel of
        "sanity" ->
            [{group, smoke}];
        "full" ->
            [{group, unit}, {group, integration}];
        "heavy" ->
            Base = [{group, unit}, {group, integration}],
            %% Check if quarantine tests should be included
            case is_quarantine_enabled() of
                true -> Base ++ [{group, quarantine}];
                false -> Base
            end;
        _ ->
            [{group, unit}]
    end.

groups() ->
    [
        {smoke, [sequence], [test_alert_rule_evaluation_basic]},
        {unit, [sequence],
            [
                test_alert_rule_evaluation_basic,
                test_alert_rule_thresholds,
                test_alert_rule_time_windows,
                test_alert_rule_priority_and_dedup,
                test_alert_rule_storage_and_retrieval,
                test_circuit_breaker_alert_conditions
            ]},
        {integration, [sequence],
            [
                test_alert_rule_integration_with_metrics
            ]},
        {quarantine, [sequence],
            [
                test_quarantined_alert_rule
            ]}
    ].

init_per_suite(Config) ->
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    ensure_alerts_started(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = router_alerts:reset(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

ensure_alerts_started() ->
    case whereis(router_alerts) of
        undefined ->
            {ok, _Pid} = router_alerts:start_link(),
            ok;
        _Pid ->
            ok
    end.

%% ## Basic alert rule evaluation

test_alert_rule_evaluation_basic(_Config) ->
    Rule = #alert_rule{
        id = <<"test_rule_1">>,
        metric = publish_latency,
        threshold = 100,
        window_ms = 1000,
        severity = warning
    },
    ok = router_alerts:add_rule(Rule),
    ok = router_alerts:record_metric(publish_latency, 150),
    {ok, _} = router_alerts:evaluate_all_rules(),
    Alerts = router_alerts:get_active_alerts(),
    ?assertMatch([_ | _], Alerts),
    [Alert | _] = Alerts,
    ?assertEqual(<<"test_rule_1">>, maps:get(id, Alert)).

%% ## Alert thresholds

test_alert_rule_thresholds(_Config) ->
    Rule = #alert_rule{
        id = <<"threshold_rule">>,
        metric = error_rate,
        threshold = 0.1,
        window_ms = 1000,
        severity = critical
    },
    ok = router_alerts:add_rule(Rule),
    ok = router_alerts:record_metric(error_rate, 0.05),
    {ok, _} = router_alerts:evaluate_all_rules(),
    ?assertEqual([], router_alerts:get_active_alerts()),

    ok = router_alerts:record_metric(error_rate, 0.15),
    {ok, _} = router_alerts:evaluate_all_rules(),
    ?assertMatch([_], router_alerts:get_active_alerts()).

%% ## Time window behavior

test_alert_rule_time_windows(_Config) ->
    Rule = #alert_rule{
        id = <<"window_rule">>,
        metric = queue_depth,
        threshold = 10,
        window_ms = 200,
        severity = warning
    },
    ok = router_alerts:add_rule(Rule),
    ok = router_alerts:record_metric(queue_depth, 15),
    {ok, _} = router_alerts:evaluate_all_rules(),
    ?assertMatch([_], router_alerts:get_active_alerts()),

    %% In this simple test, we don't have a real time window because 
    %% evaluate_all_rules just evaluates against the current metric value.
    %% If we want to test time windows, we'd need to mock time or the metric store.
    ok.

%% ## Priority and de-duplication

test_alert_rule_priority_and_dedup(_Config) ->
    Rule1 = #alert_rule{
        id = <<"rule_warning">>,
        metric = publish_latency,
        threshold = 100,
        window_ms = 1000,
        severity = warning
    },
    Rule2 = #alert_rule{
        id = <<"rule_critical">>,
        metric = publish_latency,
        threshold = 200,
        window_ms = 1000,
        severity = critical
    },
    ok = router_alerts:add_rule(Rule1),
    ok = router_alerts:add_rule(Rule2),

    ok = router_alerts:record_metric(publish_latency, 250),
    {ok, _} = router_alerts:evaluate_all_rules(),
    Alerts = router_alerts:get_active_alerts(),
    ?assertEqual(2, length(Alerts)).

%% ## Storage and retrieval

test_alert_rule_storage_and_retrieval(_Config) ->
    Rule = #alert_rule{
        id = <<"store_rule">>,
        metric = memory_usage,
        threshold = 80,
        window_ms = 1000,
        severity = warning
    },
    ok = router_alerts:add_rule(Rule),

    StoredRules = router_alerts:list_rules(),
    Found = lists:filter(fun(R) -> maps:get(id, R) =:= <<"store_rule">> end, StoredRules),
    ?assertEqual(1, length(Found)).

%% ## Integration with metrics

test_alert_rule_integration_with_metrics(_Config) ->
    Rule = #alert_rule{
        id = <<"metrics_rule">>,
        metric = nats_disconnects,
        threshold = 1,
        window_ms = 1000,
        severity = critical
    },
    ok = router_alerts:add_rule(Rule),

    ok = router_alerts:record_metric(nats_disconnects, 2),
    {ok, _} = router_alerts:evaluate_all_rules(),
    Alerts = router_alerts:get_active_alerts(),
    ?assertMatch([_], Alerts),

    [Alert] = Alerts,
    ?assertEqual(critical, maps:get(severity, Alert)).

%% ## Quarantine example

test_quarantined_alert_rule(_Config) ->
    %% Intentionally slow/flaky check for quarantine mechanics validation.
    Rule = #alert_rule{
        id = <<"quarantine_rule">>,
        metric = publish_latency,
        threshold = 50,
        window_ms = 100,
        severity = warning
    },
    ok = router_alerts:add_rule(Rule),
    ok = router_alerts:record_metric(publish_latency, 60),
    {ok, _} = router_alerts:evaluate_all_rules(),
    Alerts = router_alerts:get_active_alerts(),
    ?assertMatch([_ | _], Alerts).

%% ## Circuit breaker alert conditions

test_circuit_breaker_alert_conditions(_Config) ->
    %% Placeholder: implement real cb-driven alert condition checks.
    ok.

%% Helper function to check if quarantine tests are enabled
is_quarantine_enabled() ->
    %% Can be controlled via environment variable or always return false
    os:getenv("ENABLE_QUARANTINE_TESTS", "false") =:= "true".
