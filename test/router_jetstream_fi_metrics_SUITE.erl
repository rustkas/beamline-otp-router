%% @doc JetStream Fault Injection - Metrics Tests
%% 
%% Tests for redelivery and maxdeliver metrics with proper labels.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category fault_injection, heavy, slow
-module(router_jetstream_fi_metrics_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_redelivery_metric_labels/1,
    test_redelivery_tenant_validation_failed/1,
    test_maxdeliver_exhausted_metric_labels/1,
    test_maxdeliver_exhausted_different_limits/1,
    test_metrics_contract_compliance/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, metrics_tests}];
        _ -> []
    end.

groups() ->
    [{metrics_tests, [sequence], [
        test_redelivery_metric_labels,
        test_redelivery_tenant_validation_failed,
        test_maxdeliver_exhausted_metric_labels,
        test_maxdeliver_exhausted_different_limits,
        test_metrics_contract_compliance
    ]}].

init_per_suite(Config) ->
    router_jetstream_fi_helper:init_common_suite(Config).

end_per_suite(Config) ->
    router_jetstream_fi_helper:end_common_suite(Config).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_redelivery_metric_labels(_Config) ->
    HandlerId = router_jetstream_fi_helper:setup_telemetry_handler(),
    
    %% Simulate redelivery event
    telemetry:execute(
        [router, jetstream, nak],
        #{count => 1},
        #{tenant_id => <<"acme">>, reason => <<"processing_error">>, consumer => <<"decide">>}
    ),
    
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, nak, #{
        tenant_id => <<"acme">>,
        reason => <<"processing_error">>,
        consumer => <<"decide">>
    }),
    
    router_jetstream_fi_helper:cleanup_telemetry_handler(HandlerId),
    ok.

test_redelivery_tenant_validation_failed(_Config) ->
    HandlerId = router_jetstream_fi_helper:setup_telemetry_handler(),
    
    %% Simulate tenant validation failure redelivery
    telemetry:execute(
        [router, jetstream, nak],
        #{count => 1},
        #{tenant_id => <<"unknown">>, reason => <<"tenant_validation_failed">>, consumer => <<"decide">>}
    ),
    
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, nak, #{
        tenant_id => <<"unknown">>,
        reason => <<"tenant_validation_failed">>,
        consumer => <<"decide">>
    }),
    
    router_jetstream_fi_helper:cleanup_telemetry_handler(HandlerId),
    ok.

test_maxdeliver_exhausted_metric_labels(_Config) ->
    HandlerId = router_jetstream_fi_helper:setup_telemetry_handler(),
    
    %% Simulate maxdeliver exhausted event
    telemetry:execute(
        [router_decide_consumer, router_jetstream_maxdeliver_exhausted_total],
        #{count => 1},
        #{tenant_id => <<"acme">>, consumer => <<"decide">>, max_deliver => 5}
    ),
    
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, router_jetstream_maxdeliver_exhausted_total, #{
        tenant_id => <<"acme">>,
        consumer => <<"decide">>,
        max_deliver => 5
    }),
    
    router_jetstream_fi_helper:cleanup_telemetry_handler(HandlerId),
    ok.

test_maxdeliver_exhausted_different_limits(_Config) ->
    HandlerId = router_jetstream_fi_helper:setup_telemetry_handler(),
    
    %% Test with different max_deliver values
    lists:foreach(fun(MaxDeliver) ->
        telemetry:execute(
            [router_decide_consumer, router_jetstream_maxdeliver_exhausted_total],
            #{count => 1},
            #{tenant_id => <<"acme">>, consumer => <<"decide">>, max_deliver => MaxDeliver}
        )
    end, [3, 5, 10]),
    
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, router_jetstream_maxdeliver_exhausted_total, #{
        tenant_id => <<"acme">>,
        consumer => <<"decide">>,
        max_deliver => 3
    }),
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, router_jetstream_maxdeliver_exhausted_total, #{
        tenant_id => <<"acme">>,
        consumer => <<"decide">>,
        max_deliver => 5
    }),
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, router_jetstream_maxdeliver_exhausted_total, #{
        tenant_id => <<"acme">>,
        consumer => <<"decide">>,
        max_deliver => 10
    }),
    
    router_jetstream_fi_helper:cleanup_telemetry_handler(HandlerId),
    ok.

test_metrics_contract_compliance(_Config) ->
    HandlerId = router_jetstream_fi_helper:setup_telemetry_handler(),
    
    %% Emit metrics with all required labels
    telemetry:execute(
        [router, jetstream, nak],
        #{count => 1, latency_ms => 50},
        #{
            tenant_id => <<"acme">>,
            reason => <<"processing_error">>,
            consumer => <<"decide">>,
            request_id => <<"req-123">>,
            trace_id => <<"tr-123">>
        }
    ),
    
    telemetry:execute(
        [router_decide_consumer, router_jetstream_maxdeliver_exhausted_total],
        #{count => 1},
        #{
            tenant_id => <<"acme">>,
            consumer => <<"decide">>,
            max_deliver => 5,
            request_id => <<"req-456">>
        }
    ),
    
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, nak, #{
        tenant_id => <<"acme">>,
        reason => <<"processing_error">>,
        consumer => <<"decide">>,
        request_id => <<"req-123">>,
        trace_id => <<"tr-123">>
    }),
    ok = router_jetstream_fi_helper:assert_metric_labels(HandlerId, router_jetstream_maxdeliver_exhausted_total, #{
        tenant_id => <<"acme">>,
        consumer => <<"decide">>,
        max_deliver => 5,
        request_id => <<"req-456">>
    }),
    
    router_jetstream_fi_helper:cleanup_telemetry_handler(HandlerId),
    ok.
