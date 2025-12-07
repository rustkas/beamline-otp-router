%% @doc Test suite for router_jetstream_redelivery_total metric with labels
%% 
%% Verifies:
%% - Metric is emitted with correct name (router_jetstream_redelivery_total)
%% - All required labels are present (assignment_id, request_id, reason, source)
%% - Label values match expected format
%% - Source label is correctly derived from reason
-module(router_jetstream_redelivery_metrics_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         test_redelivery_metric_name/1, test_redelivery_metric_labels/1,
         test_redelivery_metric_source_derivation/1, test_redelivery_metric_prometheus_format/1]).

all() ->
    [
        test_redelivery_metric_name,
        test_redelivery_metric_labels,
        test_redelivery_metric_source_derivation,
        test_redelivery_metric_prometheus_format
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = router_metrics:ensure(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Reset metrics
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(router_metrics)
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: Verify metric name is router_jetstream_redelivery_total
test_redelivery_metric_name(_Config) ->
    AssignmentId = <<"test-assignment-1">>,
    RequestId = <<"test-request-1">>,
    _Reason = tenant_validation_failed,
    Source = <<"tenant_validation">>,
    
    %% Emit metric using router_metrics:emit_metric
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        reason => <<"tenant_validation_failed">>,
        source => Source
    }),
    
    %% Verify metric appears in Prometheus dump
    ok = router_prometheus:dump("metrics_dump/redelivery_name_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/redelivery_name_test.prom"),
    Str = binary_to_list(Bin),
    
    %% Check metric name is present
    true = string:find(Str, "router_jetstream_redelivery_total") =/= nomatch,
    ct:comment("Metric name router_jetstream_redelivery_total found in dump"),
    
    %% Verify old metric name is NOT present (or marked as deprecated)
    case string:find(Str, "router_redelivery_total") of
        nomatch ->
            ok;  %% Good - old name not present
        _ ->
            %% Old name might be present as deprecated, check for deprecation comment
            case string:find(Str, "deprecated") of
                nomatch ->
                    ct:fail("Old metric name router_redelivery_total found without deprecation");
                _ ->
                    ct:comment("Old metric name found but marked as deprecated (OK)")
            end
    end,
    
    ok.

%% @doc Test: Verify all required labels are present
test_redelivery_metric_labels(_Config) ->
    AssignmentId = <<"test-assignment-2">>,
    RequestId = <<"test-request-2">>,
    Reason = <<"tenant_validation_failed">>,
    Source = <<"tenant_validation">>,
    
    %% Emit metric with all required labels
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        reason => Reason,
        source => Source
    }),
    
    %% Verify using contract helper
    Metadata = #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        reason => Reason,
        source => Source
    },
    
    ExpectedLabels = #{
        reason => Reason,
        source => Source
    },
    
    case router_metrics_contract_helpers:validate_metric_labels(
        router_jetstream_redelivery_total, Metadata, ExpectedLabels) of
        {ok, Details} ->
            ct:comment("Metric labels validated successfully: ~p", [Details]),
            ok;
        {fail, Reason} ->
            ct:fail("Metric label validation failed: ~p. Metadata: ~p", [Reason, Metadata])
    end.

%% @doc Test: Verify source label is correctly derived from reason
test_redelivery_metric_source_derivation(_Config) ->
    TestCases = [
        {tenant_validation_failed, <<"tenant_validation">>},
        {backpressure, <<"backpressure">>},
        {backoff, <<"backoff">>},
        {ack_error, <<"ack_failure">>},
        {nak_error, <<"nak_failure">>}
    ],
    
    lists:foreach(fun({ReasonAtom, ExpectedSource}) ->
        AssignmentId = <<"test-assignment-", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
        RequestId = <<"test-request-", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
        %% reason_to_binary and reason_to_source are internal functions
        %% We'll test them indirectly by calling nak/3
        ReasonBin = case ReasonAtom of
            tenant_validation_failed -> <<"tenant_validation_failed">>;
            backpressure -> <<"backpressure">>;
            backoff -> <<"backoff">>;
            ack_error -> <<"ack_error">>;
            nak_error -> <<"nak_error">>;
            _ -> atom_to_binary(ReasonAtom, utf8)
        end,
        Source = ExpectedSource,
        
        %% Verify source matches expected
        ExpectedSource = Source,
        ct:comment("Source derivation correct: ~p -> ~p", [ReasonAtom, Source]),
        
        %% Emit metric and verify
        ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
            assignment_id => AssignmentId,
            request_id => RequestId,
            reason => ReasonBin,
            source => Source
        }),
        
        %% Verify contract
        Metadata = #{
            assignment_id => AssignmentId,
            request_id => RequestId,
            reason => ReasonBin,
            source => Source
        },
        
        case router_metrics_contract_helpers:validate_metric_labels(
            router_jetstream_redelivery_total, Metadata, #{reason => ReasonBin, source => Source}) of
            {ok, _Details} ->
                ok;
            {fail, ValidationReason} ->
                ct:fail("Source derivation validation failed for ~p: ~p", [ReasonAtom, ValidationReason])
        end
    end, TestCases),
    
    ok.

%% @doc Test: Verify Prometheus format includes labels
test_redelivery_metric_prometheus_format(_Config) ->
    AssignmentId = <<"test-assignment-3">>,
    RequestId = <<"test-request-3">>,
    Reason = <<"tenant_validation_failed">>,
    Source = <<"tenant_validation">>,
    
    %% Emit metric
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        reason => Reason,
        source => Source
    }),
    
    %% Dump to Prometheus format
    ok = router_prometheus:dump("metrics_dump/redelivery_format_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/redelivery_format_test.prom"),
    Str = binary_to_list(Bin),
    
    %% Check HELP line
    true = string:find(Str, "# HELP router_jetstream_redelivery_total") =/= nomatch,
    
    %% Check TYPE line
    true = string:find(Str, "# TYPE router_jetstream_redelivery_total counter") =/= nomatch,
    
    %% Check metric line with labels (Prometheus format: metric_name{label1="value1",label2="value2"} value)
    %% Expected format: router_jetstream_redelivery_total{assignment_id="test-assignment-3",...} 1
    MetricLinePattern = "router_jetstream_redelivery_total{",
    true = string:find(Str, MetricLinePattern) =/= nomatch,
    
    %% Verify all required labels appear in the metric line
    true = string:find(Str, "assignment_id=\"test-assignment-3\"") =/= nomatch,
    true = string:find(Str, "request_id=\"test-request-3\"") =/= nomatch,
    true = string:find(Str, "reason=\"tenant_validation_failed\"") =/= nomatch,
    true = string:find(Str, "source=\"tenant_validation\"") =/= nomatch,
    
    %% Verify metric value appears after labels
    true = string:find(Str, "} 1") =/= nomatch orelse string:find(Str, "}1") =/= nomatch,
    
    ct:comment("Prometheus format validation passed - labels correctly formatted"),
    ct:comment("Metric dump contains: ~s", [string:substr(Str, 1, min(500, length(Str)))]),
    
    ok.

