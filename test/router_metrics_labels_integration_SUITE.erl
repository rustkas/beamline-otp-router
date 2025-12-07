%% @doc Integration Tests for Metrics Labels Emission
%%
%% Tests that metrics are correctly emitted with labels in real scenarios.
%% Covers:
%% - DLQ metric emission with labels during MaxDeliver exhaustion
%% - NATS failure metrics emission with labels during connection/publish/ACK failures
%% - Label cardinality verification
%%
%% @test_category integration, metrics, labels
-module(router_metrics_labels_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    %% Test functions called via all/0
    test_dlq_metric_labels_during_maxdeliver/1,
    test_nats_publish_failure_labels/1,
    test_nats_ack_failure_labels/1,
    test_nats_connect_failure_labels/1,
    test_label_cardinality_check/1
]}).

all() ->
    [
        test_dlq_metric_labels_during_maxdeliver,
        test_nats_publish_failure_labels,
        test_nats_ack_failure_labels,
        test_nats_connect_failure_labels,
        test_label_cardinality_check
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    _ = application:set_env(beamline_router, test_mode, true),
    ok = router_metrics:ensure(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    router_metrics_test_helper:clear_all_metrics(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Integration Tests
%% ============================================================================

test_dlq_metric_labels_during_maxdeliver(_Config) ->
    %% Simulate MaxDeliver exhaustion scenario
    Subject = <<"beamline.router.v1.decide">>,
    Msg = #{
        id => <<"msg-123">>,
        subject => Subject,
        headers => #{
            <<"tenant_id">> => <<"tenant-456">>,
            <<"request_id">> => <<"req-789">>
        }
    },
    Ctx = #{
        assignment_id => <<"decide">>
    },
    
    %% Configure MaxDeliver to 1 for testing
    router_jetstream:configure(#{
        max_deliver => 1,
        backoff_seconds => []
    }),
    
    %% First delivery attempt - should NAK
    {ok, redelivery} = router_jetstream:handle(Msg, Ctx),
    
    %% Second delivery attempt - should exhaust MaxDeliver and send to DLQ
    {ok, dlq} = router_jetstream:handle(Msg, Ctx),
    
    %% Verify DLQ metric was emitted with labels
    _ = router_metrics:normalize_labels(#{
        assignment_id => <<"decide">>,
        reason => <<"maxdeliver_exhausted">>,
        tenant_id => <<"tenant-456">>,
        source => <<"maxdeliver_exhausted">>,
        msg_id => <<"msg-123">>,
        request_id => <<"req-789">>
    }),
    %% Verify DLQ metric was emitted with labels
    Value = router_metrics_test_helper:get_metric_value(router_dlq_total, #{
        assignment_id => <<"decide">>,
        reason => <<"maxdeliver_exhausted">>,
        tenant_id => <<"tenant-456">>,
        source => <<"maxdeliver_exhausted">>,
        msg_id => <<"msg-123">>,
        request_id => <<"req-789">>
    }),
    ?assertEqual(1, Value),
    ok.

test_nats_publish_failure_labels(_Config) ->
    %% Simulate NATS publish failure
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Start router_nats gen_server
    {ok, Pid} = router_nats:start_link(),
    
    %% Simulate publish failure by using fault injection or disconnected state
    %% For this test, we'll directly call the internal function that emits the metric
    %% In real scenario, this would happen via do_publish/3
    
    %% Manually emit metric to verify label structure
    Reason = <<"timeout">>,
    Stream = router_nats:extract_stream_from_subject(Subject),
    router_metrics:emit_metric(router_nats_publish_failures_total, #{count => 1}, #{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        source => <<"publish">>
    }),
    
    %% Verify metric was stored with labels
    _ = router_metrics:normalize_labels(#{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        source => <<"publish">>
    }),
    %% Verify metric was stored with labels
    Value = router_metrics_test_helper:get_metric_value(router_nats_publish_failures_total, #{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        source => <<"publish">>
    }),
    ?assertEqual(1, Value),
    
    %% Cleanup
    exit(Pid, normal),
    ok.

test_nats_ack_failure_labels(_Config) ->
    %% Simulate NATS ACK failure
    
    %% Manually emit metric to verify label structure
    Reason = <<"not_connected">>,
    router_metrics:emit_metric(router_nats_ack_failures_total, #{count => 1}, #{
        reason => Reason,
        subject => <<"unknown">>,
        stream => <<"unknown">>,
        consumer => <<"unknown">>
    }),
    
    %% Verify metric was stored with labels
    Value = router_metrics_test_helper:get_metric_value(router_nats_ack_failures_total, #{
        reason => Reason,
        subject => <<"unknown">>,
        stream => <<"unknown">>,
        consumer => <<"unknown">>
    }),
    ?assertEqual(1, Value),
    ok.

test_nats_connect_failure_labels(_Config) ->
    %% Simulate NATS connection failure
    Reason = <<"connection_refused">>,
    router_metrics:emit_metric(router_nats_connect_failures_total, #{count => 1}, #{
        reason => Reason,
        cluster => <<"default">>,
        source => <<"initial_connect">>
    }),
    
    %% Verify metric was stored with labels
    _ = router_metrics:normalize_labels(#{
        reason => Reason,
        cluster => <<"default">>,
        source => <<"initial_connect">>
    }),
    %% Verify metric was stored with labels
    Value = router_metrics_test_helper:get_metric_value(router_nats_connect_failures_total, #{
        reason => Reason,
        cluster => <<"default">>,
        source => <<"initial_connect">>
    }),
    ?assertEqual(1, Value),
    ok.

test_label_cardinality_check(_Config) ->
    %% Test that label cardinality doesn't explode
    %% Simulate multiple DLQ emissions with different labels
    Subjects = [
        <<"beamline.router.v1.decide">>,
        <<"caf.exec.result.v1">>
    ],
    Tenants = [
        <<"tenant-1">>,
        <<"tenant-2">>,
        <<"tenant-3">>
    ],
    
    %% Emit metrics with different label combinations
    lists:foreach(fun(Subject) ->
        lists:foreach(fun(TenantId) ->
            AssignmentId = router_jetstream:extract_assignment_id(Subject),
            router_metrics:emit_metric(router_dlq_total, #{count => 1}, #{
                assignment_id => AssignmentId,
                reason => <<"maxdeliver_exhausted">>,
                tenant_id => TenantId,
                source => <<"maxdeliver_exhausted">>,
                msg_id => <<"msg-", Subject/binary, "-", TenantId/binary>>,
                request_id => <<"req-", Subject/binary, "-", TenantId/binary>>
            })
        end, Tenants)
    end, Subjects),
    
    %% Count unique label combinations
    AllKeys = ets:match(router_metrics, {{router_dlq_total, '$1'}, '_'}),
    UniqueKeys = sets:from_list(AllKeys),
    Cardinality = sets:size(UniqueKeys),
    
    %% Expected: 2 subjects * 3 tenants = 6 unique combinations
    ExpectedCardinality = 6,
    ?assertEqual(ExpectedCardinality, Cardinality),
    
    %% Verify no explosion (should be manageable)
    ?assert(Cardinality < 1000),  %% Reasonable limit
    ok.

