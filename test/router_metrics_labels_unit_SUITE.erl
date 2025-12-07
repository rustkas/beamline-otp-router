%% @doc Unit Tests for Metrics Labels Helper Functions
%%
%% Tests for label extraction and normalization functions used in metrics emission.
%% Covers:
%% - router_jetstream.erl helper functions (extract_assignment_id, extract_tenant_id, extract_request_id)
%% - router_nats.erl helper functions (error_to_reason, extract_stream_from_subject)
%%
%% @test_category unit, metrics, labels
-module(router_metrics_labels_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [all/0, init_per_suite/1, end_per_suite/1,
                                    init_per_testcase/2, end_per_testcase/2]}).

all() ->
    [
        %% router_jetstream helper functions
        test_extract_assignment_id_decide,
        test_extract_assignment_id_results,
        test_extract_assignment_id_unknown,
        test_extract_tenant_id_from_headers,
        test_extract_tenant_id_from_payload,
        test_extract_tenant_id_missing,
        test_extract_request_id_from_headers,
        test_extract_request_id_from_payload,
        test_extract_request_id_missing,
        
        %% router_nats helper functions
        test_error_to_reason_atom,
        test_error_to_reason_binary,
        test_error_to_reason_timeout,
        test_error_to_reason_connection_closed,
        test_error_to_reason_unknown,
        test_extract_stream_from_subject_decide,
        test_extract_stream_from_subject_results,
        test_extract_stream_from_subject_unknown,
        
        %% Integration: metric emission with labels
        test_dlq_metric_emission_with_labels,
        test_nats_publish_failure_emission_with_labels,
        test_nats_ack_failure_emission_with_labels
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    _ = application:set_env(beamline_router, test_mode, true),
    ok = router_metrics:ensure(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear metrics ETS table before each test
    router_metrics_test_helper:clear_all_metrics(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for router_jetstream helper functions
%% ============================================================================

test_extract_assignment_id_decide(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    %% Call internal function via code:load/1 or use direct call
    %% Since function is exported, we can call it directly
    AssignmentId = router_jetstream:extract_assignment_id(Subject),
    ?assertEqual(<<"decide">>, AssignmentId),
    ok.

test_extract_assignment_id_results(_Config) ->
    Subject = <<"caf.exec.result.v1">>,
    AssignmentId = router_jetstream:extract_assignment_id(Subject),
    %% For "caf.exec.result.v1", pattern is [caf, exec, result, v1], so Assignment should be "result"
    ?assertEqual(<<"result">>, AssignmentId),
    ok.

test_extract_assignment_id_unknown(_Config) ->
    Subject = <<"unknown.subject">>,
    AssignmentId = router_jetstream:extract_assignment_id(Subject),
    ?assertEqual(<<"unknown">>, AssignmentId),
    ok.

test_extract_tenant_id_from_headers(_Config) ->
    Msg = #{
        headers => #{
            <<"tenant_id">> => <<"tenant-123">>
        }
    },
    TenantId = router_jetstream:extract_tenant_id(Msg),
    ?assertEqual(<<"tenant-123">>, TenantId),
    ok.

test_extract_tenant_id_from_payload(_Config) ->
    Msg = #{
        payload => #{
            <<"tenant_id">> => <<"tenant-456">>
        }
    },
    TenantId = router_jetstream:extract_tenant_id(Msg),
    ?assertEqual(<<"tenant-456">>, TenantId),
    ok.

test_extract_tenant_id_missing(_Config) ->
    Msg = #{},
    TenantId = router_jetstream:extract_tenant_id(Msg),
    ?assertEqual(<<"unknown">>, TenantId),
    ok.

test_extract_request_id_from_headers(_Config) ->
    Msg = #{
        headers => #{
            <<"request_id">> => <<"req-789">>
        }
    },
    RequestId = router_jetstream:extract_request_id(Msg),
    ?assertEqual(<<"req-789">>, RequestId),
    ok.

test_extract_request_id_from_payload(_Config) ->
    Msg = #{
        payload => #{
            <<"request_id">> => <<"req-abc">>
        }
    },
    RequestId = router_jetstream:extract_request_id(Msg),
    ?assertEqual(<<"req-abc">>, RequestId),
    ok.

test_extract_request_id_missing(_Config) ->
    Msg = #{},
    RequestId = router_jetstream:extract_request_id(Msg),
    ?assertEqual(<<"unknown">>, RequestId),
    ok.

%% ============================================================================
%% Tests for router_nats helper functions
%% ============================================================================

test_error_to_reason_atom(_Config) ->
    Reason = router_nats:error_to_reason(timeout),
    ?assertEqual(<<"timeout">>, Reason),
    ok.

test_error_to_reason_binary(_Config) ->
    Reason = router_nats:error_to_reason(<<"custom_error">>),
    ?assertEqual(<<"custom_error">>, Reason),
    ok.

test_error_to_reason_timeout(_Config) ->
    Reason = router_nats:error_to_reason(timeout),
    ?assertEqual(<<"timeout">>, Reason),
    ok.

test_error_to_reason_connection_closed(_Config) ->
    Reason = router_nats:error_to_reason(connection_closed),
    ?assertEqual(<<"connection_closed">>, Reason),
    ok.

test_error_to_reason_unknown(_Config) ->
    Reason = router_nats:error_to_reason({complex, error, term}),
    %% Should convert to binary representation
    ?assert(is_binary(Reason)),
    ok.

test_extract_stream_from_subject_decide(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    Stream = router_nats:extract_stream_from_subject(Subject),
    ?assertEqual(<<"router-stream">>, Stream),
    ok.

test_extract_stream_from_subject_results(_Config) ->
    Subject = <<"caf.exec.result.v1">>,
    Stream = router_nats:extract_stream_from_subject(Subject),
    ?assertEqual(<<"caf-stream">>, Stream),
    ok.

test_extract_stream_from_subject_unknown(_Config) ->
    Subject = <<"unknown.subject">>,
    Stream = router_nats:extract_stream_from_subject(Subject),
    ?assertEqual(<<"unknown">>, Stream),
    ok.

%% ============================================================================
%% Integration tests: metric emission with labels
%% ============================================================================

test_dlq_metric_emission_with_labels(_Config) ->
    %% Simulate DLQ metric emission with labels
    AssignmentId = <<"decide">>,
    Reason = <<"maxdeliver_exhausted">>,
    TenantId = <<"tenant-123">>,
    Source = <<"maxdeliver_exhausted">>,
    MsgId = <<"msg-abc">>,
    RequestId = <<"req-xyz">>,
    
    router_metrics:emit_metric(router_dlq_total, #{count => 1}, #{
        assignment_id => AssignmentId,
        reason => Reason,
        tenant_id => TenantId,
        source => Source,
        msg_id => MsgId,
        request_id => RequestId
    }),
    
    %% Verify metric was stored with labels
    _ = router_metrics:normalize_labels(#{
        assignment_id => AssignmentId,
        reason => Reason,
        tenant_id => TenantId,
        source => Source,
        msg_id => MsgId,
        request_id => RequestId
    }),
    %% Verify metric was stored with labels
    Value = router_metrics_test_helper:get_metric_value(router_dlq_total, #{
        assignment_id => AssignmentId,
        reason => Reason,
        tenant_id => TenantId,
        source => Source,
        msg_id => MsgId,
        request_id => RequestId
    }),
    ?assertEqual(1, Value),
    ok.

test_nats_publish_failure_emission_with_labels(_Config) ->
    %% Simulate NATS publish failure metric emission with labels
    Reason = <<"timeout">>,
    Subject = <<"beamline.router.v1.decide">>,
    Stream = <<"router-stream">>,
    Source = <<"publish">>,
    
    router_metrics:emit_metric(router_nats_publish_failures_total, #{count => 1}, #{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        source => Source
    }),
    
    %% Verify metric was stored with labels
    _ = router_metrics:normalize_labels(#{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        source => Source
    }),
    %% Verify metric was stored with labels
    Value = router_metrics_test_helper:get_metric_value(router_nats_publish_failures_total, #{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        source => Source
    }),
    ?assertEqual(1, Value),
    ok.

test_nats_ack_failure_emission_with_labels(_Config) ->
    %% Simulate NATS ACK failure metric emission with labels
    Reason = <<"not_connected">>,
    Subject = <<"beamline.router.v1.decide">>,
    Stream = <<"router-stream">>,
    Consumer = <<"router-decide-consumer">>,
    
    router_metrics:emit_metric(router_nats_ack_failures_total, #{count => 1}, #{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        consumer => Consumer
    }),
    
    %% Verify metric was stored with labels
    _ = router_metrics:normalize_labels(#{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        consumer => Consumer
    }),
    %% Verify metric was stored with labels
    Value = router_metrics_test_helper:get_metric_value(router_nats_ack_failures_total, #{
        reason => Reason,
        subject => Subject,
        stream => Stream,
        consumer => Consumer
    }),
    ?assertEqual(1, Value),
    ok.

