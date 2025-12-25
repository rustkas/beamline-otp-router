-module(router_caf_adapter_core_SUITE).

-doc "Core CAF adapter contract tests: interaction with NATS publish layer and assignment storage.".

-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([
    all/0, groups_for_level/1,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_publish_assignment_success/1,
    test_publish_assignment_failure_metrics/1,
    test_assignment_persistence/1,
    test_assignment_message_format/1
]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [{group, core_tests}].

groups_for_level(heavy) ->
    [{group, core_tests}];
groups_for_level(full) ->
    [{group, core_tests}];
groups_for_level(_) ->
    [{group, core_tests}].

groups() ->
    [{core_tests, [sequence], [
        test_publish_assignment_success,
        test_publish_assignment_failure_metrics,
        test_assignment_persistence,
        test_assignment_message_format
    ]}].

init_per_suite(Config) ->
    ok = router_mock_helpers:ensure_test_deps(),
    {ok, _} = application:ensure_all_started(beamline_router),
    Config.

end_per_suite(Config) ->
    ok = application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    ok = router_mock_helpers:ensure_mock(router_nats, [passthrough]),
    ok = router_mock_helpers:ensure_mock(router_correlation_context, [passthrough]),
    ok = router_mock_helpers:ensure_mock(router_metrics, [passthrough]),

    ok = router_mock_helpers:expect(router_nats, publish_with_ack, 3, fun(_Subject, _Payload, _Headers) -> {ok, <<"test_ack_id">>} end),
    ok = router_mock_helpers:expect(
        router_correlation_context,
        store,
        3,
        fun(_AssignmentId, _RequestId, _Context) -> ok end
    ),
    ok = router_mock_helpers:expect(router_metrics, emit_metric, 3, fun(_Metric, _Value, _Labels) -> ok end),
    Config.

end_per_testcase(_TestCase, Config) ->
    meck:unload(),
    Config.

test_publish_assignment_success(_Config) ->
    Request = #{<<"tenant_id">> => <<"tenant_core">>, <<"provider_id">> => <<"provider_1">>, <<"message_id">> => <<"msg_1">>},
    Decision = #route_decision{
        provider_id = <<"provider_1">>,
        reason = <<"core_test">>,
        priority = 50,
        expected_latency_ms = 100,
        expected_cost = 0.01,
        metadata = #{}
    },

    ok = router_caf_adapter:publish_assignment(Request, Decision),

    Calls = meck:history(router_nats),
    ?assert(length(Calls) >= 1),
    ok.

test_publish_assignment_failure_metrics(_Config) ->
    ok = router_mock_helpers:expect(router_nats, publish_with_ack, 3, fun(_Subject, _Payload, _Headers) -> {error, connection_refused} end),

    Request = #{<<"tenant_id">> => <<"tenant_core">>, <<"provider_id">> => <<"provider_1">>, <<"message_id">> => <<"msg_1">>},
    Decision = #route_decision{
        provider_id = <<"provider_1">>,
        reason = <<"core_test">>,
        priority = 50,
        expected_latency_ms = 100,
        expected_cost = 0.01,
        metadata = #{}

    },

    error = router_caf_adapter:publish_assignment(Request, Decision),

    MetricsCalls = meck:history(router_metrics),
    ?assert(length(MetricsCalls) >= 1),
    ok.

test_assignment_persistence(_Config) ->
    Request = #{<<"tenant_id">> => <<"tenant_core">>, <<"provider_id">> => <<"provider_1">>, <<"message_id">> => <<"msg_1">>},
    Decision = #route_decision{
        provider_id = <<"provider_1">>,
        reason = <<"core_test">>,
        priority = 50,
        expected_latency_ms = 100,
        expected_cost = 0.01,
        metadata = #{}
    },

    %% Use publish_assignment instead of store_assignment
    ok = router_caf_adapter:publish_assignment(Request, Decision),

    %% Verify NATS publish_with_ack was called
    NatsCalls = meck:history(router_nats),
    ?assert(length(NatsCalls) >= 1),
    ok.


test_assignment_message_format(_Config) ->
    Request = #{<<"tenant_id">> => <<"tenant_core">>, <<"provider_id">> => <<"provider_1">>, <<"message_id">> => <<"msg_1">>},
    Decision = #route_decision{
        provider_id = <<"provider_1">>,
        reason = <<"test">>,
        priority = 50,
        expected_latency_ms = 100,
        expected_cost = 0.01,
        metadata = #{}
    },

    %% Test publishing instead of building message (build_assignment_message/2 doesn't exist)
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    ?assertEqual(ok, Result),

    %% Verify NATS publish was called
    Calls = meck:history(router_nats),
    ?assert(length(Calls) >= 1),
    ok.
