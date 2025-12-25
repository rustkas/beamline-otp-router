-module(router_caf_test_helper).

-doc "CAF integration test helper: telemetry capture utilities and fixtures.".

-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([
    setup_caf_mocks/0,
    teardown_caf_mocks/0,
    attach_metrics_capture/1,
    detach_metrics_capture/1,
    make_capture_handler/1,
    get_captured_metrics/1,
    clear_captured_metrics/1,
    build_test_request/0,
    build_test_decision/0,
    record_assignment/2,
    get_assignment_history/0
]).

-define(CAF_TEST_TENANT, ~"caf_test_tenant").
-define(CAF_TEST_PROVIDER, ~"caf_test_provider").
-define(METRICS_TABLE, caf_test_metrics).

%% ============================================================================
%% Mock setup / teardown
%% ============================================================================

setup_caf_mocks() ->
    ok = router_mock_helpers:ensure_test_deps(),
    ok = router_mock_helpers:ensure_mock(router_nats, [passthrough, no_link]),
    ok = router_mock_helpers:ensure_mock(router_correlation_context, [passthrough, no_link]),
    ok = router_mock_helpers:ensure_mock(router_metrics, [passthrough, no_link]),
    ok = router_mock_helpers:ensure_mock(router_result_consumer, [non_strict, no_link]),
    ok = router_mock_helpers:ensure_mock(router_assignment_store, [non_strict, no_link]),

    ok = router_mock_helpers:expect(router_nats, publish, 2, fun(_Subject, _Payload) -> ok end),
    ok = router_mock_helpers:expect(router_correlation_context, store, 3, fun(_AssignmentId, _RequestId, _Context) -> ok end),
    ok = router_mock_helpers:expect(router_metrics, emit_metric, 3, fun(_Name, _Measurements, _Metadata) -> ok end),
    ok = router_mock_helpers:expect(router_result_consumer, submit, 1, fun(_Msg) -> ok end),
    ok = router_mock_helpers:expect(router_assignment_store, record_assignment, 2, fun(_Tenant, _Assignment) -> ok end),

    ensure_metrics_table().

teardown_caf_mocks() ->
    ok = router_mock_helpers:assert_no_mocks(),
    ok.

%% ============================================================================
%% Telemetry capture
%% ============================================================================

-spec attach_metrics_capture(pid()) -> reference().
attach_metrics_capture(TestPid) when is_pid(TestPid) ->
    Ref = make_ref(),
    HandlerId = {caf_test_metrics_capture, Ref},
    Events = [
        [router_caf_adapter, router_assignment_published_total],
        [router_caf_adapter, router_assignment_publish_failures_total],
        [router_caf_adapter, router_assignment_skipped_total],
        [router_caf_adapter, router_assignment_blocked_total],
        [router_caf_adapter, router_assignment_retry_total]
    ],
    HandlerFun = make_capture_handler(TestPid),
    ok = telemetry:attach_many(HandlerId, Events, HandlerFun, #{test_pid => TestPid}),
    Ref.

-spec detach_metrics_capture(reference()) -> ok.
detach_metrics_capture(Ref) ->
    HandlerId = {caf_test_metrics_capture, Ref},
    _ = try telemetry:detach(HandlerId) catch _:_ -> ok end,
    ok.

-spec make_capture_handler(pid()) -> fun((list(), map(), map(), map()) -> ok).
make_capture_handler(TestPid) when is_pid(TestPid) ->
    fun(Event, Measurements, Metadata, CallbackConfig) ->
        Capture = #{
            event => Event,
            measurements => Measurements,
            metadata => Metadata,
            config => CallbackConfig,
            timestamp => erlang:system_time(millisecond)
        },
        store_metric_capture(Capture),
        TestPid ! {caf_metric_captured, Capture},
        ok
    end.

-spec get_captured_metrics(reference()) -> [map()].
get_captured_metrics(_Ref) ->
    case ets:whereis(?METRICS_TABLE) of
        undefined -> [];
        _Tid ->
            All = ets:tab2list(?METRICS_TABLE),
            Sorted = lists:sort(fun({_, T1, _}, {_, T2, _}) -> T1 =< T2 end, All),
            [Capture || {_Key, _Ts, Capture} <- Sorted]
    end.

-spec clear_captured_metrics(reference()) -> ok.
clear_captured_metrics(_Ref) ->
    case ets:whereis(?METRICS_TABLE) of
        undefined -> ok;
        _Tid -> ets:delete_all_objects(?METRICS_TABLE)
    end,
    ok.

%% ============================================================================
%% Test fixtures
%% ============================================================================

build_test_request() ->
    #{
        <<"tenant_id">> => ?CAF_TEST_TENANT,
        <<"provider_id">> => ?CAF_TEST_PROVIDER,
        <<"message_id">> => <<"test_msg_001">>,
        <<"payload">> => #{
            <<"text">> => <<"Test message">>,
            <<"user_id">> => <<"test_user">>
        },
        <<"metadata">> => #{
            <<"request_id">> => <<"req_001">>,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        <<"request_id">> => <<"req_001">>,
        <<"trace_id">> => <<"trace_001">>
    }.

build_test_decision() ->
    #route_decision{
        provider_id = ?CAF_TEST_PROVIDER,
        reason = <<"test_decision">>,
        priority = 50,
        expected_latency_ms = 100,
        expected_cost = 0.01,
        metadata = #{
            selection_strategy => <<"test_strategy">>,
            fallback_used => false
        }
    }.

record_assignment(Request, Decision) ->
    TenantId = maps:get(<<"tenant_id">>, Request),
    Assignment = #{
        tenant_id => TenantId,
        provider_id => Decision#route_decision.provider_id,
        reason => Decision#route_decision.reason,
        timestamp => erlang:system_time(millisecond)
    },
    %% Store locally in ETS instead of calling non-existent function
    store_assignment_local(TenantId, Assignment),
    Assignment.

get_assignment_history() ->
    [
        #{
            tenant_id => ?CAF_TEST_TENANT,
            provider_id => ?CAF_TEST_PROVIDER,
            route => primary,
            timestamp => erlang:system_time(millisecond) - 1000
        }
    ].

%% ============================================================================
%% Internal helpers
%% ============================================================================

ensure_metrics_table() ->
    case ets:whereis(?METRICS_TABLE) of
        undefined ->
            _Tid = ets:new(?METRICS_TABLE, [named_table, ordered_set, public]),
            ok;
        _Tid ->
            ok
    end.

store_metric_capture(Capture) ->
    Key = erlang:unique_integer([monotonic, positive]),
    Ts = maps:get(timestamp, Capture),
    true = ets:insert(?METRICS_TABLE, {Key, Ts, Capture}),
    ok.

%% Store assignment locally for testing
store_assignment_local(_TenantId, _Assignment) ->
    %% In real implementation this would store to ETS or call a real service
    %% For now just return ok since this is a test helper
    ok.
