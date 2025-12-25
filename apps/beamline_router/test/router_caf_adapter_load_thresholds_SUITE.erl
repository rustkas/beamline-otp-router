-module(router_caf_adapter_load_thresholds_SUITE).

-doc "CAF adapter load tests: threshold enforcement and metrics under load.".



-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([test_caf_timeout_triggers_retry/1,
         test_caf_retry_exhaustion_emits_metrics/1,
         test_caf_error_response_reflected_in_metrics/1,
         test_caf_degraded_mode_metric_emitted/1]).

all() ->
    Level =
        case os:getenv("ROUTER_TEST_LEVEL") of
            "heavy" ->
                heavy;
            "full" ->
                full;
            _ ->
                fast
        end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, load_threshold_tests}];
groups_for_level(full) ->
    [{group, load_threshold_tests}];
groups_for_level(_) -> %% fast - exclude slow tests
    [].

groups() ->
    [{load_threshold_tests,
      [sequence],
      [test_caf_timeout_triggers_retry,
       test_caf_retry_exhaustion_emits_metrics,
       test_caf_error_response_reflected_in_metrics,
       test_caf_degraded_mode_metric_emitted]}].

init_per_suite(Config) ->
    ct:pal("### init_per_suite: ensure load store and setup", []),

    case router_caf_adapter_load_store:ensure() of
        ok ->
            ok;
        {error, EnsureError} ->
            ct:fail("init_per_suite: failed to ensure load store: ~p", [EnsureError])
    end,

    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_policy_store, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    ct:pal("### end_per_suite: cleaning up", []),

    application:stop(beamline_router),
    router_mock_helpers:unload(router_nats),

    _ = router_caf_adapter_load_store:reset(),

    Config.

init_per_testcase(_TestCase, Config) ->
    %% Reset store before each test
    case router_caf_adapter_load_store:reset() of
        ok ->
            ok;
        {error, ResetError} ->
            ct:fail("init_per_testcase: failed to reset load store: ~p", [ResetError])
    end,

    %% Ensure mock expectations do not leak between test cases.
    ok = router_mock_helpers:setup_router_nats_mock(),
    router_mock_helpers:reset(router_nats),

    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% Test: CAF timeout triggers retry
%% Verifies Router retries assignment publication when CAF/NATS responds with timeout
test_caf_timeout_triggers_retry(_Config) ->
    %% Define handler IDs before try block to make them safe in after clause
    HandlerId = {?MODULE, test_timeout_retry},
    PublishedHandlerId = {?MODULE, test_timeout_published},
    try
            Request =
                #{~"version" => ~"1",
                  ~"request_id" => ~"req-timeout-test",
                  ~"trace_id" => ~"tr-timeout-test",
                  ~"tenant_id" => ~"test-tenant",
                  ~"task" =>
                      #{~"type" => ~"text.generate", ~"payload_ref" => ~"s3://bucket/key"},
                  ~"push_assignment" => true},

            Decision =
                #route_decision{provider_id = ~"openai",
                                reason = ~"weighted",
                                priority = 50,
                                expected_latency_ms = 850,
                                expected_cost = 0.012,
                                metadata = #{}},

            %% Track retry metrics via helper
            telemetry:attach(HandlerId,
                             [router_caf_adapter, router_assignment_retry_total],
                             fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
                                _ = router_caf_adapter_load_store:inc_retry_count(),
                                _ = router_caf_adapter_load_store:record_retry(Metadata)
                             end,
                             #{}),

            %% Track published metrics via helper
            telemetry:attach(PublishedHandlerId,
                             [router_caf_adapter, router_assignment_published_total],
                             fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
                                _ = router_caf_adapter_load_store:record_published(Metadata)
                             end,
                             #{}),

            %% Mock NATS: First publish_with_ack fails with timeout, second succeeds
            meck:expect(router_nats,
                        publish_with_ack,
                        fun(_Subject, _Json, _Headers) ->
                           {ok, Count} = router_caf_adapter_load_store:inc_publish_call_count(),
                           case Count of
                               1 ->
                                   %% First call: timeout (simulating slow/overloaded CAF)
                                   %% router_nats returns {error, Reason} where Reason can be {timeout, _} or atom
                                   {error, timeout};
                               _ ->
                                   %% Subsequent calls: succeed
                                   {ok,
                                    <<"pub-ack-",
                                      (integer_to_binary(erlang:unique_integer([positive])))/binary>>}
                           end
                        end),

            %% Set retry configuration for testing
            ok = application:set_env(beamline_router, caf_max_retries, 3),
            ok = application:set_env(beamline_router, caf_retry_base_ms, 50),  %% Fast for testing

            %% Publish assignment
            Result = router_caf_adapter:publish_assignment(Request, Decision),

            %% Allow processing (bounded wait for retry and final publish)
            test_helpers:wait_for_condition(fun() ->
                                               case router_caf_adapter_load_store:get_publish_call_count()
                                               of
                                                   {ok, C} -> C >= 2;
                                                   _ -> false
                                               end
                                            end,
                                            5000),

            %% Verify result is ok (retry succeeded)
            case Result of
                ok ->
                    ok;
                error ->
                    ct:fail("Assignment publication failed after retry")
            end,

            %% Verify retry metric was emitted
            test_helpers:wait_for_condition(fun() ->
                                               case router_caf_adapter_load_store:get_retry() of
                                                   {ok, _} -> true;
                                                   _ -> false
                                               end
                                            end,
                                            2000),

            case router_caf_adapter_load_store:get_retry() of
                {ok, Metadata} ->
                    AssignmentId = maps:get(assignment_id, Metadata, undefined),
                    RequestId = maps:get(request_id, Metadata, undefined),
                    Retries = maps:get(retries, Metadata, undefined),
                    true = AssignmentId =/= undefined,
                    true = RequestId =/= undefined,
                    true = Retries =/= undefined,
                    true = Retries >= 1,
                    ok;
                not_found ->
                    ct:fail("Retry metric was not emitted");
                {error, RetryError} ->
                    ct:fail("Failed to get retry metric: ~p", [RetryError])
            end,

            %% Verify published metric was emitted (after successful retry)
            test_helpers:wait_for_condition(fun() ->
                                               case router_caf_adapter_load_store:get_published() of
                                                   {ok, _} -> true;
                                                   _ -> false
                                               end
                                            end,
                                            2000),

            case router_caf_adapter_load_store:get_published() of
                {ok, PublishedMetadata} ->
                    AssignmentId2 = maps:get(assignment_id, PublishedMetadata, undefined),
                    RequestId2 = maps:get(request_id, PublishedMetadata, undefined),
                    true = AssignmentId2 =/= undefined,
                    true = RequestId2 =/= undefined,
                    ok;
                not_found ->
                    ct:fail("Published metric was not emitted");
                {error, PublishedError} ->
                    ct:fail("Failed to get published metric: ~p", [PublishedError])
            end,

            telemetry:detach(HandlerId),
            telemetry:detach(PublishedHandlerId),
        ok
    after
        _ = try telemetry:detach(HandlerId) catch _:_ -> ok end,
        _ = try telemetry:detach(PublishedHandlerId) catch _:_ -> ok end
    end.

%% Test: CAF retry exhaustion emits metrics
%% Verifies Router emits retry_exhausted metric when all retries fail
test_caf_retry_exhaustion_emits_metrics(_Config) ->
    %% Define handler IDs before try block
    HandlerId = {?MODULE, test_retry_exhaustion},
    FailureHandlerId = {?MODULE, test_retry_failure},
    try
            Request =
                #{~"version" => ~"1",
                  ~"request_id" => ~"req-exhaustion-test",
                  ~"trace_id" => ~"tr-exhaustion-test",
                  ~"tenant_id" => ~"test-tenant",
                  ~"task" =>
                      #{~"type" => ~"text.generate", ~"payload_ref" => ~"s3://bucket/key"},
                  ~"push_assignment" => true},

            Decision =
                #route_decision{provider_id = ~"openai",
                                reason = ~"weighted",
                                priority = 50,
                                expected_latency_ms = 850,
                                expected_cost = 0.012,
                                metadata = #{}},

            %% Track retry exhaustion metrics via helper
            telemetry:attach(HandlerId,
                             [router_caf_adapter, router_retry_exhausted_total],
                             fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
                                _ = router_caf_adapter_load_store:record_exhausted(Metadata)
                             end,
                             #{}),

            %% Track publish failures via helper
            telemetry:attach(FailureHandlerId,
                             [router_caf_adapter, router_assignment_publish_failures_total],
                             fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
                                _ = router_caf_adapter_load_store:record_failure(Metadata)
                             end,
                             #{}),

            %% Mock NATS: Always fail with timeout (simulating overloaded CAF)
            meck:expect(router_nats,
                        publish_with_ack,
                        fun(_Subject, _Json, _Headers) ->
                           %% router_nats returns {error, Reason} where Reason is atom or tuple
                           {error, timeout}
                        end),

            %% Set retry configuration for testing (low retries to exhaust quickly)
            ok = application:set_env(beamline_router, caf_max_retries, 2),
            ok = application:set_env(beamline_router, caf_retry_base_ms, 50),  %% Fast for testing

            %% Publish assignment (should fail after retries exhausted)
            Result = router_caf_adapter:publish_assignment(Request, Decision),

            %% Allow processing (bounded wait for retries to exhaust)
            test_helpers:wait_for_condition(fun() ->
                                               case router_caf_adapter_load_store:get_exhausted() of
                                                   {ok, _} -> true;
                                                   _ -> false
                                               end
                                            end,
                                            5000),

            %% Verify result is error (retries exhausted)
            case Result of
                error ->
                    ok;
                ok ->
                    ct:fail("Assignment publication should have failed after retry exhaustion")
            end,

            %% Verify retry exhaustion metric was emitted
            case router_caf_adapter_load_store:get_exhausted() of
                {ok, Metadata} ->
                    AssignmentId = maps:get(assignment_id, Metadata, undefined),
                    RequestId = maps:get(request_id, Metadata, undefined),
                    ErrorKind = maps:get(error_kind, Metadata, undefined),
                    Retries = maps:get(retries, Metadata, undefined),
                    true = AssignmentId =/= undefined,
                    true = RequestId =/= undefined,
                    true = ErrorKind =/= undefined,
                    timeout = ErrorKind,  %% Should be timeout error
                    true = Retries =/= undefined,
                    true = Retries >= 2,  %% Should have attempted at least 2 retries
                    ok;
                not_found ->
                    ct:fail("Retry exhaustion metric was not emitted");
                {error, ExhaustedError} ->
                    ct:fail("Failed to get exhausted metric: ~p", [ExhaustedError])
            end,

            telemetry:detach(HandlerId),
            telemetry:detach(FailureHandlerId),
        ok
    after
        _ = try telemetry:detach(HandlerId) catch _:_ -> ok end,
        _ = try telemetry:detach(FailureHandlerId) catch _:_ -> ok end
    end.

%% Test: CAF error response reflected in metrics
%% Verifies Router emits appropriate metrics when CAF/NATS returns errors (not timeouts)
test_caf_error_response_reflected_in_metrics(_Config) ->
    %% Define handler ID before try block
    HandlerId = {?MODULE, test_error_response},
    try
            Request =
                #{~"version" => ~"1",
                  ~"request_id" => ~"req-error-test",
                  ~"trace_id" => ~"tr-error-test",
                  ~"tenant_id" => ~"test-tenant",
                  ~"task" =>
                      #{~"type" => ~"text.generate", ~"payload_ref" => ~"s3://bucket/key"},
                  ~"push_assignment" => true},

            Decision =
                #route_decision{provider_id = ~"openai",
                                reason = ~"weighted",
                                priority = 50,
                                expected_latency_ms = 850,
                                expected_cost = 0.012,
                                metadata = #{}},

            %% Track publish failures via helper
            telemetry:attach(HandlerId,
                             [router_caf_adapter, router_assignment_publish_failures_total],
                             fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
                                _ = router_caf_adapter_load_store:record_failure(Metadata)
                             end,
                             #{}),

            %% Mock NATS: Return connection_failed error (simulating CAF/NATS unavailable)
            meck:expect(router_nats,
                        publish_with_ack,
                        fun(_Subject, _Json, _Headers) ->
                           %% router_nats returns {error, Reason} where Reason is atom or tuple
                           {error, connection_failed}
                        end),

            %% Set retry configuration for testing
            ok = application:set_env(beamline_router, caf_max_retries, 1),  %% Single retry attempt
            ok = application:set_env(beamline_router, caf_retry_base_ms, 50),

            %% Publish assignment (should fail after retry)
            Result = router_caf_adapter:publish_assignment(Request, Decision),

            %% Allow processing (bounded wait for failure)
            test_helpers:wait_for_condition(fun() ->
                                               case router_caf_adapter_load_store:get_failure() of
                                                   {ok, _} -> true;
                                                   not_found ->
                                                       %% Debug: log current state
                                                       ct:pal("### DEBUG: waiting for failure metric, current state: not_found",
                                                              []),
                                                       false;
                                                   {error, Err} ->
                                                       ct:pal("### DEBUG: store error while waiting: ~p",
                                                              [Err]),
                                                       false
                                               end
                                            end,
                                            5000),

            %% Verify result is error
            case Result of
                error ->
                    ok;
                ok ->
                    ct:fail("Assignment publication should have failed")
            end,

            %% Verify failure metric was emitted with correct error_kind
            case router_caf_adapter_load_store:get_failure() of
                {ok, Metadata} ->
                    AssignmentId = maps:get(assignment_id, Metadata, undefined),
                    RequestId = maps:get(request_id, Metadata, undefined),
                    ErrorKind = maps:get(error_kind, Metadata, undefined),
                    Subject = maps:get(subject, Metadata, undefined),
                    true = AssignmentId =/= undefined,
                    true = RequestId =/= undefined,
                    true = ErrorKind =/= undefined,
                    connection_failed = ErrorKind,  %% Should be connection_failed error
                    true = Subject =/= undefined,
                    ok;
                not_found ->
                    ct:fail("Publish failure metric was not emitted");
                {error, FailureError} ->
                    ct:fail("Failed to get failure metric: ~p", [FailureError])
            end,

            telemetry:detach(HandlerId),
        ok
    after
        _ = try telemetry:detach(HandlerId) catch _:_ -> ok end
    end.

%% Test: Degraded Mode metric emitted when retries exhausted
%% Verifies router_degraded_mode_active_total is emitted with expected labels
test_caf_degraded_mode_metric_emitted(_Config) ->
    HandlerId = {?MODULE, test_degraded_mode},
    try
        Request =
            #{~"version" => ~"1",
              ~"request_id" => ~"req-degraded-test",
              ~"trace_id" => ~"tr-degraded-test",
              ~"tenant_id" => ~"test-tenant",
              ~"task" =>
                  #{~"type" => ~"text.generate", ~"payload_ref" => ~"s3://bucket/key"},
              ~"push_assignment" => true},

        Decision =
            #route_decision{provider_id = ~"openai",
                            reason = ~"weighted",
                            priority = 50,
                            expected_latency_ms = 200,
                            expected_cost = 0.01,
                            metadata = #{}},

        telemetry:attach(HandlerId,
                         [router_caf_adapter, router_degraded_mode_active_total],
                         fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
                            _ = router_caf_adapter_load_store:record_degraded(Metadata)
                         end,
                         #{}),

        meck:expect(router_nats,
                    publish_with_ack,
                    fun(_Subject, _Json, _Headers) ->
                       {error, connection_failed}
                    end),

        ok = application:set_env(beamline_router, caf_max_retries, 2),
        ok = application:set_env(beamline_router, caf_retry_base_ms, 50),

        _Result = router_caf_adapter:publish_assignment(Request, Decision),

        test_helpers:wait_for_condition(fun() ->
                                            case router_caf_adapter_load_store:get_degraded() of
                                                {ok, _} -> true;
                                                _ -> false
                                            end
                                        end,
                                        5000),

        case router_caf_adapter_load_store:get_degraded() of
            {ok, Metadata} ->
                AssignmentId = maps:get(assignment_id, Metadata, undefined),
                RequestId = maps:get(request_id, Metadata, undefined),
                TenantId = maps:get(tenant_id, Metadata, undefined),
                Subject = maps:get(subject, Metadata, undefined),
                ErrorKind = maps:get(error_kind, Metadata, undefined),
                Retries = maps:get(retries, Metadata, undefined),
                true = AssignmentId =/= undefined,
                true = RequestId =/= undefined,
                true = TenantId =/= undefined,
                true = Subject =/= undefined,
                true = ErrorKind =/= undefined,
                connection_failed = ErrorKind,
                true = is_integer(Retries),
                true = Retries >= 2,
                ok;
            not_found ->
                ct:fail("Degraded mode metric was not emitted");
            {error, E} ->
                ct:fail("Failed to get degraded metric: ~p", [E])
        end,

        telemetry:detach(HandlerId),
        ok
    after
        _ = try telemetry:detach(HandlerId) catch _:_ -> ok end
    end.
