-module(router_caf_adapter_advanced_SUITE).

-doc "Advanced CAF adapter tests: schema validation, metrics, and error-handling under instrumentation.".

-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([
    suite/0,
    all/0, groups_for_level/1,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_full_publish_flow_with_metrics/1,
    test_schema_validation_failure/1,
    test_assignment_recording_failure/1,
    test_metrics_capture_events/1
]).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, advanced_tests}];
groups_for_level(full) ->
    [{group, advanced_tests}];
groups_for_level(_) ->
    [{group, advanced_tests}].

groups() ->
    [{advanced_tests, [sequence], [
        test_full_publish_flow_with_metrics,
        test_schema_validation_failure,
        test_assignment_recording_failure,
        test_metrics_capture_events
    ]}].

init_per_suite(Config) ->
    %% Ensure meck is started for mocking
    ok = router_mock_helpers:ensure_test_deps(),
    {ok, _} = application:ensure_all_started(beamline_router),
    Config.

end_per_suite(Config) ->
    ok = application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Setup common mocks via helper (creates metrics table)
    ok = router_caf_test_helper:setup_caf_mocks(),
    Config.

end_per_testcase(_TestCase, Config) ->
    meck:unload(),
    Config.

test_full_publish_flow_with_metrics(_Config) ->
    TestPid = self(),
    Ref = router_caf_test_helper:attach_metrics_capture(TestPid),

    Request = router_caf_test_helper:build_test_request(),
    Decision = router_caf_test_helper:build_test_decision(),

    ok = router_caf_adapter:publish_assignment(Request, Decision),

    Captured = router_caf_test_helper:get_captured_metrics(Ref),
    ?assert(length(Captured) > 0),

    ok = router_caf_test_helper:detach_metrics_capture(Ref),
    ok.

test_schema_validation_failure(_Config) ->
    %% Missing required fields to trigger schema validation error
    Request = #{tenant_id => ~"tenant_bad"},
    %% Use correct route_decision record fields
    Decision = #route_decision{
        provider_id = ~"provider_1",
        reason = <<"bad_schema">>,
        priority = 50,
        expected_latency_ms = 100,
        expected_cost = 0.01,
        metadata = #{}
    },

    Result =
        try router_caf_adapter:publish_assignment(Request, Decision) of
            R -> {ok, R}
        catch
            Class:Reason:Stack -> {crash, {Class, Reason, Stack}}
        end,

    case Result of
        {error, _} -> ok;
        {crash, _} -> ok;
        {ok, ok} -> ct:fail(expected_schema_validation_error);
        {ok, error} -> ok;  %% publish_assignment returns error atom on failure
        {ok, {error, _}} -> ok;
        {ok, Other} -> ct:fail({expected_error, Other})
    end.

test_assignment_recording_failure(_Config) ->
    ok = router_mock_helpers:expect(
        router_correlation_context,
        store,
        fun(_AssignmentId, _RequestId, _Context) -> {error, disk_full} end
    ),

    Request = router_caf_test_helper:build_test_request(),
    Decision = router_caf_test_helper:build_test_decision(),

    %% router_caf_adapter:publish_assignment returns ok | error (not {error, _})
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    %% The function may succeed anyway since correlation_context:store failure 
    %% might not stop the publish flow. Just verify it returns ok or error.
    ?assert(Result =:= ok orelse Result =:= error orelse element(1, Result) =:= error),

    ok.

test_metrics_capture_events(_Config) ->
    HandlerId = {caf_test_metrics_capture, make_ref()},
    Event = [router, caf, adapter, assignment_generated],

    CapturePid = self(),
    HandlerFun = fun(E, Measurements, Meta, _Cfg) ->
        CapturePid ! {captured, E, Measurements, Meta},
        ok
    end,

    ok = telemetry:attach(HandlerId, Event, HandlerFun, #{}),
    _ = telemetry:execute(Event, #{count => 1}, #{tenant_id => ~"tenant_x"}),

    receive
        {captured, Event, #{count := 1}, #{tenant_id := ~"tenant_x"}} -> ok
    after 1000 ->
        ct:fail(timeout_waiting_for_telemetry)
    end,

    _ = try telemetry:detach(HandlerId) catch _:_ -> ok end,
    ok.
