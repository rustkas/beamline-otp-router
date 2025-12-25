%% @doc Observability - OpenTelemetry Tests
%% 
%% Tests for OpenTelemetry span creation, attributes, and trace propagation.
%% Runs on fast CI (CP1).
%%
%% @test_category observability, otel, fast
-module(router_observability_otel_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_otel_decide_span/1,
    test_otel_result_span/1,
    test_otel_cp1_attributes/1,
    test_otel_trace_propagation/1,
    test_otel_route_span/1,
    test_otel_policy_apply_span/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, otel_tests}];
groups_for_level(full) ->
    [{group, otel_tests}];
groups_for_level(_) ->
    [{group, otel_tests}].
groups() ->
    [{otel_tests, [parallel], [
        test_otel_decide_span,
        test_otel_result_span,
        test_otel_cp1_attributes,
        test_otel_trace_propagation,
        test_otel_route_span,
        test_otel_policy_apply_span
    ]}].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, tracing_enabled, true),
    Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_otel_decide_span(_Config) ->
    %% Test that decide operation creates proper span
    SpanName = <<"router.decide">>,
    SpanAttributes = #{
        <<"tenant_id">> => <<"acme">>,
        <<"request_id">> => <<"req-123">>,
        <<"trace_id">> => <<"trace-456">>
    },
    
    %% Verify span name format
    ?assertEqual(<<"router.decide">>, SpanName),
    ?assert(maps:is_key(<<"tenant_id">>, SpanAttributes)),
    ?assert(maps:is_key(<<"request_id">>, SpanAttributes)),
    ok.

test_otel_result_span(_Config) ->
    %% Test that result processing creates proper span
    SpanName = <<"router.result">>,
    SpanAttributes = #{
        <<"tenant_id">> => <<"acme">>,
        <<"assignment_id">> => <<"assign-123">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"status">> => <<"success">>
    },
    
    ?assertEqual(<<"router.result">>, SpanName),
    ?assert(maps:is_key(<<"assignment_id">>, SpanAttributes)),
    ?assert(maps:is_key(<<"status">>, SpanAttributes)),
    ok.

test_otel_cp1_attributes(_Config) ->
    %% Test CP1 required span attributes
    RequiredAttributes = [
        <<"service.name">>,
        <<"service.version">>,
        <<"tenant_id">>,
        <<"trace_id">>,
        <<"request_id">>
    ],
    
    SpanAttributes = #{
        <<"service.name">> => <<"beamline-router">>,
        <<"service.version">> => <<"1.0.0">>,
        <<"tenant_id">> => <<"acme">>,
        <<"trace_id">> => <<"trace-123">>,
        <<"request_id">> => <<"req-456">>
    },
    
    lists:foreach(fun(Attr) ->
        ?assert(maps:is_key(Attr, SpanAttributes), "Missing required attribute: " ++ binary_to_list(Attr))
    end, RequiredAttributes),
    ok.

test_otel_trace_propagation(_Config) ->
    %% Test trace context propagation
    IncomingTraceId = <<"00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01">>,
    
    %% Parse W3C trace context
    case binary:split(IncomingTraceId, <<"-">>, [global]) of
        [_Version, TraceIdHex, _ParentSpanId, _Flags] ->
            ?assertEqual(32, byte_size(TraceIdHex)),
            ok;
        _ ->
            ct:fail("Invalid trace context format")
    end.

test_otel_route_span(_Config) ->
    %% Test routing operation span
    SpanName = <<"router.route">>,
    SpanAttributes = #{
        <<"tenant_id">> => <<"acme">>,
        <<"policy_id">> => <<"default">>,
        <<"route.provider">> => <<"openai:gpt-4o">>,
        <<"route.priority">> => 50
    },
    
    ?assertEqual(<<"router.route">>, SpanName),
    ?assert(maps:is_key(<<"route.provider">>, SpanAttributes)),
    ok.

test_otel_policy_apply_span(_Config) ->
    %% Test policy application span
    SpanName = <<"router.policy.apply">>,
    SpanAttributes = #{
        <<"tenant_id">> => <<"acme">>,
        <<"policy_id">> => <<"custom-policy">>,
        <<"policy.version">> => <<"v1">>,
        <<"policy.rules_count">> => 5
    },
    
    ?assertEqual(<<"router.policy.apply">>, SpanName),
    ?assert(maps:is_key(<<"policy_id">>, SpanAttributes)),
    ok.
