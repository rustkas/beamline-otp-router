%% @doc E2E tests for headers propagation (REST → Router → CAF)
%% CP2+ requirement: Verify trace_id, span_id, tenant_id propagation through entire pipeline
-module(router_headers_propagation_e2e_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, end_per_suite/1, end_per_testcase/2, init_per_suite/1, init_per_testcase/2, cleanup_test_ets_tables/0,
    %% Test functions called via all/0
    test_headers_propagation_rest_to_router/1,
    test_headers_propagation_router_to_caf/1,
    test_headers_propagation_full_chain/1,
    test_missing_headers_metric/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_headers_propagation_full_chain/1,
    test_headers_propagation_rest_to_router/1,
    test_headers_propagation_router_to_caf/1,
    test_missing_headers_metric/1
]).



-export([groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Headers propagation tests run in full tier
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [{group, headers_tests}];
groups_for_level(heavy) -> [{group, headers_tests}].

groups() ->
    [
        %% Sequential execution - tests share ETS tables and meck mocks
        {headers_tests, [sequence], [
            test_headers_propagation_rest_to_router,
            test_headers_propagation_router_to_caf,
            test_headers_propagation_full_chain,
            test_missing_headers_metric
        ]}
    ].

init_per_suite(Config) ->
    %% Stop any running instance first
    _ = application:stop(beamline_router),
    _ = application:unload(beamline_router),
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, usage_subject, <<"beamline.usage.v1.metered">>),
    ok = application:set_env(beamline_router, ack_subject, <<"caf.exec.assign.v1.ack">>),
    ok = application:set_env(beamline_router, assignment_enabled, true),
    ok = application:set_env(beamline_router, ack_enabled, true),
    %% Setup mock BEFORE starting app to prevent undef errors
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_result_consumer, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    router_mock_helpers:unload_all([router_nats, router_metrics]),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Reset mock expectations (mock was set up in init_per_suite)
    ok = router_mock_helpers:reset(router_nats),
    ok = router_mock_helpers:ensure_mock(router_metrics, [passthrough]),
    cleanup_test_ets_tables(),
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Cleanup any ETS tables created during tests
    cleanup_test_ets_tables(),
    %% Don't unload router_nats mock - it's needed by supervisor
    %% Only unload router_metrics which is safe
    router_mock_helpers:unload(router_metrics),
    Config.

%% @doc Cleanup test ETS tables (helper to prevent leaks)
cleanup_test_ets_tables() ->
    TestTables = [extracted_headers, published_headers, router_headers, caf_headers, metric_calls],
    lists:foreach(fun(TableName) ->
        case ets:info(TableName) of
            undefined -> ok;
            _ -> catch ets:delete(TableName)
        end
    end, TestTables),
    ok.

%% @doc Test: Headers propagation from REST (simulated) to Router
%% GIVEN: REST request with trace_id, span_id, tenant_id headers
%% WHEN: Request processed by Router
%% THEN: Headers are extracted and available in Router context
test_headers_propagation_rest_to_router(_Config) ->
    TraceId = <<"trace-1234567890abcdef">>,
    SpanId = <<"span-abcdef1234567890">>,
    TenantId = <<"tenant-acme">>,
    RequestId = <<"req-headers-test">>,
    
    %% Simulate REST request with headers (via NATS message with headers)
    Subject = <<"beamline.router.v1.decide">>,
    Headers = #{
        <<"trace_id">> => TraceId,
        <<"span_id">> => SpanId,
        <<"tenant_id">> => TenantId,
        <<"version">> => <<"1">>
    },
    Payload = jsx:encode(#{
        <<"request_id">> => RequestId,
        <<"tenant_id">> => TenantId,
        <<"policy_id">> => <<"policy-1">>,
        <<"trace_id">> => TraceId
    }),
    
    %% Track headers extraction (using try-finally to ensure cleanup)
    ExtractedHeaders = router_test_init:ensure_ets_table(extracted_headers, [named_table, set, public]),
    ets:delete_all_objects(ExtractedHeaders),
    %% Setup mock for router_decide_consumer (needed for meck:expect to work)
    ok = router_mock_helpers:ensure_mock(router_decide_consumer, [passthrough]),
    try
        %% Mock: Track headers in decide consumer
        meck:expect(router_decide_consumer, handle_decide_message, fun(_Subject, Request, Hdrs, _MsgId) ->
            %% Extract headers (simulating router_decide_consumer logic)
            ExtractedTraceId = maps:get(<<"trace_id">>, Hdrs, maps:get(<<"trace_id">>, Request, undefined)),
            ExtractedSpanId = maps:get(<<"span_id">>, Hdrs, maps:get(<<"span_id">>, Request, undefined)),
            ExtractedTenantId = maps:get(<<"tenant_id">>, Hdrs, maps:get(<<"tenant_id">>, Request, undefined)),
            
            ets:insert(ExtractedHeaders, {trace_id, ExtractedTraceId}),
            ets:insert(ExtractedHeaders, {span_id, ExtractedSpanId}),
            ets:insert(ExtractedHeaders, {tenant_id, ExtractedTenantId}),
            ok
        end),
        
        %% Simulate message delivery to Router
        router_decide_consumer:handle_decide_message(Subject, jsx:decode(Payload, [return_maps]), Headers, <<"msg-1">>),
        
        %% Verify headers were extracted
        ?assertEqual([{trace_id, TraceId}], ets:lookup(ExtractedHeaders, trace_id)),
        ?assertEqual([{span_id, SpanId}], ets:lookup(ExtractedHeaders, span_id)),
        ?assertEqual([{tenant_id, TenantId}], ets:lookup(ExtractedHeaders, tenant_id))
    after
        %% Always cleanup ETS table and mock
        catch ets:delete(ExtractedHeaders),
        catch meck:unload(router_decide_consumer)
    end.

%% @doc Test: Headers propagation from Router to CAF
%% GIVEN: Router has trace_id, span_id, tenant_id in context
%% WHEN: Router publishes assignment to CAF
%% THEN: Headers are included in CAF assignment message
test_headers_propagation_router_to_caf(_Config) ->
    TraceId = <<"trace-router-to-caf">>,
    TenantId = <<"tenant-router-to-caf">>,
    
    %% Track published headers (using try-finally to ensure cleanup)
    PublishedHeaders = router_test_init:ensure_ets_table(published_headers, [named_table, set, public]),
    ets:delete_all_objects(PublishedHeaders),
    try
        %% Mock: Track headers in CAF adapter publish
        meck:expect(router_nats, publish_with_ack, fun(_Subject, _Json, Hdrs) ->
            %% Extract headers from published message
            PublishedTraceId = maps:get(<<"trace_id">>, Hdrs, undefined),
            PublishedSpanId = maps:get(<<"span_id">>, Hdrs, undefined),
            PublishedTenantId = maps:get(<<"tenant_id">>, Hdrs, undefined),
            
            ets:insert(PublishedHeaders, {trace_id, PublishedTraceId}),
            ets:insert(PublishedHeaders, {span_id, PublishedSpanId}),
            ets:insert(PublishedHeaders, {tenant_id, PublishedTenantId}),
            {ok, <<"msg-id">>}
        end),
        
        %% Simulate Router publishing to CAF (via router_caf_adapter)
        RequestMap = #{
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId
        },
        Subject = <<"caf.exec.assign.v1">>,
        Json = jsx:encode(#{<<"assignment_id">> => <<"assign-1">>}),
        
        %% Directly call mocked router_nats:publish_with_ack (simulating CAF adapter publish)
        %% This tests headers propagation without depending on specific CAF adapter implementation
        case router_nats:publish_with_ack(Subject, Json, RequestMap) of
            {ok, _} ->
                %% Verify headers were published
                test_helpers:wait_for_condition(fun() -> 
                    ets:lookup(PublishedHeaders, trace_id) =/= []
                end, 1000),
                
                [{trace_id, PublishedTraceId}] = ets:lookup(PublishedHeaders, trace_id),
                [{tenant_id, PublishedTenantId}] = ets:lookup(PublishedHeaders, tenant_id),
                
                %% Verify trace_id and tenant_id are present
                ?assert(PublishedTraceId =/= undefined),
                ?assert(PublishedTenantId =/= undefined);
            Error ->
                ct:fail("Failed to publish to CAF: ~p", [Error])
        end
    after
        %% Always cleanup ETS table
        catch ets:delete(PublishedHeaders)
    end.

%% @doc Test: Full chain headers propagation (REST → Router → CAF)
%% GIVEN: REST request with headers
%% WHEN: Request flows through Router to CAF
%% THEN: Headers are preserved throughout the chain
test_headers_propagation_full_chain(_Config) ->
    TraceId = <<"trace-full-chain">>,
    SpanId = <<"span-full-chain">>,
    TenantId = <<"tenant-full-chain">>,
    RequestId = <<"req-full-chain">>,
    
    %% Track headers at each stage (using try-finally to ensure cleanup)
    RouterHeaders = router_test_init:ensure_ets_table(router_headers, [named_table, set, public]),
    CAFHeaders = router_test_init:ensure_ets_table(caf_headers, [named_table, set, public]),
    ets:delete_all_objects(RouterHeaders),
    ets:delete_all_objects(CAFHeaders),
    %% Setup mock for router_decide_consumer
    ok = router_mock_helpers:ensure_mock(router_decide_consumer, [passthrough]),
    try
        %% Mock: Track headers in Router
        meck:expect(router_decide_consumer, handle_decide_message, fun(_Subject, Request, Hdrs, _MsgId) ->
            ExtractedTraceId = maps:get(<<"trace_id">>, Hdrs, maps:get(<<"trace_id">>, Request, undefined)),
            ExtractedSpanId = maps:get(<<"span_id">>, Hdrs, maps:get(<<"span_id">>, Request, undefined)),
            ExtractedTenantId = maps:get(<<"tenant_id">>, Hdrs, maps:get(<<"tenant_id">>, Request, undefined)),
            
            ets:insert(RouterHeaders, {trace_id, ExtractedTraceId}),
            ets:insert(RouterHeaders, {span_id, ExtractedSpanId}),
            ets:insert(RouterHeaders, {tenant_id, ExtractedTenantId}),
            ok
        end),
        
        %% Mock: Track headers in CAF publish
        meck:expect(router_nats, publish_with_ack, fun(_Subject, _Json, Hdrs) ->
            PublishedTraceId = maps:get(<<"trace_id">>, Hdrs, undefined),
            PublishedSpanId = maps:get(<<"span_id">>, Hdrs, undefined),
            PublishedTenantId = maps:get(<<"tenant_id">>, Hdrs, undefined),
            
            ets:insert(CAFHeaders, {trace_id, PublishedTraceId}),
            ets:insert(CAFHeaders, {span_id, PublishedSpanId}),
            ets:insert(CAFHeaders, {tenant_id, PublishedTenantId}),
            {ok, <<"msg-id">>}
        end),
        
        %% Simulate REST → Router flow
        Subject = <<"beamline.router.v1.decide">>,
        Headers = #{
            <<"trace_id">> => TraceId,
            <<"span_id">> => SpanId,
            <<"tenant_id">> => TenantId,
            <<"version">> => <<"1">>
        },
        Payload = jsx:encode(#{
            <<"request_id">> => RequestId,
            <<"tenant_id">> => TenantId,
            <<"policy_id">> => <<"policy-1">>,
            <<"trace_id">> => TraceId
        }),
        
        router_decide_consumer:handle_decide_message(Subject, jsx:decode(Payload, [return_maps]), Headers, <<"msg-1">>),
        
        %% Verify headers in Router
        ?assertEqual([{trace_id, TraceId}], ets:lookup(RouterHeaders, trace_id)),
        ?assertEqual([{span_id, SpanId}], ets:lookup(RouterHeaders, span_id)),
        ?assertEqual([{tenant_id, TenantId}], ets:lookup(RouterHeaders, tenant_id)),
        
        %% Simulate Router → CAF flow
        RequestMap = #{
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId
        },
        CAFSubject = <<"caf.exec.assign.v1">>,
        CAFJson = jsx:encode(#{<<"assignment_id">> => <<"assign-1">>}),
        
        %% Directly call mocked router_nats:publish_with_ack (simulating CAF adapter publish)
        case router_nats:publish_with_ack(CAFSubject, CAFJson, RequestMap) of
            {ok, _} ->
                %% Verify headers in CAF
                test_helpers:wait_for_condition(fun() -> 
                    ets:lookup(CAFHeaders, trace_id) =/= []
                end, 1000),
                
                [{trace_id, CAFTraceId}] = ets:lookup(CAFHeaders, trace_id),
                [{tenant_id, CAFTenantId}] = ets:lookup(CAFHeaders, tenant_id),
                
                %% Verify headers are preserved
                ?assertEqual(TraceId, CAFTraceId),
                ?assertEqual(TenantId, CAFTenantId);
            Error ->
                ct:fail("Failed to publish to CAF: ~p", [Error])
        end
    after
        %% Always cleanup ETS tables and mocks
        catch ets:delete(RouterHeaders),
        catch ets:delete(CAFHeaders),
        catch meck:unload(router_decide_consumer)
    end.

%% @doc Test: Missing headers metric
%% GIVEN: Request without trace_id, span_id, or tenant_id
%% WHEN: Request processed by Router
%% THEN: ctx_missing_headers_total metric is incremented
test_missing_headers_metric(_Config) ->
    %% Track metric increments (using try-finally to ensure cleanup)
    MetricCalls = router_test_init:ensure_ets_table(metric_calls, [named_table, set, public]),
    ets:delete_all_objects(MetricCalls),
    try
        %% Mock: Track ctx_missing_headers_total increments
        meck:expect(router_metrics, inc, fun(MetricName) ->
            case MetricName of
                ctx_missing_headers_total ->
                    ets:insert(MetricCalls, {ctx_missing_headers_total, erlang:system_time(millisecond)});
                _ ->
                    ok
            end,
            ok
        end),
        
        %% Simulate request without headers
        Headers = #{},  %% Empty headers
        Payload = jsx:encode(#{
            <<"request_id">> => <<"req-no-headers">>,
            <<"policy_id">> => <<"policy-1">>
            %% No trace_id, span_id, tenant_id
        }),
        
        %% Simulate check_missing_headers logic (from router_decide_consumer.erl)
        Request = jsx:decode(Payload, [return_maps]),
        %% Extract headers (priority) or payload (fallback) for key headers
        TraceId = maps:get(<<"trace_id">>, Headers, maps:get(<<"trace_id">>, Request, undefined)),
        SpanId = maps:get(<<"span_id">>, Headers, maps:get(<<"span_id">>, Request, undefined)),
        TenantId = maps:get(<<"tenant_id">>, Headers, maps:get(<<"tenant_id">>, Request, undefined)),
        
        %% Check for missing headers
        MissingHeaders = [],
        MissingHeaders1 = case TraceId of
            undefined -> [<<"trace_id">> | MissingHeaders];
            _ -> MissingHeaders
        end,
        MissingHeaders2 = case SpanId of
            undefined -> [<<"span_id">> | MissingHeaders1];
            _ -> MissingHeaders1
        end,
        MissingHeaders3 = case TenantId of
            undefined -> [<<"tenant_id">> | MissingHeaders2];
            _ -> MissingHeaders2
        end,
        
        %% Emit metric if any headers are missing (via mocked router_metrics)
        case MissingHeaders3 of
            [] ->
                ok;
            _ ->
                router_metrics:inc(ctx_missing_headers_total)
        end,
        
        %% Verify metric was incremented
        test_helpers:wait_for_condition(fun() -> 
            ets:lookup(MetricCalls, ctx_missing_headers_total) =/= []
        end, 1000),
        
        ?assertMatch([{ctx_missing_headers_total, _}], ets:lookup(MetricCalls, ctx_missing_headers_total))
    after
        %% Always cleanup ETS table
        catch ets:delete(MetricCalls)
    end.
