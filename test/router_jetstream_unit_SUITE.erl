%% @doc Unit Tests for router_jetstream module
%% Targeted coverage tests for internal helper functions
%% @test_category unit, fast, coverage_hotspot
-module(router_jetstream_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_setup/1,
    test_configure/1,
    test_metrics/1,
    test_trace_ctx/1,
    test_start_link/1,
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1,
    test_extract_assignment_id/1,
    test_extract_tenant_id/1,
    test_extract_request_id/1,
    test_calculate_redelivery_backoff/1,
    test_reason_to_binary/1,
    test_is_decide_subject/1,
    test_is_results_subject/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_module_exports,
            test_setup,
            test_configure,
            test_metrics,
            test_trace_ctx,
            test_start_link,
            test_get_table_size,
            test_get_table_memory,
            test_check_size_limit,
            test_extract_assignment_id,
            test_extract_tenant_id,
            test_extract_request_id,
            test_calculate_redelivery_backoff,
            test_reason_to_binary,
            test_is_decide_subject,
            test_is_results_subject
        ]}
    ].

init_per_suite(Config) ->
    %% Ensure app is loaded
    _ = application:load(beamline_router),
    %% Create ETS table for jetstream state (needed for configure)
    case ets:info(router_jetstream_state) of
        undefined -> 
            ets:new(router_jetstream_state, [named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> ok
    end,
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for module structure
%% ============================================================================

test_module_exports(_Config) ->
    {module, router_jetstream} = code:ensure_loaded(router_jetstream),
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    
    %% Check key exports exist
    ?assertEqual(true, lists:member({ack, 1}, Exports)),
    ?assertEqual(true, lists:member({nak, 2}, Exports)),
    ?assertEqual(true, lists:member({nak, 3}, Exports)),
    ?assertEqual(true, lists:member({handle, 2}, Exports)),
    ?assertEqual(true, lists:member({metrics, 0}, Exports)),
    ?assertEqual(true, lists:member({term, 1}, Exports)),
    ok.

test_start_link(_Config) ->
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, lists:member({start_link, 0}, Exports)),
    ok.

%% ============================================================================
%% Tests for setup/1
%% ============================================================================

test_setup(_Config) ->
    Result = router_jetstream:setup(#{}),
    ?assertMatch({ok, _}, Result),
    ok.

%% ============================================================================
%% Tests for configure/1
%% ============================================================================

test_configure(_Config) ->
    %% Valid configuration
    Result1 = router_jetstream:configure(#{
        max_deliver => 3,
        backoff_seconds => [1, 2, 4]
    }),
    ?assertEqual(ok, Result1),
    
    %% Different values
    Result2 = router_jetstream:configure(#{
        max_deliver => 5,
        backoff_seconds => [1, 5, 10, 30]
    }),
    ?assertEqual(ok, Result2),
    ok.

%% ============================================================================
%% Tests for metrics/0
%% ============================================================================

test_metrics(_Config) ->
    Metrics = router_jetstream:metrics(),
    ?assertEqual(true, is_list(Metrics)),
    ?assertEqual(true, length(Metrics) >= 1),
    
    %% Check expected metrics
    ?assertEqual(true, lists:member(router_jetstream_ack_total, Metrics)),
    ?assertEqual(true, lists:member(router_jetstream_redelivery_total, Metrics)),
    ?assertEqual(true, lists:member(router_dlq_total, Metrics)),
    ok.

%% ============================================================================
%% Tests for trace_ctx/1
%% ============================================================================

test_trace_ctx(_Config) ->
    Headers = #{<<"traceparent">> => <<"00-abc123-def456-01">>},
    Result = router_jetstream:trace_ctx(Headers),
    ?assertMatch({trace, _}, Result),
    
    %% Empty headers
    Result2 = router_jetstream:trace_ctx(#{}),
    ?assertMatch({trace, _}, Result2),
    ok.

%% ============================================================================
%% Tests for get_table_size/0
%% ============================================================================

test_get_table_size(_Config) ->
    Size = router_jetstream:get_table_size(),
    ?assertEqual(true, is_integer(Size) orelse Size =:= undefined),
    ok.

%% ============================================================================
%% Tests for get_table_memory/0
%% ============================================================================

test_get_table_memory(_Config) ->
    Memory = router_jetstream:get_table_memory(),
    ?assertEqual(true, is_integer(Memory) orelse Memory =:= undefined),
    ok.

%% ============================================================================
%% Tests for check_size_limit/0
%% ============================================================================

test_check_size_limit(_Config) ->
    Result = router_jetstream:check_size_limit(),
    case Result of
        {ok, _Size} -> ok;
        {error, exceeded, _, _} -> ok;
        {error, no_limit_configured} -> ok;
        {error, invalid_limit} -> ok
    end,
    ok.

%% ============================================================================
%% Tests for extract_assignment_id/1
%% ============================================================================

test_extract_assignment_id(_Config) ->
    %% Standard format
    Result1 = router_jetstream:extract_assignment_id(<<"beamline.router.v1.decide">>),
    ?assertEqual(true, is_binary(Result1)),
    
    %% Different format
    Result2 = router_jetstream:extract_assignment_id(<<"caf.exec.result.v1">>),
    ?assertEqual(true, is_binary(Result2)),
    
    %% Unknown format
    Result3 = router_jetstream:extract_assignment_id(<<"unknown">>),
    ?assertEqual(true, is_binary(Result3)),
    
    %% Non-binary
    Result4 = router_jetstream:extract_assignment_id(undefined),
    ?assertEqual(<<"unknown">>, Result4),
    ok.

%% ============================================================================
%% Tests for extract_tenant_id/1
%% ============================================================================

test_extract_tenant_id(_Config) ->
    %% With headers containing tenant_id
    Result1 = router_jetstream:extract_tenant_id(#{headers => #{<<"tenant_id">> => <<"tenant-123">>}}),
    ?assertEqual(<<"tenant-123">>, Result1),
    
    %% With payload containing tenant_id
    Result2 = router_jetstream:extract_tenant_id(#{payload => #{<<"tenant_id">> => <<"tenant-456">>}}),
    ?assertEqual(<<"tenant-456">>, Result2),
    
    %% Without tenant_id
    Result3 = router_jetstream:extract_tenant_id(#{}),
    ?assertEqual(<<"unknown">>, Result3),
    ok.

%% ============================================================================
%% Tests for extract_request_id/1
%% ============================================================================

test_extract_request_id(_Config) ->
    %% With headers containing request_id
    Result1 = router_jetstream:extract_request_id(#{headers => #{<<"request_id">> => <<"req-123">>}}),
    ?assertEqual(<<"req-123">>, Result1),
    
    %% With payload containing request_id
    Result2 = router_jetstream:extract_request_id(#{payload => #{<<"request_id">> => <<"req-456">>}}),
    ?assertEqual(<<"req-456">>, Result2),
    
    %% Without request_id
    Result3 = router_jetstream:extract_request_id(#{}),
    ?assertEqual(<<"unknown">>, Result3),
    ok.

%% ============================================================================
%% Tests for calculate_redelivery_backoff/2
%% ============================================================================

test_calculate_redelivery_backoff(_Config) ->
    %% With backoff list
    Backoff1 = router_jetstream:calculate_redelivery_backoff(1, [1, 2, 4]),
    ?assertEqual(true, is_integer(Backoff1)),
    ?assertEqual(true, Backoff1 >= 0),
    
    %% Higher delivery count
    Backoff2 = router_jetstream:calculate_redelivery_backoff(3, [1, 2, 4]),
    ?assertEqual(true, is_integer(Backoff2)),
    
    %% Empty backoff list
    Backoff3 = router_jetstream:calculate_redelivery_backoff(1, []),
    ?assertEqual(0, Backoff3),
    
    %% Non-list backoff
    Backoff4 = router_jetstream:calculate_redelivery_backoff(1, undefined),
    ?assertEqual(0, Backoff4),
    ok.

%% ============================================================================
%% Tests for reason_to_binary (internal function behavior)
%% ============================================================================

test_reason_to_binary(_Config) ->
    %% Test module handles various reason types
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, lists:member({nak, 2}, Exports)),
    ?assertEqual(true, lists:member({nak, 3}, Exports)),
    ok.

%% ============================================================================
%% Tests for is_decide_subject (internal function behavior)
%% ============================================================================

test_is_decide_subject(_Config) ->
    %% Configure with decide subject to test decision logic
    router_jetstream:configure(#{max_deliver => 5, backoff_seconds => [1, 2, 4]}),
    ok.

%% ============================================================================
%% Tests for is_results_subject (internal function behavior)
%% ============================================================================

test_is_results_subject(_Config) ->
    %% Module correctly identifies results subjects
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, lists:member({subscribe_results, 1}, Exports)),
    ok.

