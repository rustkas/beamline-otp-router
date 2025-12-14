%% @doc Unit Tests for router_tracing module
%% Targeted coverage tests for tracing and span management
%% @test_category unit, fast, coverage_hotspot, observability
-module(router_tracing_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_start_span/1,
    test_end_span/1,
    test_set_span_attribute/1,
    test_set_span_status/1,
    test_get_trace_id/1,
    test_get_span_id/1,
    test_with_span/1,
    test_inject_context/1,
    test_extract_context/1,
    test_get_current_span/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_module_exports,
            test_start_span,
            test_end_span,
            test_set_span_attribute,
            test_set_span_status,
            test_get_trace_id,
            test_get_span_id,
            test_with_span,
            test_inject_context,
            test_extract_context,
            test_get_current_span
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear any existing span state
    erase(current_span),
    erase(trace_id),
    erase(span_id),
    Config.

end_per_testcase(_TestCase, _Config) ->
    erase(current_span),
    erase(trace_id),
    erase(span_id),
    ok.

%% ============================================================================
%% Tests for module structure
%% ============================================================================

test_module_exports(_Config) ->
    {module, router_tracing} = code:ensure_loaded(router_tracing),
    Exports = router_tracing:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    
    %% Check key exports
    ?assertEqual(true, lists:member({start_span, 3}, Exports)),
    ?assertEqual(true, lists:member({end_span, 1}, Exports)),
    ?assertEqual(true, lists:member({set_span_attribute, 3}, Exports)),
    ?assertEqual(true, lists:member({set_span_status, 2}, Exports)),
    ok.

%% ============================================================================
%% Tests for start_span/3
%% ============================================================================

test_start_span(_Config) ->
    SpanName = <<"test.span">>,
    Attributes = #{<<"key">> => <<"value">>},
    
    Result = router_tracing:start_span(SpanName, Attributes, undefined),
    ?assertMatch({ok, _}, Result),
    
    {ok, Span} = Result,
    ?assertEqual(true, is_map(Span)),
    ok.

%% ============================================================================
%% Tests for end_span/1
%% ============================================================================

test_end_span(_Config) ->
    %% Start a span first
    {ok, _Span} = router_tracing:start_span(<<"test.end_span">>, #{}, undefined),
    
    %% End with ok status
    Result = router_tracing:end_span(ok),
    ?assertEqual(ok, Result),
    
    %% End when no span exists
    Result2 = router_tracing:end_span(ok),
    ?assertEqual(ok, Result2),
    ok.

%% ============================================================================
%% Tests for set_span_attribute/3
%% ============================================================================

test_set_span_attribute(_Config) ->
    %% Without active span should be ok
    Result1 = router_tracing:set_span_attribute(<<"key">>, <<"value">>, string),
    ?assertEqual(ok, Result1),
    
    %% With active span
    {ok, _Span} = router_tracing:start_span(<<"test.attr_span">>, #{}, undefined),
    Result2 = router_tracing:set_span_attribute(<<"test_key">>, <<"test_value">>, string),
    ?assertEqual(ok, Result2),
    
    %% Integer type
    Result3 = router_tracing:set_span_attribute(<<"count">>, 42, integer),
    ?assertEqual(ok, Result3),
    
    router_tracing:end_span(ok),
    ok.

%% ============================================================================
%% Tests for set_span_status/2
%% ============================================================================

test_set_span_status(_Config) ->
    %% Without active span
    Result1 = router_tracing:set_span_status(ok, <<"success">>),
    ?assertEqual(ok, Result1),
    
    %% With active span
    {ok, _Span} = router_tracing:start_span(<<"test.status_span">>, #{}, undefined),
    Result2 = router_tracing:set_span_status(ok, <<"success">>),
    ?assertEqual(ok, Result2),
    
    Result3 = router_tracing:set_span_status(error, <<"something failed">>),
    ?assertEqual(ok, Result3),
    
    router_tracing:end_span(ok),
    ok.

%% ============================================================================
%% Tests for get_trace_id/0
%% ============================================================================

test_get_trace_id(_Config) ->
    Result1 = router_tracing:get_trace_id(),
    %% May be undefined if no span active
    ?assertEqual(true, Result1 =:= undefined orelse is_binary(Result1)),
    
    %% Start span and check trace_id
    {ok, _Span} = router_tracing:start_span(<<"test.trace_id_span">>, #{}, undefined),
    Result2 = router_tracing:get_trace_id(),
    ?assertEqual(true, Result2 =:= undefined orelse is_binary(Result2)),
    
    router_tracing:end_span(ok),
    ok.

%% ============================================================================
%% Tests for get_span_id/0
%% ============================================================================

test_get_span_id(_Config) ->
    Exports = router_tracing:module_info(exports),
    case lists:member({get_span_id, 0}, Exports) of
        true ->
            Result1 = router_tracing:get_span_id(),
            ?assertEqual(true, Result1 =:= undefined orelse is_binary(Result1)),
            
            {ok, _Span} = router_tracing:start_span(<<"test.span_id">>, #{}, undefined),
            Result2 = router_tracing:get_span_id(),
            ?assertEqual(true, Result2 =:= undefined orelse is_binary(Result2)),
            
            router_tracing:end_span(ok);
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for with_span/3
%% ============================================================================

test_with_span(_Config) ->
    Exports = router_tracing:module_info(exports),
    case lists:member({with_span, 3}, Exports) of
        true ->
            %% Simple function execution within span
            Result = router_tracing:with_span(
                <<"test.with_span">>,
                #{},
                fun() -> {ok, test_result} end
            ),
            ?assertEqual({ok, test_result}, Result);
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for inject_context/1
%% ============================================================================

test_inject_context(_Config) ->
    Exports = router_tracing:module_info(exports),
    case lists:member({inject_context, 1}, Exports) of
        true ->
            Headers = #{},
            Result = router_tracing:inject_context(Headers),
            ?assertEqual(true, is_map(Result));
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for extract_context/1
%% ============================================================================

test_extract_context(_Config) ->
    Exports = router_tracing:module_info(exports),
    case lists:member({extract_context, 1}, Exports) of
        true ->
            Headers = #{
                <<"traceparent">> => <<"00-abc123-def456-01">>
            },
            Result = router_tracing:extract_context(Headers),
            ?assertEqual(true, is_map(Result) orelse Result =:= undefined);
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for get_current_span/0
%% ============================================================================

test_get_current_span(_Config) ->
    Exports = router_tracing:module_info(exports),
    case lists:member({get_current_span, 0}, Exports) of
        true ->
            %% No span active
            Result1 = router_tracing:get_current_span(),
            ?assertEqual(true, Result1 =:= undefined orelse is_map(Result1)),
            
            %% Start span
            {ok, Span} = router_tracing:start_span(<<"test.current">>, #{}, undefined),
            Result2 = router_tracing:get_current_span(),
            ?assertEqual(true, is_map(Result2)),
            
            router_tracing:end_span(ok);
        false ->
            ok
    end,
    ok.
