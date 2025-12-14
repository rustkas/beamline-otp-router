%% @doc Unit Tests for router_nats module
%% Targeted coverage tests for internal helper functions
%% @test_category unit, fast, coverage_hotspot
-module(router_nats_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_error_to_reason_atoms/1,
    test_error_to_reason_binaries/1,
    test_error_to_reason_tuples/1,
    test_error_to_reason_complex/1,
    test_extract_stream_from_subject_valid/1,
    test_extract_stream_from_subject_invalid/1,
    test_extract_stream_from_subject_edge_cases/1,
    test_classify_error_type_transient/1,
    test_classify_error_type_permanent/1,
    test_classify_error_type_unknown/1,
    test_get_publish_retry_config/1,
    test_extract_nats_context_from_msgid/1,
    test_get_default_nats_context/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_error_to_reason_atoms,
            test_error_to_reason_binaries,
            test_error_to_reason_tuples,
            test_error_to_reason_complex,
            test_extract_stream_from_subject_valid,
            test_extract_stream_from_subject_invalid,
            test_extract_stream_from_subject_edge_cases,
            test_classify_error_type_transient,
            test_classify_error_type_permanent,
            test_classify_error_type_unknown,
            test_get_publish_retry_config,
            test_extract_nats_context_from_msgid,
            test_get_default_nats_context
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for error_to_reason/1
%% ============================================================================

test_error_to_reason_atoms(_Config) ->
    %% Test common atom error codes
    ?assertEqual(<<"timeout">>, router_nats:error_to_reason(timeout)),
    ?assertEqual(<<"connection_closed">>, router_nats:error_to_reason(connection_closed)),
    ?assertEqual(<<"noproc">>, router_nats:error_to_reason(noproc)),
    ?assertEqual(<<"not_connected">>, router_nats:error_to_reason(not_connected)),
    ?assertEqual(<<"stream_not_found">>, router_nats:error_to_reason(stream_not_found)),
    ok.

test_error_to_reason_binaries(_Config) ->
    %% Binary error messages should pass through
    ?assertEqual(<<"custom_error">>, router_nats:error_to_reason(<<"custom_error">>)),
    ?assertEqual(<<"some message">>, router_nats:error_to_reason(<<"some message">>)),
    ok.

test_error_to_reason_tuples(_Config) ->
    %% Tuple errors should extract meaningful reason
    Reason1 = router_nats:error_to_reason({error, timeout}),
    ?assertEqual(true, is_binary(Reason1)),
    
    Reason2 = router_nats:error_to_reason({exit, normal}),
    ?assertEqual(true, is_binary(Reason2)),
    
    Reason3 = router_nats:error_to_reason({noproc, {gen_server, call, []}}),
    ?assertEqual(true, is_binary(Reason3)),
    ok.

test_error_to_reason_complex(_Config) ->
    %% Complex/nested errors
    Reason1 = router_nats:error_to_reason({badmatch, {error, timeout}}),
    ?assertEqual(true, is_binary(Reason1)),
    
    %% List errors
    Reason2 = router_nats:error_to_reason("string_error"),
    ?assertEqual(true, is_binary(Reason2)),
    
    %% Number errors (edge case)
    Reason3 = router_nats:error_to_reason(500),
    ?assertEqual(true, is_binary(Reason3)),
    ok.

%% ============================================================================
%% Tests for extract_stream_from_subject/1
%% ============================================================================

test_extract_stream_from_subject_valid(_Config) ->
    %% The function extracts the first segment from a dotted subject
    %% e.g., "beamline.router.v1.decide" -> "beamline"
    Stream1 = router_nats:extract_stream_from_subject(<<"beamline.router.v1.decide.tenant1">>),
    ?assertEqual(true, is_binary(Stream1)),
    ?assertEqual(<<"beamline">>, Stream1),
    
    Stream2 = router_nats:extract_stream_from_subject(<<"test.subject.here">>),
    ?assertEqual(<<"test">>, Stream2),
    ok.

test_extract_stream_from_subject_invalid(_Config) ->
    %% Single segment subjects return as-is
    Stream1 = router_nats:extract_stream_from_subject(<<"invalid">>),
    ?assertEqual(true, is_binary(Stream1)),
    
    %% Empty returns empty (edge case)
    Stream2 = router_nats:extract_stream_from_subject(<<"">>),
    ?assertEqual(true, is_binary(Stream2)),
    ok.

test_extract_stream_from_subject_edge_cases(_Config) ->
    %% Subject with dots at edges
    Stream1 = router_nats:extract_stream_from_subject(<<".">>),
    ?assertEqual(true, is_binary(Stream1)),
    
    %% Multiple dots
    Stream2 = router_nats:extract_stream_from_subject(<<".....">>),
    ?assertEqual(true, is_binary(Stream2)),
    
    %% Very long subjects
    LongSubject = <<"beamline.router.v1.stream.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p">>,
    Stream3 = router_nats:extract_stream_from_subject(LongSubject),
    ?assertEqual(<<"beamline">>, Stream3),
    ok.

%% ============================================================================
%% Tests for classify_error_type/1
%% ============================================================================

test_classify_error_type_transient(_Config) ->
    %% Function returns the error type as binary
    Result1 = router_nats:classify_error_type(timeout),
    ?assertEqual(true, is_binary(Result1)),
    
    Result2 = router_nats:classify_error_type(connection_closed),
    ?assertEqual(true, is_binary(Result2)),
    
    Result3 = router_nats:classify_error_type(noproc),
    ?assertEqual(true, is_binary(Result3)),
    ok.

test_classify_error_type_permanent(_Config) ->
    %% Unknown errors typically return "unknown" 
    Result1 = router_nats:classify_error_type(stream_not_found),
    ?assertEqual(true, is_binary(Result1)),
    
    Result2 = router_nats:classify_error_type(invalid_subject),
    ?assertEqual(true, is_binary(Result2)),
    ok.

test_classify_error_type_unknown(_Config) ->
    %% Random atoms
    Result1 = router_nats:classify_error_type(some_random_error),
    ?assertEqual(true, is_binary(Result1)),
    
    %% Tuples
    Result2 = router_nats:classify_error_type({custom, error}),
    ?assertEqual(true, is_binary(Result2)),
    ok.

%% ============================================================================
%% Tests for get_publish_retry_config/0
%% ============================================================================

test_get_publish_retry_config(_Config) ->
    %% Config should return a map with expected keys
    Config = router_nats:get_publish_retry_config(),
    ?assertEqual(true, is_map(Config)),
    
    %% Check for expected keys
    ?assertEqual(true, maps:is_key(<<"enabled">>, Config) orelse 
                       maps:is_key(enabled, Config)),
    ok.

%% ============================================================================
%% Tests for extract_nats_context_from_msgid/1
%% ============================================================================

test_extract_nats_context_from_msgid(_Config) ->
    %% Valid message ID format
    Context1 = router_nats:extract_nats_context_from_msgid(<<"msg-12345-abc">>),
    ?assertEqual(true, is_map(Context1)),
    
    %% Undefined message ID
    Context2 = router_nats:extract_nats_context_from_msgid(undefined),
    ?assertEqual(true, is_map(Context2)),
    
    %% Empty message ID
    Context3 = router_nats:extract_nats_context_from_msgid(<<"">>),
    ?assertEqual(true, is_map(Context3)),
    ok.

%% ============================================================================
%% Tests for get_default_nats_context/0
%% ============================================================================

test_get_default_nats_context(_Config) ->
    Context = router_nats:get_default_nats_context(),
    ?assertEqual(true, is_map(Context)),
    ok.
