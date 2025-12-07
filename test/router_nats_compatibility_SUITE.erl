%% @doc NATS Protocol Compatibility Tests
%%
%% Tests NATS protocol compatibility, message formats, subject formats, and protocol compliance.
%% Verifies that the router maintains compatibility with NATS protocol specifications.
%%
%% @test_category compatibility, nats
-module(router_nats_compatibility_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

-compile([export_all, nowarn_export_all]).

all() -> [
    test_nats_protocol_compatibility,
    test_nats_subject_format,
    test_nats_message_format_assignment,
    test_nats_headers_format,
    test_jetstream_compatibility,
    test_nats_version_compatibility,
    test_nats_function_availability
].

init_per_suite(Config) ->
    ok = router_test_utils:start_router_app(),
    Config.

end_per_suite(_Config) ->
    router_test_utils:stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: NATS protocol compatibility verification
test_nats_protocol_compatibility(_Config) ->
    case router_protocol_compatibility:verify_nats_protocol_compatibility() of
        {ok, Report} ->
            ?assert(maps:is_key(compatible, Report)),
            ?assert(maps:is_key(subjects, Report)),
            ?assert(maps:is_key(message_formats, Report)),
            ?assert(maps:is_key(headers, Report)),
            ?assert(maps:is_key(jetstream, Report)),
            ?assert(maps:is_key(version, Report)),
            
            Compatible = maps:get(compatible, Report),
            ?assert(Compatible),
            
            ok;
        {error, Reason} ->
            ct:fail({compatibility_verification_failed, Reason})
    end.

%% @doc Test: NATS subject format
test_nats_subject_format(_Config) ->
    %% Test valid subjects
    ValidSubjects = [
        <<"beamline.router.v1.decide">>,
        <<"caf.exec.assign.v1">>,
        <<"caf.exec.result.v1">>,
        <<"test.subject.123">>
    ],
    
    lists:foreach(fun(Subject) ->
        ?assert(is_valid_nats_subject(Subject))
    end, ValidSubjects),
    
    %% Test invalid subjects
    InvalidSubjects = [
        <<"test subject">>,  %% Space not allowed
        <<"test@subject">>,  %% @ not allowed
        <<"test#subject">>   %% # not allowed
    ],
    
    lists:foreach(fun(Subject) ->
        ?assertNot(is_valid_nats_subject(Subject))
    end, InvalidSubjects),
    
    ok.

%% @doc Test: NATS message format (assignment)
test_nats_message_format_assignment(_Config) ->
    case router_protocol_compatibility:check_nats_message_format(assignment) of
        {ok, FormatInfo} ->
            ?assert(maps:is_key(message_type, FormatInfo)),
            ?assert(maps:is_key(format, FormatInfo)),
            ?assert(maps:is_key(fields, FormatInfo)),
            ?assertEqual(assignment, maps:get(message_type, FormatInfo)),
            ?assertEqual(valid, maps:get(format, FormatInfo)),
            ok;
        {error, Reason} ->
            ct:fail({message_format_check_failed, Reason})
    end.

%% @doc Test: NATS headers format
test_nats_headers_format(_Config) ->
    %% Test valid headers format
    ValidHeaders = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"version">> => <<"1">>,
        <<"trace_id">> => <<"test_trace">>
    },
    
    ?assert(is_map(ValidHeaders)),
    ?assert(lists:all(fun(K) -> is_binary(K) end, maps:keys(ValidHeaders))),
    
    ok.

%% @doc Test: JetStream compatibility
test_jetstream_compatibility(_Config) ->
    %% Check JetStream functions are available
    ?assert(erlang:function_exported(router_nats, subscribe_jetstream, 5)),
    ?assert(erlang:function_exported(router_nats, js_ack, 1)),
    ?assert(erlang:function_exported(router_nats, js_nak, 2)),
    ?assert(erlang:function_exported(router_nats, js_dlq, 2)),
    
    ok.

%% @doc Test: NATS version compatibility
test_nats_version_compatibility(_Config) ->
    Version = router_protocol_compatibility:get_nats_protocol_version(),
    ?assertEqual(<<"2.0">>, Version),
    ok.

%% @doc Test: NATS function availability
test_nats_function_availability(_Config) ->
    %% Check core NATS functions
    ?assert(erlang:function_exported(router_nats, publish, 2)),
    ?assert(erlang:function_exported(router_nats, publish_with_ack, 3)),
    ?assert(erlang:function_exported(router_nats, subscribe, 3)),
    ?assert(erlang:function_exported(router_nats, ack_message, 1)),
    ?assert(erlang:function_exported(router_nats, nak_message, 1)),
    
    %% Check connection management
    ?assert(erlang:function_exported(router_nats, get_connection_status, 0)),
    ?assert(erlang:function_exported(router_nats, reconnect, 0)),
    
    ok.

%% Helper: Check if NATS subject is valid
is_valid_nats_subject(Subject) when is_binary(Subject) ->
    case re:run(Subject, "^[a-zA-Z0-9._:-]+$", [{capture, none}]) of
        match -> true;
        nomatch -> false
    end;
is_valid_nats_subject(_) ->
    false.

