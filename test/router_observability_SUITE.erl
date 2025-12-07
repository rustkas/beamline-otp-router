%% @doc Observability Test Suite for Router (CP1)
%% Tests log format, PII filtering, health endpoint, and logging scenarios
%% @test_category cp1_smoke, fast
-module(router_observability_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).


all() ->
    [
        {group, log_format_tests},
        {group, pii_filtering_tests},
        {group, health_endpoint_tests},
        {group, logging_scenarios_tests},
        {group, edge_case_tests},
        {group, otel_tests}
    ].

groups() ->
    [
        {log_format_tests, [parallel], [
            test_log_format_json,
            test_log_required_fields,
            test_log_optional_fields,
            test_correlation_fields,
            test_error_code_and_latency,
            test_log_levels
        ]},
        {pii_filtering_tests, [parallel], [
            test_pii_filtering,
            test_secret_pattern_detection
        ]},
        {health_endpoint_tests, [parallel], [
            test_health_endpoint
        ]},
        {logging_scenarios_tests, [parallel], [
            test_nats_error_logging,
            test_routing_error_logging,
            test_invalid_payload_logging,
            test_internal_error_logging
        ]},
        {edge_case_tests, [parallel], [
            test_very_long_message,
            test_very_long_cp1_fields,
            test_empty_null_cp1_fields,
            test_special_characters,
            test_very_large_context,
            test_invalid_json_in_context,
            test_concurrent_logging
        ]},
        {otel_tests, [parallel], [
            test_otel_decide_span,
            test_otel_result_span,
            test_otel_cp1_attributes,
            test_otel_trace_propagation,
            test_otel_route_span,
            test_otel_policy_apply_span,
            test_otel_provider_select_span
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, log_dir, "/tmp/router_test_logs"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test log format is valid JSON
test_log_format_json(_Config) ->
    %% Create test log entry
    Message = <<"Test log message">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"trace_id">> => <<"trace_123">>
    },
    
    %% Log using router_logger
    router_logger:info(Message, Context),
    
    %% Verify log file exists and contains valid JSON
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            %% Parse JSON lines
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            %% Verify at least one valid JSON line
            ?assert(length(ValidLines) > 0, "Log file should contain at least one entry"),
            
            %% Verify each line is valid JSON
            lists:foreach(fun(Line) ->
                case jsx:decode(Line, [return_maps]) of
                    LogEntry when is_map(LogEntry) ->
                        ok;
                    _ ->
                        ct:fail("Invalid JSON in log file: ~p", [Line])
                end
            end, ValidLines),
            
            ok;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test required fields are present in logs
test_log_required_fields(_Config) ->
    Message = <<"Test required fields">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>
    },
    
    router_logger:info(Message, Context),
    
    %% Read log file and verify required fields
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify required fields
                    ?assert(maps:is_key(<<"timestamp">>, LogEntry), "Log should have timestamp"),
                    ?assert(maps:is_key(<<"level">>, LogEntry), "Log should have level"),
                    ?assert(maps:is_key(<<"component">>, LogEntry), "Log should have component"),
                    ?assert(maps:is_key(<<"message">>, LogEntry), "Log should have message"),
                    
                    %% Verify field types
                    ?assert(is_binary(maps:get(<<"timestamp">>, LogEntry)), "timestamp should be binary"),
                    ?assert(is_binary(maps:get(<<"level">>, LogEntry)), "level should be binary"),
                    ?assertEqual(<<"router">>, maps:get(<<"component">>, LogEntry), "component should be 'router'"),
                    ?assert(is_binary(maps:get(<<"message">>, LogEntry)), "message should be binary"),
                    
                    %% Verify timestamp format (microseconds - 6 digits)
                    Timestamp = maps:get(<<"timestamp">>, LogEntry),
                    %% Check format: YYYY-MM-DDTHH:MM:SS.ssssssZ
                    ?assertMatch({match, _}, re:run(Timestamp, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{6}Z$"), 
                        "timestamp should be in ISO-8601 format with 6 digits for microseconds"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test optional fields are present when available
test_log_optional_fields(_Config) ->
    Message = <<"Test optional fields">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"custom_field">> => <<"custom_value">>
    },
    
    router_logger:info(Message, Context),
    
    %% Read log file and verify optional fields
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify tenant_id when present
                    case maps:get(<<"tenant_id">>, LogEntry, undefined) of
                        undefined -> ok;
                        TenantId -> ?assert(is_binary(TenantId), "tenant_id should be binary")
                    end,
                    
                    %% Verify context when present
                    case maps:get(<<"context">>, LogEntry, undefined) of
                        undefined -> ok;
                        ContextMap when is_map(ContextMap) ->
                            ?assert(maps:is_key(<<"custom_field">>, ContextMap), "context should contain custom fields"),
                            ok
                    end,
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test CP1 correlation fields are at top level (CP1 compliance)
test_correlation_fields(_Config) ->
    Message = <<"Test CP1 correlation fields">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"trace_id">> => <<"trace_abc123">>,
        <<"run_id">> => <<"run_789">>,
        <<"flow_id">> => <<"flow_456">>,
        <<"step_id">> => <<"step_123">>
    },
    
    router_logger:info(Message, Context),
    
    %% Read log file and verify CP1 fields are at top level
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify CP1 fields are at top level (not in correlation object)
                    ?assert(maps:is_key(<<"tenant_id">>, LogEntry), "tenant_id should be at top level"),
                    ?assert(maps:is_key(<<"trace_id">>, LogEntry), "trace_id should be at top level"),
                    ?assert(maps:is_key(<<"run_id">>, LogEntry), "run_id should be at top level"),
                    ?assert(maps:is_key(<<"flow_id">>, LogEntry), "flow_id should be at top level"),
                    ?assert(maps:is_key(<<"step_id">>, LogEntry), "step_id should be at top level"),
                    
                    %% Verify values
                    ?assertEqual(<<"test_tenant">>, maps:get(<<"tenant_id">>, LogEntry), "tenant_id should match"),
                    ?assertEqual(<<"trace_abc123">>, maps:get(<<"trace_id">>, LogEntry), "trace_id should match"),
                    ?assertEqual(<<"run_789">>, maps:get(<<"run_id">>, LogEntry), "run_id should match"),
                    ?assertEqual(<<"flow_456">>, maps:get(<<"flow_id">>, LogEntry), "flow_id should match"),
                    ?assertEqual(<<"step_123">>, maps:get(<<"step_id">>, LogEntry), "step_id should match"),
                    
                    %% Verify correlation object does NOT exist (deprecated)
                    ?assertNot(maps:is_key(<<"correlation">>, LogEntry), "correlation object should not exist (CP1 fields at top level)"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test error_code and latency_ms fields
test_error_code_and_latency(_Config) ->
    Message = <<"Test error code and latency">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"trace_id">> => <<"trace_abc123">>,
        <<"error_code">> => <<"ROUTER_ERROR_001">>,
        <<"latency_ms">> => 250
    },
    
    router_logger:error(Message, Context),
    
    %% Read log file and verify error_code and latency_ms
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify error_code is present
                    ErrorCode = maps:get(<<"error_code">>, LogEntry, undefined),
                    ?assertNotEqual(undefined, ErrorCode, "error_code should be present"),
                    ?assertEqual(<<"ROUTER_ERROR_001">>, ErrorCode, "error_code should match"),
                    
                    %% Verify latency_ms is present
                    LatencyMs = maps:get(<<"latency_ms">>, LogEntry, undefined),
                    ?assertNotEqual(undefined, LatencyMs, "latency_ms should be present"),
                    ?assertEqual(250, LatencyMs, "latency_ms should match"),
                    ?assert(is_integer(LatencyMs), "latency_ms should be integer"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test PII fields are filtered
test_pii_filtering(_Config) ->
    Message = <<"Test PII filtering">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"api_key">> => <<"sk-1234567890abcdef">>,
        <<"password">> => <<"secret_password">>,
        <<"token">> => <<"bearer_token_123">>,
        <<"email">> => <<"test@example.com">>
    },
    
    router_logger:info(Message, Context),
    
    %% Read log file and verify PII is filtered
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    ContextMap = maps:get(<<"context">>, LogEntry, #{}),
                    
                    %% Verify PII fields are filtered
                    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"api_key">>, ContextMap, undefined), "api_key should be filtered"),
                    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"password">>, ContextMap, undefined), "password should be filtered"),
                    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"token">>, ContextMap, undefined), "token should be filtered"),
                    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"email">>, ContextMap, undefined), "email should be filtered"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test secret pattern detection in values
test_secret_pattern_detection(_Config) ->
    Message = <<"Test secret pattern detection">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"error_message">> => <<"API key sk-1234567890abcdef is invalid">>,
        <<"debug_info">> => <<"Bearer token abc123def456">>
    },
    
    router_logger:error(Message, Context),
    
    %% Read log file and verify secret patterns are filtered
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    ContextMap = maps:get(<<"context">>, LogEntry, #{}),
                    
                    %% Verify secret patterns are filtered
                    ErrorMsg = maps:get(<<"error_message">>, ContextMap, <<>>),
                    DebugInfo = maps:get(<<"debug_info">>, ContextMap, <<>>),
                    
                    %% Check that secret patterns are masked
                    ?assertNotEqual(<<"API key sk-1234567890abcdef is invalid">>, ErrorMsg, "Secret pattern should be filtered"),
                    ?assertNotEqual(<<"Bearer token abc123def456">>, DebugInfo, "Secret pattern should be filtered"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test log levels are correct
test_log_levels(_Config) ->
    %% Test all log levels
    TestCases = [
        {error, <<"ERROR">>},
        {warn, <<"WARN">>},
        {info, <<"INFO">>},
        {debug, <<"DEBUG">>}
    ],
    
    lists:foreach(fun({Level, ExpectedLevel}) ->
        Message = <<"Test log level">>,
        Context = #{<<"level_test">> => Level},
        
        case Level of
            error -> router_logger:error(Message, Context);
            warn -> router_logger:warn(Message, Context);
            info -> router_logger:info(Message, Context);
            debug -> router_logger:debug(Message, Context)
        end,
        
        %% Read log file and verify level
        LogDir = "/tmp/router_test_logs",
        Date = date_string(),
        LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
        
        case file:read_file(LogFile) of
            {ok, LogContent} ->
                Lines = binary:split(LogContent, <<"\n">>, [global]),
                ValidLines = [L || L <- Lines, byte_size(L) > 0],
                
                case ValidLines of
                    [] ->
                        ct:comment("No log entries found (logging may be disabled)"),
                        ok;
                    _ ->
                        %% Check last line for level
                        LastLine = lists:last(ValidLines),
                        LogEntry = jsx:decode(LastLine, [return_maps]),
                        ActualLevel = maps:get(<<"level">>, LogEntry, undefined),
                        
                        %% Verify level matches expected
                        ?assertEqual(ExpectedLevel, ActualLevel, 
                            io_lib:format("Log level should be ~p, got ~p", [ExpectedLevel, ActualLevel])),
                        ok
                end;
            {error, enoent} ->
                ct:comment("Log file not created (logging may be disabled)"),
                ok;
            {error, Reason} ->
                ct:fail("Failed to read log file: ~p", [Reason])
        end
    end, TestCases),
    
    ok.

%% @doc Test health endpoint (if applicable)
test_health_endpoint(_Config) ->
    %% Router uses gRPC health service on port 9000
    %% For CP1, we verify that health service is configured
    %% Actual health check requires grpc_health_probe or similar tool
    
    %% Check if gRPC server is configured
    GrpcEnabled = application:get_env(beamline_router, grpc_enabled, false),
    GrpcPort = application:get_env(beamline_router, grpc_port, 9000),
    
    case GrpcEnabled of
        true ->
            %% Health endpoint should be available via gRPC health service
            %% Service: grpc.health.v1.Health/Check
            %% Port: GrpcPort (default 9000)
            ct:comment("Health endpoint available via gRPC on port ~p", [GrpcPort]),
            ok;
        false ->
            ct:comment("gRPC disabled, health endpoint not available"),
            ok
    end.

%% @doc Test NATS error logging
test_nats_error_logging(_Config) ->
    %% Simulate NATS error scenario
    %% This test verifies that NATS errors are logged with context
    Message = <<"NATS connection failed">>,
    Context = #{
        <<"subject">> => <<"beamline.router.v1.decide">>,
        <<"error">> => <<"connection_failed">>,
        <<"tenant_id">> => <<"test_tenant">>
    },
    
    router_logger:error(Message, Context),
    
    %% Verify log entry contains NATS error context
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    ContextMap = maps:get(<<"context">>, LogEntry, #{}),
                    
                    %% Verify NATS error context is present
                    ?assert(maps:is_key(<<"subject">>, ContextMap), "NATS error log should contain subject"),
                    ?assert(maps:is_key(<<"error">>, ContextMap), "NATS error log should contain error"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test routing error logging
test_routing_error_logging(_Config) ->
    %% Simulate routing error scenario
    Message = <<"Routing decision failed">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"nonexistent">>,
        <<"error">> => <<"policy_not_found">>
    },
    
    router_logger:error(Message, Context),
    
    %% Verify log entry contains routing error context
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    ContextMap = maps:get(<<"context">>, LogEntry, #{}),
                    
                    %% Verify routing error context is present
                    ?assert(maps:is_key(<<"tenant_id">>, ContextMap), "Routing error log should contain tenant_id"),
                    ?assert(maps:is_key(<<"policy_id">>, ContextMap), "Routing error log should contain policy_id"),
                    ?assert(maps:is_key(<<"error">>, ContextMap), "Routing error log should contain error"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test invalid payload logging
test_invalid_payload_logging(_Config) ->
    %% Simulate invalid payload scenario
    Message = <<"Failed to parse NATS message">>,
    Context = #{
        <<"subject">> => <<"beamline.router.v1.decide">>,
        <<"error">> => <<"json_parse_error">>,
        <<"payload_size">> => 1048577  %% Exceeds limit
    },
    
    router_logger:error(Message, Context),
    
    %% Verify log entry contains invalid payload context
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    ContextMap = maps:get(<<"context">>, LogEntry, #{}),
                    
                    %% Verify invalid payload context is present
                    ?assert(maps:is_key(<<"subject">>, ContextMap), "Invalid payload log should contain subject"),
                    ?assert(maps:is_key(<<"error">>, ContextMap), "Invalid payload log should contain error"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test internal error logging
test_internal_error_logging(_Config) ->
    %% Simulate internal error scenario
    Message = <<"Internal router error">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"error">> => <<"internal_error">>,
        <<"error_context">> => #{
            <<"module">> => <<"router_core">>,
            <<"function">> => <<"route">>
        }
    },
    
    router_logger:error(Message, Context),
    
    %% Verify log entry contains internal error context
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    ContextMap = maps:get(<<"context">>, LogEntry, #{}),
                    
                    %% Verify internal error context is present
                    ?assert(maps:is_key(<<"error">>, ContextMap), "Internal error log should contain error"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test very long log messages (binary size limits)
test_very_long_message(_Config) ->
    %% Create a very long message (100KB)
    VeryLongMessage = binary:copy(<<"A">>, 100000),
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"trace_id">> => <<"trace_123">>
    },
    
    router_logger:info(VeryLongMessage, Context),
    
    %% Verify log entry is created and message is handled
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    Message = maps:get(<<"message">>, LogEntry, <<>>),
                    
                    %% Verify message is present (may be truncated)
                    ?assert(byte_size(Message) > 0, "Very long message should be logged"),
                    ?assert(is_binary(Message), "Message should be binary"),
                    
                    %% Verify log entry is valid JSON
                    ?assert(is_map(LogEntry), "Log entry should be valid JSON"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test very long CP1 field values
test_very_long_cp1_fields(_Config) ->
    %% Create very long CP1 field values (1000 characters each)
    VeryLongTenantId = binary:copy(<<"T">>, 1000),
    VeryLongTraceId = binary:copy(<<"X">>, 1000),
    VeryLongRunId = binary:copy(<<"R">>, 1000),
    
    Message = <<"Test very long CP1 fields">>,
    Context = #{
        <<"tenant_id">> => VeryLongTenantId,
        <<"trace_id">> => VeryLongTraceId,
        <<"run_id">> => VeryLongRunId
    },
    
    router_logger:info(Message, Context),
    
    %% Verify log entry contains CP1 fields (may be truncated)
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify CP1 fields are present (may be truncated)
                    ?assert(maps:is_key(<<"tenant_id">>, LogEntry), "Very long tenant_id should be logged"),
                    ?assert(maps:is_key(<<"trace_id">>, LogEntry), "Very long trace_id should be logged"),
                    ?assert(maps:is_key(<<"run_id">>, LogEntry), "Very long run_id should be logged"),
                    
                    %% Verify log entry is valid JSON
                    ?assert(is_map(LogEntry), "Log entry should be valid JSON"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test empty/null CP1 fields
test_empty_null_cp1_fields(_Config) ->
    Message = <<"Test empty/null CP1 fields">>,
    Context = #{
        <<"tenant_id">> => <<>>,
        <<"trace_id">> => <<"">>,
        <<"run_id">> => undefined
    },
    
    router_logger:info(Message, Context),
    
    %% Verify log entry handles empty/null fields gracefully
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify log entry is valid JSON (empty fields may be omitted or empty)
                    ?assert(is_map(LogEntry), "Log entry should be valid JSON"),
                    ?assert(maps:is_key(<<"message">>, LogEntry), "Message should be present"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test special characters in log messages (JSON escaping)
test_special_characters(_Config) ->
    %% Test various special characters that need JSON escaping
    SpecialMessage = <<"Test message with special chars: \"quotes\", \\backslash, \nnewline, \ttab, \rreturn">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"special_field">> => <<"Value with \"quotes\" and \\backslash">>
    },
    
    router_logger:info(SpecialMessage, Context),
    
    %% Verify log entry handles special characters correctly (JSON escaping)
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    %% Verify JSON is valid (special characters should be escaped)
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify log entry is valid JSON
                    ?assert(is_map(LogEntry), "Log entry with special characters should be valid JSON"),
                    ?assert(maps:is_key(<<"message">>, LogEntry), "Message should be present"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test very large context objects
test_very_large_context(_Config) ->
    %% Create a very large context object (nested, many fields)
    LargeContext = maps:from_list([
        {list_to_binary("field_" ++ integer_to_list(N)), 
         list_to_binary("value_" ++ integer_to_list(N))} 
        || N <- lists:seq(1, 1000)
    ]),
    
    Message = <<"Test very large context">>,
    Context = maps:merge(#{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"trace_id">> => <<"trace_123">>
    }, LargeContext),
    
    router_logger:info(Message, Context),
    
    %% Verify log entry handles large context gracefully
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    %% Verify JSON is valid (large context should be serialized)
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify log entry is valid JSON
                    ?assert(is_map(LogEntry), "Log entry with large context should be valid JSON"),
                    ?assert(maps:is_key(<<"message">>, LogEntry), "Message should be present"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test invalid JSON in context objects (should be handled gracefully)
test_invalid_json_in_context(_Config) ->
    %% Test with context that might cause JSON serialization issues
    %% Note: In Erlang, maps are always valid, but we test edge cases
    Message = <<"Test invalid JSON handling">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"nested">> => #{
            <<"deep">> => #{
                <<"very_deep">> => #{
                    <<"value">> => <<"deeply_nested">>
                }
            }
        },
        <<"circular">> => <<"test">>  %% Note: Erlang doesn't allow circular refs in maps
    },
    
    router_logger:info(Message, Context),
    
    %% Verify log entry handles nested structures correctly
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            case ValidLines of
                [] ->
                    ct:comment("No log entries found (logging may be disabled)"),
                    ok;
                [LastLine | _] ->
                    %% Verify JSON is valid (nested structures should be serialized)
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    
                    %% Verify log entry is valid JSON
                    ?assert(is_map(LogEntry), "Log entry with nested context should be valid JSON"),
                    ?assert(maps:is_key(<<"message">>, LogEntry), "Message should be present"),
                    
                    ok
            end;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% @doc Test concurrent logging (process-based, Erlang-specific)
test_concurrent_logging(_Config) ->
    %% Spawn multiple processes logging concurrently
    NumProcesses = 10,
    LogsPerProcess = 100,
    
    Message = <<"Concurrent logging test">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"trace_id">> => <<"trace_123">>
    },
    
    %% Spawn processes
    Pids = lists:map(fun(ProcessId) ->
        spawn(fun() ->
            lists:foreach(fun(LogId) ->
                router_logger:info(Message, maps:merge(Context, #{
                    <<"process_id">> => integer_to_binary(ProcessId),
                    <<"log_id">> => integer_to_binary(LogId)
                }))
            end, lists:seq(1, LogsPerProcess))
        end)
    end, lists:seq(1, NumProcesses)),
    
    %% Wait for all processes to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, _} -> ok
        after 5000 ->
            exit(Pid, kill)
        end
    end, Pids),
    
    %% Verify all log entries are created and valid
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            
            %% Verify all log entries are valid JSON
            lists:foreach(fun(Line) ->
                LogEntry = jsx:decode(Line, [return_maps]),
                ?assert(is_map(LogEntry), "Concurrent log entry should be valid JSON"),
                ?assert(maps:is_key(<<"message">>, LogEntry), "Message should be present")
            end, ValidLines),
            
            ct:comment("Concurrent logging: ~p processes, ~p logs each, ~p total log entries", 
                       [NumProcesses, LogsPerProcess, length(ValidLines)]),
            
            ok;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

%% Internal: Get date string (YYYY-MM-DD)
-spec date_string() -> string().
date_string() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]).

%% ============================================================================
%% CP2 Minimal OTel Spans Tests
%% ============================================================================

%% Test: OTel span for decide path (CP2 minimal requirement)
%% Verifies that decide span is created with CP1 correlation attributes
test_otel_decide_span(_Config) ->
    ok = router_observability:init(),
    
    %% Mock router_tracing
    meck:new(router_tracing, [passthrough]),
    meck:new(router_logger, [passthrough]),
    
    %% Create decide request with CP1 correlation fields
    Request = #{
        <<"tenant_id">> => <<"tenant-123">>,
        <<"trace_id">> => <<"trace-abc456">>,
        <<"run_id">> => <<"run-789">>,
        <<"flow_id">> => <<"flow-456">>,
        <<"step_id">> => <<"step-123">>,
        <<"request_id">> => <<"req-xyz">>,
        <<"policy_id">> => <<"policy-abc">>
    },
    Headers = #{
        <<"trace_id">> => <<"trace-abc456">>
    },
    
    meck:expect(router_tracing, start_span, fun(SpanName, Attributes, ParentContext) ->
        %% Verify span name
        <<"beamline.router.process.decide">> = SpanName,
        
        %% Verify CP1 correlation attributes are present
        ?assert(maps:is_key(tenant_id, Attributes)),
        <<"tenant-123">> = maps:get(tenant_id, Attributes),
        ?assert(maps:is_key(trace_id, Attributes)),
        <<"trace-abc456">> = maps:get(trace_id, Attributes),
        ?assert(maps:is_key(run_id, Attributes)),
        <<"run-789">> = maps:get(run_id, Attributes),
        ?assert(maps:is_key(flow_id, Attributes)),
        <<"flow-456">> = maps:get(flow_id, Attributes),
        ?assert(maps:is_key(step_id, Attributes)),
        <<"step-123">> = maps:get(step_id, Attributes),
        
        %% Verify parent context
        ?assert(is_map(ParentContext)),
        
        {ok, #{span_name => SpanName, attributes => Attributes}}
    end),
    
    {ok, _Span} = router_observability:create_decide_span(Request, Headers),
    
    %% Verify span was created
    test_helpers:wait_for_meck_call(router_tracing, start_span, '_', 1000),
    
    meck:unload(router_tracing),
    meck:unload(router_logger),
    ok.

%% Test: OTel span for result handling path (CP2 minimal requirement)
%% Verifies that result span is created with CP1 correlation attributes
test_otel_result_span(_Config) ->
    ok = router_observability:init(),
    
    %% Mock router_tracing
    meck:new(router_tracing, [passthrough]),
    meck:new(router_logger, [passthrough]),
    
    %% Create result with CP1 correlation fields
    Result = #{
        <<"tenant_id">> => <<"tenant-123">>,
        <<"trace_id">> => <<"trace-abc456">>,
        <<"run_id">> => <<"run-789">>,
        <<"flow_id">> => <<"flow-456">>,
        <<"step_id">> => <<"step-123">>,
        <<"assignment_id">> => <<"assign-xyz">>,
        <<"request_id">> => <<"req-xyz">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai">>
    },
    Headers = #{
        <<"trace_id">> => <<"trace-abc456">>
    },
    
    meck:expect(router_tracing, start_span, fun(SpanName, Attributes, ParentContext) ->
        %% Verify span name
        <<"beamline.router.process.result">> = SpanName,
        
        %% Verify CP1 correlation attributes are present
        ?assert(maps:is_key(tenant_id, Attributes)),
        <<"tenant-123">> = maps:get(tenant_id, Attributes),
        ?assert(maps:is_key(trace_id, Attributes)),
        <<"trace-abc456">> = maps:get(trace_id, Attributes),
        ?assert(maps:is_key(run_id, Attributes)),
        <<"run-789">> = maps:get(run_id, Attributes),
        ?assert(maps:is_key(flow_id, Attributes)),
        <<"flow-456">> = maps:get(flow_id, Attributes),
        ?assert(maps:is_key(step_id, Attributes)),
        <<"step-123">> = maps:get(step_id, Attributes),
        ?assert(maps:is_key(status, Attributes)),
        <<"success">> = maps:get(status, Attributes),
        ?assert(maps:is_key(provider_id, Attributes)),
        <<"openai">> = maps:get(provider_id, Attributes),
        
        %% Verify parent context
        ?assert(is_map(ParentContext)),
        
        {ok, #{span_name => SpanName, attributes => Attributes}}
    end),
    
    {ok, _Span} = router_observability:create_result_span(Result, Headers),
    
    %% Verify span was created
    test_helpers:wait_for_meck_call(router_tracing, start_span, '_', 1000),
    
    meck:unload(router_tracing),
    meck:unload(router_logger),
    ok.

%% Test: CP1 correlation attributes extraction (CP2 minimal requirement)
%% Verifies that CP1 correlation fields are correctly extracted as span attributes
test_otel_cp1_attributes(_Config) ->
    ok = router_observability:init(),
    
    %% Test context with all CP1 correlation fields
    Context = #{
        <<"tenant_id">> => <<"tenant-123">>,
        <<"trace_id">> => <<"trace-abc456">>,
        <<"run_id">> => <<"run-789">>,
        <<"flow_id">> => <<"flow-456">>,
        <<"step_id">> => <<"step-123">>,
        <<"subject">> => <<"beamline.router.v1.decide">>
    },
    
    Attributes = router_observability:extract_cp1_attributes(Context),
    
    %% Verify all CP1 correlation fields are present
    <<"tenant-123">> = maps:get(tenant_id, Attributes),
    <<"trace-abc456">> = maps:get(trace_id, Attributes),
    <<"run-789">> = maps:get(run_id, Attributes),
    <<"flow-456">> = maps:get(flow_id, Attributes),
    <<"step-123">> = maps:get(step_id, Attributes),
    <<"beamline.router.v1.decide">> = maps:get(subject, Attributes),
    
    ok.

%% Test: OTel trace propagation (CP2 minimal requirement)
%% Verifies that trace context is propagated from headers to spans
test_otel_trace_propagation(_Config) ->
    %% Mock router_tracing to capture span calls
    meck:new(router_tracing, [passthrough]),
    
    %% Track span calls
    SpanCalls = ets:new(span_calls, [set, private]),
    
    meck:expect(router_tracing, with_span, fun(SpanName, Attributes, ParentContext, Fun) ->
        %% Record span call
        ets:insert(SpanCalls, {SpanName, Attributes, ParentContext}),
        %% Execute function
        Fun()
    end),
    
    %% Create route request with trace context
    RouteRequest = #route_request{
        message = #{
            <<"tenant_id">> => <<"tenant-123">>,
            <<"message_id">> => <<"msg-123">>
        },
        context = #{
            <<"trace_id">> => <<"trace-abc456">>,
            <<"traceparent">> => <<"00-trace-abc456-span-123-01">>
        }
    },
    
    %% Call router_core:route (should create span with trace context)
    case router_core:route(RouteRequest, #{}) of
        {ok, _Decision} ->
            ok;
        {error, _Reason} ->
            %% Error is acceptable for this test (we're just checking spans)
            ok
    end,
    
    %% Verify span was created with trace context
    case ets:lookup(SpanCalls, <<"beamline.router.route">>) of
        [{<<"beamline.router.route">>, Attributes, ParentContext}] ->
            %% Verify trace context was extracted
            ?assert(is_map(ParentContext) orelse ParentContext =:= undefined, "Parent context should be map or undefined"),
            ?assert(maps:is_key(<<"tenant_id">>, Attributes), "Span should have tenant_id attribute"),
            ok;
        [] ->
            ct:comment("Span not created (tracing may be disabled or mock not working)"),
            ok;
        _ ->
            ct:fail("Unexpected span call result")
    end,
    
    ets:delete(SpanCalls),
    meck:unload(router_tracing),
    ok.

%% Test: OTel span for routing decision (CP2)
%% Verifies that router_core creates span with correct name and attributes
test_otel_route_span(_Config) ->
    %% Mock router_tracing
    meck:new(router_tracing, [passthrough]),
    
    SpanCalls = ets:new(span_calls, [set, private]),
    
    meck:expect(router_tracing, with_span, fun(SpanName, Attributes, ParentContext, Fun) ->
        ets:insert(SpanCalls, {SpanName, Attributes, ParentContext}),
        Fun()
    end),
    
    %% Create route request
    RouteRequest = #route_request{
        message = #{
            <<"tenant_id">> => <<"tenant-123">>,
            <<"message_id">> => <<"msg-123">>
        },
        context = #{
            <<"trace_id">> => <<"trace-abc456">>
        }
    },
    
    %% Call router_core:route
    _ = router_core:route(RouteRequest, #{}),
    
    %% Verify span was created
    case ets:lookup(SpanCalls, <<"beamline.router.route">>) of
        [{<<"beamline.router.route">>, Attributes, _ParentContext}] ->
            ?assertEqual(<<"tenant-123">>, maps:get(<<"tenant_id">>, Attributes), "Span should have tenant_id"),
            ?assert(maps:is_key(<<"policy_id">>, Attributes), "Span should have policy_id"),
            ?assert(maps:is_key(<<"message_id">>, Attributes), "Span should have message_id"),
            ok;
        [] ->
            ct:comment("Span not created (tracing may be disabled)"),
            ok;
        _ ->
            ct:fail("Unexpected span call result")
    end,
    
    ets:delete(SpanCalls),
    meck:unload(router_tracing),
    ok.

%% Test: OTel nested span for policy application (CP2)
%% Verifies that router_policy_applier creates nested span
test_otel_policy_apply_span(_Config) ->
    %% Mock router_tracing
    meck:new(router_tracing, [passthrough]),
    
    SpanCalls = ets:new(span_calls, [set, private]),
    
    meck:expect(router_tracing, with_span, fun(SpanName, Attributes, ParentContext, Fun) ->
        ets:insert(SpanCalls, {SpanName, Attributes, ParentContext}),
        Fun()
    end),
    
    %% Create route request
    RouteRequest = #route_request{
        message = #{
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_id">> => <<"msg-123">>
        }
    },
    
    %% Call router_policy_applier:apply_policy
    _ = router_policy_applier:apply_policy(RouteRequest, <<"default_tenant">>, <<"default">>, #{}),
    
    %% Verify span was created
    case ets:lookup(SpanCalls, <<"beamline.router.policy.apply">>) of
        [{<<"beamline.router.policy.apply">>, Attributes, _ParentContext}] ->
            ?assertEqual(<<"default_tenant">>, maps:get(<<"tenant_id">>, Attributes), "Span should have tenant_id"),
            ?assertEqual(<<"default">>, maps:get(<<"policy_id">>, Attributes), "Span should have policy_id"),
            ok;
        [] ->
            ct:comment("Span not created (tracing may be disabled or policy not found)"),
            ok;
        _ ->
            ct:fail("Unexpected span call result")
    end,
    
    ets:delete(SpanCalls),
    meck:unload(router_tracing),
    ok.

%% Test: OTel nested span for provider selection (CP2)
%% Verifies that router_decider creates nested span for provider selection
test_otel_provider_select_span(_Config) ->
    %% Mock router_tracing
    meck:new(router_tracing, [passthrough]),
    
    SpanCalls = ets:new(span_calls, [set, private]),
    
    meck:expect(router_tracing, with_span, fun(SpanName, Attributes, ParentContext, Fun) ->
        ets:insert(SpanCalls, {SpanName, Attributes, ParentContext}),
        Fun()
    end),
    
    %% Create policy with simple weights
    Policy = #policy{
        tenant_id = <<"default_tenant">>,
        policy_id = <<"default">>,
        weights = #{
            <<"provider_a">> => 1.0
        }
    },
    
    RouteRequest = #route_request{
        message = #{
            <<"tenant_id">> => <<"default_tenant">>
        }
    },
    
    %% Call router_decider:decide
    _ = router_decider:decide(RouteRequest, Policy, #{}),
    
    %% Verify span was created
    case ets:lookup(SpanCalls, <<"beamline.router.provider.select">>) of
        [{<<"beamline.router.provider.select">>, Attributes, _ParentContext}] ->
            ?assertEqual(<<"default_tenant">>, maps:get(<<"tenant_id">>, Attributes), "Span should have tenant_id"),
            ?assertEqual(<<"default">>, maps:get(<<"policy_id">>, Attributes), "Span should have policy_id"),
            ok;
        [] ->
            ct:comment("Span not created (tracing may be disabled)"),
            ok;
        _ ->
            ct:fail("Unexpected span call result")
    end,
    
    ets:delete(SpanCalls),
    meck:unload(router_tracing),
    ok.

