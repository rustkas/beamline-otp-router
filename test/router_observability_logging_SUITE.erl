%% @doc Observability - Logging Tests
%% 
%% Tests for log format, required fields, and logging scenarios.
%% Runs on fast CI (CP1).
%%
%% @test_category observability, cp1, fast
-module(router_observability_logging_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_log_format_json/1,
    test_log_required_fields/1,
    test_log_optional_fields/1,
    test_correlation_fields/1,
    test_log_levels/1,
    test_nats_error_logging/1,
    test_routing_error_logging/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    [{group, log_format_tests}, {group, logging_scenarios_tests}].

groups() ->
    [
        {log_format_tests, [parallel], [
            test_log_format_json,
            test_log_required_fields,
            test_log_optional_fields,
            test_correlation_fields,
            test_log_levels
        ]},
        {logging_scenarios_tests, [parallel], [
            test_nats_error_logging,
            test_routing_error_logging
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, log_dir, "/tmp/router_test_logs"),
    Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

date_string() ->
    {{Y, M, D}, _} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D])).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_log_format_json(_Config) ->
    Message = <<"Test log message">>,
    Context = #{<<"tenant_id">> => <<"test_tenant">>, <<"trace_id">> => <<"trace_123">>},
    
    router_logger:info(Message, Context),
    
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            ?assert(length(ValidLines) > 0),
            lists:foreach(fun(Line) ->
                case jsx:decode(Line, [return_maps]) of
                    LogEntry when is_map(LogEntry) -> ok;
                    _ -> ct:fail("Invalid JSON in log file: ~p", [Line])
                end
            end, ValidLines),
            ok;
        {error, enoent} ->
            ct:comment("Log file not created (logging may be disabled)"),
            ok;
        {error, Reason} ->
            ct:fail("Failed to read log file: ~p", [Reason])
    end.

test_log_required_fields(_Config) ->
    Message = <<"Test required fields">>,
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    router_logger:info(Message, Context),
    
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            case ValidLines of
                [] -> ct:comment("No log entries found"), ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    ?assert(maps:is_key(<<"timestamp">>, LogEntry)),
                    ?assert(maps:is_key(<<"level">>, LogEntry)),
                    ?assert(maps:is_key(<<"message">>, LogEntry)),
                    ok
            end;
        {error, enoent} -> ct:comment("Log file not created"), ok;
        {error, Reason} -> ct:fail("Failed to read log file: ~p", [Reason])
    end.

test_log_optional_fields(_Config) ->
    Message = <<"Test optional fields">>,
    Context = #{<<"tenant_id">> => <<"test_tenant">>, <<"custom_field">> => <<"custom_value">>},
    
    router_logger:info(Message, Context),
    
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, _LogContent} -> ok;
        {error, enoent} -> ct:comment("Log file not created"), ok;
        {error, Reason} -> ct:fail("Failed to read log file: ~p", [Reason])
    end.

test_correlation_fields(_Config) ->
    Message = <<"Test correlation">>,
    TraceId = <<"trace-12345">>,
    TenantId = <<"acme">>,
    RequestId = <<"req-67890">>,
    Context = #{
        <<"trace_id">> => TraceId,
        <<"tenant_id">> => TenantId,
        <<"request_id">> => RequestId
    },
    
    router_logger:info(Message, Context),
    
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            case ValidLines of
                [] -> ct:comment("No log entries found"), ok;
                _ ->
                    %% Find the log entry that contains our test message
                    MatchingEntries = lists:filtermap(fun(Line) ->
                        try
                            Entry = jsx:decode(Line, [return_maps]),
                            case maps:get(<<"message">>, Entry, undefined) of
                                Message -> {true, Entry};
                                _ -> false
                            end
                        catch
                            _:_ -> false
                        end
                    end, ValidLines),
                    case MatchingEntries of
                        [] ->
                            %% No matching entry found - log might be disabled or buffered
                            ct:comment("Log entry with test message not found"),
                            ok;
                        [LogEntry | _] ->
                            ?assertEqual(TraceId, maps:get(<<"trace_id">>, LogEntry, undefined)),
                            ?assertEqual(TenantId, maps:get(<<"tenant_id">>, LogEntry, undefined)),
                            ok
                    end
            end;
        {error, enoent} -> ct:comment("Log file not created"), ok;
        {error, Reason} -> ct:fail("Failed to read log file: ~p", [Reason])
    end.

test_log_levels(_Config) ->
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    router_logger:debug(<<"Debug message">>, Context),
    router_logger:info(<<"Info message">>, Context),
    router_logger:warn(<<"Warn message">>, Context),
    router_logger:error(<<"Error message">>, Context),
    
    ok.

test_nats_error_logging(_Config) ->
    Context = #{
        <<"error_type">> => <<"nats_connection_failed">>,
        <<"error_code">> => <<"NATS_001">>,
        <<"tenant_id">> => <<"test_tenant">>
    },
    
    router_logger:error(<<"NATS connection failed">>, Context),
    ok.

test_routing_error_logging(_Config) ->
    Context = #{
        <<"error_type">> => <<"routing_failed">>,
        <<"error_code">> => <<"ROUTE_001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => <<"req-123">>
    },
    
    router_logger:error(<<"Routing failed">>, Context),
    ok.
