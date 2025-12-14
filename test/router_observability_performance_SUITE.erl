%% @doc Performance Test Suite for Router Observability (CP1)
%% Tests logging throughput, PII filtering latency, JSON serialization performance
%% @test_category performance, slow
-module(router_observability_performance_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_concurrent_logging_performance/1,
    test_json_serialization_performance/1,
    test_log_generation_throughput/1,
    test_memory_usage_during_logging/1,
    test_pii_filtering_performance/1
]).



all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

%% Performance tests only run in heavy tier
groups_for_level(heavy) ->
    [{group, performance_tests}];
groups_for_level(_) -> %% fast, full, sanity
    [].

groups() ->
    [
        {performance_tests, [parallel], [
            test_log_generation_throughput,
            test_pii_filtering_performance,
            test_json_serialization_performance,
            test_memory_usage_during_logging,
            test_concurrent_logging_performance
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, log_dir, "/tmp/router_perf_test_logs"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test log generation throughput (logs/second)
test_log_generation_throughput(_Config) ->
    NumLogs = 10000,
    Message = <<"Performance test message">>,
    _Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"trace_id">> => <<"trace_abc123">>,
        <<"run_id">> => <<"run_789">>
    },
    
    %% Measure time for generating logs
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(_) ->
        router_logger:info(Message, _Context)
    end, lists:seq(1, NumLogs)),
    
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMicroseconds = EndTime - StartTime,
    ElapsedSeconds = ElapsedMicroseconds / 1000000.0,
    LogsPerSecond = NumLogs / ElapsedSeconds,
    
    ct:comment("Generated ~p logs in ~.2f seconds (~.2f logs/second)", 
               [NumLogs, ElapsedSeconds, LogsPerSecond]),
    
    %% Assert: Should handle at least 1000 logs/second
    ?assert(LogsPerSecond >= 1000.0, 
            io_lib:format("Log throughput too low: ~.2f logs/second (expected >= 1000)", 
                         [LogsPerSecond])),
    
    ok.

%% @doc Test PII filtering performance (time per log entry)
test_pii_filtering_performance(_Config) ->
    NumLogs = 1000,
    Message = <<"Performance test with PII">>,
    Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"api_key">> => <<"secret_key_12345">>,
        <<"password">> => <<"password123">>,
        <<"email">> => <<"user@example.com">>,
        <<"token">> => <<"access_token_abc123">>,
        <<"nested">> => #{
            <<"secret">> => <<"nested_secret">>,
            <<"api_key">> => <<"nested_api_key">>
        }
    },
    
    %% Measure time for PII filtering
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(_) ->
        router_logger:info(Message, Context)
    end, lists:seq(1, NumLogs)),
    
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMicroseconds = EndTime - StartTime,
    TimePerLog = ElapsedMicroseconds / NumLogs,
    
    ct:comment("PII filtering: ~.2f microseconds per log entry (~p logs)", 
               [TimePerLog, NumLogs]),
    
    %% Assert: PII filtering should add less than 100 microseconds per log entry
    ?assert(TimePerLog < 100.0, 
            io_lib:format("PII filtering too slow: ~.2f microseconds per log (expected < 100)", 
                         [TimePerLog])),
    
    ok.

%% @doc Test JSON serialization performance
test_json_serialization_performance(_Config) ->
    NumLogs = 5000,
    Message = <<"JSON serialization performance test">>,
    Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"trace_id">> => <<"trace_abc123">>,
        <<"run_id">> => <<"run_789">>,
        <<"flow_id">> => <<"flow_456">>,
        <<"step_id">> => <<"step_123">>,
        <<"latency_ms">> => 250,
        <<"error_code">> => <<"ROUTER_ERROR_001">>,
        <<"context">> => #{
            <<"subject">> => <<"beamline.router.v1.decide">>,
            <<"policy_id">> => <<"policy_456">>,
            <<"provider">> => <<"openai">>
        }
    },
    
    %% Measure time for JSON serialization
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(_) ->
        router_logger:info(Message, Context)
    end, lists:seq(1, NumLogs)),
    
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMicroseconds = EndTime - StartTime,
    TimePerLog = ElapsedMicroseconds / NumLogs,
    
    ct:comment("JSON serialization: ~.2f microseconds per log entry (~p logs)", 
               [TimePerLog, NumLogs]),
    
    %% Assert: JSON serialization should be less than 50 microseconds per log entry
    ?assert(TimePerLog < 50.0, 
            io_lib:format("JSON serialization too slow: ~.2f microseconds per log (expected < 50)", 
                         [TimePerLog])),
    
    ok.

%% @doc Test memory usage during logging
test_memory_usage_during_logging(_Config) ->
    NumLogs = 1000,
    Message = <<"Memory usage test">>,
    Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"trace_id">> => <<"trace_abc123">>
    },
    
    %% Get initial memory
    {memory, InitialMemory} = erlang:process_info(self(), memory),
    
    %% Generate logs
    lists:foreach(fun(_) ->
        router_logger:info(Message, Context)
    end, lists:seq(1, NumLogs)),
    
    %% Force garbage collection
    garbage_collect(),
    
    %% Get final memory
    {memory, FinalMemory} = erlang:process_info(self(), memory),
    MemoryDelta = FinalMemory - InitialMemory,
    MemoryPerLog = MemoryDelta / NumLogs,
    
    ct:comment("Memory usage: ~p bytes delta (~.2f bytes per log)", 
               [MemoryDelta, MemoryPerLog]),
    
    %% Assert: Memory overhead should be reasonable (less than 1KB per log entry)
    ?assert(MemoryPerLog < 1024.0, 
            io_lib:format("Memory overhead too high: ~.2f bytes per log (expected < 1024)", 
                         [MemoryPerLog])),
    
    ok.

%% @doc Test concurrent logging performance (Erlang/OTP specific)
test_concurrent_logging_performance(_Config) ->
    NumProcesses = 10,
    LogsPerProcess = 1000,
    TotalLogs = NumProcesses * LogsPerProcess,
    Message = <<"Concurrent logging test">>,
    _Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"trace_id">> => <<"trace_abc123">>
    },
    
    %% Spawn concurrent processes
    StartTime = erlang:monotonic_time(microsecond),
    
    Pids = lists:map(fun(ProcessId) ->
        spawn(fun() ->
            lists:foreach(fun(_) ->
                router_logger:info(Message, #{
                    <<"tenant_id">> => <<"tenant_123">>,
                    <<"trace_id">> => <<"trace_abc123">>,
                    <<"process_id">> => integer_to_binary(ProcessId)
                })
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
    
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMicroseconds = EndTime - StartTime,
    ElapsedSeconds = ElapsedMicroseconds / 1000000.0,
    LogsPerSecond = TotalLogs / ElapsedSeconds,
    
    ct:comment("Concurrent logging: ~p processes, ~p logs each, ~.2f logs/second total", 
               [NumProcesses, LogsPerProcess, LogsPerSecond]),
    
    %% Assert: Concurrent logging should maintain at least 5000 logs/second
    ?assert(LogsPerSecond >= 5000.0, 
            io_lib:format("Concurrent logging throughput too low: ~.2f logs/second (expected >= 5000)", 
                         [LogsPerSecond])),
    
    ok.
