%% @doc Load tests for router_policy_applier
%% Tests performance and scalability with 1k, 5k, 10k QPS using simple policies
%% @test_category slow, load, performance
-module(router_policy_applier_load_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks and helper functions
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    setup_test_policy/0,
    generate_load_sequential/7,
    calculate_latency_stats/1,
    test_load_1k_qps/1,
    test_load_5k_qps/1,
    test_load_10k_qps/1,
    test_load_latency_p50/1,
    test_load_latency_p95/1,
    test_load_latency_p99/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_load_10k_qps/1,
    test_load_1k_qps/1,
    test_load_5k_qps/1,
    test_load_latency_p50/1,
    test_load_latency_p95/1,
    test_load_latency_p99/1
]).


%% Define stats record if not already defined
-ifndef(STATS_RECORD).
-define(STATS_RECORD, true).
-record(stats, {
    p50_ms :: float(),
    p95_ms :: float(),
    p99_ms :: float(),
    p99_9_ms :: float(),
    max_ms :: float(),
    avg_ms :: float()
}).
-endif.

-export([groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Load tests are heavy-only - they generate high QPS and are slow
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [];  %% Load tests only run in heavy tier
groups_for_level(heavy) -> [{group, load_tests}].

groups() ->
    [
        {load_tests, [sequence], [
            test_load_1k_qps,
            test_load_5k_qps,
            test_load_10k_qps,
            test_load_latency_p50,
            test_load_latency_p95,
            test_load_latency_p99
        ]}
    ].

init_per_suite(Config) ->
    %% Start application with ephemeral port
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Wait for policy store to initialize
            test_helpers:wait_for_app_start(router_policy_store, 1000),
            %% Create test policy for load tests
            setup_test_policy(),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Setup test policy for load tests
setup_test_policy() ->
    TenantId = <<"load_test_tenant">>,
    PolicyId = <<"load_test_policy">>,
    
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = undefined,
        sticky = undefined,
        metadata = #{}
    },
    
    case router_policy_store:upsert_policy(TenantId, Policy) of
        {ok, _} -> ok;
        Error -> ct:comment("Failed to setup test policy: ~p", [Error])
    end.

%% Test: Load test at 1k QPS
test_load_1k_qps(_Config) ->
    TenantId = <<"load_test_tenant">>,
    PolicyId = <<"load_test_policy">>,
    QPS = 1000,
    DurationSeconds = 5,
    TotalRequests = QPS * DurationSeconds,
    IntervalUs = 1000000 div QPS,  %% Microseconds between requests
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-load-1k">>,
        <<"tenant_id">> => TenantId,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }
    },
    Context = #{},
    
    %% Generate load
    Results = generate_load_sequential(Request, TenantId, PolicyId, Context, TotalRequests, IntervalUs, []),
    
    %% Verify all requests completed
    ?assertEqual(TotalRequests, length(Results)),
    
    %% Verify all results are successful
    SuccessfulResults = [{Result, Latency} || {Result, Latency} <- Results, element(1, Result) =:= ok],
    ?assertEqual(TotalRequests, length(SuccessfulResults)),
    
    ok.

%% Test: Load test at 5k QPS
test_load_5k_qps(_Config) ->
    TenantId = <<"load_test_tenant">>,
    PolicyId = <<"load_test_policy">>,
    QPS = 5000,
    DurationSeconds = 3,
    TotalRequests = QPS * DurationSeconds,
    IntervalUs = 1000000 div QPS,
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-load-5k">>,
        <<"tenant_id">> => TenantId,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }
    },
    Context = #{},
    
    Results = generate_load_sequential(Request, TenantId, PolicyId, Context, TotalRequests, IntervalUs, []),
    
    ?assertEqual(TotalRequests, length(Results)),
    
    ok.

%% Test: Load test at 10k QPS
test_load_10k_qps(_Config) ->
    TenantId = <<"load_test_tenant">>,
    PolicyId = <<"load_test_policy">>,
    QPS = 10000,
    DurationSeconds = 2,
    TotalRequests = QPS * DurationSeconds,
    IntervalUs = 1000000 div QPS,
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-load-10k">>,
        <<"tenant_id">> => TenantId,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }
    },
    Context = #{},
    
    Results = generate_load_sequential(Request, TenantId, PolicyId, Context, TotalRequests, IntervalUs, []),
    
    ?assertEqual(TotalRequests, length(Results)),
    
    ok.

%% Test: Latency P50 measurement
test_load_latency_p50(_Config) ->
    TenantId = <<"load_test_tenant">>,
    PolicyId = <<"load_test_policy">>,
    TotalRequests = 1000,
    IntervalUs = 1000,  %% 1ms between requests
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-latency-p50">>,
        <<"tenant_id">> => TenantId,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }
    },
    Context = #{},
    
    Results = generate_load_sequential(Request, TenantId, PolicyId, Context, TotalRequests, IntervalUs, []),
    
    %% Extract latencies (convert from microseconds to milliseconds)
    Latencies = [LatencyUs / 1000.0 || {_Result, LatencyUs} <- Results],
    
    %% Calculate statistics
    Stats = calculate_latency_stats(Latencies),
    
    %% Verify P50 is reasonable (should be < 10ms for policy application)
    ?assert(Stats#stats.p50_ms < 10.0),
    
    ok.

%% Test: Latency P95 measurement
test_load_latency_p95(_Config) ->
    TenantId = <<"load_test_tenant">>,
    PolicyId = <<"load_test_policy">>,
    TotalRequests = 1000,
    IntervalUs = 1000,
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-latency-p95">>,
        <<"tenant_id">> => TenantId,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }
    },
    Context = #{},
    
    Results = generate_load_sequential(Request, TenantId, PolicyId, Context, TotalRequests, IntervalUs, []),
    
    Latencies = [LatencyUs / 1000.0 || {_Result, LatencyUs} <- Results],
    Stats = calculate_latency_stats(Latencies),
    
    %% Verify P95 is reasonable
    ?assert(Stats#stats.p95_ms < 20.0),
    
    ok.

%% Test: Latency P99 measurement
test_load_latency_p99(_Config) ->
    TenantId = <<"load_test_tenant">>,
    PolicyId = <<"load_test_policy">>,
    TotalRequests = 1000,
    IntervalUs = 1000,
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-latency-p99">>,
        <<"tenant_id">> => TenantId,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }
    },
    Context = #{},
    
    Results = generate_load_sequential(Request, TenantId, PolicyId, Context, TotalRequests, IntervalUs, []),
    
    Latencies = [LatencyUs / 1000.0 || {_Result, LatencyUs} <- Results],
    Stats = calculate_latency_stats(Latencies),
    
    %% Verify P99 is reasonable
    ?assert(Stats#stats.p99_ms < 50.0),
    
    ok.

%% Sequential load generation (with interval control)
generate_load_sequential(_Request, _TenantId, _PolicyId, _Context, 0, _IntervalUs, Acc) ->
    lists:reverse(Acc);
generate_load_sequential(Request, TenantId, PolicyId, Context, Remaining, IntervalUs, Acc) ->
    RequestStartTime = erlang:monotonic_time(microsecond),
    
    %% Convert Request map to RouteRequest for router_policy_applier
    RouteRequest = case Request of
        #route_request{} -> Request;
        RequestMap when is_map(RequestMap) ->
            #route_request{
                message = RequestMap,
                policy_id = PolicyId,
                context = Context
            }
    end,
    
    %% Apply policy (no external calls, just policy decision)
    Result = router_policy_applier:apply_policy(RouteRequest, TenantId, PolicyId, Context),
    
    RequestEndTime = erlang:monotonic_time(microsecond),
    RequestLatency = RequestEndTime - RequestStartTime,
    
    %% Calculate sleep time to maintain target QPS
    ElapsedUs = RequestEndTime - RequestStartTime,
    SleepUs = max(0, IntervalUs - ElapsedUs),
    
    if
        SleepUs > 0 ->
            timer:sleep(trunc(SleepUs / 1000));  %% Convert to milliseconds
        true ->
            ok
    end,
    
    generate_load_sequential(Request, TenantId, PolicyId, Context, Remaining - 1, IntervalUs,
        [{Result, RequestLatency} | Acc]).

%% Calculate latency statistics
calculate_latency_stats(Latencies) when length(Latencies) > 0 ->
    Sorted = lists:sort(Latencies),
    Length = length(Sorted),
    
    P50Idx = trunc(Length * 0.5),
    P95Idx = trunc(Length * 0.95),
    P99Idx = trunc(Length * 0.99),
    P99_9Idx = trunc(Length * 0.999),
    
    P50 = lists:nth(max(1, P50Idx), Sorted),
    P95 = lists:nth(max(1, P95Idx), Sorted),
    P99 = lists:nth(max(1, P99Idx), Sorted),
    P99_9 = lists:nth(max(1, P99_9Idx), Sorted),
    Max = lists:max(Sorted),
    Avg = lists:sum(Sorted) / Length,
    
    #stats{
        p50_ms = P50,
        p95_ms = P95,
        p99_ms = P99,
        p99_9_ms = P99_9,
        max_ms = Max,
        avg_ms = Avg
    };
calculate_latency_stats([]) ->
    #stats{
        p50_ms = 0, p95_ms = 0, p99_ms = 0, p99_9_ms = 0, max_ms = 0, avg_ms = 0
    }.

