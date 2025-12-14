%% @doc Performance Load Tests
%%
%% Tests Router performance under load:
%% - 1000 sequential DecideRequest with push_assignment=true
%% - High concurrency scenarios (100+ concurrent requests)
%% - Sustained load (1 hour+)
%%
%% @test_category performance, load
-module(router_performance_load_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").
-include("flow_pb.hrl").

-compile([export_all, nowarn_export_all]).
-compile({nowarn_unused_function, [all/0]}).

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

%% Load tests only run in heavy tier
groups_for_level(heavy) ->
    [{group, load_tests}];
groups_for_level(_) -> %% fast, full, sanity
    [].

groups() ->
    [
        {load_tests, [sequence], [
            test_1000_sequential_requests,
            test_100_concurrent_requests,
            test_sustained_load
        ]}
    ].

init_per_suite(Config) ->
    %% Start Router application
    ok = router_suite_helpers:start_router_suite(),
    
    %% Create test policy
    TenantId = <<"test_tenant_perf">>,
    PolicyId = <<"test_policy_perf">>,
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
    {ok, _} = router_policy_store:upsert_policy(TenantId, PolicyId, Policy, undefined),
    
    [{tenant_id, TenantId}, {policy_id, PolicyId} | Config].

end_per_suite(Config) ->
    %% Cleanup
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    router_policy_store:delete_policy(TenantId, PolicyId, undefined),
    router_suite_helpers:stop_router_suite(),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = router_test_utils:ensure_circuit_breaker_alive(),
    ok = router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch meck:unload(router_nats),
    ok.

%% @doc Test: 1000 sequential DecideRequest with push_assignment=true
test_1000_sequential_requests(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Mock NATS to track assignment publishes - no passthrough to avoid noproc
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    AssignmentPublishCount = router_test_init:ensure_ets_table(assignment_publish_count, [named_table, set, private]),
    ets:insert(AssignmentPublishCount, {count, 0}),
    
    meck:expect(router_nats, publish, fun(Subject, _Payload) ->
        case binary:match(Subject, <<"caf.exec.assign">>) of
            nomatch -> ok;
            _ ->
                ets:update_counter(AssignmentPublishCount, count, 1, {count, 0}),
                ok
        end
    end),
    
    %% Enable push_assignment globally
    ok = application:set_env(beamline_router, caf_push_assignment_enabled, true),
    
    %% Create route request with push_assignment=true
    Message = #'Message'{
        message_id = <<"msg_perf_1">>,
        tenant_id = TenantId,
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = PolicyId,
        context = []
    },
    _ = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    _ = router_grpc_test_helper:create_context_without_auth(),
    
    %% Measure time
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Send 1000 sequential requests via decide consumer (which handles push_assignment)
    Subject = <<"beamline.router.v1.decide">>,
    Results = lists:map(fun(N) ->
        %% Create DecideRequest JSON with push_assignment=true
        DecideRequest = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => <<"req_perf_", (integer_to_binary(N))/binary>>,
            <<"trace_id">> => <<"tr_perf_", (integer_to_binary(N))/binary>>,
            <<"tenant_id">> => TenantId,
            <<"task">> => #{
                <<"type">> => <<"chat">>,
                <<"payload">> => <<"Hello">>
            },
            <<"policy_id">> => PolicyId,
            <<"push_assignment">> => true
        },
        RequestJson = jsx:encode(DecideRequest),
        MsgId = <<"msg_perf_", (integer_to_binary(N))/binary>>,
        
        try
            %% Process via decide consumer (handles push_assignment)
            router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
            timer:sleep(1),  %% Small delay to allow async processing
            {ok, processed}
        catch
            Class:Reason ->
                {error, {Class, Reason}}
        end
    end, lists:seq(1, 1000)),
    
    %% Wait for all async assignment publishes to complete
    timer:sleep(2000),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Get assignment publish count
    [{count, AssignmentCount}] = ets:lookup(AssignmentPublishCount, count),
    
    %% Calculate statistics
    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
    ErrorCount = 1000 - SuccessCount,
    AvgLatency = Duration / 1000,
    Throughput = case Duration > 0 of
        true -> 1000 / (Duration / 1000);
        false -> 0
    end,
    
    ct:pal("Performance Results:~n"
           "  Total requests: 1000~n"
           "  Successful: ~p~n"
           "  Errors: ~p~n"
           "  Total time: ~p ms~n"
           "  Average latency: ~p ms~n"
           "  Throughput: ~p requests/second~n"
           "  Assignment publishes: ~p~n",
           [SuccessCount, ErrorCount, Duration, AvgLatency, Throughput, AssignmentCount]),
    
    %% Verify most requests succeeded
    ?assert(SuccessCount >= 900),
    ?assert(AvgLatency < 100),
    ?assert(Throughput > 100),
    %% Verify assignments were published (at least 90% should have push_assignment=true)
    ?assert(AssignmentCount >= 900, "At least 900 assignments should be published"),
    
    %% Cleanup
    ets:delete_all_objects(AssignmentPublishCount),
    meck:unload(router_nats),
    ok.

%% @doc Test: High concurrency scenarios (100+ concurrent requests)
test_100_concurrent_requests(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Create route request
    Message = #'Message'{
        message_id = <<"msg_perf_2">>,
        tenant_id = TenantId,
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = PolicyId,
        context = []
    },
    _ = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Measure time
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Send 100 concurrent requests
    Parent = self(),
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            %% Update message_id for each request
            UpdatedMessage = Message#'Message'{message_id = <<"msg_perf_", (integer_to_binary(N))/binary>>},
            UpdatedRequestPb = RouteRequestPb#'RouteRequest'{message = UpdatedMessage},
            UpdatedRequest = flow_pb:encode_msg(UpdatedRequestPb, 'RouteRequest'),
            Result = try
                router_grpc:decide(Ctx, UpdatedRequest)
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end,
            Parent ! {result, Result}
        end)
    end, lists:seq(1, 100)),
    
    %% Wait for all results
    Results = lists:map(fun(_) ->
        receive
            {result, Result} -> Result
        after 5000 ->
            {error, timeout}
        end
    end, Pids),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Calculate statistics
    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
    ErrorCount = 100 - SuccessCount,
    AvgLatency = Duration / 100,
    Throughput = 100 / (Duration / 1000),  %% requests per second
    
    ct:pal("Concurrency Results:~n"
           "  Total requests: 100~n"
           "  Successful: ~p~n"
           "  Errors: ~p~n"
           "  Total time: ~p ms~n"
           "  Average latency: ~p ms~n"
           "  Throughput: ~p requests/second~n",
           [SuccessCount, ErrorCount, Duration, AvgLatency, Throughput]),
    
    %% Verify most requests succeeded
    ?assert(SuccessCount >= 90),
    ?assert(AvgLatency < 200),
    ?assert(Throughput > 50),
    ok.

%% @doc Test: Sustained load (1 hour+)
%% NOTE: This test runs for a shorter duration in CI (1 minute) to avoid timeout
test_sustained_load(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Determine test duration (1 minute in CI, 1 hour in manual runs)
    TestDuration = case os:getenv("CI") of
        "true" -> 60;  %% 1 minute in CI
        _ -> 3600      %% 1 hour in manual runs
    end,
    
    %% Create route request
    Message = #'Message'{
        message_id = <<"msg_perf_3">>,
        tenant_id = TenantId,
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = PolicyId,
        context = []
    },
    _ = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Measure time
    StartTime = erlang:monotonic_time(second),
    EndTime = StartTime + TestDuration,
    RequestCount = 0,
    SuccessCount = 0,
    ErrorCount = 0,
    
    %% Run sustained load
    {FinalRequestCount, FinalSuccessCount, FinalErrorCount} = 
        loop_until(EndTime, fun() ->
            UpdatedMessage = Message#'Message'{
                message_id = <<"msg_perf_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>
            },
            UpdatedRequestPb = RouteRequestPb#'RouteRequest'{message = UpdatedMessage},
            UpdatedRequest = flow_pb:encode_msg(UpdatedRequestPb, 'RouteRequest'),
            case try
                router_grpc:decide(Ctx, UpdatedRequest)
            catch
                _:_ -> {error, request_failed}
            end of
                {ok, _, _} -> {ok, 1, 0};
                _ -> {error, 1, 1}
            end
        end, {RequestCount, SuccessCount, ErrorCount}),
    
    ActualDuration = erlang:monotonic_time(second) - StartTime,
    Throughput = case ActualDuration > 0 of
        true -> FinalRequestCount / ActualDuration;
        false -> 0
    end,
    SuccessRate = case FinalRequestCount > 0 of
        true -> (FinalSuccessCount / FinalRequestCount) * 100;
        false -> 0
    end,
    
    ct:pal("Sustained Load Results:~n"
           "  Duration: ~p seconds~n"
           "  Total requests: ~p~n"
           "  Successful: ~p~n"
           "  Errors: ~p~n"
           "  Success rate: ~p%~n"
           "  Throughput: ~p requests/second~n",
           [ActualDuration, FinalRequestCount, FinalSuccessCount, FinalErrorCount, SuccessRate, Throughput]),
    
    %% Verify sustained performance
    ?assert(FinalRequestCount > 0),
    ?assert(SuccessRate >= 95.0),
    ?assert(Throughput > 10.0),
    ok.

%% Helper: Loop until end time
loop_until(EndTime, Fun, Acc) ->
    case erlang:monotonic_time(second) >= EndTime of
        true ->
            Acc;
        false ->
            case Fun() of
                {ok, ReqInc, SuccInc} ->
                    {ReqCount, SuccCount, ErrCount} = Acc,
                    loop_until(EndTime, Fun, {ReqCount + ReqInc, SuccCount + SuccInc, ErrCount});
                {error, ReqInc, ErrInc} ->
                    {ReqCount, SuccCount, ErrCount} = Acc,
                    loop_until(EndTime, Fun, {ReqCount + ReqInc, SuccCount, ErrCount + ErrInc})
            end
    end.
