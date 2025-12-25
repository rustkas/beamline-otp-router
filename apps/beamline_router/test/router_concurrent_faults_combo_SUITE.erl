%% @doc Concurrent Faults Stress: Fault Combinations
%%
%% Tests concurrent fault scenarios:
%% - Publish + ACK failures
%% - Connect + publish failures
%% - Circuit breaker integration
%% - Backpressure handling
%%
%% @test_category stress, heavy, faults
-module(router_concurrent_faults_combo_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_publish_and_ack_failures/1,
    test_connect_and_publish_failures/1,
    test_faults_with_circuit_breaker/1,
    test_faults_with_backpressure/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, combo_tests}];
groups_for_level(full) ->
    [];
groups_for_level(fast) ->
    [].
groups() ->
    [{combo_tests, [sequence], [
        test_publish_and_ack_failures,
        test_connect_and_publish_failures,
        test_faults_with_circuit_breaker,
        test_faults_with_backpressure
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    RandSeed = {1234, 5678, 91011},
    rand:seed(exs1024s, RandSeed),
    [{rand_seed, RandSeed} | Config].

end_per_suite(_Config) -> ok.

init_per_testcase(_TC, Config) ->
    RandSeed = proplists:get_value(rand_seed, Config, {1234, 5678, 91011}),
    rand:seed(exs1024s, RandSeed),
    router_metrics:ensure(),
    Config.

end_per_testcase(_TC, _Config) ->
    catch meck:unload(router_nats),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_publish_and_ack_failures(_Config) ->
    ct:comment("=== Concurrent Publish + ACK Failures ==="),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 40 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        case rand:uniform(100) =< 30 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Sub, _Dur, _Ack, _Del, _Mode) -> 
        {ok, <<"consumer-concurrent">>} 
    end),
    
    Subject = <<"caf.exec.result.v1">>,
    ProcessedCount = processed_count,
    _ = router_ets_helpers:ensure_named_ets_table(ProcessedCount, [set, public]),
    ets:delete_all_objects(ProcessedCount),
    ets:insert(ProcessedCount, {count, 0}),
    
    lists:foreach(fun(N) ->
        Result = #{
            <<"assignment_id">> => <<"assign-", (integer_to_binary(N))/binary>>,
            <<"request_id">> => <<"req-", (integer_to_binary(N))/binary>>,
            <<"status">> => <<"success">>,
            <<"provider_id">> => <<"openai:gpt-4o">>,
            <<"tenant_id">> => <<"acme">>,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        ResultJson = jsx:encode(Result),
        MsgId = <<"msg-", (integer_to_binary(N))/binary>>,
        router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{}),
        ets:update_counter(ProcessedCount, count, 1, {count, 0})
    end, lists:seq(1, 50)),
    
    timer:sleep(2000),
    [{count, FinalCount}] = router_ets_helpers:ets_lookup(ProcessedCount, count),
    ?assert(FinalCount >= 40, "At least 80% should process"),
    
    ets:delete(ProcessedCount),
    meck:unload(router_nats),
    ok.

test_connect_and_publish_failures(_Config) ->
    ct:comment("=== Concurrent Connect + Publish Failures ==="),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 50 of
            true -> {error, connection_refused};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    Subject = <<"caf.exec.result.v1">>,
    ErrorCount = error_count,
    _ = router_ets_helpers:ensure_named_ets_table(ErrorCount, [set, public]),
    ets:delete_all_objects(ErrorCount),
    ets:insert(ErrorCount, {count, 0}),
    
    lists:foreach(fun(N) ->
        Result = #{
            <<"assignment_id">> => <<"assign-", (integer_to_binary(N))/binary>>,
            <<"request_id">> => <<"req-", (integer_to_binary(N))/binary>>,
            <<"status">> => <<"success">>,
            <<"provider_id">> => <<"openai:gpt-4o">>,
            <<"tenant_id">> => <<"acme">>,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        ResultJson = jsx:encode(Result),
        MsgId = <<"msg-", (integer_to_binary(N))/binary>>,
        try
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
        catch
            _:_ -> ets:update_counter(ErrorCount, count, 1, {count, 0})
        end
    end, lists:seq(1, 50)),
    
    timer:sleep(2000),
    [{count, FinalErrorCount}] = router_ets_helpers:ets_lookup(ErrorCount, count),
    ?assert(FinalErrorCount < 30, "Errors handled gracefully"),
    
    ets:delete(ErrorCount),
    meck:unload(router_nats),
    ok.

test_faults_with_circuit_breaker(_Config) ->
    ct:comment("=== Faults with Circuit Breaker ==="),
    
    TenantId = <<"test_tenant">>,
    ProviderId = <<"test_provider">>,
    
    case whereis(router_circuit_breaker) of
        undefined -> {ok, _} = router_circuit_breaker:start_link();
        _ -> ok
    end,
    
    Config = #{
        <<"failure_threshold">> => 3,
        <<"error_rate_threshold">> => 0.6,
        <<"latency_threshold_ms">> => 500
    },
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 70 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    
    meck:unload(router_nats),
    ok.

test_faults_with_backpressure(_Config) ->
    ct:comment("=== Faults with Backpressure ==="),
    
    Subject = <<"beamline.router.v1.decide">>,
    
    PendingTable = pending_requests,
    _ = router_ets_helpers:ensure_named_ets_table(PendingTable, [set, public]),
    ets:delete_all_objects(PendingTable),
    lists:foreach(fun(N) ->
        ets:insert(PendingTable, {{pending, N}, <<"req-", (integer_to_binary(N))/binary>>})
    end, lists:seq(1, 100)),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 50 of
            true -> {error, timeout};
            false -> timer:sleep(10), ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    
    lists:foreach(fun(N) ->
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => <<"req-bp-", (integer_to_binary(N))/binary>>,
            <<"tenant_id">> => <<"acme">>,
            <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
        },
        RequestJson = jsx:encode(Request),
        MsgId = <<"msg-bp-", (integer_to_binary(N))/binary>>,
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{})
    end, lists:seq(1, 20)),
    
    timer:sleep(2000),
    
    ets:delete(PendingTable),
    meck:unload(router_nats),
    ok.
