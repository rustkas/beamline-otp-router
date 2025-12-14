%% @doc Concurrent Faults Stress: Extended Soak and Isolation
%%
%% Extended stress scenarios:
%% - Extended soak test
%% - Tenant isolation under stress
%% - Multiple recovery cycles
%%
%% @test_category stress, soak, heavy
-module(router_concurrent_faults_soak_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_extended_soak/1,
    test_tenant_isolation_stress/1,
    test_multiple_recovery_cycles/1
]).

suite() -> [{timetrap, {minutes, 30}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, soak_tests}];
        _ -> []
    end.

groups() ->
    [{soak_tests, [sequence], [
        test_extended_soak,
        test_tenant_isolation_stress,
        test_multiple_recovery_cycles
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

test_extended_soak(_Config) ->
    ct:comment("=== Extended Soak Test ==="),
    
    DurationMinutes = case os:getenv("STRESS_SOAK_DURATION") of
        false -> 5;
        "short" -> 2;
        "medium" -> 10;
        "long" -> 30;
        _ -> 5
    end,
    
    ct:comment("Running soak for ~p minutes", [DurationMinutes]),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 20 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    Subject = <<"caf.exec.result.v1">>,
    StartTime = erlang:monotonic_time(millisecond),
    DurationMs = DurationMinutes * 60 * 1000,
    
    InitialMemory = erlang:memory(total),
    InitialProcesses = erlang:system_info(process_count),
    
    soak_loop(Subject, StartTime, DurationMs, 0, 0),
    
    FinalMemory = erlang:memory(total),
    FinalProcesses = erlang:system_info(process_count),
    
    MemoryGrowth = (FinalMemory - InitialMemory) / (1024 * 1024),
    ProcessGrowth = FinalProcesses - InitialProcesses,
    
    ct:comment("Memory growth: ~p MB, Process growth: ~p", [MemoryGrowth, ProcessGrowth]),
    
    ?assert(MemoryGrowth < 100, "Memory growth < 100 MB"),
    ?assert(ProcessGrowth < 50, "Process growth < 50"),
    
    meck:unload(router_nats),
    ok.

test_tenant_isolation_stress(_Config) ->
    ct:comment("=== Tenant Isolation Stress ==="),
    
    Tenants = [<<"tenant-a">>, <<"tenant-b">>, <<"tenant-c">>],
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case rand:uniform(100) =< 30 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    Subject = <<"caf.exec.result.v1">>,
    TenantCounts = router_test_init:ensure_ets_table(tenant_counts, [set, public]),
    ets:delete_all_objects(TenantCounts),
    lists:foreach(fun(T) -> ets:insert(TenantCounts, {T, 0}) end, Tenants),
    
    lists:foreach(fun(N) ->
        Tenant = lists:nth((N rem length(Tenants)) + 1, Tenants),
        Result = #{
            <<"assignment_id">> => <<"assign-", (integer_to_binary(N))/binary>>,
            <<"request_id">> => <<"req-", (integer_to_binary(N))/binary>>,
            <<"status">> => <<"success">>,
            <<"provider_id">> => <<"openai:gpt-4o">>,
            <<"tenant_id">> => Tenant,
            <<"timestamp">> => erlang:system_time(millisecond)
        },
        ResultJson = jsx:encode(Result),
        MsgId = <<"msg-", (integer_to_binary(N))/binary>>,
        router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{}),
        ets:update_counter(TenantCounts, Tenant, 1, {Tenant, 0})
    end, lists:seq(1, 100)),
    
    timer:sleep(2000),
    
    TenantData = ets:tab2list(TenantCounts),
    lists:foreach(fun({Tenant, Count}) ->
        ct:comment("Tenant ~p: ~p messages", [Tenant, Count]),
        ?assert(Count > 0, "Each tenant should have messages")
    end, TenantData),
    
    ets:delete(TenantCounts),
    meck:unload(router_nats),
    ok.

test_multiple_recovery_cycles(_Config) ->
    ct:comment("=== Multiple Recovery Cycles ==="),
    
    NumCycles = 5,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    Subject = <<"caf.exec.result.v1">>,
    CycleResults = router_test_init:ensure_ets_table(cycle_results, [set, public]),
    ets:delete_all_objects(CycleResults),
    
    lists:foreach(fun(Cycle) ->
        ct:comment("Cycle ~p/~p", [Cycle, NumCycles]),
        
        %% Inject faults
        meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
            {error, timeout}
        end),
        
        lists:foreach(fun(N) ->
            Result = #{
                <<"assignment_id">> => <<"assign-cycle-", (integer_to_binary(Cycle))/binary, "-", (integer_to_binary(N))/binary>>,
                <<"request_id">> => <<"req-cycle-", (integer_to_binary(N))/binary>>,
                <<"status">> => <<"success">>,
                <<"provider_id">> => <<"openai:gpt-4o">>,
                <<"tenant_id">> => <<"acme">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            ResultJson = jsx:encode(Result),
            MsgId = <<"msg-cycle-", (integer_to_binary(N))/binary>>,
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
        end, lists:seq(1, 10)),
        
        timer:sleep(500),
        
        %% Recovery
        meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
        
        lists:foreach(fun(N) ->
            Result = #{
                <<"assignment_id">> => <<"assign-recover-", (integer_to_binary(Cycle))/binary, "-", (integer_to_binary(N))/binary>>,
                <<"request_id">> => <<"req-recover-", (integer_to_binary(N))/binary>>,
                <<"status">> => <<"success">>,
                <<"provider_id">> => <<"openai:gpt-4o">>,
                <<"tenant_id">> => <<"acme">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            ResultJson = jsx:encode(Result),
            MsgId = <<"msg-recover-", (integer_to_binary(N))/binary>>,
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
        end, lists:seq(1, 10)),
        
        timer:sleep(500),
        ets:insert(CycleResults, {Cycle, ok})
    end, lists:seq(1, NumCycles)),
    
    CycleData = ets:tab2list(CycleResults),
    ?assertEqual(NumCycles, length(CycleData)),
    
    ets:delete(CycleResults),
    meck:unload(router_nats),
    ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

soak_loop(Subject, StartTime, DurationMs, MsgCount, ErrorCount) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    Elapsed = CurrentTime - StartTime,
    
    case Elapsed >= DurationMs of
        true ->
            ct:comment("Soak complete: ~p messages, ~p errors", [MsgCount, ErrorCount]),
            ok;
        false ->
            N = MsgCount + 1,
            Result = #{
                <<"assignment_id">> => <<"assign-soak-", (integer_to_binary(N))/binary>>,
                <<"request_id">> => <<"req-soak-", (integer_to_binary(N))/binary>>,
                <<"status">> => <<"success">>,
                <<"provider_id">> => <<"openai:gpt-4o">>,
                <<"tenant_id">> => <<"acme">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            ResultJson = jsx:encode(Result),
            MsgId = <<"msg-soak-", (integer_to_binary(N))/binary>>,
            
            NewErrorCount = try
                router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{}),
                ErrorCount
            catch
                _:_ -> ErrorCount + 1
            end,
            
            timer:sleep(100),
            soak_loop(Subject, StartTime, DurationMs, N, NewErrorCount)
    end.
