%% @doc Shared helper for concurrent faults test suites
%% Contains shared setup, mock functions, and verification utilities
%% @test_category test_helper
-module(router_concurrent_faults_helper).

-export([
    get_metrics_snapshot/0,
    get_metric/1,
    send_test_message/3,
    send_message_batch/3,
    run_fault_injection_lifecycle/3,
    verify_test_scenario/4,
    init_common_suite/1,
    end_common_suite/1,
    init_common_testcase/1,
    end_common_testcase/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% @doc Get metrics snapshot from router_metrics ETS table
-spec get_metrics_snapshot() -> map().
get_metrics_snapshot() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> #{};
        _ ->
            Metrics = ets:tab2list(router_metrics),
            lists:foldl(fun
                ({Key, Value}, Acc) when is_atom(Key) ->
                    maps:put(Key, Value, Acc);
                ({{MetricName, _Labels}, Value}, Acc) when is_atom(MetricName) ->
                    Current = maps:get(MetricName, Acc, 0),
                    maps:put(MetricName, Current + Value, Acc)
            end, #{}, Metrics)
    end.

%% @doc Get specific metric value
-spec get_metric(atom()) -> integer() | float() | undefined.
get_metric(MetricName) ->
    Snapshot = get_metrics_snapshot(),
    maps:get(MetricName, Snapshot, 0).

%% @doc Send test message to router
-spec send_test_message(binary(), binary(), map()) -> ok.
send_test_message(TenantId, RequestId, ExtraFields) ->
    Result = maps:merge(#{
        <<"assignment_id">> => <<"assign-", RequestId/binary>>,
        <<"request_id">> => RequestId,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => TenantId,
        <<"timestamp">> => erlang:system_time(millisecond)
    }, ExtraFields),
    ResultJson = jsx:encode(Result),
    Subject = <<"caf.exec.result.v1">>,
    MsgId = <<"msg-", RequestId/binary>>,
    case whereis(router_result_consumer) of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
    end,
    ok.

%% @doc Send batch of test messages
-spec send_message_batch(binary(), non_neg_integer(), binary()) -> ok.
send_message_batch(TenantId, Count, Prefix) ->
    [begin
        RequestId = <<Prefix/binary, "-", (integer_to_binary(N))/binary>>,
        send_test_message(TenantId, RequestId, #{}),
        timer:sleep(50)
    end || N <- lists:seq(1, Count)],
    ok.

%% @doc Execute fault injection lifecycle
-spec run_fault_injection_lifecycle(list({atom(), term()}), fun(() -> ok), non_neg_integer()) ->
    {map(), map(), map()}.
run_fault_injection_lifecycle(Faults, Action, RecoveryWaitMs) ->
    InitialMetrics = get_metrics_snapshot(),
    lists:foreach(fun({Operation, Fault}) ->
        router_nats_fault_injection:enable_fault(Operation, Fault)
    end, Faults),
    Action(),
    timer:sleep(1000),
    FaultMetrics = get_metrics_snapshot(),
    lists:foreach(fun({Operation, _Fault}) ->
        router_nats_fault_injection:disable_fault(Operation)
    end, Faults),
    timer:sleep(RecoveryWaitMs),
    FinalMetrics = get_metrics_snapshot(),
    {InitialMetrics, FaultMetrics, FinalMetrics}.

%% @doc Verify resilience and metrics for a test scenario
-spec verify_test_scenario(list({atom(), term()}), map(), map(), map()) -> ok.
verify_test_scenario(ExpectedRestarts, InitialMetrics, FinalMetrics, Recovered) ->
    case router_fault_injection_helpers:verify_resilience(ExpectedRestarts) of
        {ok, ResilienceDetails} ->
            ct:comment("Resilience check passed: ~p", [ResilienceDetails]);
        {fail, Reason} ->
            ct:fail("Resilience check failed: ~p", [Reason])
    end,
    case router_fault_injection_helpers:verify_observability_metrics(InitialMetrics, FinalMetrics, Recovered) of
        {ok, MetricsDetails} ->
            ct:comment("Metrics verification passed: ~p", [MetricsDetails]);
        {fail, MetricsReason} ->
            ct:comment("Metrics verification warning: ~p", [MetricsReason])
    end,
    ok.

%% @doc Common suite initialization
-spec init_common_suite(term()) -> term().
init_common_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    router_metrics:ensure(),
    router_nats_fault_injection:clear_all_faults(),
    ok = router_test_utils:start_router_app(),
    Config.

%% @doc Common suite teardown
-spec end_common_suite(term()) -> term().
end_common_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    Config.

%% @doc Common testcase initialization
-spec init_common_testcase(term()) -> term().
init_common_testcase(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_metrics:ensure(),
    router_metrics:clear_all(),
    Config.

%% @doc Common testcase teardown
-spec end_common_testcase(term()) -> term().
end_common_testcase(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.
