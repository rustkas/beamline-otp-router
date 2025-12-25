%% @doc Helper for triple-fault contract tests
%%
%% Shared functionality:
%% - Metrics snapshot
%% - Test message sending
%% - Contract invariant verification
%%
%% @test_category helper
-module(router_triple_fault_helper).

-export([
    init_suite/1,
    end_suite/1,
    init_testcase/1,
    end_testcase/1,
    get_metrics_snapshot/0,
    send_test_message/3,
    verify_contract_invariants/3
]).

%% @doc Initialize suite
init_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    router_metrics:ensure(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    Config.

init_testcase(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(router_metrics)
    end,
    Config.

end_testcase(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% @doc Get metrics snapshot
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
                    maps:put(MetricName, Current + Value, Acc);
                (_, Acc) -> Acc
            end, #{}, Metrics)
    end.

%% @doc Send test message
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

%% @doc Verify contract invariants
-spec verify_contract_invariants(map(), map(), map()) -> ok.
verify_contract_invariants(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    verify_maxdeliver_semantics(InitialMetrics, FinalMetrics, ExpectedBehavior),
    verify_redelivery_limits(InitialMetrics, FinalMetrics, ExpectedBehavior),
    ok.

verify_maxdeliver_semantics(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    InitialMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, InitialMetrics, 0),
    FinalMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, FinalMetrics, 0),
    MaxDeliverExhaustedDelta = FinalMaxDeliverExhausted - InitialMaxDeliverExhausted,
    ExpectedMaxDeliverExhaustion = maps:get(expected_maxdeliver_exhaustion, ExpectedBehavior, 0),
    Tolerance = maps:get(maxdeliver_tolerance, ExpectedBehavior, 2),
    true = (abs(MaxDeliverExhaustedDelta - ExpectedMaxDeliverExhaustion) =< Tolerance),
    ok.

verify_redelivery_limits(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    RedeliveryDelta = FinalRedelivery - InitialRedelivery,
    MaxExpectedRedelivery = maps:get(max_redelivery, ExpectedBehavior, 100),
    true = (RedeliveryDelta =< MaxExpectedRedelivery),
    ok.
