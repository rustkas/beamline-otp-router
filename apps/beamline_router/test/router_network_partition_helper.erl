%% @doc Test helper for network partition test suites
%% Contains shared setup, mock functions, and verification utilities
%% @test_category test_helper
-module(router_network_partition_helper).

-export([
    get_metrics_snapshot/0,
    verify_network_partition_contracts/3,
    verify_maxdeliver_semantics/3,
    verify_redelivery_limits/3,
    verify_metrics_correctness/3,
    verify_data_guarantees/3,
    verify_latency_bounds/3,
    verify_packet_loss_tolerance/3,
    init_common_suite/1,
    end_common_suite/1,
    init_common_testcase/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% @doc Get metrics snapshot
-spec get_metrics_snapshot() -> map().
get_metrics_snapshot() ->
    router_metrics:ensure(),
    try
        Metrics = router_r10_metrics:dump_metrics(),
        lists:foldl(fun
            ({Key, Value}, Acc) when is_atom(Key) ->
                maps:put(Key, Value, Acc);
            ({{MetricName, _LabelsKey}, Value}, Acc) when is_atom(MetricName) ->
                Current = maps:get(MetricName, Acc, 0),
                maps:put(MetricName, Current + Value, Acc);
            (_, Acc) ->
                Acc
        end, #{}, Metrics)
    catch
        _:_ -> #{}
    end.

%% @doc Verify network partition contract invariants
-spec verify_network_partition_contracts(map(), map(), map()) -> ok.
verify_network_partition_contracts(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    verify_maxdeliver_semantics(InitialMetrics, FinalMetrics, ExpectedBehavior),
    verify_redelivery_limits(InitialMetrics, FinalMetrics, ExpectedBehavior),
    verify_metrics_correctness(InitialMetrics, FinalMetrics, ExpectedBehavior),
    verify_data_guarantees(InitialMetrics, FinalMetrics, ExpectedBehavior),
    verify_latency_bounds(InitialMetrics, FinalMetrics, ExpectedBehavior),
    verify_packet_loss_tolerance(InitialMetrics, FinalMetrics, ExpectedBehavior),
    ok.

%% @doc Verify MaxDeliver semantics
-spec verify_maxdeliver_semantics(map(), map(), map()) -> ok.
verify_maxdeliver_semantics(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    InitialMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, InitialMetrics, 0),
    FinalMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, FinalMetrics, 0),
    MaxDeliverExhaustedDelta = FinalMaxDeliverExhausted - InitialMaxDeliverExhausted,
    ExpectedMaxDeliverExhaustion = maps:get(expected_maxdeliver_exhaustion, ExpectedBehavior, 0),
    Tolerance = maps:get(maxdeliver_tolerance, ExpectedBehavior, 2),
    ?assert((abs(MaxDeliverExhaustedDelta - ExpectedMaxDeliverExhaustion) =< Tolerance)),
    ok.

%% @doc Verify redelivery limits
-spec verify_redelivery_limits(map(), map(), map()) -> ok.
verify_redelivery_limits(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    RedeliveryDelta = FinalRedelivery - InitialRedelivery,
    MaxExpectedRedelivery = maps:get(max_redelivery, ExpectedBehavior, 100),
    ?assert((RedeliveryDelta =< MaxExpectedRedelivery)),
    ok.

%% @doc Verify metrics correctness
-spec verify_metrics_correctness(map(), map(), map()) -> ok.
verify_metrics_correctness(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    InitialConnectionLost = maps:get(router_nats_connection_lost_total, InitialMetrics, 0),
    FinalConnectionLost = maps:get(router_nats_connection_lost_total, FinalMetrics, 0),
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    FinalConnectionFailures = maps:get(router_nats_connection_failures_total, FinalMetrics, 0),
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FinalPublishFailures = maps:get(router_nats_publish_failures_total, FinalMetrics, 0),
    PartitionInjected = maps:get(partition_injected, ExpectedBehavior, true),
    case PartitionInjected of
        true ->
            ?assert(FinalConnectionLost > InitialConnectionLost orelse
                    FinalConnectionFailures > InitialConnectionFailures orelse
                    FinalPublishFailures > InitialPublishFailures);
        false ->
            ok
    end,
    ok.

%% @doc Verify data guarantees (placeholder)
-spec verify_data_guarantees(map(), map(), map()) -> ok.
verify_data_guarantees(_InitialMetrics, _FinalMetrics, _ExpectedBehavior) ->
    ok.

%% @doc Verify latency bounds (placeholder)
-spec verify_latency_bounds(map(), map(), map()) -> ok.
verify_latency_bounds(_InitialMetrics, _FinalMetrics, _ExpectedBehavior) ->
    ok.

%% @doc Verify packet loss tolerance (placeholder)
-spec verify_packet_loss_tolerance(map(), map(), map()) -> ok.
verify_packet_loss_tolerance(_InitialMetrics, _FinalMetrics, _ExpectedBehavior) ->
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
    case router_network_partition_store:ensure() of
        ok -> ok;
        {error, Reason} -> ct:fail("init_per_suite: failed to ensure partition store: ~p", [Reason])
    end,
    Config.

%% @doc Common suite teardown
-spec end_common_suite(term()) -> term().
end_common_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    _ = router_network_partition_store:reset(),
    Config.

%% @doc Common testcase initialization
-spec init_common_testcase(term()) -> term().
init_common_testcase(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    case router_network_partition_store:reset() of
        ok -> ok;
        {error, Reason} -> ct:fail("init_per_testcase: failed to reset partition store: ~p", [Reason])
    end,
    Config.
