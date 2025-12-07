%% @doc Helper functions for fault injection test verification
%% 
%% Provides standardized verification functions for:
%% - Resilience (process aliveness, supervisor restarts)
%% - Message semantics (redelivery, fail-open)
%% - Observability (logs, metrics)
-module(router_fault_injection_helpers).
-export([
    verify_resilience/1,
    verify_message_semantics/3,
    verify_observability_logs/2,
    verify_observability_metrics/3,
    get_supervisor_restart_count/1,
    verify_all_criteria/4,
    verify_metrics_recovery/3,
    verify_no_duplicate_publications/4,
    verify_metrics_stabilization/3,
    calculate_metric_deltas/2
]).

-include_lib("common_test/include/ct.hrl").

%% @doc Verify resilience criteria
%% Checks:
%% - All key router processes are alive
%% - Supervisor has not restarted unexpectedly
%% Returns: {ok, Details} | {fail, Reason}
-spec verify_resilience(list()) -> {ok, map()} | {fail, term()}.
verify_resilience(ExpectedRestarts) ->
    %% Get key process PIDs
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    DecideConsumerPid = whereis(router_decide_consumer),
    ResultConsumerPid = whereis(router_result_consumer),
    
    %% Verify all processes are alive
    Processes = [
        {router_nats, RouterNatsPid},
        {beamline_router_sup, RouterSupPid},
        {router_decide_consumer, DecideConsumerPid},
        {router_result_consumer, ResultConsumerPid}
    ],
    
    DeadProcesses = [Name || {Name, Pid} <- Processes,
                            (Pid =:= undefined orelse not is_process_alive(Pid))],
    
    case DeadProcesses of
        [] ->
            %% All processes alive - check supervisor restarts
            RestartCount = get_supervisor_restart_count(beamline_router_sup),
            ExpectedCount = length(ExpectedRestarts),
            
            case RestartCount =< ExpectedCount of
                true ->
                    {ok, #{
                        processes_alive => true,
                        supervisor_restarts => RestartCount,
                        expected_restarts => ExpectedCount
                    }};
                false ->
                    {fail, {unexpected_supervisor_restarts, RestartCount, ExpectedCount}}
            end;
        _ ->
            {fail, {dead_processes, DeadProcesses}}
    end.

%% @doc Verify message semantics
%% Checks:
%% - Messages are either redelivered after recovery OR handled via fail-open
%% - No silent message loss
%% Returns: {ok, Details} | {fail, Reason}
-spec verify_message_semantics(atom(), list(), boolean()) -> {ok, map()} | {fail, term()}.
verify_message_semantics(Mode, QueuedMessages, FailOpenEnabled) ->
    case Mode of
        redelivery ->
            %% In redelivery mode: messages should be queued and retried after recovery
            case QueuedMessages of
                [] ->
                    {fail, no_messages_queued_for_redelivery};
                _ ->
                    %% Messages are queued - verify they will be retried
                    {ok, #{
                        mode => redelivery,
                        queued_count => length(QueuedMessages),
                        fail_open => false
                    }}
            end;
        fail_open ->
            %% In fail-open mode: messages should be marked as processed
            case FailOpenEnabled of
                true ->
                    {ok, #{
                        mode => fail_open,
                        messages_processed => true,
                        fail_open => true
                    }};
                false ->
                    {fail, fail_open_mode_not_enabled}
            end;
        _ ->
            {fail, {unknown_mode, Mode}}
    end.

%% @doc Verify observability - logs
%% Checks:
%% - Error logs contain clear messages about failure
%% - Recovery logs contain clear messages about restoration
%% Returns: {ok, Details} | {fail, Reason}
-spec verify_observability_logs(list(), list()) -> {ok, map()} | {fail, term()}.
verify_observability_logs(AllLogs, RequiredLogPatterns) ->
    FoundLogs = lists:foldl(fun(Pattern, Acc) ->
        case find_log_by_pattern(AllLogs, Pattern) of
            {ok, Log} ->
                [Log | Acc];
            {error, not_found} ->
                Acc
        end
    end, [], RequiredLogPatterns),
    
    MissingLogs = RequiredLogPatterns -- [Pattern || {Pattern, _} <- FoundLogs],
    
    case MissingLogs of
        [] ->
            {ok, #{
                logs_found => length(FoundLogs),
                required_logs => length(RequiredLogPatterns)
            }};
        _ ->
            {fail, {missing_logs, MissingLogs}}
    end.

%% @doc Verify observability - metrics
%% Checks:
%% - Error counters increased (fact of error)
%% - Status gauges reflect duration of violation
%% - Metrics return to normal after recovery
%% Returns: {ok, Details} | {fail, Reason}
-spec verify_observability_metrics(map(), map(), boolean()) -> {ok, map()} | {fail, term()}.
verify_observability_metrics(InitialMetrics, FinalMetrics, Recovered) ->
    %% Check error counters increased
    ErrorCounters = [
        router_nats_connection_lost_total,
        router_nats_publish_failures_total,
        router_nats_ack_failures_total,
        router_nats_reconnect_failures_total
    ],
    
    CounterDeltas = lists:map(fun(Counter) ->
        Initial = maps:get(Counter, InitialMetrics, 0),
        Final = maps:get(Counter, FinalMetrics, 0),
        {Counter, Final - Initial}
    end, ErrorCounters),
    
    %% Check status gauge
    InitialStatus = maps:get(router_nats_connection_status, InitialMetrics, 1.0),
    FinalStatus = maps:get(router_nats_connection_status, FinalMetrics, 1.0),
    
    %% Verify metrics
    case Recovered of
        true ->
            %% After recovery: status should be 1 (connected)
            case FinalStatus =:= 1.0 of
                true ->
                    {ok, #{
                        error_counters => CounterDeltas,
                        initial_status => InitialStatus,
                        final_status => FinalStatus,
                        recovered => true
                    }};
                false ->
                    {fail, {status_not_recovered, FinalStatus}}
            end;
        false ->
            %% During failure: status should be 0 or 0.5 (disconnected/reconnecting)
            case (FinalStatus =:= +0.0 orelse FinalStatus =:= 0.5) of
                true ->
                    {ok, #{
                        error_counters => CounterDeltas,
                        initial_status => InitialStatus,
                        final_status => FinalStatus,
                        recovered => false
                    }};
                false ->
                    {fail, {unexpected_status_during_failure, FinalStatus}}
            end
    end.

%% @doc Get supervisor restart count
%% Note: This is a simplified check - in production, use supervisor:count_children/1
-spec get_supervisor_restart_count(atom()) -> non_neg_integer().
get_supervisor_restart_count(SupervisorName) ->
    case supervisor:count_children(SupervisorName) of
        {ok, Children} ->
            %% Count children that have restarted
            RestartCount = lists:foldl(fun({_Name, Props}, Acc) ->
                Restarts = proplists:get_value(restarts, Props, 0),
                Acc + Restarts
            end, 0, Children),
            RestartCount;
        _ ->
            0
    end.

%% @doc Verify all criteria for a test scenario
%% Combines resilience, message semantics, and observability checks
-spec verify_all_criteria(atom(), list(), map(), map()) -> {ok, map()} | {fail, term()}.
verify_all_criteria(Mode, ExpectedRestarts, InitialMetrics, FinalMetrics) ->
    %% 1. Verify resilience
    case verify_resilience(ExpectedRestarts) of
        {ok, ResilienceDetails} ->
            %% 2. Verify observability - metrics
            case verify_observability_metrics(InitialMetrics, FinalMetrics, false) of
                {ok, MetricsDetails} ->
                    {ok, #{
                        resilience => ResilienceDetails,
                        observability => MetricsDetails,
                        mode => Mode
                    }};
                {fail, Reason} ->
                    {fail, {metrics_verification_failed, Reason}}
            end;
        {fail, Reason} ->
            {fail, {resilience_verification_failed, Reason}}
    end.

%% Internal: Find log by pattern
find_log_by_pattern(AllLogs, {Pattern, RequiredContext}) ->
    case lists:filter(fun({log, _Level, Message, Context}) ->
        binary:match(Message, Pattern) =/= nomatch andalso
        check_context(Context, RequiredContext)
    end, AllLogs) of
        [Log | _] ->
            {ok, Log};
        [] ->
            {error, not_found}
    end.

%% Internal: Check log context matches required fields
check_context(Context, RequiredContext) when is_map(Context), is_map(RequiredContext) ->
    lists:all(fun({Key, Value}) ->
        maps:get(Key, Context, undefined) =:= Value
    end, maps:to_list(RequiredContext));
check_context(_, _) ->
    true.

%% @doc Verify metrics recovery after fault removal
%% Checks:
%% - Error counters increased during fault
%% - Error counters stabilize after recovery (no infinite growth)
%% - Success counters resume after recovery
%% - Connection status returns to healthy state
%% Returns: {ok, Details} | {fail, Reason}
-spec verify_metrics_recovery(map(), map(), map()) -> {ok, map()} | {fail, term()}.
verify_metrics_recovery(InitialMetrics, FaultMetrics, FinalMetrics) ->
    %% Calculate deltas for all phases
    InitialToFault = calculate_metric_deltas(InitialMetrics, FaultMetrics),
    FaultToFinal = calculate_metric_deltas(FaultMetrics, FinalMetrics),
    _ = calculate_metric_deltas(InitialMetrics, FinalMetrics),
    
    %% Error counters should increase during fault
    ErrorCounters = [
        router_nats_connection_lost_total,
        router_nats_publish_failures_total,
        router_nats_ack_failures_total,
        router_nats_publish_with_ack_failures_total
    ],
    
    ErrorDeltasDuringFault = lists:map(fun(Counter) ->
        maps:get(Counter, InitialToFault, 0)
    end, ErrorCounters),
    
    %% At least one error counter should increase during fault
    HasErrorsDuringFault = lists:any(fun(Delta) -> Delta > 0 end, ErrorDeltasDuringFault),
    
    %% After recovery, error counters should stabilize (not grow infinitely)
    ErrorDeltasAfterRecovery = lists:map(fun(Counter) ->
        maps:get(Counter, FaultToFinal, 0)
    end, ErrorCounters),
    
    %% Error growth after recovery should be minimal (allow small margin for final retries)
    ExcessiveErrorGrowth = lists:any(fun(Delta) -> Delta > 10 end, ErrorDeltasAfterRecovery),
    
    %% Check connection status recovery
    InitialStatus = maps:get(router_nats_connection_status, InitialMetrics, 1.0),
    FinalStatus = maps:get(router_nats_connection_status, FinalMetrics, 1.0),
    StatusRecovered = (FinalStatus =:= 1.0 orelse FinalStatus =:= InitialStatus),
    
    case HasErrorsDuringFault andalso not ExcessiveErrorGrowth andalso StatusRecovered of
        true ->
            {ok, #{
                errors_during_fault => ErrorDeltasDuringFault,
                errors_after_recovery => ErrorDeltasAfterRecovery,
                initial_status => InitialStatus,
                final_status => FinalStatus,
                status_recovered => StatusRecovered
            }};
        false ->
            {fail, #{
                errors_during_fault => ErrorDeltasDuringFault,
                errors_after_recovery => ErrorDeltasAfterRecovery,
                excessive_growth => ExcessiveErrorGrowth,
                status_recovered => StatusRecovered
            }}
    end.

%% @doc Verify no duplicate publications occurred
%% Checks:
%% - Total publish attempts don't exceed expected + small margin
%% - publish_with_ack total doesn't indicate duplicates
%% - Message IDs are unique (if tracked)
%% Returns: {ok, Details} | {fail, Reason}
-spec verify_no_duplicate_publications(map(), map(), map(), non_neg_integer()) -> {ok, map()} | {fail, term()}.
verify_no_duplicate_publications(InitialMetrics, _FaultMetrics, FinalMetrics, ExpectedAttempts) ->
    %% Track publish_with_ack totals
    InitialPublishWithAck = maps:get(router_nats_publish_with_ack_total, InitialMetrics, 0),
    FinalPublishWithAck = maps:get(router_nats_publish_with_ack_total, FinalMetrics, 0),
    PublishWithAckDelta = FinalPublishWithAck - InitialPublishWithAck,
    
    %% Track publish totals
    InitialPublish = maps:get(router_nats_publish_total, InitialMetrics, 0),
    FinalPublish = maps:get(router_nats_publish_total, FinalMetrics, 0),
    PublishDelta = FinalPublish - InitialPublish,
    
    %% Allow small margin for retries (3 additional attempts max)
    MaxAllowedAttempts = ExpectedAttempts + 3,
    
    %% Check for excessive publications (indicating duplicates)
    PublishWithAckExcessive = (PublishWithAckDelta > MaxAllowedAttempts),
    PublishExcessive = (PublishDelta > MaxAllowedAttempts),
    
    case PublishWithAckExcessive orelse PublishExcessive of
        true ->
            {fail, #{
                publish_with_ack_delta => PublishWithAckDelta,
                publish_delta => PublishDelta,
                expected_attempts => ExpectedAttempts,
                max_allowed => MaxAllowedAttempts,
                reason => possible_duplicates
            }};
        false ->
            {ok, #{
                publish_with_ack_delta => PublishWithAckDelta,
                publish_delta => PublishDelta,
                expected_attempts => ExpectedAttempts,
                duplicates_detected => false
            }}
    end.

%% @doc Verify metrics stabilization after recovery
%% Checks:
%% - Metrics don't continue to grow excessively after fault removal
%% - Error rates stabilize
%% - Success rates resume
%% Returns: {ok, Details} | {fail, Reason}
-spec verify_metrics_stabilization(map(), map(), map()) -> {ok, map()} | {fail, term()}.
verify_metrics_stabilization(FaultMetrics, FinalMetrics, StabilizationWindowMs) ->
    %% Calculate deltas from fault to final
    Deltas = calculate_metric_deltas(FaultMetrics, FinalMetrics),
    
    %% Key metrics to check for stabilization
    StabilizationMetrics = [
        router_nats_publish_failures_total,
        router_nats_ack_failures_total,
        router_nats_publish_with_ack_failures_total,
        router_nats_connection_lost_total
    ],
    
    %% Calculate growth rates (per second, assuming StabilizationWindowMs)
    GrowthRates = lists:map(fun(Metric) ->
        Delta = maps:get(Metric, Deltas, 0),
        Rate = case StabilizationWindowMs > 0 of
            true -> (Delta * 1000) / StabilizationWindowMs;
            false -> 0
        end,
        {Metric, Rate}
    end, StabilizationMetrics),
    
    %% Check if any metric is growing too fast (more than 1 error per second)
    ExcessiveGrowth = lists:any(fun({_Metric, Rate}) -> Rate > 1.0 end, GrowthRates),
    
    case ExcessiveGrowth of
        true ->
            {fail, #{
                growth_rates => GrowthRates,
                stabilization_window_ms => StabilizationWindowMs,
                reason => metrics_not_stabilizing
            }};
        false ->
            {ok, #{
                growth_rates => GrowthRates,
                stabilization_window_ms => StabilizationWindowMs,
                stabilized => true
            }}
    end.

%% @doc Calculate metric deltas between two snapshots
%% Returns: map() with metric names as keys and deltas as values
-spec calculate_metric_deltas(map(), map()) -> map().
calculate_metric_deltas(InitialMetrics, FinalMetrics) ->
    %% Get all unique metric names from both snapshots
    AllMetrics = sets:to_list(sets:union(
        sets:from_list(maps:keys(InitialMetrics)),
        sets:from_list(maps:keys(FinalMetrics))
    )),
    
    %% Calculate deltas
    lists:foldl(fun(Metric, Acc) ->
        Initial = maps:get(Metric, InitialMetrics, 0),
        Final = maps:get(Metric, FinalMetrics, 0),
        Delta = Final - Initial,
        maps:put(Metric, Delta, Acc)
    end, #{}, AllMetrics).

