%%%-------------------------------------------------------------------
%%% @doc
%%% R10 Circuit Breaker CLI commands for router_ctl
%%% @end
%%%-------------------------------------------------------------------
-module(router_ctl_r10).

-export([status/2, help/0]).

-type tenant_id() :: binary().
-type provider_id() :: binary().

-define(RUNBOOK_URL, "https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md").

%%--------------------------------------------------------------------
%% @doc
%% Get R10 circuit breaker status for a tenant/provider pair
%% Usage: router_ctl r10 status <tenant> <provider>
%% @end
%%--------------------------------------------------------------------
-spec status(tenant_id(), provider_id()) -> ok.
status(TenantId, ProviderId) ->
    %% Ensure router application is started
    ok = ensure_router_started(),
    
    io:format("~n╔══════════════════════════════════════════════════════════════╗~n", []),
    io:format("║  R10 Circuit Breaker Status                                    ║~n", []),
    io:format("╚══════════════════════════════════════════════════════════════╝~n", []),
    io:format("~nTenant ID:  ~s~n", [TenantId]),
    io:format("Provider ID: ~s~n", [ProviderId]),
    io:format("~n"),
    
    %% Get detailed status
    StatusRes = router_circuit_breaker:get_status(TenantId, ProviderId),
    case StatusRes of
        {ok, StatusMap} ->
            print_detailed_status(StatusMap, TenantId, ProviderId);
        {error, not_found} ->
            io:format("Status: not_found (no circuit state yet, treated as closed)~n", []),
            io:format("~nThis circuit breaker has not been initialized yet.~n", []),
            io:format("It will be created automatically on first request.~n", []),
            io:format("~nRunbook: ~s~n", [?RUNBOOK_URL]),
            ok;
        Other ->
            io:format("ERROR: Failed to get status: ~p~n", [Other]),
            erlang:halt(1)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Print help message for R10 commands
%% @end
%%--------------------------------------------------------------------
-spec help() -> ok.
help() ->
    io:format("~nR10 Circuit Breaker Commands:~n", []),
    io:format("  r10 status <tenant> <provider>  - Show circuit breaker status~n", []),
    io:format("  r10 help                        - Show this help message~n", []),
    io:format("~nRunbook: ~s~n", [?RUNBOOK_URL]),
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ensure_router_started() ->
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> ok;
        {error, {_App, Reason}} ->
            io:format("ERROR: Failed to start beamline_router: ~p~n", [Reason]),
            erlang:halt(1)
    end.

print_detailed_status(StatusMap, TenantId, ProviderId) ->
    State = maps:get(state, StatusMap),
    StateChangedAt = maps:get(state_changed_at, StatusMap),
    HalfOpenCalls = maps:get(half_open_calls_count, StatusMap),
    FailureCount = maps:get(failure_count, StatusMap),
    SuccessCount = maps:get(success_count, StatusMap),
    Config = maps:get(config, StatusMap),
    
    %% Format timestamp
    Now = erlang:system_time(millisecond),
    ElapsedMs = Now - StateChangedAt,
    ElapsedSeconds = ElapsedMs / 1000,
    StateChangedTime = format_timestamp(StateChangedAt),
    
    %% Print state with visual indicator
    io:format("┌─ State ─────────────────────────────────────────────────────┐~n", []),
    print_state_with_indicator(State),
    io:format("│ Last state change: ~s (~.1f seconds ago)~n", [StateChangedTime, ElapsedSeconds]),
    io:format("└──────────────────────────────────────────────────────────────┘~n", []),
    io:format("~n"),
    
    %% Print counters
    io:format("┌─ Counters ──────────────────────────────────────────────────┐~n", []),
    io:format("│ Consecutive failures: ~p~n", [FailureCount]),
    io:format("│ Consecutive successes: ~p~n", [SuccessCount]),
    case State of
        half_open ->
            io:format("│ Half-open probe attempts: ~p~n", [HalfOpenCalls]),
            MaxAttempts = maps:get(<<"half_open_max_attempts">>, Config, 3),
            io:format("│ Half-open max attempts: ~p~n", [MaxAttempts]);
        _ ->
            ok
    end,
    io:format("└──────────────────────────────────────────────────────────────┘~n", []),
    io:format("~n"),
    
    %% Print metrics
    io:format("┌─ Metrics ───────────────────────────────────────────────────┐~n", []),
    
    %% Trigger reason
    TriggerRes = router_r10_metrics:get_latest_trigger_reason(TenantId, ProviderId),
    print_trigger_reason(TriggerRes),
    
    %% Error rate
    ErrorRateValue = router_r10_metrics:get_metric_value(
                       router_circuit_breaker_error_rate,
                       #{tenant_id => TenantId, provider_id => ProviderId}
                      ),
    ErrorRate = if
                    ErrorRateValue =:= 0 -> undefined;
                    true -> ErrorRateValue
                end,
    print_error_rate(ErrorRate),
    
    %% Timeout remaining (for open state)
    case State of
        open ->
            TimeoutRemainingValue = router_r10_metrics:get_metric_value(
                                      router_circuit_breaker_timeout_remaining_ms,
                                      #{tenant_id => TenantId, provider_id => ProviderId}
                                     ),
            TimeoutRemaining = if
                                  TimeoutRemainingValue =:= 0 -> undefined;
                                  true -> TimeoutRemainingValue
                              end,
            print_timeout_remaining(TimeoutRemaining);
        _ ->
            ok
    end,
    
    io:format("└──────────────────────────────────────────────────────────────┘~n", []),
    io:format("~n"),
    
    %% Print configuration
    io:format("┌─ Configuration ──────────────────────────────────────────────┐~n", []),
    FailureThreshold = maps:get(<<"failure_threshold">>, Config, 5),
    ErrorRateThreshold = maps:get(<<"error_rate_threshold">>, Config, 0.5),
    TimeoutMs = maps:get(<<"timeout_ms">>, Config, 60000),
    LatencyThresholdMs = maps:get(<<"latency_threshold_ms">>, Config, 5000),
    io:format("│ Failure threshold: ~p consecutive failures~n", [FailureThreshold]),
    io:format("│ Error rate threshold: ~.1f% (~.2f)~n", [ErrorRateThreshold * 100, ErrorRateThreshold]),
    io:format("│ Latency threshold: ~p ms~n", [LatencyThresholdMs]),
    io:format("│ Open timeout: ~p ms (~.1f seconds)~n", [TimeoutMs, TimeoutMs / 1000]),
    io:format("└──────────────────────────────────────────────────────────────┘~n", []),
    io:format("~n"),
    
    %% Print should allow check
    io:format("┌─ Request Handling ───────────────────────────────────────────┐~n", []),
    ShouldAllowRes = router_circuit_breaker:should_allow(TenantId, ProviderId),
    print_should_allow(ShouldAllowRes, State),
    io:format("└──────────────────────────────────────────────────────────────┘~n", []),
    io:format("~n"),
    
    %% Print runbook link
    io:format("📖 Runbook: ~s~n", [?RUNBOOK_URL]),
    io:format("~n"),
    ok.

print_state_with_indicator(closed) ->
    io:format("│ State: CLOSED ✓ (normal operation, requests flow through)~n", []);
print_state_with_indicator(open) ->
    io:format("│ State: OPEN ✗ (circuit is open, requests fail immediately)~n", []);
print_state_with_indicator(half_open) ->
    io:format("│ State: HALF_OPEN ⚠ (probing for recovery, limited requests)~n", []);
print_state_with_indicator(State) ->
    io:format("│ State: ~p (unknown state)~n", [State]).

print_trigger_reason({ok, Reason}) ->
    io:format("│ Last trigger reason: ~s~n", [Reason]);
print_trigger_reason({error, not_found}) ->
    io:format("│ Last trigger reason: none (circuit never opened)~n", []);
print_trigger_reason(Other) ->
    io:format("│ Last trigger reason: ~p~n", [Other]).

print_error_rate(undefined) ->
    io:format("│ Error rate: not available~n", []);
print_error_rate(ErrorRate) when is_number(ErrorRate) ->
    Percentage = ErrorRate * 100,
    Threshold = 50.0,
    Indicator = if Percentage >= Threshold -> "⚠"; true -> "✓" end,
    io:format("│ Error rate: ~s ~.2f% (~.4f)~n", [Indicator, Percentage, ErrorRate]);
print_error_rate(Other) ->
    io:format("│ Error rate: ~p~n", [Other]).

print_timeout_remaining(undefined) ->
    ok;
print_timeout_remaining(TimeoutMs) when is_integer(TimeoutMs) ->
    Seconds = TimeoutMs / 1000,
    Minutes = Seconds / 60,
    if
        Minutes >= 1 ->
            io:format("│ Timeout remaining: ~.1f minutes (~.1f seconds)~n", [Minutes, Seconds]);
        true ->
            io:format("│ Timeout remaining: ~.1f seconds (~p ms)~n", [Seconds, TimeoutMs])
    end.

print_should_allow({ok, true}, _State) ->
    io:format("│ Should allow: YES ✓ (requests will be processed)~n", []);
print_should_allow({error, circuit_open}, open) ->
    io:format("│ Should allow: NO ✗ (circuit is open, requests blocked)~n", []);
print_should_allow({error, circuit_open}, _State) ->
    io:format("│ Should allow: NO ✗ (circuit is open)~n", []);
print_should_allow(Other, _State) ->
    io:format("│ Should allow: ~p~n", [Other]).

format_timestamp(TimestampMs) when is_integer(TimestampMs) ->
    Seconds = TimestampMs div 1000,
    Microseconds = (TimestampMs rem 1000) * 1000,
    DateTime = calendar:system_time_to_universal_time({Seconds, Microseconds}, millisecond),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC", 
                                 [Year, Month, Day, Hour, Min, Sec])).

