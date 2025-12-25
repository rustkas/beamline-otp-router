-module(router_extension_health).

-doc "Extension Health Monitoring".
%% CP3: Health metrics collection and monitoring
-export([get_health/1, get_all_health/0, get_health_summary/0]).

-ignore_xref([
  {router_extension_health, get_health, 1},
  {router_extension_health, get_all_health, 0}
]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_extension_health]).

%% Returns: {ok, HealthMap} | {error, Reason}
-spec get_health(binary()) -> {ok, map()} | {error, term()}.
get_health(ExtensionId) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "SELECT extension_id, last_success, last_failure, success_count, "
                        "failure_count, avg_latency_ms, p50_latency_ms, p95_latency_ms, p99_latency_ms, "
                        "last_latency_ms, latency_samples, circuit_breaker_state, "
                        "circuit_breaker_opened_at, updated_at "
                        "FROM extension_health WHERE extension_id = $1",
                case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
                    {ok, _, [{ExtId, LastSuccess, LastFailure, SuccessCount, FailureCount,
                              AvgLatencyMs, P50LatencyMs, P95LatencyMs, P99LatencyMs,
                              LastLatencyMs, LatencySamples, CircuitState, CircuitOpenedAt, UpdatedAt}]} ->
                        Total = SuccessCount + FailureCount,
                        SuccessRate = case Total > 0 of
                            true -> SuccessCount / Total;
                            false -> 0.0
                        end,
                        Health = #{
                            extension_id => ExtId,
                            last_success => LastSuccess,
                            last_failure => LastFailure,
                            success_count => SuccessCount,
                            failure_count => FailureCount,
                            success_rate => SuccessRate,
                            avg_latency_ms => AvgLatencyMs,
                            p50_latency_ms => P50LatencyMs,
                            p95_latency_ms => P95LatencyMs,
                            p99_latency_ms => P99LatencyMs,
                            last_latency_ms => LastLatencyMs,
                            latency_samples => LatencySamples,
                            circuit_breaker_state => CircuitState,
                            circuit_breaker_opened_at => CircuitOpenedAt,
                            updated_at => UpdatedAt
                        },
                        {ok, Health};
                    {ok, _, []} ->
                        {error, not_found};
                    {error, Reason} ->
                        {error, Reason}
                end;
            _ ->
                {error, database_not_available}
        end
    catch
        error:undef ->
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
    end.

%% Returns: {ok, [HealthMap]} | {error, Reason}
-spec get_all_health() -> {ok, [map()]} | {error, term()}.
get_all_health() ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "SELECT extension_id, last_success, last_failure, success_count, "
                        "failure_count, avg_latency_ms, p50_latency_ms, p95_latency_ms, p99_latency_ms, "
                        "last_latency_ms, latency_samples, circuit_breaker_state, "
                        "circuit_breaker_opened_at, updated_at "
                        "FROM extension_health ORDER BY extension_id",
                case catch apply(epgsql, equery, [Conn, Query, []]) of
                    {ok, _, Rows} ->
                        HealthList = lists:map(fun(Row) ->
                            {ExtId, LastSuccess, LastFailure, SuccessCount, FailureCount,
                             AvgLatencyMs, P50LatencyMs, P95LatencyMs, P99LatencyMs,
                             LastLatencyMs, LatencySamples, CircuitState, CircuitOpenedAt, UpdatedAt} = Row,
                            Total = SuccessCount + FailureCount,
                            SuccessRate = case Total > 0 of
                                true -> SuccessCount / Total;
                                false -> 0.0
                            end,
                            #{
                                extension_id => ExtId,
                                last_success => LastSuccess,
                                last_failure => LastFailure,
                                success_count => SuccessCount,
                                failure_count => FailureCount,
                                success_rate => SuccessRate,
                                avg_latency_ms => AvgLatencyMs,
                                p50_latency_ms => P50LatencyMs,
                                p95_latency_ms => P95LatencyMs,
                                p99_latency_ms => P99LatencyMs,
                                last_latency_ms => LastLatencyMs,
                                latency_samples => LatencySamples,
                                circuit_breaker_state => CircuitState,
                                circuit_breaker_opened_at => CircuitOpenedAt,
                                updated_at => UpdatedAt
                            }
                        end, Rows),
                        {ok, HealthList};
                    {error, Reason} ->
                        {error, Reason}
                end;
            _ ->
                {error, database_not_available}
        end
    catch
        error:undef ->
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
    end.

%% Returns: {ok, SummaryMap} | {error, Reason}
-spec get_health_summary() -> {ok, map()} | {error, term()}.
get_health_summary() ->
    try
        case get_all_health() of
            {ok, HealthList} ->
                Total = length(HealthList),
                Healthy = length([H || H <- HealthList, maps:get(circuit_breaker_state, H) =:= ~"closed"]),
                Unhealthy = length([H || H <- HealthList, maps:get(circuit_breaker_state, H) =:= ~"open"]),
                HalfOpen = length([H || H <- HealthList, maps:get(circuit_breaker_state, H) =:= ~"half_open"]),
                
                SuccessRates = [maps:get(success_rate, H, 0.0) || H <- HealthList],
                AvgSuccessRate = case SuccessRates of
                    [] -> 0.0;
                    _ -> lists:sum(SuccessRates) / length(SuccessRates)
                end,
                
                Latencies = [maps:get(avg_latency_ms, H, 0.0) || H <- HealthList, maps:get(avg_latency_ms, H, null) =/= null],
                AvgLatency = case Latencies of
                    [] -> 0.0;
                    _ -> lists:sum(Latencies) / length(Latencies)
                end,
                
                P95Latencies = [maps:get(p95_latency_ms, H, 0.0) || H <- HealthList, maps:get(p95_latency_ms, H, null) =/= null],
                AvgP95Latency = case P95Latencies of
                    [] -> 0.0;
                    _ -> lists:sum(P95Latencies) / length(P95Latencies)
                end,
                
                Summary = #{
                    total => Total,
                    healthy => Healthy,
                    unhealthy => Unhealthy,
                    half_open => HalfOpen,
                    avg_success_rate => AvgSuccessRate,
                    avg_latency_ms => AvgLatency,
                    avg_p95_latency_ms => AvgP95Latency
                },
                {ok, Summary};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ ->
            {error, unexpected_error}
    end.
