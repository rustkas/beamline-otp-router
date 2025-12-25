-module(router_intake_backpressure).

-doc "Intake Backpressure Module".
%% Detects and handles backpressure/overload conditions for Router intake
%% 
%% ⚠️ EXPERIMENTAL (partial implementation) ⚠️
%% 
%% CP2: Backpressure detection and metrics implemented
%% CP3/Pre-Release: Complete Gateway → Router integration, real-time JetStream queries, production-ready policies
%% 
%% ⚠️ STUB IMPLEMENTATION NOTES (CP3/Pre-Release - Productionization):
%% - Real-time JetStream consumer info queries: Framework implemented, requires actual NATS connection
%%   See: try_real_time_jetstream_query/1 for implementation details
%% - P95 calculation from histogram metrics: Framework implemented, requires histogram samples or Prometheus
%%   See: try_calculate_p95_from_histogram/1 for implementation details
%% - Gateway → Router backpressure integration: Requires Gateway changes (external dependency)
%% - End-to-end overload scenarios testing: Requires test implementation
%% - Production-ready backpressure policies: Requires policy configuration
%% - Full observability integration: Requires observability setup (metrics, alerts, dashboards)
%% 
%% Reference: docs/archive/dev/CP2_TECH_DEBT_SUMMARY.md (Router: Backpressure Logic section)

-export([check_backpressure/1, get_backpressure_status/1]).
-export([get_jetstream_pending/1, get_processing_latency_p95/1, get_inflight_count/1]).
-export([get_detailed_backpressure_status/1, get_backpressure_policy/1, apply_backpressure_policy/2]).
-export([track_backpressure_event/2, get_backpressure_events/1, get_backpressure_metrics/1]).
-export([get_backpressure_recovery_status/1, check_backpressure_recovery/1]).

-ignore_xref([
  {router_intake_backpressure, get_backpressure_status, 1},
  {router_intake_backpressure, get_jetstream_pending, 1},
  {router_intake_backpressure, get_processing_latency_p95, 1},
  {router_intake_backpressure, get_inflight_count, 1}
]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_intake_backpressure]).

-spec check_backpressure(binary()) -> {backpressure_status(), non_neg_integer()} | {error, term()}.
check_backpressure(Subject) ->
    %% Validate input
    case validate_subject(Subject) of
        {error, Reason} ->
            router_logger:error(~"Router intake backpressure check failed: invalid subject", #{
                ~"subject" => Subject,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"event" => ~"backpressure_check_invalid_subject"
            }),
            {error, {invalid_subject, Reason}};
        ok ->
            check_backpressure_impl(Subject)
    end.

%% Internal: Check backpressure status implementation (after validation)
-spec check_backpressure_impl(binary()) -> {backpressure_status(), non_neg_integer()}.
check_backpressure_impl(Subject) ->
    Pending = get_jetstream_pending(Subject),
    LatencyP95 = get_processing_latency_p95(Subject),
    InFlight = get_inflight_count(Subject),
    
    %% Get and validate thresholds
    Thresholds = get_backpressure_thresholds(),
    QueueOverload = maps:get(queue_overload, Thresholds, 1000),
    LatencyOverload = maps:get(latency_overload_ms, Thresholds, 5000),
    InFlightOverload = maps:get(inflight_overload, Thresholds, 500),
    
    %% Check overload conditions
    QueueOverloaded = Pending > QueueOverload,
    LatencyOverloaded = LatencyP95 > LatencyOverload,
    InFlightOverloaded = InFlight > InFlightOverload,
    
    %% Determine backpressure status
    Status = case {QueueOverloaded, LatencyOverloaded, InFlightOverloaded} of
        {true, true, _} -> {backpressure_active, 30};  %% Both queue and latency overloaded
        {true, _, true} -> {backpressure_active, 30};  %% Both queue and in-flight overloaded
        {_, true, true} -> {backpressure_active, 30};  %% Both latency and in-flight overloaded
        {true, false, false} -> {backpressure_warning, 0};  %% Only queue overloaded
        {false, true, false} -> {backpressure_warning, 0};  %% Only latency overloaded
        {false, false, true} -> {backpressure_warning, 0};  %% Only in-flight overloaded
        {false, false, false} -> {backpressure_inactive, 0}  %% No overload
    end,
    
    %% Emit backpressure status metric
    BackpressureActive = case Status of
        {backpressure_active, _} -> 1;
        _ -> 0
    end,
    
    emit_backpressure_status(Subject, BackpressureActive, Pending, LatencyP95, InFlight),
    
    %% Check for recovery BEFORE updating history to avoid overwriting previous status
    case Status of
        {backpressure_inactive, _} ->
            check_backpressure_recovery(Subject),
            track_backpressure_status_history(Subject, Status);
        _ ->
            track_backpressure_status_history(Subject, Status)
    end,
    
    %% Apply policy to determine action
    PolicyAction = apply_backpressure_policy(Subject, element(1, Status)),
    
    %% Log backpressure events
    case Status of
        {backpressure_active, RetryAfter} ->
            router_logger:warn(~"Router intake backpressure active", #{
                ~"subject" => Subject,
                ~"pending_messages" => Pending,
                ~"latency_p95_ms" => LatencyP95,
                ~"inflight_messages" => InFlight,
                ~"retry_after_seconds" => RetryAfter,
                ~"policy_action" => maps:get(action, PolicyAction, reject)
            }),
            emit_backpressure_triggered(Subject, ~"overload"),
            %% Track active event
            track_backpressure_event(Subject, #{
                type => ~"backpressure_active",
                pending_messages => Pending,
                latency_p95_ms => LatencyP95,
                inflight_messages => InFlight,
                retry_after_seconds => RetryAfter
            });
        {backpressure_warning, _} ->
            router_logger:info(~"Router intake backpressure warning", #{
                ~"subject" => Subject,
                ~"pending_messages" => Pending,
                ~"latency_p95_ms" => LatencyP95,
                ~"inflight_messages" => InFlight
            }),
            %% Track warning event
            track_backpressure_event(Subject, #{
                type => ~"backpressure_warning",
                pending_messages => Pending,
                latency_p95_ms => LatencyP95,
                inflight_messages => InFlight
            });
        {backpressure_inactive, _} ->
            ok
    end,
    
    Status.

-spec get_backpressure_thresholds() -> map().
get_backpressure_thresholds() ->
    QueueOverload = application:get_env(beamline_router, queue_overload, 1000),
    LatencyOverload = application:get_env(beamline_router, latency_overload_ms, 5000),
    InFlightOverload = application:get_env(beamline_router, inflight_overload, 500),
    
    %% Validate thresholds
    ValidQueueOverload = max(1, QueueOverload),
    ValidLatencyOverload = max(1, LatencyOverload),
    ValidInFlightOverload = max(1, InFlightOverload),
    
    #{
        queue_overload => ValidQueueOverload,
        latency_overload_ms => ValidLatencyOverload,
        inflight_overload => ValidInFlightOverload
    }.

-spec get_backpressure_status(binary()) -> backpressure_status().
get_backpressure_status(Subject) ->
    %% Validate subject
    case validate_subject(Subject) of
        {error, _Reason} ->
            backpressure_inactive;
        ok ->
            get_backpressure_status_internal(Subject)
    end.

-spec track_backpressure_status_history(binary(), {backpressure_status(), non_neg_integer()}) -> ok.
track_backpressure_status_history(Subject, {Status, _RetryAfter}) ->
    try
        Table = router_backpressure_status_history,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        ets:insert(Table, {Subject, Status, erlang:system_time(millisecond)}),
        ok
    catch
        Error:CatchReason ->
            router_logger:debug(~"Failed to track backpressure status history", #{
                ~"subject" => Subject,
                ~"status" => Status,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"backpressure_status_history_tracking_failed"
            }),
            ok
    end.

-spec get_backpressure_status_internal(binary()) -> backpressure_status().
get_backpressure_status_internal(Subject) ->
    Pending = get_jetstream_pending(Subject),
    LatencyP95 = get_processing_latency_p95(Subject),
    InFlight = get_inflight_count(Subject),
    
    %% Get and validate thresholds
    Thresholds = get_backpressure_thresholds(),
    QueueOverload = maps:get(queue_overload, Thresholds, 1000),
    LatencyOverload = maps:get(latency_overload_ms, Thresholds, 5000),
    InFlightOverload = maps:get(inflight_overload, Thresholds, 500),
    
    QueueOverloaded = Pending > QueueOverload,
    LatencyOverloaded = LatencyP95 > LatencyOverload,
    InFlightOverloaded = InFlight > InFlightOverload,
    
    case {QueueOverloaded, LatencyOverloaded, InFlightOverloaded} of
        {true, true, _} -> backpressure_active;
        {true, _, true} -> backpressure_active;
        {_, true, true} -> backpressure_active;
        {true, false, false} -> backpressure_warning;
        {false, true, false} -> backpressure_warning;
        {false, false, true} -> backpressure_warning;
        {false, false, false} -> backpressure_inactive
    end.

%% 
%% ⚠️ STUB IMPLEMENTATION ⚠️
%% CP3/Release: Query JetStream consumer info via NATS API for real-time values.
%% Current behavior: Tries a real-time query (which is a stub) and falls back to cached ETS values.
%%
%% Implementation notes:
%% - Real-time queries require actual NATS connection (not mock mode)
%% - JetStream API: $JS.API.CONSUMER.INFO.{stream}.{consumer}
%% - Response includes: num_pending, num_waiting, etc.
%% - Fallback to cached ETS values when NATS unavailable or query fails
%%
%% Framework structure:
%% - `try_real_time_jetstream_query/1`: Attempts real-time query (stub)
%% - `get_jetstream_pending_cached/1`: Fallback to cached ETS values
%% - `update_pending_cache/2`: Updates cache with real-time value (when available)
-spec get_jetstream_pending(binary()) -> non_neg_integer().
get_jetstream_pending(Subject) ->
    %% Try real-time query first (requires actual NATS connection)
    case try_real_time_jetstream_query(Subject) of
        {ok, Pending} when is_integer(Pending) ->
            %% Update cache with real-time value
            update_pending_cache(Subject, Pending),
            Pending;
        {error, nats_unavailable} ->
            %% NATS unavailable - fallback to cached ETS values
            get_jetstream_pending_cached(Subject);
        {error, _Reason} ->
            %% Query failed - fallback to cached values
            get_jetstream_pending_cached(Subject)
    end.

%% 
%% Returns: {ok, Pending} | {error, Reason}
%% Requires: Actual NATS connection (not mock mode)
%%
%% ⚠️ STUB IMPLEMENTATION: Returns error when real NATS unavailable
%% CP3/Release: Implement actual NATS JetStream API query
%%
%% Implementation notes:
%% - Requires actual NATS connection (not mock mode)
%% - JetStream API endpoint: `$JS.API.CONSUMER.INFO.{stream}.{consumer}`
%% - Request payload: JSON with consumer name
%% - Response includes: `num_pending`, `num_waiting`, `delivered`, etc.
%% - Should handle errors gracefully (connection lost, timeout, etc.)
%%
%% Current behavior:
%% - Returns {error, nats_unavailable} in mock mode
%% - Returns {error, not_implemented} in real NATS mode (stub)
%% - Safe fallback to cached values when query fails
-spec try_real_time_jetstream_query(binary()) -> {ok, non_neg_integer()} | {error, term()}.
try_real_time_jetstream_query(Subject) ->
    %% Validate subject
    case validate_subject(Subject) of
        {error, Reason} ->
            {error, {invalid_subject, Reason}};
        ok ->
            %% Check if NATS is in mock mode
            case application:get_env(beamline_router, nats_mode, mock) of
                mock ->
                    %% Mock mode - cannot query real NATS
                    router_metrics:emit_metric(router_intake_jetstream_query_total, #{count => 1}, #{
                        method => ~"real_time",
                        subject => Subject,
                        error => ~"nats_unavailable"
                    }),
                    {error, nats_unavailable};
                _ ->
                    %% Real NATS mode - attempt query
                    %% Extract stream and consumer from subject for query preparation
                    _ = prepare_jetstream_query_config(Subject),
                    %% ⚠️ STUB IMPLEMENTATION: Returns {error, not_implemented}
                    %% CP3/Release: Implement actual NATS JetStream API query
                    %%
                    %% Implementation requirements:
                    %% - Use router_nats:request("$JS.API.CONSUMER.INFO.stream.consumer", Payload, Timeout)
                    %% - Use QueryConfig to build request payload
                    %% - Parse response JSON to get num_pending
                    %% - Handle errors gracefully (connection lost, timeout, consumer not found, etc.)
                    %% - Return {ok, Pending} on success or {error, Reason} on failure
                    %%
                    %% Current behavior:
                    %% - Returns {error, not_implemented} (stub)
                    %% - Safe fallback to cached values when query fails
                    router_metrics:emit_metric(router_intake_jetstream_query_total, #{count => 1}, #{
                        method => ~"real_time",
                        subject => Subject,
                        error => ~"not_implemented"
                    }),
                    {error, not_implemented}
            end
    end.

%% Extracts stream and consumer information for query preparation
-spec prepare_jetstream_query_config(binary()) -> map().
prepare_jetstream_query_config(Subject) ->
    %% Extract stream name from subject pattern
    Stream = extract_stream_from_subject(Subject),
    %% Extract consumer from subject or use default
    Consumer = extract_consumer_from_subject(Subject),
    %% Build query subject
    QuerySubject = build_jetstream_query_subject(Stream, Consumer),
    
    #{
        subject => Subject,
        stream => Stream,
        consumer => Consumer,
        query_subject => QuerySubject,
        timeout_ms => application:get_env(beamline_router, jetstream_query_timeout_ms, 5000)
    }.

-spec extract_stream_from_subject(binary()) -> binary().
extract_stream_from_subject(Subject) ->
    %% Common patterns: "beamline.router.v1.decide" -> "router-stream"
    case binary:split(Subject, ~".", [global]) of
        [~"beamline", StreamName, _, _] ->
            <<StreamName/binary, "-stream">>;
        [StreamName, ~"exec", _, _] ->
            <<StreamName/binary, "-stream">>;
        _ ->
            %% Fallback: use first part
            case binary:split(Subject, ~".") of
                [FirstPart, _] -> FirstPart;
                [FirstPart] -> FirstPart;
                _ -> ~"unknown-stream"
            end
    end.

-spec extract_consumer_from_subject(binary()) -> binary().
extract_consumer_from_subject(Subject) ->
    %% Try to get consumer from config or use default based on subject
    case application:get_env(beamline_router, nats_js_durable_group_decide, undefined) of
        undefined ->
            %% Default consumer based on subject pattern
            case binary:match(Subject, ~"decide") of
                nomatch ->
                    case binary:match(Subject, ~"result") of
                        nomatch -> ~"default-consumer";
                        _ -> ~"router-results"
                    end;
                _ -> ~"router-decide-consumer"
            end;
        Consumer -> Consumer
    end.

-spec build_jetstream_query_subject(binary(), binary()) -> binary().
build_jetstream_query_subject(Stream, Consumer) ->
    <<"$JS.API.CONSUMER.INFO.", Stream/binary, ".", Consumer/binary>>.

-spec get_jetstream_pending_cached(binary()) -> non_neg_integer().
get_jetstream_pending_cached(Subject) ->
    Table = router_jetstream_pending_cache,
    case ets:whereis(Table) of
        undefined ->
            %% Table not initialized - return 0
            0;
        _ ->
            case ets:lookup(Table, Subject) of
                [{Subject, Pending, _Timestamp}] ->
                    Pending;
                [] ->
                    0
            end
    end.

-spec update_pending_cache(binary(), non_neg_integer()) -> ok.
update_pending_cache(Subject, Pending) ->
    Table = router_jetstream_pending_cache,
    case ets:whereis(Table) of
        undefined ->
            %% Table not initialized - create it
            _ = ets:new(Table, [named_table, public, {write_concurrency, true}]),
            ets:insert(Table, {Subject, Pending, erlang:system_time(millisecond)});
        _ ->
            ets:insert(Table, {Subject, Pending, erlang:system_time(millisecond)})
    end,
    ok.

%% 
%% ⚠️ STUB IMPLEMENTATION ⚠️
%% CP3/Release: Calculate P95 from histogram metrics for real-time values.
%% Current behavior: Tries a real-time calculation (which is a stub) and falls back to cached ETS values.
%%
%% Implementation notes:
%% - Real-time calculation requires histogram samples or Prometheus histogram
%% - Options: Collect latency samples in sliding window, or query Prometheus histogram
%% - Fallback to cached ETS values when histogram unavailable or calculation fails
%%
%% Framework structure:
%% - `try_calculate_p95_from_histogram/1`: Attempts real-time calculation (stub)
%% - `get_processing_latency_p95_cached/1`: Fallback to cached ETS values
%% - `update_latency_cache/2`: Updates cache with calculated value (when available)
-spec get_processing_latency_p95(binary()) -> non_neg_integer().
get_processing_latency_p95(Subject) ->
    %% Try real-time calculation from histogram/samples first
    case try_calculate_p95_from_histogram(Subject) of
        {ok, P95Ms} when is_integer(P95Ms) ->
            %% Update cache with calculated value
            update_latency_cache(Subject, P95Ms),
            P95Ms;
        {error, histogram_unavailable} ->
            %% Histogram unavailable - fallback to cached ETS values
            get_processing_latency_p95_cached(Subject);
        {error, _Reason} ->
            %% Calculation failed - fallback to cached values
            get_processing_latency_p95_cached(Subject)
    end.

%% 
%% Returns: {ok, P95Ms} | {error, Reason}
%% Requires: Histogram samples or Prometheus histogram query
%%
%% ⚠️ STUB IMPLEMENTATION: Attempts to get samples from ETS table
%% CP3/Release: Implement full histogram sample collection and Prometheus integration
%%
%% Implementation notes:
%% - Option 1: Collect latency samples in ETS table (`router_intake_latency_samples`)
%% - Option 2: Query Prometheus histogram_quantile if Prometheus integration available
%% - Both approaches supported by framework
%% - Fallback to cached values when histogram unavailable
%%
%% Current behavior:
%% - Attempts to get samples from ETS table (if exists)
%% - Calculates P95 using percentile algorithm
%% - Falls back to Prometheus query (stub) if samples unavailable
%% - Safe fallback to cached values when calculation fails
-spec try_calculate_p95_from_histogram(binary()) -> {ok, non_neg_integer()} | {error, term()}.
try_calculate_p95_from_histogram(Subject) ->
    %% Validate subject
    case validate_subject(Subject) of
        {error, Reason} ->
            {error, {invalid_subject, Reason}};
        ok ->
            %% Option 1: Try to get samples from latency samples table (if exists)
            case get_latency_samples_for_subject(Subject) of
                {ok, Samples} when is_list(Samples), length(Samples) > 0 ->
                    %% Calculate P95 from samples
                    Sorted = lists:sort(Samples),
                    P95Ms = calculate_percentile(Sorted, 0.95),
                    %% Emit metric for P95 calculation
                    router_metrics:emit_metric(router_intake_p95_calculation_total, #{count => 1}, #{
                        method => ~"histogram_samples",
                        subject => Subject,
                        sample_count => integer_to_binary(length(Samples))
                    }),
                    {ok, trunc(P95Ms)};
                {ok, []} ->
                    %% No samples available
                    router_metrics:emit_metric(router_intake_p95_calculation_total, #{count => 1}, #{
                        method => ~"histogram_samples",
                        subject => Subject,
                        error => ~"no_samples"
                    }),
                    {error, histogram_unavailable};
                {error, Reason} ->
                    %% Samples table not available - try Prometheus histogram
                    router_metrics:emit_metric(router_intake_p95_calculation_total, #{count => 1}, #{
                        method => ~"histogram_samples",
                        subject => Subject,
                        error => error_to_binary(Reason)
                    }),
                    try_calculate_p95_from_prometheus_histogram(Subject)
            end
    end.

-spec validate_subject(binary()) -> ok | {error, term()}.
validate_subject(Subject) when is_binary(Subject) ->
    case byte_size(Subject) > 0 andalso byte_size(Subject) < 1024 of
        true -> ok;
        false -> {error, invalid_subject_length}
    end;
validate_subject(_) ->
    {error, invalid_subject_type}.

-spec error_to_binary(term()) -> binary().
error_to_binary(Error) when is_binary(Error) -> Error;
error_to_binary(Error) when is_atom(Error) -> atom_to_binary(Error, utf8);
error_to_binary(Error) -> iolist_to_binary(io_lib:format("~p", [Error])).

%% Internal: Sanitize error for logging (masks secrets)
-spec sanitize_error_for_logging(term()) -> binary() | term().
sanitize_error_for_logging(Error) ->
    %% Convert error to binary for pattern matching
    ErrorBin = case is_binary(Error) of
        true -> Error;
        false -> iolist_to_binary(io_lib:format("~p", [Error]))
    end,
    %% Check for common secret patterns in error message
    case re:run(ErrorBin, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
        match ->
            ~"[REDACTED: contains sensitive data]";
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.

%% Returns: {ok, [float()]} | {error, Reason}
-spec get_latency_samples_for_subject(binary()) -> {ok, [float()]} | {error, term()}.
get_latency_samples_for_subject(Subject) ->
    %% Check if latency samples table exists
    Table = router_intake_latency_samples,
    case ets:whereis(Table) of
        undefined ->
            %% Table not initialized - samples not being collected
            {error, table_not_initialized};
        _ ->
            %% Get samples for this subject (keep last N samples for sliding window)
            WindowSize = application:get_env(beamline_router, latency_samples_window_size, 1000),
            case ets:lookup(Table, Subject) of
                [{Subject, Samples}] when is_list(Samples) ->
                    %% Keep only last WindowSize samples
                    RecentSamples = lists:sublist(Samples, WindowSize),
                    {ok, RecentSamples};
                [] ->
                    {ok, []};
                _ ->
                    {error, invalid_format}
            end
    end.

%% Uses same algorithm as router_stress_perf_monitor:percentile/2
-spec calculate_percentile(list(float()), float()) -> float().
calculate_percentile(Sorted, Percentile) ->
    Count = length(Sorted),
    case Count > 0 of
        true ->
            Index = max(1, round(Count * Percentile)),
            lists:nth(Index, Sorted);
        false ->
            0.0
    end.

%% 
%% Returns: {ok, P95Ms} | {error, Reason}
%%
%% ⚠️ STUB IMPLEMENTATION: Returns error (Prometheus integration not available)
%% CP3/Release: Implement Prometheus histogram query
%%
%% Implementation notes:
%% - Requires Prometheus integration (router_prometheus or similar)
%% - Query: `histogram_quantile(0.95, rate(router_intake_processing_latency_seconds_bucket[5m]))`
%% - Should filter by subject label if available
%% - Should handle errors gracefully (Prometheus unavailable, query timeout, etc.)
%%
%% Current behavior:
%% - Returns {error, prometheus_unavailable} (stub)
%% - Safe fallback to cached values when query fails
-spec try_calculate_p95_from_prometheus_histogram(binary()) -> {ok, non_neg_integer()} | {error, term()}.
try_calculate_p95_from_prometheus_histogram(_) ->
    %% ⚠️ STUB IMPLEMENTATION: Returns {error, prometheus_unavailable}
    %% CP3/Release: Query Prometheus histogram_quantile if Prometheus integration available
    %%
    %% Implementation requirements:
    %% - Requires Prometheus integration (router_prometheus or similar)
    %% - Query: `histogram_quantile(0.95, rate(router_intake_processing_latency_seconds_bucket[5m]))`
    %% - Should filter by subject label if available
    %% - Should handle errors gracefully (Prometheus unavailable, query timeout, etc.)
    %% - Return {ok, P95Ms} on success or {error, Reason} on failure
    %%
    %% Current behavior:
    %% - Returns {error, prometheus_unavailable} (stub)
    %% - Safe fallback to cached values when query fails
    {error, prometheus_unavailable}.

-spec get_processing_latency_p95_cached(binary()) -> non_neg_integer().
get_processing_latency_p95_cached(Subject) ->
    Table = router_intake_latency_cache,
    case ets:whereis(Table) of
        undefined ->
            %% Table not initialized - return 0
            0;
        _ ->
            case ets:lookup(Table, {Subject, p95}) of
                [{{Subject, p95}, LatencyMs, _Timestamp}] ->
                    LatencyMs;
                [] ->
                    0
            end
    end.

-spec update_latency_cache(binary(), non_neg_integer()) -> ok.
update_latency_cache(Subject, P95Ms) ->
    Table = router_intake_latency_cache,
    case ets:whereis(Table) of
        undefined ->
            %% Table not initialized - create it
            _ = ets:new(Table, [named_table, public, {write_concurrency, true}]),
            ets:insert(Table, {{Subject, p95}, P95Ms, erlang:system_time(millisecond)});
        _ ->
            ets:insert(Table, {{Subject, p95}, P95Ms, erlang:system_time(millisecond)})
    end,
    ok.

-spec get_inflight_count(binary()) -> non_neg_integer().
get_inflight_count(Subject) ->
    %% Track in-flight messages in ETS table
    Table = router_intake_inflight,
    case ets:whereis(Table) of
        undefined ->
            %% Table not initialized - return 0
            0;
        _ ->
            %% Count messages for this subject
            MatchSpec = [{{Subject, '_'}, [], [true]}],
            length(ets:select(Table, MatchSpec))
    end.

-spec emit_backpressure_status(binary(), 0 | 1, non_neg_integer(), non_neg_integer(), non_neg_integer()) -> ok.
emit_backpressure_status(Subject, Active, Pending, LatencyP95, InFlight) ->
    telemetry:execute(?TELEMETRY_PREFIX ++ [backpressure_status], #{
        active => Active,
        pending_messages => Pending,
        latency_p95_ms => LatencyP95,
        inflight_messages => InFlight
    }, #{
        subject => Subject
    }),
    ok.

-spec emit_backpressure_triggered(binary(), binary()) -> ok.
emit_backpressure_triggered(Subject, Trigger) ->
    telemetry:execute(?TELEMETRY_PREFIX ++ [backpressure_triggered], #{
        count => 1
    }, #{
        subject => Subject,
        trigger => Trigger
    }),
    ok.

-spec get_detailed_backpressure_status(binary()) -> map().
get_detailed_backpressure_status(Subject) ->
    %% Validate subject
    case validate_subject(Subject) of
        {error, _Reason} ->
            #{error => invalid_subject};
        ok ->
            Pending = get_jetstream_pending(Subject),
            LatencyP95 = get_processing_latency_p95(Subject),
            InFlight = get_inflight_count(Subject),
            Status = get_backpressure_status(Subject),
            Thresholds = get_backpressure_thresholds(),
            Policy = get_backpressure_policy(Subject),
            RecoveryStatus = get_backpressure_recovery_status(Subject),
            
            #{
                subject => Subject,
                status => Status,
                metrics => #{
                    pending_messages => Pending,
                    latency_p95_ms => LatencyP95,
                    inflight_messages => InFlight
                },
                thresholds => Thresholds,
                policy => Policy,
                recovery => RecoveryStatus,
                timestamp => erlang:system_time(millisecond)
            }
    end.

-spec get_backpressure_policy(binary()) -> map().
get_backpressure_policy(_) ->
    %% Load policy from application config or ETS
    case application:get_env(beamline_router, backpressure_policy, undefined) of
        undefined ->
            %% Default policy
            get_default_backpressure_policy();
        PolicyConfig when is_map(PolicyConfig) ->
            %% Validate and return policy
            validate_backpressure_policy(PolicyConfig);
        _ ->
            get_default_backpressure_policy()
    end.

-spec get_default_backpressure_policy() -> map().
get_default_backpressure_policy() ->
    #{
        retry_after_seconds => 30,
        max_retry_attempts => 3,
        backoff_strategy => exponential,
        recovery_threshold => 0.5,
        alert_on_active => true,
        alert_on_warning => false
    }.

-spec validate_backpressure_policy(map()) -> map().
validate_backpressure_policy(Policy) ->
    Default = get_default_backpressure_policy(),
    Validated = maps:merge(Default, Policy),
    %% Ensure all required fields are present and valid
    maps:merge(Validated, #{
        retry_after_seconds => max(1, maps:get(retry_after_seconds, Validated, 30)),
        max_retry_attempts => max(1, maps:get(max_retry_attempts, Validated, 3)),
        backoff_strategy => maps:get(backoff_strategy, Validated, exponential),
        recovery_threshold => max(0.0, min(1.0, maps:get(recovery_threshold, Validated, 0.5))),
        alert_on_active => maps:get(alert_on_active, Validated, true),
        alert_on_warning => maps:get(alert_on_warning, Validated, false)
    }).

-spec apply_backpressure_policy(binary(), backpressure_status()) -> map().
apply_backpressure_policy(Subject, Status) ->
    Policy = get_backpressure_policy(Subject),
    case Status of
        backpressure_active ->
            RetryAfter = maps:get(retry_after_seconds, Policy, 30),
            #{
                action => reject,
                retry_after_seconds => RetryAfter,
                max_retry_attempts => maps:get(max_retry_attempts, Policy, 3),
                backoff_strategy => maps:get(backoff_strategy, Policy, exponential),
                alert => maps:get(alert_on_active, Policy, true)
            };
        backpressure_warning ->
            #{
                action => continue,
                retry_after_seconds => 0,
                alert => maps:get(alert_on_warning, Policy, false)
            };
        backpressure_inactive ->
            #{
                action => continue,
                retry_after_seconds => 0,
                alert => false
            }
    end.

-spec track_backpressure_event(binary(), map()) -> ok.
track_backpressure_event(Subject, EventInfo) when is_binary(Subject), is_map(EventInfo) ->
    try
        Table = router_backpressure_events,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, ordered_set, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        Event = {
            erlang:system_time(millisecond),
            Subject,
            EventInfo
        },
        ets:insert(Table, Event),
        
        %% Emit event metric
        EventType = maps:get(type, EventInfo, ~"unknown"),
        router_metrics:emit_metric(router_backpressure_events_total, #{count => 1}, #{
            subject => Subject,
            event_type => EventType
        }),
        
        ok
    catch
        Error:CatchReason ->
            router_logger:debug(~"Failed to track backpressure event", #{
                ~"subject" => Subject,
                ~"event_type" => maps:get(type, EventInfo, ~"unknown"),
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"backpressure_event_tracking_failed"
            }),
            ok
    end.

-spec get_backpressure_events(binary()) -> [map()].
get_backpressure_events(Subject) ->
    try
        Table = router_backpressure_events,
        case ets:whereis(Table) of
            undefined ->
                [];
            _Tid ->
                MatchSpec = [{{'$1', Subject, '$2'}, [], ['$_']}],
                Events = ets:select(Table, MatchSpec),
                lists:map(fun({Timestamp, _Subject, EventInfo}) ->
                    maps:merge(EventInfo, #{
                        timestamp => Timestamp,
                        subject => Subject
                    })
                end, Events)
        end
    catch
        Error:CatchReason ->
            router_logger:debug(~"Failed to get backpressure events", #{
                ~"subject" => Subject,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"backpressure_events_get_failed"
            }),
            []
    end.

-spec get_backpressure_metrics(binary()) -> map().
get_backpressure_metrics(Subject) ->
    try
        Events = get_backpressure_events(Subject),
        ActiveEvents = [E || E <- Events, maps:get(type, E, ~"unknown") =:= ~"backpressure_active"],
        WarningEvents = [E || E <- Events, maps:get(type, E, ~"unknown") =:= ~"backpressure_warning"],
        RecoveryEvents = [E || E <- Events, maps:get(type, E, ~"unknown") =:= ~"recovery"],
        
        #{
            subject => Subject,
            total_events => length(Events),
            active_events => length(ActiveEvents),
            warning_events => length(WarningEvents),
            recovery_events => length(RecoveryEvents),
            last_event_time => case Events of
                [] -> undefined;
                _ -> lists:max([maps:get(timestamp, E, 0) || E <- Events])
            end
        }
    catch
        _:_ ->
            #{}
    end.

-spec get_backpressure_recovery_status(binary()) -> map().
get_backpressure_recovery_status(Subject) ->
    try
        Table = router_backpressure_recovery,
        case ets:whereis(Table) of
            undefined ->
                #{recovered => false, recovery_time => undefined};
            _Tid ->
                case ets:lookup(Table, Subject) of
                    [{Subject, RecoveryInfo}] ->
                        RecoveryInfo;
                    [] ->
                        #{recovered => false, recovery_time => undefined}
                end
        end
    catch
        Error:CatchReason ->
            router_logger:debug(~"Failed to get backpressure recovery status", #{
                ~"subject" => Subject,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"backpressure_recovery_status_get_failed"
            }),
            #{recovered => false, recovery_time => undefined}
    end.

-spec check_backpressure_recovery(binary()) -> {ok, boolean()} | {error, term()}.
check_backpressure_recovery(Subject) ->
    try
        CurrentStatus = get_backpressure_status(Subject),
        PreviousStatus = get_previous_backpressure_status(Subject),
        
        Recovered = case {PreviousStatus, CurrentStatus} of
            {backpressure_active, backpressure_inactive} -> true;
            {backpressure_active, backpressure_warning} -> true;
            {backpressure_warning, backpressure_inactive} -> true;
            _ -> false
        end,
        
        case Recovered of
            true ->
                %% Track recovery
                track_backpressure_recovery(Subject),
                {ok, true};
            false ->
                {ok, false}
        end
    catch
        _:CatchReason ->
            {error, CatchReason}
    end.

-spec get_previous_backpressure_status(binary()) -> backpressure_status().
get_previous_backpressure_status(Subject) ->
    try
        Table = router_backpressure_status_history,
        case ets:whereis(Table) of
            undefined ->
                backpressure_inactive;
            _Tid ->
                case ets:lookup(Table, Subject) of
                    [{Subject, Status, _Timestamp}] ->
                        Status;
                    [] ->
                        backpressure_inactive
                end
        end
    catch
        _:_ ->
            backpressure_inactive
    end.

-spec track_backpressure_recovery(binary()) -> ok.
track_backpressure_recovery(Subject) ->
    try
        %% Update recovery table
        Table = router_backpressure_recovery,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        RecoveryInfo = #{
            recovered => true,
            recovery_time => erlang:system_time(millisecond)
        },
        ets:insert(Table, {Subject, RecoveryInfo}),
        
        %% Track recovery event
        track_backpressure_event(Subject, #{
            type => ~"recovery",
            recovery_time => erlang:system_time(millisecond)
        }),
        
        %% Emit recovery metric
        router_metrics:emit_metric(router_backpressure_recovery_total, #{count => 1}, #{
            subject => Subject
        }),
        
        ok
    catch
        Error:CatchReason ->
            router_logger:debug(~"Failed to check backpressure recovery", #{
                ~"subject" => Subject,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"backpressure_recovery_check_failed"
            }),
            ok
    end.

%% Types
-type backpressure_status() :: backpressure_active | backpressure_warning | backpressure_inactive.
