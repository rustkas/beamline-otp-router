%% @doc Shared test helper for capturing and asserting router metrics.
%%
%% This module provides a centralized, reusable harness for capturing
%% router_metrics:emit_metric/3 and router_metrics:inc/1 calls in tests.
%%
%% == Usage ==
%% 1. Call `setup/0` or `setup/1` in init_per_testcase to install meck expectations.
%% 2. Call `wait_for_metric/2` or `wait_for_metric/3` to wait for a specific metric.
%% 3. Call `get_all_metrics/0` to get all captured metrics.
%% 4. Call `teardown/0` in end_per_testcase to clean up.
%%
%% == Metrics Contract (from router_nats.erl) ==
%%
%% === Publish Operations ===
%% | Metric Name                              | Type       | Labels                                  |
%% |------------------------------------------|------------|-----------------------------------------|
%% | router_nats_publish_total                | inc        | (none)                                  |
%% | router_nats_publish_attempts_total       | emit_metric| status, retry_count                     |
%% | router_nats_publish_failures_total       | emit_metric| reason, subject, stream, source         |
%% | router_nats_publish_errors_total         | emit_metric| error_type                              |
%% | router_nats_publish_latency_seconds      | emit_metric| (none - just value)                     |
%%
%% === Publish With Ack Operations ===
%% | router_nats_publish_with_ack_total       | inc        | (none)                                  |
%% | router_nats_publish_with_ack_failures_total | emit_metric| reason, subject, stream, source      |
%%
%% === Connection Operations ===
%% | router_nats_connection_established_total | inc        | (none)                                  |
%% | router_nats_connection_lost_total        | inc        | (none)                                  |
%% | router_nats_connection_restored_total    | inc        | (none)                                  |
%% | router_nats_connection_status            | emit_metric| state (value: 0/0.5/1)                  |
%% | router_nats_connect_failures_total       | emit_metric| reason, cluster, source                 |
%% | router_nats_reconnect_attempts_total     | inc        | (none)                                  |
%% | router_nats_reconnect_failures_total     | emit_metric| reason, cluster, attempt                |
%% | router_nats_reconnection_exhausted_total | inc        | (none)                                  |
%%
%% @end
-module(router_metrics_test_helper).

-export([
    setup/0,
    setup/1,
    teardown/0,
    clear_all_metrics/0,
    clear/0,
    wait_for_metric/2,
    wait_for_metric/3,
    wait_for_inc/2,
    wait_for_inc/3,
    get_metric_value/2,
    get_all_metrics/0,
    get_metrics_by_name/1,
    metric_table/0,
    debug_dump/0
]).

-define(METRIC_TABLE, router_metrics_test_captures).
-define(DEFAULT_TIMEOUT_MS, 2000).
-define(POLL_INTERVAL_MS, 50).

%% @doc Setup metric capture with default options.
%% Installs meck expectations for router_metrics:emit_metric/3 and inc/1.
-spec setup() -> ok.
setup() ->
    setup([]).

%% @doc Setup metric capture with options.
%% Options:
%%   - {debug, true} - Enable ct:pal logging for every captured metric.
-spec setup(list()) -> ok.
setup(Options) ->
    %% Create the capture table if not exists
    case ets:whereis(?METRIC_TABLE) of
        undefined ->
            ets:new(?METRIC_TABLE, [named_table, bag, public, {write_concurrency, true}]);
        _ ->
            ets:delete_all_objects(?METRIC_TABLE)
    end,
    
    %% Ensure meck is loaded for router_metrics - always recreate to ensure clean state
    %% Check if already mocked (valid or invalid) - must unload before creating new mock
    IsMocked = try
        _ = meck:validate(router_metrics),
        true  %% meck:validate returns true/false but doesn't throw = module is mocked
    catch
        error:{not_mocked, router_metrics} -> false
    end,
    
    case IsMocked of
        true ->
            %% Already mocked (valid or invalid) - unload and recreate
            catch meck:unload(router_metrics),
            meck:new(router_metrics, [passthrough]);
        false ->
            %% Not mocked yet - create new mock
            meck:new(router_metrics, [passthrough])
    end,
    
    Debug = proplists:get_value(debug, Options, false),
    
    %% Install emit_metric/3 capture
    meck:expect(router_metrics, emit_metric, fun(MetricName, Measurements, Labels) ->
        Timestamp = erlang:system_time(millisecond),
        UniqueRef = make_ref(),
        Record = {emit_metric, MetricName, Measurements, Labels, Timestamp, UniqueRef},
        ets:insert(?METRIC_TABLE, Record),
        case Debug of
            true -> ct:pal("METRIC CAPTURED: emit_metric(~p, ~p, ~p)", [MetricName, Measurements, Labels]);
            false -> ok
        end,
        ok
    end),
    
    %% Install inc/1 capture
    meck:expect(router_metrics, inc, fun(MetricName) ->
        Timestamp = erlang:system_time(millisecond),
        UniqueRef = make_ref(),
        Record = {inc, MetricName, #{count => 1}, #{}, Timestamp, UniqueRef},
        ets:insert(?METRIC_TABLE, Record),
        case Debug of
            true -> ct:pal("METRIC CAPTURED: inc(~p)", [MetricName]);
            false -> ok
        end,
        ok
    end),
    
    %% Verify meck is properly installed by checking the module exports are mocked
    true = meck:validate(router_metrics),
    
    ok.

%% @doc Teardown metric capture - unload meck if we own it.
-spec teardown() -> ok.
teardown() ->
    catch meck:unload(router_metrics),
    ok.

%% @doc Clear captured metrics (alias for clear/0 for backwards compatibility).
-spec clear_all_metrics() -> ok.
clear_all_metrics() ->
    clear().

%% @doc Clear all captured metrics without tearing down the mock.
-spec clear() -> ok.
clear() ->
    case ets:whereis(?METRIC_TABLE) of
        undefined -> ok;
        _ -> ets:delete_all_objects(?METRIC_TABLE)
    end,
    ok.

%% @doc Get the metric capture table name.
-spec metric_table() -> atom().
metric_table() ->
    ?METRIC_TABLE.

%% @doc Wait for an emit_metric call with the given name.
%% Uses default timeout of 2000ms.
-spec wait_for_metric(atom(), pos_integer()) -> ok | {error, timeout}.
wait_for_metric(MetricName, ExpectedCount) ->
    wait_for_metric(MetricName, ExpectedCount, ?DEFAULT_TIMEOUT_MS).

%% @doc Wait for emit_metric calls with the given name until count is reached.
-spec wait_for_metric(atom(), pos_integer(), pos_integer()) -> ok | {error, timeout}.
wait_for_metric(MetricName, ExpectedCount, TimeoutMs) ->
    wait_for_metric_loop(MetricName, ExpectedCount, TimeoutMs, emit_metric).

%% @doc Wait for an inc call with the given name.
-spec wait_for_inc(atom(), pos_integer()) -> ok | {error, timeout}.
wait_for_inc(MetricName, ExpectedCount) ->
    wait_for_inc(MetricName, ExpectedCount, ?DEFAULT_TIMEOUT_MS).

%% @doc Wait for inc calls with the given name until count is reached.
-spec wait_for_inc(atom(), pos_integer(), pos_integer()) -> ok | {error, timeout}.
wait_for_inc(MetricName, ExpectedCount, TimeoutMs) ->
    wait_for_metric_loop(MetricName, ExpectedCount, TimeoutMs, inc).

%% @doc Get metric value by labels (delegates to router_r10_metrics) or by key from captured measurements.
-spec get_metric_value(atom(), map() | atom()) -> term() | undefined.
get_metric_value(MetricName, Labels) when is_map(Labels) ->
    case code:which(router_r10_metrics) of
        non_existing -> undefined;
        _ -> router_r10_metrics:get_metric_value(MetricName, Labels)
    end;
get_metric_value(MetricName, Key) ->
    case get_metrics_by_name(MetricName) of
        [] -> undefined;
        [{_, _, Measurements, _, _, _} | _] ->
            maps:get(Key, Measurements, undefined)
    end.

%% @private
wait_for_metric_loop(MetricName, ExpectedCount, TimeoutMs, Type) ->
    StartTime = erlang:system_time(millisecond),
    Deadline = StartTime + TimeoutMs,
    do_wait_loop(MetricName, ExpectedCount, StartTime, Deadline, Type).

do_wait_loop(MetricName, ExpectedCount, StartTime, Deadline, Type) ->
    Now = erlang:system_time(millisecond),
    case Now >= Deadline of
        true ->
            %% Timeout - report what we have
            ActualCount = count_metrics(MetricName, Type),
            ElapsedMs = Now - StartTime,
            ct:fail({metric_not_reached, ExpectedCount, ActualCount, elapsed_ms, ElapsedMs});
        false ->
            case count_metrics(MetricName, Type) >= ExpectedCount of
                true ->
                    ok;
                false ->
                    timer:sleep(?POLL_INTERVAL_MS),
                    do_wait_loop(MetricName, ExpectedCount, StartTime, Deadline, Type)
            end
    end.

%% @private
count_metrics(MetricName, Type) ->
    case ets:whereis(?METRIC_TABLE) of
        undefined -> 0;
        _ ->
            All = ets:tab2list(?METRIC_TABLE),
            length([M || {T, Name, _, _, _, _} = M <- All, T =:= Type, Name =:= MetricName])
    end.

%% @doc Get all captured metrics.
-spec get_all_metrics() -> list().
get_all_metrics() ->
    case ets:whereis(?METRIC_TABLE) of
        undefined -> [];
        _ -> ets:tab2list(?METRIC_TABLE)
    end.

%% @doc Get all captured metrics with a specific name.
-spec get_metrics_by_name(atom()) -> list().
get_metrics_by_name(MetricName) ->
    [M || {_, Name, _, _, _, _} = M <- get_all_metrics(), Name =:= MetricName].

%% @doc Debug: dump all captured metrics to ct:pal.
-spec debug_dump() -> ok.
debug_dump() ->
    Metrics = get_all_metrics(),
    ct:pal("=== CAPTURED METRICS (~p total) ===", [length(Metrics)]),
    lists:foreach(fun({Type, Name, Measurements, Labels, Ts, _Ref}) ->
        ct:pal("  ~p | ~p | ~p | ~p | ts=~p", [Type, Name, Measurements, Labels, Ts])
    end, Metrics),
    ok.
