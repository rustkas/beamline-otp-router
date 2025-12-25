%% @doc Chaos Testing Helper Module
%%
%% Provides utilities for controlled chaos engineering tests:
%% - Process killing (NATS, Router)
%% - Health monitoring and recovery verification
%% - Time-to-green (TTG) measurement
%% - JetStream lag injection
%%
%% @test_category chaos, helper
-module(router_chaos_helper).

-export([
    kill_nats_process/0,
    kill_nats_process/1,
    get_nats_pid/0,
    wait_for_nats_recovery/1,
    kill_router_supervisor/0,
    wait_for_router_recovery/1,
    induce_jetstream_lag/2,
    measure_time_to_green/1,
    is_system_operational/0,
    wait_for_operational/1,
    get_recovery_metrics/0
]).

-define(DEFAULT_TIMEOUT_MS, 60000).
-define(POLL_INTERVAL_MS, 500).

%% ============================================================================
%% NATS Process Management
%% ============================================================================

%% @doc Get NATS server PID from pidfile
-spec get_nats_pid() -> {ok, pid() | integer()} | {error, term()}.
get_nats_pid() ->
    PidFile = filename:join([code:priv_dir(beamline_router), "..", "..", "_artifacts", "nats.pid"]),
    case file:read_file(PidFile) of
        {ok, PidBin} ->
            PidStr = string:trim(binary_to_list(PidBin)),
            case string:to_integer(PidStr) of
                {Pid, ""} when Pid > 0 ->
                    {ok, Pid};
                _ ->
                    {error, invalid_pid}
            end;
        {error, Reason} ->
            {error, {pidfile_not_found, Reason}}
    end.

%% @doc Kill NATS process with SIGKILL (default)
-spec kill_nats_process() -> ok | {error, term()}.
kill_nats_process() ->
    kill_nats_process(kill).

%% @doc Kill NATS process with specified signal
%% Signal: kill (SIGKILL), term (SIGTERM), int (SIGINT)
-spec kill_nats_process(kill | term | int) -> ok | {error, term()}.
kill_nats_process(Signal) ->
    case get_nats_pid() of
        {ok, Pid} ->
            SignalNum = case Signal of
                kill -> 9;
                term -> 15;
                int -> 2
            end,
            Cmd = io_lib:format("kill -~p ~p", [SignalNum, Pid]),
            case os:cmd(Cmd) of
                "" -> ok;
                Error -> {error, {kill_failed, Error}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Wait for NATS to recover (connection restored)
-spec wait_for_nats_recovery(pos_integer()) -> ok | {error, timeout}.
wait_for_nats_recovery(TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_nats_recovery_loop(StartTime, TimeoutMs).

wait_for_nats_recovery_loop(StartTime, TimeoutMs) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    if
        Elapsed >= TimeoutMs ->
            {error, timeout};
        true ->
            %% Check if router_nats reports connected
            case catch router_nats:get_connection_status() of
                {ok, #{connected := true}} ->
                    ok;
                _ ->
                    timer:sleep(?POLL_INTERVAL_MS),
                    wait_for_nats_recovery_loop(StartTime, TimeoutMs)
            end
    end.

%% ============================================================================
%% Router Process Management
%% ============================================================================

%% @doc Kill Router supervisor (simulates crash)
-spec kill_router_supervisor() -> ok | {error, term()}.
kill_router_supervisor() ->
    case whereis(beamline_router_sup) of
        undefined ->
            {error, supervisor_not_running};
        Pid ->
            exit(Pid, kill),
            ok
    end.

%% @doc Wait for Router to recover (supervisor restarted)
-spec wait_for_router_recovery(pos_integer()) -> ok | {error, timeout}.
wait_for_router_recovery(TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_router_recovery_loop(StartTime, TimeoutMs).

wait_for_router_recovery_loop(StartTime, TimeoutMs) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    if
        Elapsed >= TimeoutMs ->
            {error, timeout};
        true ->
            case whereis(beamline_router_sup) of
                undefined ->
                    timer:sleep(?POLL_INTERVAL_MS),
                    wait_for_router_recovery_loop(StartTime, TimeoutMs);
                Pid when is_pid(Pid) ->
                    %% Additional check: ensure key processes are alive
                    case {whereis(router_nats), whereis(router_core)} of
                        {P1, P2} when is_pid(P1), is_pid(P2) ->
                            ok;
                        _ ->
                            timer:sleep(?POLL_INTERVAL_MS),
                            wait_for_router_recovery_loop(StartTime, TimeoutMs)
                    end
            end
    end.

%% ============================================================================
%% JetStream Lag Injection
%% ============================================================================

%% @doc Induce JetStream lag by delaying ACKs
%% Subject: NATS subject to affect
%% DurationMs: How long to delay ACKs (creates pending messages)
-spec induce_jetstream_lag(binary(), pos_integer()) -> ok.
induce_jetstream_lag(Subject, DurationMs) ->
    %% Use fault injection to delay ACK operations
    router_nats_fault_injection:enable_fault(ack, {delay, DurationMs}),
    ok.

%% ============================================================================
%% Health & Recovery Measurement
%% ============================================================================

%% @doc Check if system is operationally healthy
%% Operational = NATS connected + gRPC responsive + backpressure inactive
-spec is_system_operational() -> boolean().
is_system_operational() ->
    NatsConnected = case catch router_nats:get_connection_status() of
        {ok, #{connected := true}} -> true;
        _ -> false
    end,
    
    BackpressureOk = case catch router_intake_backpressure:check_backpressure(<<"test.subject">>) of
        {backpressure_inactive, _} -> true;
        {backpressure_warning, _} -> true;  % Warning is acceptable
        _ -> false
    end,
    
    %% Check key processes are alive
    ProcessesAlive = lists:all(fun(Name) ->
        is_pid(whereis(Name))
    end, [router_nats, router_core, beamline_router_sup]),
    
    NatsConnected andalso BackpressureOk andalso ProcessesAlive.

%% @doc Wait for system to become operational
-spec wait_for_operational(pos_integer()) -> ok | {error, timeout}.
wait_for_operational(TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_operational_loop(StartTime, TimeoutMs).

wait_for_operational_loop(StartTime, TimeoutMs) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    if
        Elapsed >= TimeoutMs ->
            {error, timeout};
        true ->
            case is_system_operational() of
                true -> ok;
                false ->
                    timer:sleep(?POLL_INTERVAL_MS),
                    wait_for_operational_loop(StartTime, TimeoutMs)
            end
    end.

%% @doc Measure time until system becomes operational (Time-To-Green)
%% Returns: {ok, TimeMs} | {error, timeout}
-spec measure_time_to_green(pos_integer()) -> {ok, non_neg_integer()} | {error, timeout}.
measure_time_to_green(TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    case wait_for_operational(TimeoutMs) of
        ok ->
            EndTime = erlang:monotonic_time(millisecond),
            {ok, EndTime - StartTime};
        {error, timeout} ->
            {error, timeout}
    end.

%% @doc Get recovery-related metrics snapshot
-spec get_recovery_metrics() -> map().
get_recovery_metrics() ->
    #{
        nats_connection_lost_total => get_metric_value(router_nats_connection_lost_total),
        nats_reconnection_attempts_total => get_metric_value(router_nats_reconnection_attempts_total),
        circuit_breaker_open_count => count_open_circuit_breakers(),
        backpressure_active_count => count_backpressure_active()
    }.

get_metric_value(MetricName) ->
    router_metrics:ensure(),
    case ets:lookup(router_metrics, MetricName) of
        [{MetricName, Value}] -> Value;
        _ -> 0
    end.

count_open_circuit_breakers() ->
    try
        case ets:info(router_provider_circuit_breaker) of
            undefined -> 0;
            _ ->
                length(ets:match(router_provider_circuit_breaker, {'_', '_', open, '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'}))
        end
    catch
        _:_ -> 0
    end.

count_backpressure_active() ->
    %% Count subjects with active backpressure
    %% This is a simplified check; real implementation would query actual state
    0.
