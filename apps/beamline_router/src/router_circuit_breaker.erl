-module(router_circuit_breaker).

-doc "Circuit Breaker State Machine for Provider Health Tracking".
%%
%% This module implements a per-provider circuit breaker pattern to prevent
%% cascading failures by temporarily blocking requests to unhealthy providers.
%%
%% Circuit States:
%% - closed: Normal operation, requests flow through
%% - open: Failing fast, requests fail immediately without calling provider
%% - half_open: Testing recovery, limited requests allowed
%%
%% State Transitions:
%% - closed → open: When failure_threshold or error_rate_threshold exceeded
%% - open → half_open: After timeout_ms elapsed
%% - half_open → closed: When success_threshold consecutive successes
%% - half_open → open: When any failure occurs
%%
%% Performance:
%% - State lookups are optimized using ETS tables
%% - Sliding window error rate calculation for accurate health tracking
%% - Configurable thresholds for different failure scenarios
%%
%% @see DEVELOPER_GUIDE.md For development workflow and key modules
%% @see PERFORMANCE_GUIDE.md#circuit-breaker-performance For performance optimization
%% @see src/router_r10_metrics.erl Metrics access layer
-behaviour(gen_server).

-include("beamline_router.hrl").

%% Public API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([record_state/2, record_state_with_config/3, should_allow/2, record_success/2, record_failure/2]).
-export([get_state/2, is_open/2, is_half_open/2, is_closed/2, get_status/2]).
-export([get_table_size/0, get_table_memory/0, check_size_limit/0]).
-export([force_recovery/2, force_recovery_to_half_open/2, get_recovery_status/2, reset_recovery_state/2]).

%% Internal state
-record(state, {
    table :: ets:tid() | atom()  % For named_table, this is atom (table name)
}).

%% Circuit breaker state record (stored in ETS)
-record(circuit_breaker_state, {
    key :: {tenant_id(), provider_id()},
    state :: closed | open | half_open,
    failure_count :: integer(),
    success_count :: integer(),
    state_changed_at :: integer(),  % Unix timestamp (milliseconds)
    last_failure_time :: integer() | undefined,
    last_success_time :: integer() | undefined,
    half_open_calls_count :: integer(),
    total_requests :: integer(),     % Over window (deprecated, use window_events)
    total_failures :: integer(),     % Over window (deprecated, use window_events)
    total_successes :: integer(),    % Over window (deprecated, use window_events)
    error_rate :: float(),           % Calculated: total_failures / total_requests (deprecated, use calculate_sliding_error_rate)
    window_events :: [{integer(), success | failure}],  % Sliding window: [{timestamp_ms, event_type}, ...]
    config :: map()                  % Circuit breaker configuration
}).

%% ETS table name
-define(TABLE, router_provider_circuit_breaker).

%% Default configuration
-define(DEFAULT_FAILURE_THRESHOLD, 5).
-define(DEFAULT_SUCCESS_THRESHOLD, 2).
-define(DEFAULT_TIMEOUT_MS, 60000).
-define(DEFAULT_HALF_OPEN_MAX_CALLS, 3).
-define(DEFAULT_ERROR_RATE_THRESHOLD, 0.5).
-define(DEFAULT_ERROR_RATE_WINDOW_SECONDS, 60).
-define(DEFAULT_LATENCY_THRESHOLD_MS, 5000).  %% 5 seconds for R10

%% ============================================================================
%% Public API
%% ============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% **IMPORTANT**: This function only updates configuration/state, it does NOT check
%% timeout or trigger state transitions (e.g., open → half_open).
%%
%% For timeout-based transitions, use `should_allow/2` instead, which is the ONLY
%% function that checks timeout and triggers `maybe_transition_on_timeout/1`.
%%
%% This is primarily a management/test API. Production code should use `should_allow/2`
%% to check circuit state and trigger timeout transitions.
%%
-spec record_state(tenant_id(), provider_id()) -> ok.
record_state(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {record_state, TenantId, ProviderId, undefined}, 5000).

%%
%% **IMPORTANT**: This function only updates configuration/state, it does NOT check
%% timeout or trigger state transitions (e.g., open → half_open).
%%
%% For timeout-based transitions, use `should_allow/2` instead, which is the ONLY
%% function that checks timeout and triggers `maybe_transition_on_timeout/1`.
%%
%% This is primarily a management/test API. Production code should use `should_allow/2`
%% to check circuit state and trigger timeout transitions.
%%
-spec record_state_with_config(tenant_id(), provider_id(), map() | undefined) -> ok.
record_state_with_config(TenantId, ProviderId, Config) ->
    gen_server:call(?MODULE, {record_state, TenantId, ProviderId, Config}, 5000).

%%
%% **IMPORTANT**: This is the ONLY function that:
%% 1. Checks timeout and triggers `maybe_transition_on_timeout/1` (open → half_open)
%% 2. Updates circuit breaker state based on timeout
%% 3. Returns whether request should be allowed
%%
%% This function should be called before making external requests to check circuit state
%% and trigger timeout-based state transitions.
%%
%% Do NOT use `record_state/2` or `record_state_with_config/3` to trigger timeout
%% transitions - they only update configuration and do not check timeout.
%%
-spec should_allow(tenant_id(), provider_id()) -> {ok, allow} | {error, circuit_open}.
should_allow(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {should_allow, TenantId, ProviderId}).

-spec record_success(tenant_id(), provider_id()) -> ok.
record_success(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {record_success, TenantId, ProviderId}, 5000).

-spec record_failure(tenant_id(), provider_id()) -> ok.
record_failure(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {record_failure, TenantId, ProviderId}, 5000).

-spec get_state(tenant_id(), provider_id()) -> {ok, closed | open | half_open} | {error, not_found}.
get_state(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {get_state, TenantId, ProviderId}, 5000).

%% Returns detailed status including timestamps, counters, and configuration
-spec get_status(tenant_id(), provider_id()) -> {ok, map()} | {error, not_found}.
get_status(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {get_status, TenantId, ProviderId}, 5000).

-spec is_open(tenant_id(), provider_id()) -> boolean().
is_open(TenantId, ProviderId) ->
    case get_state(TenantId, ProviderId) of
        {ok, open} -> true;
        _ -> false
    end.

-spec is_half_open(tenant_id(), provider_id()) -> boolean().
is_half_open(TenantId, ProviderId) ->
    case get_state(TenantId, ProviderId) of
        {ok, half_open} -> true;
        _ -> false
    end.

%% Use with caution - only for manual recovery scenarios
%% This bypasses the normal recovery flow and immediately closes the circuit
-spec force_recovery(tenant_id(), provider_id()) -> ok | {error, term()}.
force_recovery(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {force_recovery, TenantId, ProviderId}, 5000).

%% Use with caution - only for manual recovery scenarios
%% This bypasses the timeout and immediately transitions to half-open for testing
-spec force_recovery_to_half_open(tenant_id(), provider_id()) -> ok | {error, term()}.
force_recovery_to_half_open(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {force_recovery_to_half_open, TenantId, ProviderId}, 5000).

%% Returns detailed recovery information for monitoring and debugging
-spec get_recovery_status(tenant_id(), provider_id()) -> {ok, map()} | {error, term()}.
get_recovery_status(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {get_recovery_status, TenantId, ProviderId}, 5000).

%% Use with caution - only for manual recovery scenarios
-spec reset_recovery_state(tenant_id(), provider_id()) -> ok | {error, term()}.
reset_recovery_state(TenantId, ProviderId) ->
    gen_server:call(?MODULE, {reset_recovery_state, TenantId, ProviderId}, 5000).

-spec is_closed(tenant_id(), provider_id()) -> boolean().
is_closed(TenantId, ProviderId) ->
    case get_state(TenantId, ProviderId) of
        {ok, closed} -> true;
        _ -> false
    end.

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

init(Args) ->
    process_flag(trap_exit, true),
    try
        do_init(Args)
    catch
        Class:Reason:Stack ->
            router_logger:error(~"router_circuit_breaker init failed", #{
                ~"class" => Class,
                ~"reason" => sanitize_error_for_logging(Reason),
                ~"stack" => sanitize_error_for_logging(Stack)
            }),
            {stop, {init_failed, Class, Reason}}
    end.

%% All initialization logic is here to avoid crashes in init/1
-spec do_init(list()) -> {ok, #state{}} | {stop, term()}.
do_init(_Args) ->
    %% Ensure ETS table exists (safe creation/cleanup)
    Table = ensure_ets_table(?TABLE),
    
    State = #state{
        table = Table
    },
    
    {ok, State}.

%% Handles existing table cleanup and creation
-spec ensure_ets_table(atom()) -> atom().
ensure_ets_table(Name) when is_atom(Name) ->
    case ets:info(Name) of
        undefined ->
            %% Table doesn't exist - create it
            ets:new(Name, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true},
                {keypos, 2}  % Use the 'key' field (2nd element) as ETS key
            ]);
        _Info ->
            %% Table already exists - delete and recreate (cleanup from previous run/test)
            ets:delete(Name),
            ets:new(Name, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true},
                {keypos, 2}  % Use the 'key' field (2nd element) as ETS key
            ])
    end.

handle_call(reset_all, _From, State = #state{table = Table}) ->
    %% Safe reset: clear all states but keep process and ETS table alive
    %% This is called from test utilities, should not kill the process
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(~"Circuit breaker reset_all: ETS table undefined", #{
                ~"event" => ~"circuit_breaker_reset_all"
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(~"Circuit breaker reset_all: table cleared", #{
                ~"event" => ~"circuit_breaker_reset_all"
            }),
            {reply, ok, State}
    end;

handle_call({record_state, TenantId, ProviderId, Config}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            %% Initialize new circuit breaker state (closed by default)
            Now = erlang:system_time(millisecond),
            %% Use provided config or default config
            FinalConfig = case Config of
                CB when is_map(CB) ->
                    CB;
                _ ->
                    get_default_config()
            end,
            NewState = #circuit_breaker_state{
                key = Key,
                state = closed,
                failure_count = 0,
                success_count = 0,
                state_changed_at = Now,
                last_failure_time = undefined,
                last_success_time = undefined,
                half_open_calls_count = 0,
                total_requests = 0,
                total_failures = 0,
                total_successes = 0,
                error_rate = 0.0,
                window_events = [],
                config = FinalConfig
            },
            ets:insert(?TABLE, NewState),
            {reply, ok, State};
        [#circuit_breaker_state{} = ExistingState] ->
            %% State already exists, update config if provided
            case Config of
                CB when is_map(CB) ->
                    UpdatedState = ExistingState#circuit_breaker_state{config = CB},
                    ets:insert(?TABLE, UpdatedState),
                    {reply, ok, State};
                _ ->
                    %% No config provided, keep existing
                    {reply, ok, State}
            end
    end;

handle_call({should_allow, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            %% No state exists, allow (will be created on first call)
            {reply, {ok, allow}, State};
        [#circuit_breaker_state{config = Config} = CBState] ->
            %% Check current state and update if needed (timeout transitions)
            %% Also clean window events if needed (optimization: only when checking timeout)
            UpdatedState = maybe_transition_on_timeout(CBState),
            %% Clean window events periodically (when checking should_allow)
            WindowSeconds = maps:get(~"error_rate_window_seconds", Config, ?DEFAULT_ERROR_RATE_WINDOW_SECONDS),
            Now = erlang:system_time(millisecond),
            CleanedState = UpdatedState#circuit_breaker_state{
                window_events = clean_window_events(UpdatedState#circuit_breaker_state.window_events, WindowSeconds, Now)
            },
            case CleanedState#circuit_breaker_state.state of
                closed ->
                    ets:insert(?TABLE, CleanedState),
                    {reply, {ok, allow}, State};
                open ->
                    ets:insert(?TABLE, CleanedState),
                    %% Emit fail-fast metric
                    {TenantId, ProviderId} = CleanedState#circuit_breaker_state.key,
                    router_metrics:emit_metric(router_circuit_breaker_events_total, #{count => 1}, #{
                        tenant_id => TenantId,
                        provider_id => ProviderId,
                        event_type => ~"fail_fast"
                    }),
                    router_logger:warn(~"Circuit breaker is open, failing fast", #{
                        ~"tenant_id" => TenantId,
                        ~"provider_id" => ProviderId,
                        ~"circuit_state" => ~"open"
                    }),
                    {reply, {error, circuit_open}, State};
                half_open ->
                    %% Check half-open rate limit
                    MaxCalls = maps:get(~"half_open_max_calls", Config, ?DEFAULT_HALF_OPEN_MAX_CALLS),
                    if
                        CleanedState#circuit_breaker_state.half_open_calls_count < MaxCalls ->
                            %% Allow request, increment counter
                            NewState = CleanedState#circuit_breaker_state{
                                half_open_calls_count = CleanedState#circuit_breaker_state.half_open_calls_count + 1
                            },
                            ets:insert(?TABLE, NewState),
                            {reply, {ok, allow}, State};
                        true ->
                            %% Rate limit exceeded, fail fast
                            ets:insert(?TABLE, CleanedState),
                            %% Emit fail-fast metric
                            {TenantId, ProviderId} = CleanedState#circuit_breaker_state.key,
                            router_metrics:emit_metric(router_circuit_breaker_events_total, #{count => 1}, #{
                                tenant_id => TenantId,
                                provider_id => ProviderId,
                                event_type => ~"fail_fast"
                            }),
                            router_logger:warn(~"Circuit breaker is open, failing fast", #{
                                ~"tenant_id" => TenantId,
                                ~"provider_id" => ProviderId,
                                ~"circuit_state" => ~"half_open",
                                ~"half_open_calls_count" => CleanedState#circuit_breaker_state.half_open_calls_count,
                                ~"half_open_max_calls" => MaxCalls
                            }),
                            {reply, {error, circuit_open}, State}
                    end
            end
    end;

handle_call({record_success, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            %% No state exists, create and record success
            Now = erlang:system_time(millisecond),
            DefaultConfig = get_default_config(),
            NewState = #circuit_breaker_state{
                key = Key,
                state = closed,
                failure_count = 0,
                success_count = 1,
                state_changed_at = Now,
                last_failure_time = undefined,
                last_success_time = Now,
                half_open_calls_count = 0,
                total_requests = 1,
                total_failures = 0,
                total_successes = 1,
                error_rate = 0.0,
                window_events = [{Now, success}],
                config = DefaultConfig
            },
            ets:insert(?TABLE, NewState),
            
            %% Emit event metric
            router_metrics:emit_metric(router_circuit_breaker_events_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                event_type => ~"success"
            }),
            
            %% Emit circuit breaker state gauge (closed)
            router_metrics:emit_metric(router_circuit_breaker_state, #{value => 0.0}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                state => ~"closed"
            }),
            
            {reply, ok, State};
        [#circuit_breaker_state{} = CBState] ->
            UpdatedState = update_on_success(CBState),
            ets:insert(?TABLE, UpdatedState),
            
            %% Emit event metric
            router_metrics:emit_metric(router_circuit_breaker_events_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                event_type => ~"success"
            }),
            
            {reply, ok, State}
    end;

handle_call({record_failure, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            %% No state exists, create and record failure
            Now = erlang:system_time(millisecond),
            DefaultConfig = get_default_config(),
            NewState = #circuit_breaker_state{
                key = Key,
                state = closed,
                failure_count = 1,
                success_count = 0,
                state_changed_at = Now,
                last_failure_time = Now,
                last_success_time = undefined,
                half_open_calls_count = 0,
                total_requests = 1,
                total_failures = 1,
                total_successes = 0,
                error_rate = 1.0,
                window_events = [{Now, failure}],
                config = DefaultConfig
            },
            UpdatedState = maybe_transition_to_open(NewState),
            ets:insert(?TABLE, UpdatedState),
            
            %% Emit event metric
            router_metrics:emit_metric(router_circuit_breaker_events_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                event_type => ~"failure"
            }),
            
            %% Emit circuit breaker state gauge (closed initially, may transition to open)
            router_metrics:emit_metric(router_circuit_breaker_state, #{value => 0.0}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                state => ~"closed"
            }),
            
            {reply, ok, State};
        [#circuit_breaker_state{} = CBState] ->
            UpdatedState = update_on_failure(CBState),
            ets:insert(?TABLE, UpdatedState),
            
            %% Emit event metric
            router_metrics:emit_metric(router_circuit_breaker_events_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                event_type => ~"failure"
            }),
            
            {reply, ok, State}
    end;

handle_call({get_state, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    %% Use named table directly (keypos=2 means 'key' field is used as ETS key)
    case ets:lookup(?TABLE, Key) of
        [] ->
            {reply, {error, not_found}, State};
        [#circuit_breaker_state{state = CurrentState}] ->
            {reply, {ok, CurrentState}, State}
    end;

handle_call({get_status, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            {reply, {error, not_found}, State};
        [#circuit_breaker_state{
            state = CurrentState,
            state_changed_at = StateChangedAt,
            half_open_calls_count = HalfOpenCalls,
            failure_count = FailureCount,
            success_count = SuccessCount,
            config = Config
        }] ->
            Status = #{
                state => CurrentState,
                state_changed_at => StateChangedAt,
                half_open_calls_count => HalfOpenCalls,
                failure_count => FailureCount,
                success_count => SuccessCount,
                config => Config
            },
            {reply, {ok, Status}, State}
    end;
handle_call({force_recovery, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            {reply, {error, not_found}, State};
        [#circuit_breaker_state{} = ExistingState] ->
            Now = erlang:system_time(millisecond),
            NewState = ExistingState#circuit_breaker_state{
                state = closed,
                state_changed_at = Now,
                failure_count = 0,
                success_count = 0,
                last_success_time = Now
            },
            ets:insert(?TABLE, NewState),
            {reply, ok, State}
    end;

handle_call({force_recovery_to_half_open, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            {reply, {error, not_found}, State};
        [#circuit_breaker_state{} = ExistingState] ->
            Now = erlang:system_time(millisecond),
            NewState = ExistingState#circuit_breaker_state{
                state = half_open,
                state_changed_at = Now,
                half_open_calls_count = 0,
                window_events = [] %% Start fresh for half-open testing
            },
            ets:insert(?TABLE, NewState),
            {reply, ok, State}
    end;

handle_call({get_recovery_status, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            {reply, {error, not_found}, State};
        [#circuit_breaker_state{config = Config, state = CurrentState, state_changed_at = ChangedAt}] ->
            Now = erlang:system_time(millisecond),
            TimeoutMs = maps:get(~"timeout_ms", Config, ?DEFAULT_TIMEOUT_MS),
            Elapsed = Now - ChangedAt,
            RemainingMs = case CurrentState of
                open -> max(0, TimeoutMs - Elapsed);
                _ -> 0
            end,
            Status = #{
                state => CurrentState,
                state_changed_at => ChangedAt,
                elapsed_ms => Elapsed,
                timeout_ms => TimeoutMs,
                remaining_ms => RemainingMs
            },
            {reply, {ok, Status}, State}
    end;

handle_call({reset_recovery_state, TenantId, ProviderId}, _From, State) ->
    Key = {TenantId, ProviderId},
    case ets:lookup(?TABLE, Key) of
        [] ->
            {reply, {error, not_found}, State};
        [#circuit_breaker_state{} = ExistingState] ->
            Now = erlang:system_time(millisecond),
            NewState = ExistingState#circuit_breaker_state{
                state = closed,
                state_changed_at = Now,
                failure_count = 0,
                success_count = 0,
                last_failure_time = undefined,
                last_success_time = undefined,
                half_open_calls_count = 0,
                error_rate = 0.0,
                window_events = []
            },
            ets:insert(?TABLE, NewState),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    %% Log EXIT signals to diagnose process crashes
    router_logger:error(~"Circuit breaker received EXIT", #{
        ~"event" => ~"circuit_breaker_exit",
        ~"from" => From,
        ~"reason" => Reason
    }),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    %% Log termination to diagnose process crashes
    router_logger:error(~"Circuit breaker terminating", #{
        ~"event" => ~"circuit_breaker_terminate",
        ~"reason" => Reason
    }),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal functions
%% ============================================================================

get_default_config() ->
    #{
        ~"failure_threshold" => ?DEFAULT_FAILURE_THRESHOLD,
        ~"success_threshold" => ?DEFAULT_SUCCESS_THRESHOLD,
        ~"timeout_ms" => ?DEFAULT_TIMEOUT_MS,
        ~"half_open_max_calls" => ?DEFAULT_HALF_OPEN_MAX_CALLS,
        ~"error_rate_threshold" => ?DEFAULT_ERROR_RATE_THRESHOLD,
        ~"error_rate_window_seconds" => ?DEFAULT_ERROR_RATE_WINDOW_SECONDS,
        ~"latency_threshold_ms" => ?DEFAULT_LATENCY_THRESHOLD_MS
    }.

%% Optimized: Only checks timeout when state is open (most common case)
maybe_transition_on_timeout(#circuit_breaker_state{state = open, state_changed_at = ChangedAt, config = Config, key = {TenantId, ProviderId}} = CBState) ->
    Now = erlang:system_time(millisecond),
    TimeoutMs = maps:get(~"timeout_ms", Config, ?DEFAULT_TIMEOUT_MS),
    Elapsed = Now - ChangedAt,
    if
        Elapsed >= TimeoutMs ->
            %% Transition to half-open
            %% Clean window events on transition (start fresh for half-open testing)
            WindowSeconds = maps:get(~"error_rate_window_seconds", Config, ?DEFAULT_ERROR_RATE_WINDOW_SECONDS),
            CleanedWindowEvents = clean_window_events(CBState#circuit_breaker_state.window_events, WindowSeconds, Now),
            
            %% Emit state transition metric
            router_metrics:emit_metric(router_circuit_breaker_state_transitions_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                from => ~"open",
                to => ~"half_open"
            }),
            
            %% Emit circuit breaker state gauge
            router_metrics:emit_metric(router_circuit_breaker_state, #{value => 2.0}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                state => ~"half_open"
            }),
            
            %% Emit trigger reason metric (timeout)
            router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{value => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                reason => ~"timeout_elapsed"
            }),
            
            %% Emit timeout transition metric
            router_metrics:emit_metric(router_circuit_breaker_timeout_transitions_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                transition_type => ~"open_to_half_open"
            }),
            
            %% Log circuit breaker half-opened event with enhanced context
            router_logger:info(~"Circuit breaker half-opened for provider", #{
                ~"event" => ~"circuit_breaker_state_changed",
                ~"tenant_id" => TenantId,
                ~"provider_id" => ProviderId,
                ~"from" => ~"open",
                ~"to" => ~"half_open",
                ~"reason" => ~"timeout_elapsed",
                ~"timeout_elapsed_ms" => Elapsed,
                ~"timeout_ms" => TimeoutMs
            }),
            
            CBState#circuit_breaker_state{
                state = half_open,
                state_changed_at = Now,
                half_open_calls_count = 0,
                failure_count = 0,
                window_events = CleanedWindowEvents
            };
        true ->
            %% Emit timeout remaining metric (gauge) when circuit is open
            RemainingMs = max(0, TimeoutMs - Elapsed),
            router_metrics:emit_metric(router_circuit_breaker_timeout_remaining_ms, #{value => RemainingMs}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            CBState
    end;
maybe_transition_on_timeout(CBState) ->
    %% No timeout-based transitions for closed or half-open states
    CBState.

update_on_success(#circuit_breaker_state{state = half_open, success_count = SuccessCount, config = Config} = CBState) ->
    Now = erlang:system_time(millisecond),
    SuccessThreshold = maps:get(~"success_threshold", Config, ?DEFAULT_SUCCESS_THRESHOLD),
    WindowSeconds = maps:get(~"error_rate_window_seconds", Config, ?DEFAULT_ERROR_RATE_WINDOW_SECONDS),
    NewSuccessCount = SuccessCount + 1,
    
    %% Update sliding window
    NewWindowEvents = add_window_event(CBState#circuit_breaker_state.window_events, Now, success, WindowSeconds, Now),
    NewErrorRate = calculate_sliding_error_rate(NewWindowEvents, WindowSeconds, Now),
    
    %% Keep backward compatibility (deprecated fields)
    NewTotalRequests = CBState#circuit_breaker_state.total_requests + 1,
    NewTotalSuccesses = CBState#circuit_breaker_state.total_successes + 1,
    NewTotalFailures = CBState#circuit_breaker_state.total_failures,
    
    if
        NewSuccessCount >= SuccessThreshold ->
            %% Transition to closed
            {TenantId, ProviderId} = CBState#circuit_breaker_state.key,
            
            %% Emit state transition metric
            router_metrics:emit_metric(router_circuit_breaker_state_transitions_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                from => ~"half_open",
                to => ~"closed"
            }),
            
            %% Emit circuit breaker state gauge
            router_metrics:emit_metric(router_circuit_breaker_state, #{value => 0.0}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                state => ~"closed"
            }),
            
            %% Emit error-rate metric (gauge) - updated on state change
            router_metrics:emit_metric(router_circuit_breaker_error_rate, #{value => NewErrorRate}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            
            %% Count events in window for metrics and logging
            WindowStart = Now - (WindowSeconds * 1000),
            EventsInWindow = [{T, Type} || {T, Type} <- NewWindowEvents, T >= WindowStart],
            SuccessesInWindow = length([{T, Type} || {T, Type} <- EventsInWindow, Type =:= success]),
            TotalInWindow = length(EventsInWindow),
            
            %% Emit window metrics (counters)
            router_metrics:emit_metric(router_circuit_breaker_window_requests_total, #{count => TotalInWindow}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            router_metrics:emit_metric(router_circuit_breaker_window_failures_total, #{count => (TotalInWindow - SuccessesInWindow)}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            
            %% Log circuit breaker closed event with enhanced context
            router_logger:info(~"Circuit breaker closed for provider", #{
                ~"event" => ~"circuit_breaker_state_changed",
                ~"tenant_id" => TenantId,
                ~"provider_id" => ProviderId,
                ~"from" => ~"half_open",
                ~"to" => ~"closed",
                ~"reason" => ~"success_threshold_met",
                ~"success_count" => NewSuccessCount,
                ~"success_threshold" => SuccessThreshold,
                ~"error_rate" => NewErrorRate,
                ~"window_seconds" => WindowSeconds,
                ~"successes_in_window" => SuccessesInWindow,
                ~"total_in_window" => TotalInWindow
            }),
            
            CBState#circuit_breaker_state{
                state = closed,
                state_changed_at = Now,
                success_count = 0,
                failure_count = 0,
                last_success_time = Now,
                half_open_calls_count = 0,
                total_requests = NewTotalRequests,
                total_successes = NewTotalSuccesses,
                total_failures = NewTotalFailures,
                error_rate = NewErrorRate,
                window_events = NewWindowEvents
            };
        true ->
            %% Stay in half-open, increment success count
            %% Emit error-rate metric (gauge) - updated on success
            {TenantId, ProviderId} = CBState#circuit_breaker_state.key,
            router_metrics:emit_metric(router_circuit_breaker_error_rate, #{value => NewErrorRate}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            
            CBState#circuit_breaker_state{
                success_count = NewSuccessCount,
                last_success_time = Now,
                total_requests = NewTotalRequests,
                total_successes = NewTotalSuccesses,
                total_failures = NewTotalFailures,
                error_rate = NewErrorRate,
                window_events = NewWindowEvents
            }
    end;
update_on_success(#circuit_breaker_state{} = CBState) ->
    Now = erlang:system_time(millisecond),
    WindowSeconds = maps:get(~"error_rate_window_seconds", CBState#circuit_breaker_state.config, ?DEFAULT_ERROR_RATE_WINDOW_SECONDS),
    
    %% Update sliding window
    NewWindowEvents = add_window_event(CBState#circuit_breaker_state.window_events, Now, success, WindowSeconds, Now),
    NewErrorRate = calculate_sliding_error_rate(NewWindowEvents, WindowSeconds, Now),
    
    %% Keep backward compatibility (deprecated fields)
    NewTotalRequests = CBState#circuit_breaker_state.total_requests + 1,
    NewTotalSuccesses = CBState#circuit_breaker_state.total_successes + 1,
    
    %% Emit error-rate metric (gauge) - updated on success
    {TenantId, ProviderId} = CBState#circuit_breaker_state.key,
    router_metrics:emit_metric(router_circuit_breaker_error_rate, #{value => NewErrorRate}, #{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    
    %% Reset failure count on success (consecutive failures)
    CBState#circuit_breaker_state{
        failure_count = 0,
        last_success_time = Now,
        total_requests = NewTotalRequests,
        total_successes = NewTotalSuccesses,
        total_failures = CBState#circuit_breaker_state.total_failures,
        error_rate = NewErrorRate,
        window_events = NewWindowEvents
    }.

update_on_failure(#circuit_breaker_state{state = half_open, config = Config, key = {TenantId, ProviderId}} = CBState) ->
    Now = erlang:system_time(millisecond),
    
    %% Any failure in half-open immediately opens circuit
    WindowSeconds = maps:get(~"error_rate_window_seconds", Config, ?DEFAULT_ERROR_RATE_WINDOW_SECONDS),
    
    %% Update sliding window
    NewWindowEvents = add_window_event(CBState#circuit_breaker_state.window_events, Now, failure, WindowSeconds, Now),
    NewErrorRate = calculate_sliding_error_rate(NewWindowEvents, WindowSeconds, Now),
    
    %% Keep backward compatibility (deprecated fields)
    NewTotalRequests = CBState#circuit_breaker_state.total_requests + 1,
    NewTotalFailures = CBState#circuit_breaker_state.total_failures + 1,
    
    %% Emit state transition metric
    router_metrics:emit_metric(router_circuit_breaker_state_transitions_total, #{count => 1}, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        from => ~"half_open",
        to => ~"open"
    }),
    
    %% Emit circuit breaker state gauge
    router_metrics:emit_metric(router_circuit_breaker_state, #{value => 1.0}, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => ~"open"
    }),
    
    %% Emit trigger reason metric
    router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{value => 1}, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        reason => ~"half_open_failure"
    }),
    
    %% Emit error-rate metric (gauge)
    router_metrics:emit_metric(router_circuit_breaker_error_rate, #{value => NewErrorRate}, #{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    
    %% Log circuit breaker opened event (from half-open) with enhanced context
    WindowStart = Now - (WindowSeconds * 1000),
    EventsInWindow = [{T, Type} || {T, Type} <- NewWindowEvents, T >= WindowStart],
    FailuresInWindow = length([{T, Type} || {T, Type} <- EventsInWindow, Type =:= failure]),
    TotalInWindow = length(EventsInWindow),
    
    router_logger:warn(~"Circuit breaker opened for provider", #{
        ~"event" => ~"circuit_breaker_state_changed",
        ~"tenant_id" => TenantId,
        ~"provider_id" => ProviderId,
        ~"from" => ~"half_open",
        ~"to" => ~"open",
        ~"reason" => ~"half_open_failure",
        ~"error_rate" => NewErrorRate,
        ~"window_seconds" => WindowSeconds,
        ~"failures_in_window" => FailuresInWindow,
        ~"total_in_window" => TotalInWindow
    }),
    
    UpdatedState = CBState#circuit_breaker_state{
        state = open,
        state_changed_at = Now,
        failure_count = 1,
        success_count = 0,
        last_failure_time = Now,
        half_open_calls_count = 0,
        total_requests = NewTotalRequests,
        total_failures = NewTotalFailures,
        total_successes = CBState#circuit_breaker_state.total_successes,
        error_rate = NewErrorRate,
        window_events = NewWindowEvents
    },
    UpdatedState;
update_on_failure(#circuit_breaker_state{config = Config} = CBState) ->
    Now = erlang:system_time(millisecond),
    WindowSeconds = maps:get(~"error_rate_window_seconds", Config, ?DEFAULT_ERROR_RATE_WINDOW_SECONDS),
    NewFailureCount = CBState#circuit_breaker_state.failure_count + 1,
    
    %% Update sliding window
    NewWindowEvents = add_window_event(CBState#circuit_breaker_state.window_events, Now, failure, WindowSeconds, Now),
    NewErrorRate = calculate_sliding_error_rate(NewWindowEvents, WindowSeconds, Now),
    
    %% Keep backward compatibility (deprecated fields)
    NewTotalRequests = CBState#circuit_breaker_state.total_requests + 1,
    NewTotalFailures = CBState#circuit_breaker_state.total_failures + 1,
    
    %% Emit error-rate metric (gauge) - updated on failure
    {TenantId, ProviderId} = CBState#circuit_breaker_state.key,
    router_metrics:emit_metric(router_circuit_breaker_error_rate, #{value => NewErrorRate}, #{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    
    UpdatedState = CBState#circuit_breaker_state{
        failure_count = NewFailureCount,
        success_count = 0,
        last_failure_time = Now,
        total_requests = NewTotalRequests,
        total_failures = NewTotalFailures,
        total_successes = CBState#circuit_breaker_state.total_successes,
        error_rate = NewErrorRate,
        window_events = NewWindowEvents
    },
    maybe_transition_to_open(UpdatedState).

maybe_transition_to_open(#circuit_breaker_state{state = closed, failure_count = FailureCount, error_rate = ErrorRate, config = Config, window_events = WindowEvents, key = {TenantId, ProviderId}} = CBState) ->
    FailureThreshold = maps:get(~"failure_threshold", Config, ?DEFAULT_FAILURE_THRESHOLD),
    ErrorRateThreshold = maps:get(~"error_rate_threshold", Config, ?DEFAULT_ERROR_RATE_THRESHOLD),
    LatencyThreshold = maps:get(~"latency_threshold_ms", Config, undefined),
    WindowSeconds = maps:get(~"error_rate_window_seconds", Config, ?DEFAULT_ERROR_RATE_WINDOW_SECONDS),
    Now = erlang:system_time(millisecond),
    
    %% Calculate sliding error rate (use sliding window if available)
    SlidingErrorRate = case WindowEvents of
        [] ->
            %% Fallback to legacy error_rate if window is empty
            ErrorRate;
        _ ->
            calculate_sliding_error_rate(WindowEvents, WindowSeconds, Now)
    end,
    
    %% Check latency threshold if configured (for publish operations)
    LatencyExceeded = case LatencyThreshold of
        undefined -> false;
        _ ->
            %% Get average latency from recent events (if latency tracking is implemented)
            %% For now, this is a placeholder - actual latency should come from publish metrics
            case get_recent_latency(TenantId, ProviderId, WindowSeconds, Now) of
                {ok, AvgLatency} when is_integer(AvgLatency) ->
                    AvgLatency >= LatencyThreshold;
                _ ->
                    false
            end
    end,
    
    %% Count events in window for logging
    WindowStart = Now - (WindowSeconds * 1000),
    EventsInWindow = [{T, Type} || {T, Type} <- WindowEvents, T >= WindowStart],
    FailuresInWindow = length([{T, Type} || {T, Type} <- EventsInWindow, Type =:= failure]),
    TotalInWindow = length(EventsInWindow),
    
    if
        FailureCount >= FailureThreshold orelse SlidingErrorRate >= ErrorRateThreshold orelse LatencyExceeded ->
            %% Transition to open
            Reason = if
                LatencyExceeded -> ~"latency_threshold_exceeded";
                FailureCount >= FailureThreshold -> ~"failure_threshold_exceeded";
                true -> ~"error_rate_threshold_exceeded"
            end,
            
            %% Emit state transition metric
            router_metrics:emit_metric(router_circuit_breaker_state_transitions_total, #{count => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                from => ~"closed",
                to => ~"open"
            }),
            
            %% Emit circuit breaker state gauge
            router_metrics:emit_metric(router_circuit_breaker_state, #{value => 1.0}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                state => ~"open"
            }),
            
            %% Emit trigger reason metric
            router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{value => 1}, #{
                tenant_id => TenantId,
                provider_id => ProviderId,
                reason => Reason
            }),
            
            %% Emit error-rate metric (gauge)
            router_metrics:emit_metric(router_circuit_breaker_error_rate, #{value => SlidingErrorRate}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            
            %% Emit window metrics (counters)
            router_metrics:emit_metric(router_circuit_breaker_window_requests_total, #{count => TotalInWindow}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            router_metrics:emit_metric(router_circuit_breaker_window_failures_total, #{count => FailuresInWindow}, #{
                tenant_id => TenantId,
                provider_id => ProviderId
            }),
            
            %% Log circuit breaker opened event with enhanced context
            router_logger:warn(~"Circuit breaker opened for provider", #{
                ~"event" => ~"circuit_breaker_state_changed",
                ~"tenant_id" => TenantId,
                ~"provider_id" => ProviderId,
                ~"from" => ~"closed",
                ~"to" => ~"open",
                ~"reason" => Reason,
                ~"failure_count" => FailureCount,
                ~"error_rate" => SlidingErrorRate,
                ~"window_seconds" => WindowSeconds,
                ~"failures_in_window" => FailuresInWindow,
                ~"total_in_window" => TotalInWindow,
                ~"failure_threshold" => FailureThreshold,
                ~"error_rate_threshold" => ErrorRateThreshold
            }),
            
            CBState#circuit_breaker_state{
                state = open,
                state_changed_at = Now,
                half_open_calls_count = 0,
                error_rate = SlidingErrorRate
            };
        true ->
            %% Update error_rate even if not transitioning
            CBState#circuit_breaker_state{
                error_rate = SlidingErrorRate
            }
    end;
maybe_transition_to_open(CBState) ->
    CBState.

%% Returns: {ok, AvgLatencyMs} | {error, not_available}
-spec get_recent_latency(tenant_id(), provider_id(), integer(), integer()) -> {ok, integer()} | {error, not_available}.
get_recent_latency(_TenantId, _ProviderId, _WindowSeconds, _Now) ->
    %% Query publish latency metrics from router_metrics ETS
    %% This implementation queries the global publish latency metric (without labels)
    %% For per-tenant/provider latency, would need labeled metrics lookup with tenant_id/provider_id
    %% 
    %% Note: This is a simplified approach for R10. In production, should:
    %% 1. Query labeled metrics: router_nats_publish_latency_seconds with tenant_id/provider_id labels
    %% 2. Calculate average latency over the window
    %% 3. Use sliding window or exponential moving average
    case ets:info(router_metrics) of
        undefined ->
            {error, not_available};
        _ ->
            %% Try to get recent latency from metrics (global, without labels)
            %% For labeled metrics, would need to construct key: {router_nats_publish_latency_seconds, LabelsKey}
            case ets:lookup(router_metrics, router_nats_publish_latency_seconds) of
                [] ->
                    {error, not_available};
                [{router_nats_publish_latency_seconds, LatencySeconds}] when is_float(LatencySeconds) ->
                    LatencyMs = trunc(LatencySeconds * 1000),
                    {ok, LatencyMs};
                [{router_nats_publish_latency_seconds, LatencySeconds}] when is_integer(LatencySeconds) ->
                    %% Handle integer latency values (convert from seconds to ms)
                    LatencyMs = LatencySeconds * 1000,
                    {ok, LatencyMs};
                [{router_nats_publish_latency_seconds, LatencyMs}] when is_number(LatencyMs), LatencyMs > 100 ->
                    %% Handle direct millisecond values (heuristic: values > 100 are likely ms, not seconds)
                    {ok, trunc(LatencyMs)};
                _ ->
                    {error, not_available}
            end
    end.

%% Filters events by time window and calculates error_rate = failures / total_requests
-spec calculate_sliding_error_rate([{integer(), success | failure}], integer(), integer()) -> float().
calculate_sliding_error_rate(WindowEvents, WindowSeconds, Now) ->
    %% Filter events within window (last N seconds)
    WindowStart = Now - (WindowSeconds * 1000),  % Convert seconds to milliseconds
    EventsInWindow = [Event || {Timestamp, _} = Event <- WindowEvents, Timestamp >= WindowStart],
    
    %% Count failures and total requests in window
    FailuresInWindow = length([{T, Type} || {T, Type} <- EventsInWindow, Type =:= failure]),
    TotalInWindow = length(EventsInWindow),
    
    %% Calculate error rate
    case TotalInWindow of
        0 -> 0.0;
        _ -> FailuresInWindow / TotalInWindow
    end.

-spec clean_window_events([{integer(), success | failure}], integer(), integer()) -> [{integer(), success | failure}].
clean_window_events(WindowEvents, WindowSeconds, Now) ->
    WindowStart = Now - (WindowSeconds * 1000),  % Convert seconds to milliseconds
    [Event || {Timestamp, _} = Event <- WindowEvents, Timestamp >= WindowStart].

-spec add_window_event([{integer(), success | failure}], integer(), success | failure, integer(), integer()) -> [{integer(), success | failure}].
add_window_event(WindowEvents, Timestamp, EventType, WindowSeconds, Now) ->
    %% Add new event
    NewEvents = [{Timestamp, EventType} | WindowEvents],
    %% Clean old events (keep only events within window)
    clean_window_events(NewEvents, WindowSeconds, Now).

-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(?TABLE, size) of
        undefined -> undefined;
        Size -> Size
    end.

-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(?TABLE, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, circuit_breaker_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(?TABLE, Limit);
        _ -> {error, invalid_limit}
    end.

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

