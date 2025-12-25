-module(router_extension_circuit_breaker).

-doc "Extension Circuit Breaker".
%% CP3: Circuit breaker for frequently failing extensions
-export([check_circuit/1, record_success/1, record_failure/1, should_attempt_recovery/1]).
-export([get_all_circuit_states/0]).

-ignore_xref([
  {router_extension_circuit_breaker, get_all_circuit_states, 0},
  {router_extension_circuit_breaker, check_failure_threshold, 1},
  {router_extension_circuit_breaker, get_circuit_state, 1},
  {router_extension_circuit_breaker, get_circuit_opened_at, 1},
  {router_extension_circuit_breaker, increment_counter, 2},
  {router_extension_circuit_breaker, reset_half_open_counter, 1}
]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_extension_circuit_breaker]).

%% Circuit breaker states
-define(STATE_CLOSED, ~"closed").
-define(STATE_OPEN, ~"open").
-define(STATE_HALF_OPEN, ~"half_open").

%% Returns: {ok, allow} | {error, circuit_open}
-spec check_circuit(binary()) -> {ok, allow} | {error, circuit_open}.
check_circuit(ExtensionId) ->
    try
        case get_circuit_state(ExtensionId) of
            {ok, ?STATE_CLOSED} ->
                {ok, allow};
            {ok, ?STATE_HALF_OPEN} ->
                %% Allow limited requests in half-open state
                case check_half_open_limit(ExtensionId) of
                    {ok, allow} ->
                        {ok, allow};
                    {error, limit_exceeded} ->
                        {error, circuit_open}
                end;
            {ok, ?STATE_OPEN} ->
                %% Check if recovery should be attempted
                case should_attempt_recovery(ExtensionId) of
                    true ->
                        %% Transition to half-open
                        update_state(ExtensionId, ?STATE_HALF_OPEN),
                        {ok, allow};
                    false ->
                        {error, circuit_open}
                end;
            {error, not_found} ->
                %% No health record, assume closed
                {ok, allow};
            {error, _Reason} ->
                %% Error getting state, allow (fail-open)
                {ok, allow}
        end
    catch
        _:Reason ->
            %% Error, allow (fail-open)
            router_logger:warn(~"Circuit breaker check failed", #{
                ~"extension_id" => ExtensionId,
                ~"error" => Reason
            }),
            {ok, allow}
    end.

%% Returns: ok
-spec record_success(binary()) -> ok.
record_success(ExtensionId) ->
    try
        %% Increment success counter (atomic)
        increment_counter(ExtensionId, success_count),
        
        %% Update health metrics
        Now = calendar:universal_time(),
        HealthData = #{
            last_success => Now
        },
        router_extension_registry_db:update_health(ExtensionId, HealthData),
        
        %% Update circuit state if needed
        case get_circuit_state(ExtensionId) of
            {ok, ?STATE_HALF_OPEN} ->
                %% Success in half-open, reset counter and transition to closed
                reset_half_open_counter(ExtensionId),
                update_state(ExtensionId, ?STATE_CLOSED),
                router_logger:info(~"Circuit breaker closed after recovery", #{
                    ~"extension_id" => ExtensionId
                });
            _ ->
                ok
        end,
        
        %% Emit telemetry
        router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [success_total], #{count => 1}, #{
            extension_id => ExtensionId
        }),
        ok
    catch
        _:Reason ->
            router_logger:warn(~"Failed to record success", #{
                ~"extension_id" => ExtensionId,
                ~"error" => Reason
            }),
            ok
    end.

%% Returns: ok
-spec record_failure(binary()) -> ok.
record_failure(ExtensionId) ->
    try
        %% Increment failure counter (atomic)
        increment_counter(ExtensionId, failure_count),
        
        %% Update health metrics
        Now = calendar:universal_time(),
        HealthData = #{
            last_failure => Now
        },
        router_extension_registry_db:update_health(ExtensionId, HealthData),
        
        %% Check if circuit should open
        case get_circuit_state(ExtensionId) of
            {ok, ?STATE_HALF_OPEN} ->
                %% Failure in half-open, transition to open
                update_state(ExtensionId, ?STATE_OPEN),
                router_logger:warn(~"Circuit breaker opened after half-open failure", #{
                    ~"extension_id" => ExtensionId
                });
            {ok, ?STATE_CLOSED} ->
                %% Check if threshold exceeded
                case check_failure_threshold(ExtensionId) of
                    true ->
                        update_state(ExtensionId, ?STATE_OPEN),
                        router_logger:warn(~"Circuit breaker opened", #{
                            ~"extension_id" => ExtensionId
                        });
                    false ->
                        ok
                end;
            _ ->
                ok
        end,
        
        %% Emit telemetry
        router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [failure_total], #{count => 1}, #{
            extension_id => ExtensionId
        }),
        ok
    catch
        _:Reason ->
            router_logger:warn(~"Failed to record failure", #{
                ~"extension_id" => ExtensionId,
                ~"error" => Reason
            }),
            ok
    end.

%% Returns: ok | {error, Reason}
-spec update_state(binary(), binary()) -> ok | {error, term()}.
update_state(ExtensionId, NewState) ->
    update_state(ExtensionId, NewState, calendar:universal_time()).

update_state(ExtensionId, NewState, Timestamp) ->
    try
        HealthData = case NewState of
            ?STATE_OPEN ->
                #{
                    circuit_breaker_state => NewState,
                    circuit_breaker_opened_at => Timestamp
                };
            _ ->
                #{
                    circuit_breaker_state => NewState
                }
        end,
        router_extension_registry_db:update_health(ExtensionId, HealthData),
        ok
    catch
        _:_ ->
            {error, unexpected_error}
    end.

%% Returns: true | false
-spec should_attempt_recovery(binary()) -> boolean().
should_attempt_recovery(ExtensionId) ->
    try
        case get_circuit_opened_at(ExtensionId) of
            {ok, OpenedAt} ->
                %% Get timeout from config
                TimeoutSeconds = get_circuit_breaker_timeout(),
                Now = calendar:universal_time(),
                ElapsedSeconds = calendar:datetime_to_gregorian_seconds(Now) - 
                                 calendar:datetime_to_gregorian_seconds(OpenedAt),
                ElapsedSeconds >= TimeoutSeconds;
            {error, not_found} ->
                false;
            {error, _} ->
                false
        end
    catch
        _:_ ->
            false
    end.

%% Internal: Get circuit breaker state
get_circuit_state(ExtensionId) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "SELECT circuit_breaker_state FROM extension_health WHERE extension_id = $1",
                %% Compile-time epgsql guard to avoid xref warnings
                case catch do_query_circuit_state(Conn, Query, ExtensionId) of
                    {ok, _, [{State}]} ->
                        {ok, State};
                    {ok, _, []} ->
                        {error, not_found};
                    {error, Reason} ->
                        {error, Reason}
                end;
            _ ->
                {error, database_not_available}
        end
    catch
        _:_ ->
            {error, unexpected_error}
    end.

%% Returns: {ok, #{ExtensionId => CircuitStateMap}} | {error, Reason}
-spec get_all_circuit_states() -> {ok, map()} | {error, term()}.
get_all_circuit_states() ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "SELECT extension_id, circuit_breaker_state, circuit_breaker_opened_at, "
                        "failure_count, error_rate "
                        "FROM extension_health ORDER BY extension_id",
                case catch do_query_states(Conn, Query) of
                    {ok, _, Rows} ->
                        StatesMap = lists:foldl(fun(Row, Acc) ->
                            {ExtId, State, OpenedAt, FailureCount, ErrorRate} = Row,
                            ExtMap = #{
                                extension_id => ExtId,
                                state => State,
                                opened_at => OpenedAt,
                                failure_count => FailureCount,
                                error_rate => ErrorRate
                            },
                            maps:put(ExtId, ExtMap, Acc)
                        end, #{}, Rows),
                        {ok, StatesMap};
                    {error, Reason} ->
                        {error, Reason}
                end;
            _ ->
                {error, database_not_available}
        end
    catch
        _:_ ->
            {error, unexpected_error}
    end.

%% Internal: Get circuit breaker opened timestamp
get_circuit_opened_at(ExtensionId) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "SELECT circuit_breaker_opened_at FROM extension_health WHERE extension_id = $1",
                case catch do_query_opened_at(Conn, Query, ExtensionId) of
                    {ok, _, [{OpenedAt}]} when OpenedAt =/= null ->
                        {ok, OpenedAt};
                    {ok, _, []} ->
                        {error, not_found};
                    {error, Reason} ->
                        {error, Reason}
                end;
            _ ->
                {error, database_not_available}
        end
    catch
        _:_ ->
            {error, unexpected_error}
    end.

%% Internal: Check if half-open limit exceeded
check_half_open_limit(ExtensionId) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "SELECT half_open_max_requests, half_open_requests_count "
                        "FROM extension_health WHERE extension_id = $1",
                case catch do_query_half_open(Conn, Query, ExtensionId) of
                    {ok, _, [{MaxRequests, CurrentCount}]} ->
                        case CurrentCount >= MaxRequests of
                            true ->
                                {error, limit_exceeded};
                            false ->
                                UpdateQuery = "UPDATE extension_health SET half_open_requests_count = half_open_requests_count + 1 "
                                             "WHERE extension_id = $1",
                                catch do_exec_update_half_open(Conn, UpdateQuery, ExtensionId),
                                {ok, allow}
                        end;
                    {ok, _, []} ->
                        {ok, allow};
                    {error, _} ->
                        {ok, allow}
                end;
            _ ->
                {ok, allow}
        end
    catch
        _:_ ->
            {ok, allow}
    end.

%% Internal: Check if failure threshold exceeded (failure count or error rate)
check_failure_threshold(ExtensionId) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "SELECT failure_count, success_count, circuit_breaker_failure_threshold, "
                        "circuit_breaker_error_rate_threshold, circuit_breaker_window_seconds, "
                        "updated_at "
                        "FROM extension_health WHERE extension_id = $1",
                case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
                    {ok, _, [{FailureCount, SuccessCount, FailureThreshold, ErrorRateThreshold, WindowSeconds, UpdatedAt}]} ->
                        %% Check failure count threshold
                        FailureCountCheck = FailureCount >= FailureThreshold,
                        
                        %% Check error rate threshold (within time window)
                        ErrorRateCheck = case ErrorRateThreshold of
                            null -> false;
                            RateThreshold when is_float(RateThreshold) orelse is_integer(RateThreshold) ->
                                Total = SuccessCount + FailureCount,
                                case Total > 0 of
                                    true ->
                                        ErrorRate = FailureCount / Total,
                                        ErrorRate >= RateThreshold;
                                    false ->
                                        false
                                end;
                            _ ->
                                false
                        end,
                        
                        %% Check if within time window (if window_seconds specified)
                        WithinWindow = case WindowSeconds of
                            null -> true;
                            Window when is_integer(Window), Window > 0 ->
                                Now = calendar:universal_time(),
                                WindowStart = calendar:datetime_to_gregorian_seconds(UpdatedAt) - Window,
                                NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
                                Diff = NowSeconds - WindowStart,
                                Diff =< Window;
                            _ ->
                                true
                        end,
                        
                        %% Open circuit if either threshold exceeded and within window
                        (FailureCountCheck orelse ErrorRateCheck) andalso WithinWindow;
                    {ok, _, []} ->
                        false;
                    {error, _} ->
                        false
                end;
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end.

%% Internal: Increment counter (atomic update)
increment_counter(ExtensionId, CounterType) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Column = case CounterType of
                    success_count -> "success_count";
                    failure_count -> "failure_count"
                end,
                Query = "UPDATE extension_health SET " ++ Column ++ " = " ++ Column ++ " + 1 "
                        "WHERE extension_id = $1 RETURNING " ++ Column,
                case catch do_query_increment(Conn, Query, ExtensionId) of
                    {ok, _, [{NewValue}]} ->
                        NewValue;
                    {ok, _, []} ->
                        create_health_record(ExtensionId, CounterType),
                        1;
                    {error, _} ->
                        0
                end;
            _ ->
                0
        end
    catch
        _:_ ->
            0
    end.

%% Internal: Create health record
create_health_record(ExtensionId, CounterType) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                SuccessCount = case CounterType of
                    success_count -> 1;
                    _ -> 0
                end,
                FailureCount = case CounterType of
                    failure_count -> 1;
                    _ -> 0
                end,
                Query = "INSERT INTO extension_health (extension_id, success_count, failure_count, circuit_breaker_state) "
                        "VALUES ($1, $2, $3, $4) ON CONFLICT (extension_id) DO NOTHING",
                catch do_query_create_health(Conn, Query, ExtensionId, SuccessCount, FailureCount);
            _ ->
                ok
        end
    catch
        _:_ ->
            ok
    end.

%% Internal: Reset half-open request counter
reset_half_open_counter(ExtensionId) ->
    try
        case router_extension_registry_db:get_db_connection() of
            Conn when is_pid(Conn) ->
                Query = "UPDATE extension_health SET half_open_requests_count = 0 "
                        "WHERE extension_id = $1",
                catch do_exec_reset_half_open(Conn, Query, ExtensionId);
            _ ->
                ok
        end
    catch
        _:_ ->
            ok
    end.

%% Compile-time wrappers to avoid epgsql xref when not available
-ifdef(EPGSQL_AVAILABLE).
do_query_circuit_state(Conn, Query, ExtensionId) -> epgsql:equery(Conn, Query, [ExtensionId]).
do_query_states(Conn, Query) -> epgsql:equery(Conn, Query, []).
do_query_opened_at(Conn, Query, ExtensionId) -> epgsql:equery(Conn, Query, [ExtensionId]).
do_query_half_open(Conn, Query, ExtensionId) -> epgsql:equery(Conn, Query, [ExtensionId]).
do_exec_update_half_open(Conn, Query, ExtensionId) -> epgsql:equery(Conn, Query, [ExtensionId]).
do_query_increment(Conn, Query, ExtensionId) -> epgsql:equery(Conn, Query, [ExtensionId]).
do_query_create_health(Conn, Query, ExtensionId, SuccessCount, FailureCount) -> epgsql:equery(Conn, Query, [ExtensionId, SuccessCount, FailureCount, ?STATE_CLOSED]).
do_exec_reset_half_open(Conn, Query, ExtensionId) -> epgsql:equery(Conn, Query, [ExtensionId]).
-else.
do_query_circuit_state(_Conn, _Query, _ExtensionId) -> {error, undef}.
do_query_states(_Conn, _Query) -> {error, undef}.
do_query_opened_at(_Conn, _Query, _ExtensionId) -> {error, undef}.
do_query_half_open(_Conn, _Query, _ExtensionId) -> {error, undef}.
do_exec_update_half_open(_Conn, _Query, _ExtensionId) -> {error, undef}.
do_query_increment(_Conn, _Query, _ExtensionId) -> {error, undef}.
do_query_create_health(_Conn, _Query, _ExtensionId, _SuccessCount, _FailureCount) -> {error, undef}.
do_exec_reset_half_open(_Conn, _Query, _ExtensionId) -> {error, undef}.
-endif.

%% Internal: Get circuit breaker timeout from config
get_circuit_breaker_timeout() ->
    ExtensionRegistryConfig = application:get_env(beamline_router, extension_registry, []),
    proplists:get_value(circuit_breaker_timeout_seconds, ExtensionRegistryConfig, 60).
