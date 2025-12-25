-module(router_network_tracker).

-doc "Network Round Trip Tracking Module".
%% Provides structure and helpers for tracking network round trips and optimizing network calls
%%
%% Features:
%% - Round trip time (RTT) measurement
%% - Network call tracking
%% - Batch operation helpers
%% - Request coalescing structure
%%
%% ⚠️ NOTE: Actual network optimization requires protocol changes
%% This module provides structure and helpers for when optimization is implemented
%%
%% @see PERFORMANCE_GUIDE.md#network-optimization For network optimization guidelines

-export([
    track_round_trip/3,
    complete_round_trip/2,
    get_round_trip_time/1,
    get_network_stats/0,
    batch_operations/2,
    coalesce_requests/2
]).

-include("beamline_router.hrl").

%% OperationId: Unique operation identifier
%% OperationType: Type of operation (nats_publish, grpc_call, etc.)
%% StartTime: Operation start time in microseconds
%% Returns: {ok, RoundTripId} | {error, Reason}
-spec track_round_trip(binary(), atom(), integer()) -> {ok, binary()} | {error, term()}.
track_round_trip(OperationId, OperationType, StartTime) when is_binary(OperationId), is_atom(OperationType), is_integer(StartTime) ->
    try
        %% Generate round trip ID
        RoundTripId = generate_round_trip_id(OperationId),
        
        %% Create round trip tracking entry
        RoundTripEntry = #{
            round_trip_id => RoundTripId,
            operation_id => OperationId,
            operation_type => OperationType,
            start_time => StartTime,
            created_at => erlang:system_time(millisecond)
        },
        
        %% Store in ETS
        store_round_trip(RoundTripId, RoundTripEntry),
        
        {ok, RoundTripId}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% RoundTripId: Round trip identifier
%% EndTime: Operation end time in microseconds
%% Returns: {ok, RoundTripTime} | {error, Reason}
-spec complete_round_trip(binary(), integer()) -> {ok, integer()} | {error, term()}.
complete_round_trip(RoundTripId, EndTime) when is_binary(RoundTripId), is_integer(EndTime) ->
    try
        Table = router_network_round_trips,
        case ets:whereis(Table) of
            undefined ->
                {error, round_trip_not_found};
            _Tid ->
                case ets:lookup(Table, RoundTripId) of
                    [{RoundTripId, Entry}] ->
                        StartTime = maps:get(start_time, Entry, EndTime),
                        RoundTripTime = EndTime - StartTime,
                        
                        %% Update entry with completion
                        UpdatedEntry = maps:merge(Entry, #{
                            end_time => EndTime,
                            round_trip_time => RoundTripTime,
                            completed => true
                        }),
                        store_round_trip(RoundTripId, UpdatedEntry),
                        
                        %% Emit metric
                        OperationType = maps:get(operation_type, Entry, unknown),
                        router_metrics:emit_metric(router_network_round_trip_time_microseconds, #{value => RoundTripTime}, #{
                            operation_type => atom_to_binary(OperationType, utf8)
                        }),
                        
                        {ok, RoundTripTime};
                    [] ->
                        {error, round_trip_not_found}
                end
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec get_round_trip_time(binary()) -> {ok, integer()} | {error, term()}.
get_round_trip_time(OperationId) ->
    try
        Table = router_network_round_trips,
        case ets:whereis(Table) of
            undefined ->
                {error, round_trip_not_found};
            _Tid ->
                %% Find round trip by operation ID
                case ets:match(Table, {'$1', #{operation_id => OperationId, round_trip_time => '$2', completed => true}}) of
                    [[_, RoundTripTime]] ->
                        {ok, RoundTripTime};
                    [] ->
                        {error, round_trip_not_found};
                    _ ->
                        %% Multiple matches - get most recent
                        Matches = ets:match(Table, {'$1', #{operation_id => OperationId, round_trip_time => '$2', completed => true}}),
                        case Matches of
                            [] ->
                                {error, round_trip_not_found};
                            _ ->
                                %% Get first match
                                [[_RoundTripId, RoundTripTime] | _] = Matches,
                                {ok, RoundTripTime}
                        end
                end
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec get_network_stats() -> map().
get_network_stats() ->
    try
        Table = router_network_round_trips,
        case ets:whereis(Table) of
            undefined ->
                #{};
            _Tid ->
                %% Get all completed round trips
                AllRoundTrips = ets:match(Table, {'$1', #{round_trip_time => '$2', completed => true, operation_type => '$3'}}),
                RoundTripTimes = [RTT || [[_Id, RTT, _Type]] <- AllRoundTrips],
                
                %% Calculate statistics
                Stats = case length(RoundTripTimes) > 0 of
                    true ->
                        Sorted = lists:sort(RoundTripTimes),
                        Count = length(Sorted),
                        Total = lists:sum(Sorted),
                        Min = hd(Sorted),
                        Max = lists:last(Sorted),
                        Avg = Total / Count,
                        P95 = calculate_percentile(Sorted, 0.95),
                        P99 = calculate_percentile(Sorted, 0.99),
                        
                        #{
                            count => Count,
                            total_time_microseconds => Total,
                            min_time_microseconds => Min,
                            max_time_microseconds => Max,
                            avg_time_microseconds => trunc(Avg),
                            p95_time_microseconds => trunc(P95),
                            p99_time_microseconds => trunc(P99)
                        };
                    false ->
                        #{
                            count => 0,
                            total_time_microseconds => 0,
                            min_time_microseconds => 0,
                            max_time_microseconds => 0,
                            avg_time_microseconds => 0,
                            p95_time_microseconds => 0,
                            p99_time_microseconds => 0
                        }
                end,
                Stats
        end
    catch
        _:_ ->
            #{}
    end.

%% Operations: List of operations to batch
%% BatchFunction: Function to execute batch
%% Returns: {ok, Results} | {error, Reason}
-spec batch_operations(list(), fun((list()) -> term())) -> {ok, term()} | {error, term()}.
batch_operations(Operations, BatchFunction) when is_list(Operations), is_function(BatchFunction, 1) ->
    try
        StartTime = erlang:monotonic_time(microsecond),
        Results = BatchFunction(Operations),
        EndTime = erlang:monotonic_time(microsecond),
        Duration = EndTime - StartTime,
        
        %% Track batch operation
        router_metrics:emit_metric(router_network_batch_operations_total, #{count => 1}, #{
            operation_count => integer_to_binary(length(Operations))
        }),
        router_metrics:emit_metric(router_network_batch_duration_microseconds, #{value => Duration}, #{
            operation_count => integer_to_binary(length(Operations))
        }),
        
        {ok, Results}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% Requests: List of requests to coalesce
%% CoalesceFunction: Function to coalesce requests
%% Returns: {ok, CoalescedRequests} | {error, Reason}
-spec coalesce_requests(list(), fun((list()) -> list())) -> {ok, list()} | {error, term()}.
coalesce_requests(Requests, CoalesceFunction) when is_list(Requests), is_function(CoalesceFunction, 1) ->
    try
        StartTime = erlang:monotonic_time(microsecond),
        CoalescedRequests = CoalesceFunction(Requests),
        EndTime = erlang:monotonic_time(microsecond),
        Duration = EndTime - StartTime,
        
        %% Track coalescing
        OriginalCount = length(Requests),
        CoalescedCount = length(CoalescedRequests),
        Reduction = case OriginalCount > 0 of
            true -> (1.0 - (CoalescedCount / OriginalCount)) * 100.0;
            false -> 0.0
        end,
        
        router_metrics:emit_metric(router_network_requests_coalesced_total, #{count => 1}, #{
            original_count => integer_to_binary(OriginalCount),
            coalesced_count => integer_to_binary(CoalescedCount),
            reduction_percent => float_to_binary(Reduction, [{decimals, 2}])
        }),
        router_metrics:emit_metric(router_network_coalescing_duration_microseconds, #{value => Duration}, #{
            original_count => integer_to_binary(OriginalCount)
        }),
        
        {ok, CoalescedRequests}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec store_round_trip(binary(), map()) -> ok.
store_round_trip(RoundTripId, Entry) ->
    Table = router_network_round_trips,
    case ets:whereis(Table) of
        undefined ->
            _ = ets:new(Table, [named_table, public, {write_concurrency, true}]);
        _ ->
            ok
    end,
    ets:insert(Table, {RoundTripId, Entry}),
    ok.

-spec generate_round_trip_id(binary()) -> binary().
generate_round_trip_id(OperationId) ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    <<"rtt-", OperationId/binary, "-", (integer_to_binary(Timestamp))/binary, "-", (integer_to_binary(Random))/binary>>.

-spec calculate_percentile(list(integer()), float()) -> float().
calculate_percentile(Sorted, Percentile) ->
    Count = length(Sorted),
    case Count > 0 of
        true ->
            Index = max(1, round(Count * Percentile)),
            lists:nth(Index, Sorted);
        false ->
            0.0
    end.

