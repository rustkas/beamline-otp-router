-module(router_idem).
-ignore_xref([
    router_idem,
    {router_idem, init, 1},
    {router_idem, is_dup, 1},
    {router_idem, remember, 2},
    {router_idem, check_and_mark, 2},
    {router_idem, check_and_mark, 3},
    {router_idem, check_or_register, 3},
    {router_idem, check_or_register, 4},
    {router_idem, mark_completed, 3},
    {router_idem, mark_completed, 4},
    {router_idem, extract_idempotency_key, 2},
    {router_idem, evict, 1},
    {router_idem, evict_expired, 0},
    {router_idem, evict_all, 0},
    {router_idem, metrics, 0},
    {router_idem, cleanup, 0}
]).
-export([
    init/1, 
    is_dup/1, 
    remember/2, 
    check_and_mark/2,
    check_and_mark/3,
    check_or_register/3,
    check_or_register/4,
    mark_completed/3,
    mark_completed/4,
    extract_idempotency_key/2,
    evict/1, 
    evict_expired/0, 
    evict_all/0,
    reset/0,
    metrics/0, 
    cleanup/0,
    get_table_size/0,
    get_table_memory/0,
    check_size_limit/0
]).

-include("beamline_router.hrl").

%% ETS Table Structure:
%% Key: {KeyType, IdempotencyKey} | IdempotencyKey (binary)
%% Value: #{
%%   status => processing | completed | failed,
%%   expires_at => integer(),  %% milliseconds since epoch
%%   trace_id => binary() | undefined,
%%   span_id => binary() | undefined,
%%   request_hash => binary() | undefined,
%%   result_snapshot => map() | undefined,
%%   error_code => binary() | undefined,
%%   processed_at => integer(),  %% milliseconds since epoch
%%   additional_data => map() | undefined
%% }

%% Creates table with ordered_set for efficient range scans during cleanup
init(_Opts) ->
  case ets:info(router_idem) of
    undefined -> 
      Table = ets:new(router_idem, [
        ordered_set,  %% ordered_set for efficient range scans
        named_table, 
        public, 
        {read_concurrency, true}, 
        {write_concurrency, true}
      ]),
      %% Start periodic cleanup timer
      start_cleanup_timer(),
      Table;
    _ -> ok
  end,
  ok.

%% Returns true if key exists and hasn't expired, false otherwise
%% Automatically cleans up expired entries during lookup
is_dup(Key) ->
  Now = erlang:system_time(millisecond),
  case ets:lookup(router_idem, Key) of
    [{Key, Value}] when is_map(Value) ->
      ExpiresAt = maps:get(expires_at, Value, 0),
      case ExpiresAt > Now of
        true ->
          %% Key exists and hasn't expired - duplicate detected
          router_metrics:inc(router_idem_hits_total),
          TraceId = maps:get(trace_id, Value, undefined),
          telemetry:execute([router, idempotency, hit], #{count => 1}, #{
            key => Key,
            trace_id => TraceId
          }),
          true;
        false ->
          %% Key exists but expired - remove it and treat as new
          ets:delete(router_idem, Key),
          router_metrics:inc(router_idem_miss_total),
          telemetry:execute([router, idempotency, expired], #{count => 1}, #{key => Key}),
          false
      end;
    [{Key, Exp}] when is_integer(Exp) ->
      %% Legacy format: {Key, Exp} - backward compatibility
      case Exp > Now of
        true ->
          router_metrics:inc(router_idem_hits_total),
          telemetry:execute([router, idempotency, hit], #{count => 1}, #{key => Key}),
          true;
        false ->
          ets:delete(router_idem, Key),
          router_metrics:inc(router_idem_miss_total),
          telemetry:execute([router, idempotency, expired], #{count => 1}, #{key => Key}),
          false
      end;
    [] ->
      %% Key doesn't exist - not a duplicate
      router_metrics:inc(router_idem_miss_total),
      telemetry:execute([router, idempotency, miss], #{count => 1}, #{key => Key}),
      false
  end.

%% This is the recommended API for preventing double-execution
%% Returns: {ok, not_seen} | {ok, seen} | {error, Reason}
-spec check_and_mark(binary(), integer()) -> {ok, not_seen} | {ok, seen} | {error, term()}.
check_and_mark(Key, TTL) when is_integer(TTL), TTL > 0 ->
  check_and_mark(Key, TTL, undefined);
check_and_mark(_Key, _TTL) ->
  {error, invalid_ttl}.

%% Key: Idempotency key (binary)
%% TTL: Time-to-live in milliseconds
%% AdditionalData: Optional additional data to store (map or undefined)
%% Returns: {ok, not_seen} | {ok, seen} | {error, Reason}
-spec check_and_mark(binary(), integer(), map() | undefined) -> {ok, not_seen} | {ok, seen} | {error, term()}.
check_and_mark(Key, TTL, AdditionalData) when is_integer(TTL), TTL > 0 ->
  check_or_register(Key, TTL, AdditionalData).

%% Key: Idempotency key (binary)
%% TTL: Time-to-live in milliseconds
%% AdditionalData: Optional additional data (map or undefined)
%% Returns: {ok, not_seen} | {ok, seen, ExistingValue} | {error, Reason}
-spec check_or_register(binary(), integer(), map() | undefined) -> {ok, not_seen} | {ok, seen, map()} | {error, term()}.
check_or_register(Key, TTL, AdditionalData) ->
  check_or_register(Key, TTL, AdditionalData, undefined).

%% Key: Idempotency key (binary)
%% TTL: Time-to-live in milliseconds
%% AdditionalData: Optional additional data (map or undefined)
%% TraceContext: Optional trace context #{trace_id => binary(), span_id => binary()}
%% Returns: {ok, not_seen} | {ok, seen, ExistingValue} | {error, Reason}
-spec check_or_register(binary(), integer(), map() | undefined, map() | undefined) -> {ok, not_seen} | {ok, seen, map()} | {error, term()}.
check_or_register(Key, TTL, AdditionalData, TraceContext) when is_integer(TTL), TTL > 0 ->
  Now = erlang:system_time(millisecond),
  ExpiresAt = Now + TTL,
  
  %% Extract trace_id and span_id from TraceContext or AdditionalData
  TraceId = extract_trace_id(TraceContext, AdditionalData),
  SpanId = extract_span_id(TraceContext, AdditionalData),
  RequestHash = extract_request_hash(AdditionalData),
  
  %% Create value map with full structure
  Value = #{
    status => processing,
    expires_at => ExpiresAt,
    processed_at => Now,
    trace_id => TraceId,
    span_id => SpanId,
    request_hash => RequestHash,
    additional_data => AdditionalData
  },
  
  %% Atomic check-and-insert using ets:insert_new/2
  case ets:insert_new(router_idem, {Key, Value}) of
    true ->
      %% Key didn't exist - not seen before, now marked as processing
      router_metrics:inc(router_idem_miss_total),
      telemetry:execute([router, idempotency, remember], #{count => 1}, #{
        key => Key,
        ttl_ms => TTL,
        expires_at => ExpiresAt,
        trace_id => TraceId,
        span_id => SpanId
      }),
      {ok, not_seen};
    false ->
      %% Key already exists - check if expired or get existing value
      case ets:lookup(router_idem, Key) of
        [{Key, ExistingValue}] when is_map(ExistingValue) ->
          ExistingExpiresAt = maps:get(expires_at, ExistingValue, 0),
          ExistingStatus = maps:get(status, ExistingValue, processing),
          case ExistingExpiresAt > Now of
            true ->
              %% Key exists and hasn't expired - duplicate detected
              case ExistingStatus of
                processing ->
                  %% Race condition: another process is processing the same key
                  router_metrics:inc(router_idem_conflict_total),
                  telemetry:execute([router, idempotency, conflict], #{count => 1}, #{
                    key => Key,
                    trace_id => TraceId,
                    existing_trace_id => maps:get(trace_id, ExistingValue, undefined)
                  });
                _ ->
                  ok
              end,
              router_metrics:inc(router_idem_hits_total),
              telemetry:execute([router, idempotency, hit], #{count => 1}, #{
                key => Key,
                expires_at => ExistingExpiresAt,
                status => ExistingStatus,
                trace_id => TraceId,
                existing_trace_id => maps:get(trace_id, ExistingValue, undefined)
              }),
              {ok, seen, ExistingValue};
            false ->
              %% Key exists but expired - update it and treat as not seen
              ets:insert(router_idem, {Key, Value}),
              router_metrics:inc(router_idem_miss_total),
              telemetry:execute([router, idempotency, expired], #{count => 1}, #{
                key => Key,
                old_expires_at => ExistingExpiresAt,
                new_expires_at => ExpiresAt,
                ttl_ms => TTL,
                trace_id => TraceId
              }),
              {ok, not_seen}
          end;
        [{Key, ExistingExp}] when is_integer(ExistingExp) ->
          %% Legacy format: {Key, Exp} - backward compatibility
          case ExistingExp > Now of
            true ->
              router_metrics:inc(router_idem_hits_total),
              telemetry:execute([router, idempotency, hit], #{count => 1}, #{
                key => Key,
                expires_at => ExistingExp
              }),
              {ok, seen, #{status => completed, expires_at => ExistingExp}};
            false ->
              ets:insert(router_idem, {Key, Value}),
              router_metrics:inc(router_idem_miss_total),
              telemetry:execute([router, idempotency, expired], #{count => 1}, #{
                key => Key,
                old_expires_at => ExistingExp,
                new_expires_at => ExpiresAt,
                ttl_ms => TTL
              }),
              {ok, not_seen}
          end;
        [] ->
          %% Race condition: key was deleted between insert_new and lookup
          %% Try again (should succeed this time)
          case ets:insert_new(router_idem, {Key, Value}) of
            true ->
              router_metrics:inc(router_idem_miss_total),
              telemetry:execute([router, idempotency, remember], #{count => 1}, #{
                key => Key,
                ttl_ms => TTL,
                expires_at => ExpiresAt,
                trace_id => TraceId
              }),
              {ok, not_seen};
            false ->
              %% Still exists - treat as seen
              router_metrics:inc(router_idem_hits_total),
              telemetry:execute([router, idempotency, hit], #{count => 1}, #{
                key => Key,
                trace_id => TraceId
              }),
              {ok, seen, Value}
          end
      end
  end;
check_or_register(_Key, _TTL, _AdditionalData, _TraceContext) ->
  {error, invalid_ttl}.

%% Key: Idempotency key (binary)
%% Status: completed | failed
%% ResultSnapshot: Optional result data to store (map or undefined)
%% Returns: ok | {error, Reason}
-spec mark_completed(binary(), completed | failed, map() | undefined) -> ok | {error, term()}.
mark_completed(Key, Status, ResultSnapshot) ->
  mark_completed(Key, Status, ResultSnapshot, undefined).

%% Key: Idempotency key (binary)
%% Status: completed | failed
%% ResultSnapshot: Optional result data to store (map or undefined)
%% ErrorCode: Optional error code if status is failed (binary or undefined)
%% Returns: ok | {error, Reason}
-spec mark_completed(binary(), completed | failed, map() | undefined, binary() | undefined) -> ok | {error, term()}.
mark_completed(Key, Status, ResultSnapshot, ErrorCode) when Status =:= completed orelse Status =:= failed ->
  Now = erlang:system_time(millisecond),
  case ets:lookup(router_idem, Key) of
    [{Key, ExistingValue}] when is_map(ExistingValue) ->
      %% Update existing entry with completion status
      UpdatedValue = ExistingValue#{
        status => Status,
        result_snapshot => ResultSnapshot,
        error_code => ErrorCode,
        completed_at => Now
      },
      ets:insert(router_idem, {Key, UpdatedValue}),
      TraceId = maps:get(trace_id, ExistingValue, undefined),
      telemetry:execute([router, idempotency, completed], #{count => 1}, #{
        key => Key,
        status => Status,
        trace_id => TraceId,
        error_code => ErrorCode
      }),
      ok;
    [{Key, Exp}] when is_integer(Exp) ->
      %% Legacy format: convert to new format
      UpdatedValue = #{
        status => Status,
        expires_at => Exp,
        processed_at => Now,
        completed_at => Now,
        result_snapshot => ResultSnapshot,
        error_code => ErrorCode
      },
      ets:insert(router_idem, {Key, UpdatedValue}),
      telemetry:execute([router, idempotency, completed], #{count => 1}, #{
        key => Key,
        status => Status,
        error_code => ErrorCode
      }),
      ok;
    [] ->
      %% Key doesn't exist - cannot mark as completed
      {error, key_not_found}
  end;
mark_completed(_Key, _Status, _ResultSnapshot, _ErrorCode) ->
  {error, invalid_status}.

%% Headers: Map of headers (from NATS or HTTP)
%% Payload: Map of payload fields
%% Returns: {ok, IdempotencyKey} | {error, not_found}
-spec extract_idempotency_key(map(), map()) -> {ok, binary()} | {error, not_found}.
extract_idempotency_key(Headers, Payload) ->
  %% Try headers first (priority)
  case maps:get(~"idempotency_key", Headers, undefined) of
    undefined ->
      case maps:get(~"idempotency-key", Headers, undefined) of
        undefined ->
          case maps:get(~"Idempotency-Key", Headers, undefined) of
            undefined ->
              %% Try payload as fallback
              case maps:get(~"idempotency_key", Payload, undefined) of
                undefined ->
                  {error, not_found};
                PayloadKey ->
                  {ok, PayloadKey}
              end;
            HeaderKey ->
              {ok, HeaderKey}
          end;
        HeaderKey ->
          {ok, HeaderKey}
      end;
    HeaderKey ->
      {ok, HeaderKey}
  end.

%% TTL: Time-to-live in milliseconds
%% Returns ok on success
%% Note: For preventing double-execution, use check_and_mark/2 or check_and_mark/3 instead
remember(Key, TTL) when is_integer(TTL), TTL > 0 ->
  Now = erlang:system_time(millisecond),
  Exp = Now + TTL,
  Value = #{
    status => completed,
    expires_at => Exp,
    processed_at => Now
  },
  ets:insert(router_idem, {Key, Value}),
  telemetry:execute([router, idempotency, remember], #{count => 1}, #{
    key => Key,
    ttl_ms => TTL,
    expires_at => Exp
  }),
  ok;
remember(_Key, _TTL) ->
  {error, invalid_ttl}.

evict(Key) ->
  case ets:delete(router_idem, Key) of
    true -> ok;
    false -> ok  %% Key didn't exist, but that's fine
  end.

%% Returns number of entries evicted
evict_expired() ->
  Now = erlang:system_time(millisecond),
  ExpiredKeys = ets:foldl(fun({Key, Value}, Acc) ->
    ExpiresAt = case is_map(Value) of
      true -> maps:get(expires_at, Value, 0);
      false -> Value  %% Legacy format: integer
    end,
    case ExpiresAt =< Now of
      true -> [Key | Acc];
      false -> Acc
    end
  end, [], router_idem),
  
  %% Delete expired entries
  lists:foreach(fun(Key) ->
    ets:delete(router_idem, Key)
  end, ExpiredKeys),
  
  EvictedCount = length(ExpiredKeys),
  case EvictedCount > 0 of
    true ->
      %% Emit TTL-evictions metric
      router_metrics:inc(router_idem_evictions_total),
      telemetry:execute([router, idempotency, cleanup], #{count => EvictedCount}, #{
        cleanup_type => expired
      });
    false ->
      ok
  end,
  
  EvictedCount.

%% Use with caution - clears entire idempotency cache
evict_all() ->
  Size = ets:info(router_idem, size),
  ets:delete_all_objects(router_idem),
  telemetry:execute([router, idempotency, cleanup], #{count => Size}, #{
    cleanup_type => all
  }),
  Size.

%% Pattern: Alias for evict_all/0 to match reset/lifecycle pattern
%% Safe reset - clears ETS table but keeps table alive
-spec reset() -> integer().
reset() ->
  evict_all().

%% Protects ETS from unbounded growth
cleanup() ->
  %% Get table size before cleanup
  SizeBefore = ets:info(router_idem, size),
  
  %% Evict expired entries
  EvictedCount = evict_expired(),
  
  %% Get table size after cleanup
  SizeAfter = ets:info(router_idem, size),
  
  %% Emit metrics for table size monitoring
  telemetry:execute([router, idempotency, table_size], #{
    size => SizeAfter,
    evicted => EvictedCount
  }, #{
    size_before => SizeBefore
  }),
  
  %% Check size limit and enforce if needed
  MaxSize = application:get_env(beamline_router, idempotency_max_size, 1000000),
  case router_resource_monitor:check_table_size_limit(router_idem, MaxSize) of
    {ok, _CurrentSize} -> ok;
    {error, exceeded, CurrentSize, Limit} ->
      %% Enforce limit by evicting additional oldest entries
      AdditionalToEvict = CurrentSize - Limit,
      AdditionalEvicted = evict_oldest_entries(AdditionalToEvict),
      router_logger:warn(~"Idempotency table size limit enforced", #{
        ~"size" => CurrentSize,
        ~"limit" => Limit,
        ~"evicted_expired" => EvictedCount,
        ~"evicted_oldest" => AdditionalEvicted
      })
  end,
  
  ok.

metrics() -> [router_idem_hits_total, router_idem_miss_total, router_idem_conflict_total, router_idem_evictions_total].

-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(router_idem, size) of
        undefined -> undefined;
        Size -> Size
    end.

-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(router_idem, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, idempotency_max_size, 1000000),
    router_resource_monitor:check_table_size_limit(router_idem, MaxSize).

%% Internal functions

extract_trace_id(undefined, undefined) -> undefined;
extract_trace_id(TraceContext, _) when is_map(TraceContext) ->
  maps:get(trace_id, TraceContext, undefined);
extract_trace_id(_, AdditionalData) when is_map(AdditionalData) ->
  maps:get(trace_id, AdditionalData, undefined);
extract_trace_id(_, _) -> undefined.

extract_span_id(undefined, undefined) -> undefined;
extract_span_id(TraceContext, _) when is_map(TraceContext) ->
  maps:get(span_id, TraceContext, undefined);
extract_span_id(_, AdditionalData) when is_map(AdditionalData) ->
  maps:get(span_id, AdditionalData, undefined);
extract_span_id(_, _) -> undefined.

extract_request_hash(undefined) -> undefined;
extract_request_hash(AdditionalData) when is_map(AdditionalData) ->
  maps:get(request_hash, AdditionalData, undefined);
extract_request_hash(_) -> undefined.

%% Cleanup interval: 10% of TTL or max 60 seconds (whichever is smaller)
%% Uses spawn to avoid blocking timer process
start_cleanup_timer() ->
  TtlSeconds = application:get_env(beamline_router, idempotency_ttl_seconds, 3600),
  CleanupInterval = min(TtlSeconds div 10, 60),  %% 10% of TTL or max 60s
  case timer:apply_interval(CleanupInterval * 1000, ?MODULE, cleanup, []) of
    {ok, _TimerRef} -> ok;
    {error, Reason} ->
      router_logger:error(~"Failed to start idempotency cleanup timer", #{
        ~"error" => Reason
      }),
      ok
  end.

%% Internal: Evict oldest entries (by processed_at time)
evict_oldest_entries(Count) when Count =< 0 -> 0;
evict_oldest_entries(Count) ->
  Now = erlang:system_time(millisecond),
  %% Get all entries with their processed_at times
  AllEntries = ets:foldl(fun({Key, Value}, Acc) ->
    ProcessedAt = case is_map(Value) of
      true -> maps:get(processed_at, Value, Now);
      false -> Now  %% Legacy format
    end,
    [{Key, ProcessedAt} | Acc]
  end, [], router_idem),
  
  %% Sort by processed_at (oldest first)
  SortedEntries = lists:sort(fun({_, T1}, {_, T2}) -> T1 =< T2 end, AllEntries),
  
  %% Evict oldest N entries
  EntriesToEvict = lists:sublist(SortedEntries, Count),
  lists:foreach(fun({Key, _}) ->
    ets:delete(router_idem, Key)
  end, EntriesToEvict),
  length(EntriesToEvict).
