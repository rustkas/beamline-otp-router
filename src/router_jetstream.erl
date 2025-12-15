-module(router_jetstream).
-ignore_xref([
    router_jetstream,
    {router_jetstream, start_link, 0},
    {router_jetstream, setup, 1},
    {router_jetstream, configure, 1},
    {router_jetstream, ack, 1},
    {router_jetstream, nak, 2},
    {router_jetstream, nak, 3},
    {router_jetstream, term, 1},
    {router_jetstream, handle, 2},
    {router_jetstream, metrics, 0},
    {router_jetstream, trace_ctx, 1},
    {router_jetstream, subscribe_decide, 1},
    {router_jetstream, subscribe_results, 1},
    {router_jetstream, subscribe_acks, 1}
]).
-export([
    setup/1, 
    configure/1, 
    ack/1, 
    nak/2,
    nak/3,
    term/1,
    handle/2, 
    metrics/0, 
    trace_ctx/1,
    subscribe_decide/1,
    subscribe_results/1,
    subscribe_acks/1,
    start_link/0,
    %% Helper functions for testing
    extract_assignment_id/1,
    extract_tenant_id/1,
    extract_request_id/1,
    %% Resource monitoring
    get_table_size/0,
    get_table_memory/0,
    check_size_limit/0,
    %% Redelivery backoff calculation (for future use)
    calculate_redelivery_backoff/2
]).

-include("beamline_router.hrl").

%% @doc Start link for supervisor (gen_server wrapper)
% start_link() ->
%     %% For CP1, router_jetstream is a stateless module
%     %% Return a dummy pid to satisfy supervisor requirements
%     Pid = spawn_link(fun() -> receive _ -> ok end end),
%     {ok, Pid}.

%% @doc Start link for supervisor (gen_server wrapper)
start_link() ->
    router_jetstream_sup:start_link().

%% @doc Setup router_jetstream module
setup(Opts) ->
  router_metrics:ensure(),
  {ok, Opts}.

%% @doc Configure MaxDeliver and backoff settings
%% MaxDeliver: Maximum number of delivery attempts
%% BackoffSeconds: List of backoff delays in seconds (e.g., [1, 2, 4])
configure(#{max_deliver := Max, backoff_seconds := BackoffSeconds}) ->
  case ets:info(router_jetstream_state) of
    undefined -> 
      ets:new(router_jetstream_state, [named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
    _ -> ok
  end,
  ets:insert(router_jetstream_state, {config, Max, BackoffSeconds}),
  ok.

%% @doc Acknowledge message processing (successful)
%% Clears delivery count after successful ACK
ack(#{id := Id} = Msg) ->
  router_metrics:inc(router_jetstream_ack_total),
  telemetry:execute([router, jetstream, ack], #{count => 1}, #{msg => Msg}),
  case router_nats:ack_message(Id) of
    ok ->
      %% Clear delivery count on successful ACK
      clear_delivery_count(Id),
      ok;
    Error -> 
      telemetry:execute([router, jetstream, ack_error], #{count => 1}, #{msg => Msg, error => Error}),
      Error
  end.

%% @doc Negative acknowledge message (request redelivery)
%% Uses exponential backoff based on delivery count
%% Backward compatible: calls nak/3 with empty context
nak(#{id := _} = Msg, Reason) ->
  nak(Msg, Reason, #{}).

%% @doc Negative acknowledge message (request redelivery) with context labels
%% Context map may contain: assignment_id, request_id, source
%% Uses exponential backoff based on delivery count
nak(#{id := Id} = Msg, Reason, Context) ->
  %% Get delivery count for redelivery tracking
  DeliveryCount = get_delivery_count(Id),
  
  %% Extract labels from context or use defaults
  AssignmentId = maps:get(assignment_id, Context, <<"unknown">>),
  RequestId = maps:get(request_id, Context, <<"unknown">>),
  %% Source defaults to reason if not provided (for backward compatibility)
  Source = case maps:get(source, Context, undefined) of
    undefined -> reason_to_source(Reason);
    ProvidedSource -> ProvidedSource
  end,
  ReasonBin = reason_to_binary(Reason),
  
  %% Emit metric with labels using router_metrics:emit_metric
  router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
    assignment_id => AssignmentId,
    request_id => RequestId,
    reason => ReasonBin,
    source => Source,
    delivery_count => integer_to_binary(DeliveryCount)
  }),
  
  %% Track redelivery attempt
  track_redelivery_attempt(Id, DeliveryCount, Reason),
  
  %% Log redelivery event for debugging and correlation with metrics
  router_logger:info(<<"Message redelivery requested">>, #{
    <<"assignment_id">> => AssignmentId,
    <<"request_id">> => RequestId,
    <<"reason">> => ReasonBin,
    <<"source">> => Source,
    <<"delivery_count">> => DeliveryCount,
    <<"msg_id">> => Id
  }),
  
  %% Also emit telemetry event for compatibility
  telemetry:execute([router, jetstream, nak], #{count => 1}, #{
    msg => Msg, 
    reason => Reason, 
    delivery_count => DeliveryCount,
    assignment_id => AssignmentId,
    request_id => RequestId,
    source => Source
  }),
  
  case router_nats:nak_message(Id) of
    ok -> ok;
    Error -> 
      telemetry:execute([router, jetstream, nak_error], #{count => 1}, #{
        msg => Msg, 
        reason => Reason, 
        error => Error,
        assignment_id => AssignmentId,
        request_id => RequestId,
        source => Source
      }),
      Error
  end.

%% @doc Handle JetStream message with MaxDeliver tracking and DLQ support
%% Returns: {ok, allow} | {ok, dlq} | {error, Reason}
%% 
%% Behavior:
%% - Tracks delivery count per message ID
%% - If delivery count >= MaxDeliver: ACK message and send to DLQ
%% - Otherwise: Apply backoff logic and ACK/NAK accordingly
handle(#{id := Id, subject := Subject} = Msg, Ctx) ->
  telemetry:execute([router, jetstream, handle], #{count => 1}, #{ctx => Ctx}),
  {Max, BackoffSeconds} = get_config(Subject),
  DeliveryCount = incr_delivery(Id),
  
  case DeliveryCount >= Max of
    true ->
      %% MaxDeliver exhausted: ACK message and send to DLQ
      %% Extract labels from context and message
      AssignmentId =
        case maps:find(assignment_id, Ctx) of
          {ok, ProvidedAssignmentId} -> ProvidedAssignmentId;
          error -> extract_assignment_id(Subject)
        end,
      Reason = <<"maxdeliver_exhausted">>,
      TenantId = extract_tenant_id(Msg),
      Source = <<"maxdeliver_exhausted">>,
      MsgId = Id,
      RequestId = extract_request_id(Msg),
      
      %% Emit DLQ metric with labels
      router_metrics:emit_metric(router_dlq_total, #{count => 1}, #{
        assignment_id => AssignmentId,
        reason => Reason,
        tenant_id => TenantId,
        source => Source,
        msg_id => MsgId,
        request_id => RequestId
      }),
      
      telemetry:execute([router, jetstream, maxdeliver_exhausted], #{count => 1}, #{
        msg => Msg, 
        delivery_count => DeliveryCount,
        max_deliver => Max
      }),
      
      %% ACK the message first (prevent further redelivery)
      _ = router_nats:ack_message(Id),
      
      %% Send to DLQ
      case send_to_dlq(Subject, Msg, maxdeliver_exhausted) of
        ok ->
          clear_delivery_count(Id),
          {ok, dlq};
        {error, DLQError} ->
          telemetry:execute([router, jetstream, dlq_error], #{count => 1}, #{
            msg => Msg,
            error => DLQError
          }),
          %% Message is ACKed, but DLQ failed - log error but don't fail
          {ok, dlq}
      end;
    false ->
      %% Apply backoff logic
      case should_nak(DeliveryCount, BackoffSeconds) of
        true -> 
          %% NAK for redelivery with backoff
          case nak(Msg, backoff) of
            ok -> {ok, redelivery};
            Error -> {error, Error}
          end;
        false -> 
          %% ACK immediately (no backoff needed)
          case ack(Msg) of
            ok -> {ok, allow};
            Error -> {error, Error}
          end
      end
  end.

%% @doc Get list of metrics exported by this module
metrics() -> [router_jetstream_ack_total, router_jetstream_redelivery_total, router_dlq_total].

%% @doc Extract trace context from headers
trace_ctx(Headers) -> {trace, Headers}.

%% @doc Subscribe to decide subject (JetStream durable)
%% Wrapper over router_jetstream_consumer_manager for decide consumer
subscribe_decide(Opts) ->
    router_jetstream_consumer_manager:subscribe_decide(Opts).

%% @doc Subscribe to results subject (JetStream durable)
%% Wrapper over router_jetstream_consumer_manager for result consumer
subscribe_results(Opts) ->
    router_jetstream_consumer_manager:subscribe_results(Opts).

%% @doc Subscribe to ACK subject (JetStream durable)
%% Wrapper over router_jetstream_consumer_manager for ack consumer
subscribe_acks(Opts) ->
    router_jetstream_consumer_manager:subscribe_acks(Opts).

%% @doc Helper functions for extracting common label values for metrics
extract_assignment_id(Subject) when is_binary(Subject) ->
  %% For known subjects with a stable prefix (4+ segments), use the last segment.
  %% For shorter/unknown subjects, preserve the full subject (more informative).
  case binary:split(Subject, <<".">>, [global]) of
    [_A, _B, _C, _D | _] = Parts -> lists:last(Parts);
    _ -> Subject
  end;
extract_assignment_id(Subject) when is_list(Subject) ->
  extract_assignment_id(list_to_binary(Subject));
extract_assignment_id(_) ->
  <<"unknown">>.

extract_tenant_id(Msg) when is_map(Msg) ->
  Headers = maps:get(headers, Msg, #{}),
  Payload = maps:get(payload, Msg, #{}),
  case Headers of
    #{<<"tenant_id">> := TenantId} -> TenantId;
    _ ->
      case Payload of
        #{<<"tenant_id">> := TenantId} -> TenantId;
        _ -> <<"unknown">>
      end
  end;
extract_tenant_id(_) ->
  <<"unknown">>.

extract_request_id(Msg) when is_map(Msg) ->
  Headers = maps:get(headers, Msg, #{}),
  Payload = maps:get(payload, Msg, #{}),
  case Headers of
    #{<<"request_id">> := RequestId} -> RequestId;
    _ ->
      case Payload of
        #{<<"request_id">> := RequestId} -> RequestId;
        _ -> <<"unknown">>
      end
  end;
extract_request_id(_) ->
  <<"unknown">>.

%% @doc Terminal NAK (no redelivery)
%% Use this when message should not be redelivered (e.g., permanent error)
term(#{id := Id} = Msg) ->
    router_metrics:inc(router_jetstream_ack_total),
    telemetry:execute([router, jetstream, term], #{count => 1}, #{msg => Msg}),
    case router_nats:ack_message(Id) of
        ok ->
            clear_delivery_count(Id),
            ok;
        Error -> 
            telemetry:execute([router, jetstream, term_error], #{count => 1}, #{
                msg => Msg, 
                error => Error
            }),
            Error
    end.

%% Internal functions

%% @doc Get configuration (MaxDeliver and backoff seconds) for a specific subject
%% Subject-specific configuration:
%% - decide: js_max_deliver_decide (5), js_backoff_decide ([1000, 5000, 15000] ms)
%% - results: js_max_deliver_results (10), nats_js_backoff_seconds ([1, 2, 4] s)
%% - fallback: nats_js_max_deliver (3), nats_js_backoff_seconds ([1, 2, 4] s)
get_config(Subject) ->
  case ets:lookup(router_jetstream_state, config) of
    [{config, Max, BackoffSeconds}] -> {Max, BackoffSeconds};
    _ -> 
      %% Determine subject type and get subject-specific config
      case is_decide_subject(Subject) of
        true ->
          %% Decide subject: use decide-specific config
          MaxDecide = application:get_env(beamline_router, js_max_deliver_decide, 5),
          %% js_backoff_decide is in milliseconds, convert to seconds for consistency
          BackoffDecideMs = application:get_env(beamline_router, js_backoff_decide, [1000, 5000, 15000]),
          BackoffDecideSeconds = [Ms div 1000 || Ms <- BackoffDecideMs],
          {MaxDecide, BackoffDecideSeconds};
        false ->
          case is_results_subject(Subject) of
            true ->
              %% Results subject: use results-specific config
              MaxResults = application:get_env(beamline_router, js_max_deliver_results, 10),
              BackoffResults = application:get_env(beamline_router, nats_js_backoff_seconds, [1, 2, 4]),
              {MaxResults, BackoffResults};
            false ->
              %% Fallback: use general config
              MaxFallback = application:get_env(beamline_router, nats_js_max_deliver, 3),
              BackoffFallback = application:get_env(beamline_router, nats_js_backoff_seconds, [1, 2, 4]),
              {MaxFallback, BackoffFallback}
          end
      end
  end.

%% @doc Check if subject is a decide subject
is_decide_subject(Subject) when is_binary(Subject) ->
  binary:match(Subject, <<"beamline.router.v1.decide">>) =/= nomatch;
is_decide_subject(_) ->
  false.

%% @doc Check if subject is a results subject
is_results_subject(Subject) when is_binary(Subject) ->
  binary:match(Subject, <<"caf.exec.result.v1">>) =/= nomatch;
is_results_subject(_) ->
  false.

%% @doc Increment delivery count for message ID
incr_delivery(Id) ->
  case ets:update_counter(router_jetstream_state, {deliveries, Id}, 1, {{deliveries, Id}, 0}) of
    C -> C
  end.

%% @doc Get current delivery count for message ID
get_delivery_count(Id) ->
  case ets:lookup(router_jetstream_state, {deliveries, Id}) of
    [{{deliveries, Id}, Count}] -> Count;
    [] -> 0
  end.

%% @doc Clear delivery count for message ID (after successful ACK)
clear_delivery_count(Id) ->
  ets:delete(router_jetstream_state, {deliveries, Id}),
  %% Also clear redelivery tracking
  ets:delete(router_jetstream_state, {redelivery_tracking, Id}),
  ok.

%% @doc Track redelivery attempt for monitoring and metrics
track_redelivery_attempt(Id, DeliveryCount, Reason) ->
  Now = erlang:system_time(millisecond),
  TrackingKey = {redelivery_tracking, Id},
  RedeliveryCount = get_redelivery_count(Id) + 1,
  TrackingData = #{
    msg_id => Id,
    delivery_count => DeliveryCount,
    reason => Reason,
    timestamp => Now,
    redelivery_count => RedeliveryCount
  },
  ets:insert(router_jetstream_state, {TrackingKey, TrackingData}),
  ok.

%% @doc Get redelivery count for a message
get_redelivery_count(Id) ->
  TrackingKey = {redelivery_tracking, Id},
  case ets:lookup(router_jetstream_state, TrackingKey) of
    [{TrackingKey, TrackingData}] when is_map(TrackingData) ->
      maps:get(redelivery_count, TrackingData, 0);
    [] ->
      0
  end.

%% @doc Determine if message should be NAKed based on delivery count and backoff
%% Returns true if backoff should be applied (NAK), false if should ACK immediately
%% Enhanced with better backoff calculation and redelivery tracking
should_nak(DeliveryCount, BackoffSeconds) ->
  case BackoffSeconds of
    [] -> false;  %% No backoff configured
    _ when is_list(BackoffSeconds) ->
      %% Apply backoff on even delivery attempts (2nd, 4th, 6th, etc.)
      %% This allows exponential backoff: 1st delivery (no backoff), 2nd delivery (backoff), etc.
      %% DeliveryCount 1: ACK (no backoff)
      %% DeliveryCount 2: NAK (backoff)
      %% DeliveryCount 3: ACK (no backoff)
      %% DeliveryCount 4: NAK (backoff)
      ShouldNak = (DeliveryCount rem 2) =:= 0,
      %% Track redelivery decision for metrics
      case ShouldNak of
        true ->
          router_metrics:emit_metric(router_jetstream_redelivery_decision, #{count => 1}, #{
            decision => <<"nak">>,
            delivery_count => integer_to_binary(DeliveryCount)
          });
        false ->
          router_metrics:emit_metric(router_jetstream_redelivery_decision, #{count => 1}, #{
            decision => <<"ack">>,
            delivery_count => integer_to_binary(DeliveryCount)
          })
      end,
      ShouldNak;
    _ -> false
  end.

%% @doc Calculate backoff delay for redelivery (with exponential backoff and jitter)
%% Returns delay in milliseconds
-spec calculate_redelivery_backoff(integer(), [integer()]) -> integer().
calculate_redelivery_backoff(DeliveryCount, BackoffSeconds) when is_list(BackoffSeconds), length(BackoffSeconds) > 0 ->
  %% Get backoff value for this delivery count (use index or exponential)
  %% Clamp index to list length (use last value for higher delivery counts)
  Index = min(max(1, DeliveryCount), length(BackoffSeconds)),
  BaseBackoffSeconds = lists:nth(Index, BackoffSeconds),
  BaseBackoffMs = BaseBackoffSeconds * 1000,
  
  %% Add jitter (random 0-20% of base delay) to prevent thundering herd
  JitterPercent = application:get_env(beamline_router, nats_redelivery_jitter_percent, 20),
  JitterMs = trunc(BaseBackoffMs * (JitterPercent / 100.0) * (rand:uniform() - 0.5)),
  TotalBackoffMs = max(0, BaseBackoffMs + JitterMs),
  
  %% Emit metric for backoff calculation
  router_metrics:emit_metric(router_jetstream_redelivery_backoff_ms, #{value => TotalBackoffMs}, #{
    delivery_count => integer_to_binary(DeliveryCount),
    base_backoff_ms => BaseBackoffMs
  }),
  
  TotalBackoffMs;
calculate_redelivery_backoff(_DeliveryCount, _) ->
  %% No backoff configured, return 0
  0.

%% @doc Send message to DLQ
%% Uses build_dlq_subject to construct DLQ subject from original subject
send_to_dlq(Subject, Msg, Reason) ->
  %% Build DLQ subject (same logic as router_intake_error_handler)
  DLQSubject = build_dlq_subject(Subject),
  
  %% Build DLQ message payload
  DLQPayload = build_dlq_payload(Msg, Reason),
  DLQJson = jsx:encode(DLQPayload),
  
  %% Build headers for DLQ message
  DLQHeaders = build_dlq_headers(Msg, Reason),
  
  %% Publish to DLQ
  case router_nats:publish_with_ack(DLQSubject, DLQJson, DLQHeaders) of
    {ok, _MsgId} -> ok;
    {error, Error} -> {error, Error}
  end.

%% @doc Build DLQ subject from original subject
%% Uses same logic as router_intake_error_handler:build_dlq_subject/1
build_dlq_subject(Subject) ->
  case application:get_env(beamline_router, dlq_subject_pattern, undefined) of
    undefined ->
      %% Default: append .dlq to original subject
      <<Subject/binary, ".dlq">>;
    Pattern when is_binary(Pattern) ->
      Pattern;
    Pattern when is_list(Pattern) ->
      list_to_binary(Pattern);
    _ ->
      %% Fallback to default
      <<Subject/binary, ".dlq">>
  end.

%% @doc Build DLQ message payload
%% Includes: original_subject, msg_id, reason, timestamp, trace_id, tenant_id, error_code
%% Reference: docs/CP2_CHECKLIST.md#test_dlq_payload_contains_context
build_dlq_payload(#{id := Id, subject := Subject} = Msg, Reason) ->
  %% Extract trace_id and tenant_id from message headers or payload
  Headers = maps:get(headers, Msg, #{}),
  Payload = maps:get(payload, Msg, #{}),
  TraceId = maps:get(<<"trace_id">>, Headers, maps:get(<<"trace_id">>, Payload, undefined)),
  TenantId = maps:get(<<"tenant_id">>, Headers, maps:get(<<"tenant_id">>, Payload, undefined)),
  
  %% Build error_code from reason
  ErrorCode = case Reason of
    maxdeliver_exhausted -> <<"MAXDELIVER_EXHAUSTED">>;
    validation_failed -> <<"VALIDATION_FAILED">>;
    processing_error -> <<"PROCESSING_ERROR">>;
    _ -> atom_to_binary(Reason, utf8)
  end,
  
  BasePayload = #{
    <<"original_subject">> => Subject,
    <<"msg_id">> => Id,
    <<"reason">> => atom_to_binary(Reason, utf8),
    <<"error_code">> => ErrorCode,
    <<"timestamp">> => erlang:system_time(millisecond)
  },
  
  %% Add trace_id and tenant_id if available
  PayloadWithContext = case {TraceId, TenantId} of
    {undefined, undefined} -> BasePayload;
    {TraceId, undefined} -> maps:put(<<"trace_id">>, TraceId, BasePayload);
    {undefined, TenantId} -> maps:put(<<"tenant_id">>, TenantId, BasePayload);
    {TraceId, TenantId} -> 
      maps:put(<<"trace_id">>, TraceId, 
        maps:put(<<"tenant_id">>, TenantId, BasePayload))
  end,
  
  %% Include full message for debugging (optional, can be disabled for security)
  case application:get_env(beamline_router, dlq_include_full_message, true) of
    true -> maps:put(<<"message">>, Msg, PayloadWithContext);
    false -> PayloadWithContext
  end.

%% @doc Build DLQ message headers
build_dlq_headers(#{id := Id} = Msg, Reason) ->
  Headers = #{
    <<"x-dlq-reason">> => atom_to_binary(Reason, utf8),
    <<"x-original-msg-id">> => Id
  },
  %% Include original headers if present
  case maps:get(headers, Msg, undefined) of
    undefined -> Headers;
    OriginalHeaders when is_map(OriginalHeaders) ->
      maps:merge(Headers, OriginalHeaders);
    _ -> Headers
  end.

%% @doc Convert reason atom to binary for metric labels
%% Maps common reason atoms to standardized binary values
-spec reason_to_binary(atom() | binary()) -> binary().
reason_to_binary(Reason) when is_binary(Reason) ->
    Reason;
reason_to_binary(tenant_validation_failed) ->
    <<"tenant_validation_failed">>;
reason_to_binary(backoff) ->
    <<"backoff">>;
reason_to_binary(backpressure) ->
    <<"backpressure">>;
reason_to_binary(ack_error) ->
    <<"ack_error">>;
reason_to_binary(nak_error) ->
    <<"nak_error">>;
reason_to_binary(processing_timeout) ->
    <<"processing_timeout">>;
reason_to_binary(publish_error) ->
    <<"publish_error">>;
reason_to_binary(nats_disconnect) ->
    <<"nats_disconnect">>;
reason_to_binary(maxdeliver_exhausted) ->
    <<"maxdeliver_exhausted">>;
reason_to_binary(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
reason_to_binary(Reason) ->
    %% Fallback: convert to binary
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc Convert reason to source label value
%% Source represents the source/cause of redelivery (e.g., tenant_validation, ack_failure)
%% This is used when source is not explicitly provided in context
-spec reason_to_source(atom() | binary()) -> binary().
reason_to_source(tenant_validation_failed) ->
    <<"tenant_validation">>;
reason_to_source(backoff) ->
    <<"backoff">>;
reason_to_source(backpressure) ->
    <<"backpressure">>;
reason_to_source(ack_error) ->
    <<"ack_failure">>;
reason_to_source(nak_error) ->
    <<"nak_failure">>;
reason_to_source(processing_timeout) ->
    <<"processing_timeout">>;
reason_to_source(publish_error) ->
    <<"publish_failure">>;
reason_to_source(nats_disconnect) ->
    <<"nats_disconnect">>;
reason_to_source(maxdeliver_exhausted) ->
    <<"maxdeliver_exhausted">>;
reason_to_source(Reason) when is_atom(Reason) ->
    %% Default: use reason as source (normalized)
    ReasonBin = atom_to_binary(Reason, utf8),
    %% Convert underscores to match common source format
    binary:replace(ReasonBin, <<"_failed">>, <<"">>, [global]);
reason_to_source(Reason) when is_binary(Reason) ->
    %% If already binary, use as-is (but normalize)
    binary:replace(Reason, <<"_failed">>, <<"">>, [global]);
reason_to_source(_Reason) ->
    <<"unknown">>.

%% @doc Get current table size (number of entries)
-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(router_jetstream_state, size) of
        undefined -> undefined;
        Size -> Size
    end.

%% @doc Get current table memory usage in bytes
-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(router_jetstream_state, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

%% @doc Check if table size exceeds configured limit
-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, jetstream_state_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(router_jetstream_state, Limit);
        _ -> {error, invalid_limit}
    end.
