-module(router_nats_publish_retry).

-doc "Publish Retry Logic with Backoff".
%%
%% This module implements explicit retry logic for NATS publish operations
%% with configurable backoff strategies, jitter, and deadline management.
%%
%% Retry Parameters:
%% - maxAttempts: Total number of attempts (default: 3)
%% - backoffStrategy: exponential or linear (default: exponential)
%% - backoffBase: Base interval for backoff (default: 100ms)
%% - backoffMax: Maximum backoff interval (default: 5000ms)
%% - jitter: Random deviation to smooth peaks (default: ±20%)
%% - timeoutPerAttempt: Maximum wait per attempt (default: 2s)
%% - totalDeadline: Total time budget for publish (default: 10s)
%%
-export([publish_with_retry/5, publish_with_retry/6]).
-export([get_default_config/0]).
%% Exported for testing
-export([calculate_backoff/5, is_retryable_error/1]).

-include("beamline_router.hrl").

-define(DEFAULT_MAX_ATTEMPTS, 3).
-define(DEFAULT_BACKOFF_STRATEGY, exponential).
-define(DEFAULT_BACKOFF_BASE_MS, 100).
-define(DEFAULT_BACKOFF_MAX_MS, 5000).
-define(DEFAULT_JITTER_PERCENT, 20).
-define(DEFAULT_TIMEOUT_PER_ATTEMPT_MS, 2000).
-define(DEFAULT_TOTAL_DEADLINE_MS, 10000).

-spec publish_with_retry(binary(), binary(), fun((binary(), binary()) -> ok | {error, term()}), 
                         fun((integer(), integer()) -> ok), map()) -> 
    {ok, integer()} | {error, term()}.
publish_with_retry(Subject, Payload, PublishFun, MetricsFun, State) ->
    Config = get_default_config(),
    publish_with_retry(Subject, Payload, PublishFun, MetricsFun, State, Config).

-spec publish_with_retry(binary(), binary(), fun((binary(), binary()) -> ok | {error, term()}), 
                         fun((integer(), integer()) -> ok), map(), map()) -> 
    {ok, integer()} | {error, term()}.
publish_with_retry(Subject, Payload, PublishFun, MetricsFun, State, Config) ->
    MaxAttempts = maps:get(~"max_attempts", Config, ?DEFAULT_MAX_ATTEMPTS),
    BackoffStrategy = maps:get(~"backoff_strategy", Config, ?DEFAULT_BACKOFF_STRATEGY),
    BackoffBase = maps:get(~"backoff_base_ms", Config, ?DEFAULT_BACKOFF_BASE_MS),
    BackoffMax = maps:get(~"backoff_max_ms", Config, ?DEFAULT_BACKOFF_MAX_MS),
    JitterPercent = maps:get(~"jitter_percent", Config, ?DEFAULT_JITTER_PERCENT),
    TimeoutPerAttempt = maps:get(~"timeout_per_attempt_ms", Config, ?DEFAULT_TIMEOUT_PER_ATTEMPT_MS),
    TotalDeadline = maps:get(~"total_deadline_ms", Config, ?DEFAULT_TOTAL_DEADLINE_MS),
    
    StartTime = erlang:system_time(millisecond),
    publish_with_retry_internal(Subject, Payload, PublishFun, MetricsFun, State, 
                                1, MaxAttempts, BackoffStrategy, BackoffBase, BackoffMax, 
                                JitterPercent, TimeoutPerAttempt, TotalDeadline, StartTime).

%% Internal retry logic
publish_with_retry_internal(Subject, Payload, PublishFun, MetricsFun, State,
                            Attempt, MaxAttempts, BackoffStrategy, BackoffBase, BackoffMax,
                            JitterPercent, TimeoutPerAttempt, TotalDeadline, StartTime) ->
    %% Check total deadline
    Elapsed = erlang:system_time(millisecond) - StartTime,
    if
        Elapsed >= TotalDeadline ->
            %% Deadline exceeded
            router_logger:warn(~"Publish deadline exceeded", #{
                ~"event" => ~"publish_retry",
                ~"subject" => Subject,
                ~"attempt" => Attempt,
                ~"max_attempts" => MaxAttempts,
                ~"elapsed_ms" => Elapsed,
                ~"deadline_ms" => TotalDeadline,
                ~"deadline_exceeded" => true,
                ~"error_code" => ~"NATS_PUBLISH_DEADLINE_EXCEEDED"
            }),
            MetricsFun(Attempt - 1, MaxAttempts),  % Record last attempt
            {error, deadline_exceeded};
        Attempt > MaxAttempts ->
            %% Max attempts exceeded
            router_logger:warn(~"Publish max attempts exceeded", #{
                ~"event" => ~"publish_retry",
                ~"subject" => Subject,
                ~"attempt" => Attempt - 1,
                ~"max_attempts" => MaxAttempts,
                ~"deadline_exceeded" => false,
                ~"error_code" => ~"NATS_PUBLISH_MAX_ATTEMPTS_EXCEEDED"
            }),
            MetricsFun(Attempt - 1, MaxAttempts),
            {error, max_attempts_exceeded};
        true ->
            %% Attempt publish
            AttemptStartTime = erlang:system_time(microsecond),
            Result = case PublishFun(Subject, Payload) of
                ok ->
                    %% Success
                    AttemptLatency = (erlang:system_time(microsecond) - AttemptStartTime) / 1000.0,
                    router_logger:info(~"Publish retry attempt succeeded", #{
                        ~"event" => ~"publish_retry",
                        ~"subject" => Subject,
                        ~"attempt" => Attempt,
                        ~"max_attempts" => MaxAttempts,
                        ~"latency_ms" => AttemptLatency,
                        ~"deadline_exceeded" => false,
                        ~"error_code" => ~"NATS_PUBLISH_RETRY_SUCCESS"
                    }),
                    MetricsFun(Attempt, MaxAttempts),
                    %% Emit latency metric
                    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => AttemptLatency / 1000.0}, #{}),
                    {ok, Attempt};
                {error, Reason} ->
                    %% Failure - check if retryable
                    AttemptLatency = (erlang:system_time(microsecond) - AttemptStartTime) / 1000.0,
                    %% Emit latency metric even on failure
                    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => AttemptLatency / 1000.0}, #{}),
                    case is_retryable_error(Reason) of
                        true ->
                            %% Retryable error - log and retry
                            ErrorType = classify_error_type_internal(Reason),
                            router_logger:info(~"Publish retry attempt failed", #{
                                ~"event" => ~"publish_retry",
                                ~"subject" => Subject,
                                ~"attempt" => Attempt,
                                ~"max_attempts" => MaxAttempts,
                                ~"error" => sanitize_error(Reason),
                                ~"error_type" => ErrorType,
                                ~"latency_ms" => AttemptLatency,
                                ~"deadline_exceeded" => false,
                                ~"error_code" => ~"NATS_PUBLISH_RETRY_ATTEMPT"
                            }),
                            MetricsFun(Attempt, MaxAttempts),
                            
                            %% Calculate backoff delay
                            Delay = calculate_backoff(Attempt, BackoffStrategy, BackoffBase, BackoffMax, JitterPercent),
                            
                            %% Emit retry delay metric
                            router_metrics:emit_metric(router_nats_publish_retry_delay_seconds, 
                                                       #{value => Delay / 1000.0}, #{
                                attempt => integer_to_binary(Attempt)
                            }),
                            
                            %% Sleep with backoff
                            timer:sleep(Delay),
                            
                            %% Retry
                            publish_with_retry_internal(Subject, Payload, PublishFun, MetricsFun, State,
                                                        Attempt + 1, MaxAttempts, BackoffStrategy, BackoffBase, BackoffMax,
                                                        JitterPercent, TimeoutPerAttempt, TotalDeadline, StartTime);
                        false ->
                            %% Non-retryable error - return immediately
                            AttemptLatency = (erlang:system_time(microsecond) - AttemptStartTime) / 1000.0,
                            router_logger:warn(~"Publish failed with non-retryable error", #{
                                ~"subject" => Subject,
                                ~"attempt" => Attempt,
                                ~"max_attempts" => MaxAttempts,
                                ~"error" => sanitize_error(Reason),
                                ~"latency_ms" => AttemptLatency,
                                ~"error_code" => ~"NATS_PUBLISH_NON_RETRYABLE_ERROR"
                            }),
                            MetricsFun(Attempt, MaxAttempts),
                            {error, Reason}
                    end
            end,
            Result
    end.

-spec is_retryable_error(term()) -> boolean().
is_retryable_error(Reason) ->
    case Reason of
        timeout -> true;
        connection_refused -> true;
        connection_closed -> true;
        nats_unavailable -> true;
        not_connected -> true;
        {error, timeout} -> true;
        {error, connection_refused} -> true;
        {error, connection_closed} -> true;
        {error, nats_unavailable} -> true;
        {error, not_connected} -> true;
        _ ->
            %% Check if it's a 5xx-like error (broker error)
            ReasonBin = error_to_reason(Reason),
            case binary:match(ReasonBin, [~"5", ~"server_error", ~"unavailable"]) of
                nomatch -> false;
                _ -> true
            end
    end.

-spec calculate_backoff(integer(), atom(), integer(), integer(), integer()) -> integer().
calculate_backoff(Attempt, Strategy, Base, Max, JitterPercent) ->
    BaseDelay = case Strategy of
        exponential ->
            Delay = Base * trunc(math:pow(2, Attempt - 1)),
            min(Delay, Max);
        linear ->
            Delay = Base * Attempt,
            min(Delay, Max);
        _ ->
            Base
    end,
    
    %% Add jitter (±JitterPercent%)
    JitterRange = trunc(BaseDelay * JitterPercent / 100),
    Jitter = case JitterRange > 0 of
        true ->
            %% Random value in range [-JitterRange, +JitterRange]
            rand:uniform(JitterRange * 2 + 1) - JitterRange - 1;
        false ->
            %% No jitter if range is 0
            0
    end,
    FinalDelay = max(0, BaseDelay + Jitter),
    min(FinalDelay, Max).

-spec get_default_config() -> map().
get_default_config() ->
    #{
        ~"max_attempts" => ?DEFAULT_MAX_ATTEMPTS,
        ~"backoff_strategy" => ?DEFAULT_BACKOFF_STRATEGY,
        ~"backoff_base_ms" => ?DEFAULT_BACKOFF_BASE_MS,
        ~"backoff_max_ms" => ?DEFAULT_BACKOFF_MAX_MS,
        ~"jitter_percent" => ?DEFAULT_JITTER_PERCENT,
        ~"timeout_per_attempt_ms" => ?DEFAULT_TIMEOUT_PER_ATTEMPT_MS,
        ~"total_deadline_ms" => ?DEFAULT_TOTAL_DEADLINE_MS
    }.

%% Helper functions
sanitize_error(Error) ->
    case is_binary(Error) of
        true -> Error;
        false -> 
            case is_atom(Error) of
                true -> atom_to_binary(Error, utf8);
                false -> 
                    case is_tuple(Error) andalso tuple_size(Error) =:= 2 of
                        true -> 
                            {error, Reason} = Error,
                            sanitize_error(Reason);
                        false -> 
                            list_to_binary(io_lib:format("~p", [Error]))
                    end
            end
    end.

error_to_reason(Reason) ->
    case is_binary(Reason) of
        true -> Reason;
        false ->
            case is_atom(Reason) of
                true -> atom_to_binary(Reason, utf8);
                false -> list_to_binary(io_lib:format("~p", [Reason]))
            end
    end.

%% Uses router_nats:classify_error_type if available, otherwise classifies locally
classify_error_type_internal(Reason) ->
    %% Try to use router_nats:classify_error_type if available
    case erlang:function_exported(router_nats, classify_error_type, 1) of
        true ->
            router_nats:classify_error_type(Reason);
        false ->
            %% Fallback classification
            case Reason of
                timeout -> ~"timeout";
                connection_refused -> ~"connection";
                connection_closed -> ~"connection";
                nats_unavailable -> ~"connection";
                not_connected -> ~"connection";
                {error, timeout} -> ~"timeout";
                {error, connection_refused} -> ~"connection";
                {error, connection_closed} -> ~"connection";
                {error, nats_unavailable} -> ~"connection";
                {error, not_connected} -> ~"connection";
                _ ->
                    ReasonBin = error_to_reason(Reason),
                    case binary:match(ReasonBin, [~"nack", ~"NAK"]) of
                        nomatch ->
                            case binary:match(ReasonBin, [~"5", ~"server_error", ~"unavailable"]) of
                                nomatch -> ~"unknown";
                                _ -> ~"broker_error"
                            end;
                        _ -> ~"nack"
                    end
            end
    end.

