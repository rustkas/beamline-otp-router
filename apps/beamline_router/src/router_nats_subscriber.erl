-module(router_nats_subscriber).

-doc "NATS Subscriber".
%% @deprecated This module is deprecated in favor of router_decide_consumer.erl
%% which uses JetStream durable consumers with explicit ack policy.
%% This module will be removed in a future release.
%% 
%% Migration: Use router_decide_consumer.erl for decide message processing.
%% Subscribes to NATS subjects and handles routing requests
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([normalize_boolean/1, check_tenant_allowed/1]).  %% Exported for testing only

-include("beamline_router.hrl").

-define(SUBJECT, ~"beamline.router.v1.decide").

-record(state, {
    connection :: pid() | undefined,
    publication_monitors :: #{pid() => map()}  %% Monitor ref -> Request context for correlation
}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    %% Get timeout from config (3-arity returns value directly)
    Timeout = application:get_env(beamline_router, nats_timeout_ms, 5000),
    
    %% Subscribe to NATS subject
    case router_nats:subscribe(?SUBJECT, fun(_Subj, _Payload) -> ok end, Timeout) of
        ok ->
            {ok, #state{
                connection = undefined,
                publication_monitors = #{}
            }};
        Error ->
            %% Log subscription failure for monitoring
            router_logger:error(~"NATS subscription failed", #{
                ~"subject" => ?SUBJECT,
                ~"error" => Error
            }),
            {stop, Error}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info({nats_message, binary(), binary()} | {nats_message, binary(), binary(), binary() | undefined, map(), binary() | undefined} | {'DOWN', reference(), process, pid(), term()}, #state{}) -> {noreply, #state{}}.
handle_info({nats_message, Subject, Payload}, State) ->
    handle_nats_message(Subject, Payload, undefined),
    {noreply, State};
%% New format with ReplyTo
handle_info({nats_message, Subject, Payload, ReplyTo, _Headers, _MsgId}, State) ->
    handle_nats_message(Subject, Payload, ReplyTo),
    {noreply, State};
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    %% Handle publication process crash/exit
    %% Log error with correlation context if available
    case Reason of
        normal ->
            %% Normal exit - publication completed successfully
            ok;
        _ ->
            %% Abnormal exit - log error for monitoring
            %% Note: In production, increment assignments_failed_total here if not already done in adapter
            log_publication_error(Pid, Reason, MonitorRef)
    end,
    %% Remove monitor reference from state (if tracking)
    NewMonitors = maps:remove(MonitorRef, State#state.publication_monitors),
    {noreply, State#state{publication_monitors = NewMonitors}};
handle_info(_Info, State) ->
    {noreply, State}.

%% Internal: Handle NATS message
-spec handle_nats_message(binary(), binary(), binary() | undefined) -> ok.
handle_nats_message(Subject, Payload, ReplyTo) ->
    %% Early validation: Check payload size before parsing
    PayloadSize = byte_size(Payload),
    MaxPayloadSize = application:get_env(beamline_router, nats_max_payload_size, 1048576), %% Default: 1MB
    case PayloadSize > MaxPayloadSize of
        true ->
            router_logger:error(~"Payload size exceeds limit", #{
                ~"size" => PayloadSize,
                ~"max_size" => MaxPayloadSize,
                ~"subject" => Subject
            }),
            send_error_response(Subject, ReplyTo, #{}, ~"invalid_request", ~"Payload size exceeds limit"),
            ok;
        false ->
            %% Parse JSON payload (DecideRequest format)
    case jsx:decode(Payload, [return_maps]) of
        Request when is_map(Request) ->
                    %% Early version validation (before processing)
                    Version = maps:get(~"version", Request, undefined),
                    case Version of
                        ~"1" ->
                            handle_decide_request(Subject, ReplyTo, Request);
                        undefined ->
                            %% Build log context with CP1 correlation fields (if available)
                            LogContext = build_log_context(Request, #{
                                ~"subject" => Subject
                            }),
                            router_logger:error(~"Missing version field", LogContext),
                            send_error_response(Subject, ReplyTo, Request, ~"invalid_request", ~"Missing version field");
                        _ ->
                            %% Build log context with CP1 correlation fields (if available)
                            LogContext = build_log_context(Request, #{
                                ~"version" => Version,
                                ~"subject" => Subject,
                                ~"supported_versions" => [~"1"]
                            }),
                            router_logger:error(~"Unsupported request version", LogContext),
                            send_error_response(Subject, ReplyTo, Request, ~"invalid_request", ~"Unsupported version")
                    end;
                Error ->
                    %% Security: Sanitize error before logging to prevent secret leakage
                    SanitizedError = sanitize_error_for_logging(Error),
                    router_logger:error(~"Failed to parse NATS message", #{
                        ~"error" => SanitizedError,
                        ~"subject" => Subject,
                        ~"payload_size" => PayloadSize
                    }),
                    send_error_response(Subject, ReplyTo, #{}, ~"invalid_request", ~"Failed to parse JSON")
            end
    end.

%% Internal: Handle DecideRequest
%% Deprecated module; ReplyTo used when present.
-spec handle_decide_request(binary(), binary() | undefined, map()) -> ok.
handle_decide_request(Subject, ReplyTo, Request) ->
    %% Extract fields from DecideRequest
    TenantId = maps:get(~"tenant_id", Request, undefined),
    Task = maps:get(~"task", Request, #{}),
    PolicyId = maps:get(~"policy_id", Request, undefined),
    Constraints = maps:get(~"constraints", Request, #{}),
    RequestMetadata = maps:get(~"metadata", Request, #{}),
    
    %% Extract CP1 correlation fields (run_id, flow_id, step_id)
    RunId = maps:get(~"run_id", Request, undefined),
    FlowId = maps:get(~"flow_id", Request, undefined),
    StepId = maps:get(~"step_id", Request, undefined),
    
    %% Build message map for RouteRequest
    %% Include CP1 correlation fields (run_id, flow_id, step_id) in message for propagation
    Message = #{
        ~"tenant_id" => TenantId,
        ~"message_id" => maps:get(~"request_id", Request, undefined),
        ~"trace_id" => maps:get(~"trace_id", Request, undefined),
        ~"message_type" => maps:get(~"type", Task, ~"unknown"),
        ~"payload_ref" => maps:get(~"payload_ref", Task, undefined),
        ~"payload" => maps:get(~"payload", Task, undefined),
        ~"run_id" => RunId,
        ~"flow_id" => FlowId,
        ~"step_id" => StepId
    },
    
    %% Build context with constraints and metadata
    Context = maps:merge(Constraints, RequestMetadata),
    
    %% Create RouteRequest
    RouteRequest = #route_request{
        message = Message,
        policy_id = PolicyId,
        context = Context
    },
    
    %% Route the message (accept {Result, StopMetadata} or Result)
    CoreResult = router_core:route(RouteRequest, #{}),
    case CoreResult of
        {{ok, Decision}, _StopMetadata} ->
            %% Build DecideResponse
            Response = build_decide_response(Request, Decision),
            ResponseJson = jsx:encode(Response),
            
            %% Publish response (reply-inbox or .reply subject)
            ReplySubject = resolve_reply_subject(Subject, ReplyTo),
            router_nats:publish(ReplySubject, ResponseJson),
            
            %% Optional: push assignment to CAF if requested
            %% Check global enable flag and tenant allowlist before calling adapter
            %% Use async spawn_monitor to avoid blocking subscriber and track errors
            case should_publish_assignment(Request) of
                true ->
                    %% Extract correlation context for error logging
                    _RequestId = maps:get(~"request_id", Request, ~"unknown"),
                    _TraceId = maps:get(~"trace_id", Request, undefined),
                    TenantId = maps:get(~"tenant_id", Request, undefined),
                    %% Spawn monitored process for publication
                    %% The adapter handles retries and telemetry internally
                    %% We monitor the process to catch unexpected crashes
                    {_Pid, _MonitorRef} = spawn_monitor(fun() ->
                        router_caf_adapter:publish_assignment(Request, Decision)
                    end),
                    ok;
                false ->
                    ok
            end;
                {ok, Decision} ->
            %% Build DecideResponse
            Response = build_decide_response(Request, Decision),
                    ResponseJson = jsx:encode(Response),
            
            %% Publish response (reply-inbox or .reply subject)
                    ReplySubject = resolve_reply_subject(Subject, ReplyTo),
            router_nats:publish(ReplySubject, ResponseJson),
            
            %% Optional: push assignment to CAF if requested
            %% Check global enable flag and tenant allowlist before calling adapter
            %% Use async spawn_monitor to avoid blocking subscriber and track errors
            case should_publish_assignment(Request) of
                true ->
                    %% Extract correlation context for error logging
                    _RequestId = maps:get(~"request_id", Request, ~"unknown"),
                    _TraceId = maps:get(~"trace_id", Request, undefined),
                    TenantId = maps:get(~"tenant_id", Request, undefined),
                    %% Spawn monitored process for publication
                    %% The adapter handles retries and telemetry internally
                    %% We monitor the process to catch unexpected crashes
                    {_Pid, _MonitorRef} = spawn_monitor(fun() ->
                        router_caf_adapter:publish_assignment(Request, Decision)
                    end),
                    ok;
                false ->
                    ok
            end;
        {{error, {ErrorReason, ErrorContext}}, _StopMetadata} ->
            %% Build ErrorResponse
            ErrorResponse = build_error_response(Request, ErrorReason, ErrorContext),
            ErrorResponseJson = jsx:encode(ErrorResponse),
            ReplySubject = resolve_reply_subject(Subject, ReplyTo),
            router_nats:publish(ReplySubject, ErrorResponseJson),
            
            %% Security: Filter PII from error context before logging
            FilteredErrorContext = router_logger:filter_pii(ErrorContext),
            %% Build log context with CP1 correlation fields
            LogContext = build_log_context(Request, #{
                ~"error" => ErrorReason,
                ~"error_context" => FilteredErrorContext,
                ~"subject" => Subject
            }),
            router_logger:error(~"Routing failed", LogContext);
        {error, {ErrorReason, ErrorContext}} ->
            %% Build ErrorResponse
            ErrorResponse = build_error_response(Request, ErrorReason, ErrorContext),
            ErrorResponseJson = jsx:encode(ErrorResponse),
            ReplySubject = resolve_reply_subject(Subject, ReplyTo),
            router_nats:publish(ReplySubject, ErrorResponseJson),
            
            %% Security: Filter PII from error context before logging
            FilteredErrorContext = router_logger:filter_pii(ErrorContext),
            %% Build log context with CP1 correlation fields
            LogContext = build_log_context(Request, #{
                ~"error" => ErrorReason,
                ~"error_context" => FilteredErrorContext,
                ~"subject" => Subject
            }),
            router_logger:error(~"Routing failed", LogContext)
    end.

%% Internal: Check if assignment should be published
%% Verifies global enable flag and tenant allowlist
-spec should_publish_assignment(map()) -> boolean().
should_publish_assignment(Request) ->
    %% Check if push_assignment flag is set
    PushAssignment = maps:get(~"push_assignment", Request, false),
    case normalize_boolean(PushAssignment) of
        false ->
            false;
        true ->
            %% Check global enable flag
            case application:get_env(beamline_router, caf_push_assignment_enabled, true) of
                false ->
                    false;
                true ->
                    %% Check tenant allowlist
                    TenantId = maps:get(~"tenant_id", Request, undefined),
                    check_tenant_allowed(TenantId)
            end
    end.

%% Internal: Check if tenant is allowed
-spec check_tenant_allowed(binary() | undefined) -> boolean().
check_tenant_allowed(undefined) ->
    %% If no tenant allowlist configured, allow all
    case application:get_env(beamline_router, caf_push_assignment_allowed_tenants, undefined) of
        undefined -> true;
        _ -> false  %% Require tenant_id if allowlist is configured
    end;
check_tenant_allowed(TenantId) ->
    case application:get_env(beamline_router, caf_push_assignment_allowed_tenants, undefined) of
        undefined -> true;  %% No allowlist = allow all
        AllowedTenants when is_list(AllowedTenants) ->
            %% Support both binary and string tenant IDs
            %% Normalize TenantId to binary for comparison
            TenantIdBin = case TenantId of
                Bin when is_binary(Bin) -> Bin;
                TenantStr when is_list(TenantStr) -> list_to_binary(TenantStr);
                _ -> TenantId
            end,
            %% Check if any element in allowlist matches (supporting both binary and string)
            lists:any(fun(Allowed) ->
                case Allowed of
                    AllowedBin when is_binary(AllowedBin) -> AllowedBin =:= TenantIdBin;
                    AllowedStr when is_list(AllowedStr) -> list_to_binary(AllowedStr) =:= TenantIdBin;
                    _ -> Allowed =:= TenantId
                end
            end, AllowedTenants);
        AllowedTenants when is_map(AllowedTenants) ->
            %% Normalize TenantId to binary for map key lookup
            TenantIdBin = case TenantId of
                Bin when is_binary(Bin) -> Bin;
                TenantStr when is_list(TenantStr) -> list_to_binary(TenantStr);
                _ -> TenantId
            end,
            maps:is_key(TenantIdBin, AllowedTenants) orelse
            (is_list(TenantId) andalso maps:is_key(list_to_binary(TenantId), AllowedTenants));
        _ -> false
    end.

%% Internal: Build DecideResponse
-spec build_decide_response(map(), #route_decision{}) -> map().
build_decide_response(Request, #route_decision{
    provider_id = ProviderId,
    reason = Reason,
    priority = Priority,
    expected_latency_ms = Latency,
    expected_cost = Cost,
    metadata = Metadata
}) ->
    RequestId = maps:get(~"request_id", Request, undefined),
    TraceId = maps:get(~"trace_id", Request, undefined),
    
    %% Build decision object
    Decision = #{
        ~"provider_id" => ProviderId,
        ~"priority" => Priority,
        ~"expected_latency_ms" => Latency,
        ~"expected_cost" => Cost,
        ~"reason" => Reason
    },
    
    %% Add optional fields
    DecisionWithOptional = case Metadata of
        #{} when map_size(Metadata) > 0 ->
            maps:put(~"metadata", Metadata, Decision);
        _ ->
            Decision
    end,
    
    %% Build context
    Context = case TraceId of
        undefined -> #{~"request_id" => RequestId};
        _ -> #{~"request_id" => RequestId, ~"trace_id" => TraceId}
    end,
    
    #{
        ~"ok" => true,
        ~"decision" => DecisionWithOptional,
        ~"context" => Context
    }.

%% Internal: Build ErrorResponse
-spec build_error_response(map(), atom() | binary(), map() | binary()) -> map().
build_error_response(Request, ErrorReason, ErrorContext) ->
    RequestId = maps:get(~"request_id", Request, undefined),
    TraceId = maps:get(~"trace_id", Request, undefined),
    
    %% Handle both old format (atom, map) and new format (binary, binary)
    {ErrorCode, ErrorMessage, ErrorDetails} = case {ErrorReason, ErrorContext} of
        {Code, Context} when is_atom(Code), is_map(Context) ->
            %% Check if this is an extension error
            case is_extension_error(Code) of
                true ->
                    %% Use extension error mapper
                    {ExtCode, ExtMsg, ExtDetails} = router_extension_error_mapper:map_extension_error({error, {Code, Context}}),
                    {ExtCode, ExtMsg, ExtDetails};
                false ->
                    %% Old format: atom reason, map context
                    CodeBin = case Code of
                        missing_tenant_id -> ~"invalid_request";
                        policy_not_found -> ~"policy_not_found";
                        no_provider_available -> ~"decision_failed";
                        invalid_policy -> ~"invalid_request";
                        _ -> ~"internal"
                    end,
                    Msg = maps:get(~"context", Context, ~"Routing failed"),
                    {CodeBin, Msg, Context}
            end;
        {Code, Message} when is_binary(Code), is_binary(Message) ->
            %% New format: binary code, binary message
            {Code, Message, #{}};
        {Code, Context} when is_atom(Code) ->
            %% Check if this is an extension error
            case is_extension_error(Code) of
                true ->
                    %% Use extension error mapper (with empty context)
                    {ExtCode, ExtMsg, ExtDetails} = router_extension_error_mapper:map_extension_error({error, {Code, #{}}}),
                    {ExtCode, ExtMsg, ExtDetails};
                false ->
                    %% Fallback: atom reason, non-map context
                    CodeBin = case Code of
                        missing_tenant_id -> ~"invalid_request";
                        policy_not_found -> ~"policy_not_found";
                        no_provider_available -> ~"decision_failed";
                        invalid_policy -> ~"invalid_request";
                        _ -> ~"internal"
                    end,
                    _ = Context,  %% Context may be used in future
                    {CodeBin, ~"Routing failed", #{}}
            end
    end,
    
    %% Build error object
    Error = #{
        ~"code" => ErrorCode,
        ~"message" => ErrorMessage
    },
    
    %% Add details if available
    ErrorWithDetails = case ErrorDetails of
        DetailsMap when is_map(DetailsMap), map_size(DetailsMap) > 0 ->
            maps:put(~"details", DetailsMap, Error);
        _ ->
            Error
    end,
    
    %% Build response context
    ResponseContext = case TraceId of
        undefined -> #{~"request_id" => RequestId};
        _ -> #{~"request_id" => RequestId, ~"trace_id" => TraceId}
    end,
    
    #{
        ~"ok" => false,
        ~"error" => ErrorWithDetails,
        ~"context" => ResponseContext
    }.

%% Internal: Check if error is extension-related
-spec is_extension_error(atom()) -> boolean().
is_extension_error(extension_not_found) -> true;
is_extension_error(extension_timeout) -> true;
is_extension_error(validator_blocked) -> true;
is_extension_error(post_processor_failed) -> true;
is_extension_error(extension_circuit_open) -> true;
is_extension_error(extension_invocation_error) -> true;
is_extension_error(extension_max_retries_exceeded) -> true;
is_extension_error(extension_registry_error) -> true;
is_extension_error(extension_load_balancer_error) -> true;
is_extension_error(pipeline_too_deep) -> true;
is_extension_error(too_many_pre_processors) -> true;
is_extension_error(too_many_validators) -> true;
is_extension_error(too_many_post_processors) -> true;
is_extension_error(_) -> false.

%% Internal: Send error response (helper)
-spec send_error_response(binary(), binary() | undefined, map(), binary(), binary()) -> ok.
send_error_response(Subject, ReplyTo, Request, ErrorCode, ErrorMessage) ->
    ErrorResponse = build_error_response(Request, ErrorCode, ErrorMessage),
    ErrorResponseJson = jsx:encode(ErrorResponse),
    ReplySubject = resolve_reply_subject(Subject, ReplyTo),
    router_nats:publish(ReplySubject, ErrorResponseJson).

resolve_reply_subject(_Subject, ReplyTo) when is_binary(ReplyTo) ->
    ReplyTo;
resolve_reply_subject(Subject, _ReplyTo) ->
    <<Subject/binary, ".reply">>.

%% Internal: Normalize boolean value
%% Handles: true, false, ~"true", ~"false", 1, 0, etc.
-spec normalize_boolean(term()) -> boolean().
normalize_boolean(true) -> true;
normalize_boolean(false) -> false;
normalize_boolean(~"true") -> true;
normalize_boolean(~"false") -> false;
normalize_boolean(1) -> true;
normalize_boolean(0) -> false;
normalize_boolean(_) -> false.  %% Default to false for unknown values

%% Internal: Log publication error with correlation context
-spec log_publication_error(pid(), term(), reference()) -> ok.
log_publication_error(Pid, Reason, MonitorRef) ->
    %% Log error for monitoring/debugging
    %% In production, use structured logging with correlation context
    case erlang:function_exported(router_logger, error, 2) of
        true ->
            %% Security: Sanitize reason before logging to prevent secret leakage
            SanitizedReason = sanitize_error_for_logging(Reason),
            router_logger:error(~"CAF assignment publication process crashed", #{
                pid => Pid,
                reason => SanitizedReason,
                monitor_ref => MonitorRef
            })
    end,
    ok.

%% Internal: Build log context with CP1 correlation fields
-spec build_log_context(map(), map()) -> map().
build_log_context(Request, BaseContext) ->
    %% Extract CP1 correlation fields (run_id, flow_id, step_id)
    RunId = maps:get(~"run_id", Request, undefined),
    FlowId = maps:get(~"flow_id", Request, undefined),
    StepId = maps:get(~"step_id", Request, undefined),
    
    %% Add correlation fields to context if present
    Context1 = case RunId of
        undefined -> BaseContext;
        _ -> maps:put(~"run_id", RunId, BaseContext)
    end,
    Context2 = case FlowId of
        undefined -> Context1;
        _ -> maps:put(~"flow_id", FlowId, Context1)
    end,
    Context3 = case StepId of
        undefined -> Context2;
        _ -> maps:put(~"step_id", StepId, Context2)
    end,
    Context3.

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
