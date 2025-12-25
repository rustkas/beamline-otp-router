-module(router_logger).

-doc "JSONL logger for Router".
%%
%% This module provides structured JSON logging compatible with
%% config/observability/logging.json format.
%%
%% Log Format:
%% {
%%   "timestamp": "ISO-8601",
%%   "level": "ERROR|WARN|INFO|DEBUG",
%%   "component": "router",
%%   "message": "Human-readable message",
%%   "context": {...},
%%   "tenant_id": "...",
%%   "run_id": "...",
%%   "flow_id": "...",
%%   "step_id": "...",
%%   "trace_id": "...",
%%   "error_code": "...",
%%   "latency_ms": 123
%% }
%%
%% PII Filtering:
%% - Fields: password, api_key, secret, token, access_token, refresh_token,
%%   authorization, credit_card, ssn, email, phone
%% - NATS headers: bearer, x-api-key, x-auth-token, x-authorization (case-insensitive)
%% - Replacement: "[REDACTED]"
%% - All context maps are filtered recursively before logging

-export([
    error/2,
    error/3,
    warn/2,
    warn/3,
    info/2,
    info/3,
    debug/2,
    debug/3,
    is_enabled/0,
    filter_pii/1,
    get_log_level/0,
    set_log_level/1,
    should_log/1,
    sanitize_error_for_logging/1
]).

-define(COMPONENT, ~"router").
-define(PII_FIELDS, [
    ~"password", ~"api_key", ~"secret", ~"token",
    ~"access_token", ~"refresh_token", ~"authorization",
    ~"credit_card", ~"ssn", ~"email", ~"phone",
    %% NATS headers that may contain secrets
    ~"bearer", ~"Bearer", ~"x-api-key", ~"X-Api-Key",
    ~"x-auth-token", ~"X-Auth-Token", ~"x-authorization", ~"X-Authorization"
]).

-spec is_enabled() -> boolean().
is_enabled() ->
    case application:get_env(beamline_router, telemetry_enabled, true) of
        false -> false;
        _ -> true
    end.

-spec get_log_level() -> atom().
get_log_level() ->
    case application:get_env(beamline_router, log_level, info) of
        Level when Level =:= error; Level =:= warn; Level =:= info; Level =:= debug ->
            Level;
        _ ->
            info
    end.

-spec set_log_level(atom()) -> ok.
set_log_level(Level) when Level =:= error; Level =:= warn; Level =:= info; Level =:= debug ->
    application:set_env(beamline_router, log_level, Level),
    ok;
set_log_level(_) ->
    ok.

-spec should_log(atom()) -> boolean().
should_log(Level) when Level =:= error; Level =:= warn; Level =:= info; Level =:= debug ->
    case is_enabled() of
        false ->
            false;
        true ->
            CurrentLevel = get_log_level(),
            level_priority(Level) >= level_priority(CurrentLevel)
    end;
should_log(_) ->
    false.

%% Internal: Get level priority (higher number = more important)
-spec level_priority(atom()) -> integer().
level_priority(error) -> 4;
level_priority(warn) -> 3;
level_priority(info) -> 2;
level_priority(debug) -> 1;
level_priority(_) -> 0.

-spec error(Message :: binary() | string(), Context :: map()) -> ok.
error(Message, Context) ->
    log(error, Message, Context).

-spec error(Message :: binary() | string(), Context :: map(), Metadata :: map()) -> ok.
error(Message, Context, Metadata) ->
    log(error, Message, maps:merge(Context, Metadata)).

-spec warn(Message :: binary() | string(), Context :: map()) -> ok.
warn(Message, Context) ->
    log(warn, Message, Context).

-spec warn(Message :: binary() | string(), Context :: map(), Metadata :: map()) -> ok.
warn(Message, Context, Metadata) ->
    log(warn, Message, maps:merge(Context, Metadata)).

-spec info(Message :: binary() | string(), Context :: map()) -> ok.
info(Message, Context) ->
    log(info, Message, Context).

-spec info(Message :: binary() | string(), Context :: map(), Metadata :: map()) -> ok.
info(Message, Context, Metadata) ->
    log(info, Message, maps:merge(Context, Metadata)).

-spec debug(Message :: binary() | string(), Context :: map()) -> ok.
debug(Message, Context) ->
    log(debug, Message, Context).

-spec debug(Message :: binary() | string(), Context :: map(), Metadata :: map()) -> ok.
debug(Message, Context, Metadata) ->
    log(debug, Message, maps:merge(Context, Metadata)).

-spec log(Level :: atom(), Message :: binary() | string(), Context :: map()) -> ok.
log(Level, Message, Context) when is_map(Context) ->
    case should_log(Level) of
        true ->
            %% Build log entry
            LogEntry = build_log_entry(Level, Message, Context),
            
            %% Convert to JSON
            Json = jsx:encode(LogEntry),
            
            %% Write to file (JSONL format - one JSON object per line)
            write_jsonl(Json);
        false ->
            ok
    end;
log(_Level, _Message, _Context) ->
    ok.

-spec build_log_entry(Level :: atom(), Message :: binary() | string(), Context :: map()) -> map().
build_log_entry(Level, Message, Context) ->
    %% Get timestamp (ISO-8601 format)
    Timestamp = get_timestamp(),
    
    %% Extract CP1 fields from context
    TraceId = case extract_field(Context, ~"trace_id", trace_id) of
        undefined ->
            %% Try to get trace_id from router_tracing if available
            case erlang:function_exported(router_tracing, get_trace_id, 0) of
                true ->
                    try router_tracing:get_trace_id() catch
                        Error:Reason ->
                            router_logger:debug(~"Failed to get trace ID from router_tracing", #{
                                ~"error" => Error,
                                ~"reason" => sanitize_error_for_logging(Reason),
                                ~"event" => ~"trace_id_get_failed"
                            }),
                            undefined
                    end;
                false ->
                    undefined
            end;
        TId ->
            TId
    end,
    RunId = extract_field(Context, ~"run_id", run_id),
    FlowId = extract_field(Context, ~"flow_id", flow_id),
    StepId = extract_field(Context, ~"step_id", step_id),
    TenantId = extract_field(Context, ~"tenant_id", tenant_id),
    
    %% Extract error_code and latency
    ErrorCode = extract_field(Context, ~"error_code", error_code),
    LatencyMs = extract_field(Context, ~"latency_ms", latency_ms),
    
    %% Filter PII from context
    FilteredContext = filter_pii(Context),
    
    %% Build base entry
    BaseEntry = #{
        ~"timestamp" => Timestamp,
        ~"level" => level_to_binary(Level),
        ~"component" => ?COMPONENT,
        ~"message" => ensure_binary(Message)
    },
    
    %% Add CP1 fields at top level (when available)
    Entry1 = add_cp1_field(BaseEntry, ~"tenant_id", TenantId),
    Entry2 = add_cp1_field(Entry1, ~"run_id", RunId),
    Entry3 = add_cp1_field(Entry2, ~"flow_id", FlowId),
    Entry4 = add_cp1_field(Entry3, ~"step_id", StepId),
    Entry5 = add_cp1_field(Entry4, ~"trace_id", TraceId),
    
    %% Add context if present
    Entry6 = case maps:size(FilteredContext) > 0 of
        true -> maps:put(~"context", FilteredContext, Entry5);
        false -> Entry5
    end,
    
    %% Add error_code if present
    Entry7 = case ErrorCode of
        undefined -> Entry6;
        _ -> maps:put(~"error_code", ensure_binary(ErrorCode), Entry6)
    end,
    
    %% Add latency_ms if present
    Entry8 = case LatencyMs of
        undefined -> Entry7;
        _ when is_integer(LatencyMs) -> maps:put(~"latency_ms", LatencyMs, Entry7);
        _ when is_float(LatencyMs) -> maps:put(~"latency_ms", round(LatencyMs), Entry7);
        _ -> Entry7
    end,
    
    Entry8.

-spec add_cp1_field(Entry :: map(), Key :: binary(), Value :: term()) -> map().
add_cp1_field(Entry, Key, Value) ->
    case Value of
        undefined -> Entry;
        _ -> maps:put(Key, ensure_binary(Value), Entry)
    end.

-spec extract_field(Context :: map(), BinaryKey :: binary(), AtomKey :: atom()) -> term() | undefined.
extract_field(Context, BinaryKey, AtomKey) ->
    case maps:get(BinaryKey, Context, undefined) of
        undefined -> maps:get(AtomKey, Context, undefined);
        Value -> Value
    end.

-spec get_timestamp() -> binary().
get_timestamp() ->
    %% Use system time (microseconds since epoch)
    SystemTimeUs = erlang:system_time(microsecond),
    
    %% Convert to ISO-8601 format
    DateTime = calendar:system_time_to_universal_time(SystemTimeUs, microsecond),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    Us = SystemTimeUs rem 1000000,
    
    %% Format: YYYY-MM-DDTHH:MM:SS.ssssssZ (6 digits for microseconds)
    Timestamp = io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0BZ",
        [Year, Month, Day, Hour, Min, Sec, Us]),
    iolist_to_binary(Timestamp).

%% Recursively filters PII fields from context, including nested maps
%% Case-insensitive matching for header-like fields (e.g., "Authorization" vs "authorization")
-spec filter_pii(Context :: map()) -> map().
filter_pii(Context) when is_map(Context) ->
    maps:fold(fun
        (Key, Value, Acc) ->
            KeyBin = ensure_binary(Key),
            KeyLower = binary_to_lowercase(KeyBin),
            %% Check exact match and case-insensitive match for header-like fields
            IsPII = lists:member(KeyBin, ?PII_FIELDS) orelse
                    lists:any(fun(PIIField) ->
                        binary_to_lowercase(PIIField) =:= KeyLower
                    end, ?PII_FIELDS),
            case IsPII of
                true ->
                    %% Mask PII field
                    maps:put(KeyBin, ~"[REDACTED]", Acc);
                false ->
                    %% Recursively filter nested maps
                    case is_map(Value) of
                        true ->
                            maps:put(KeyBin, filter_pii(Value), Acc);
                        false ->
                            %% Also check if value itself contains secret patterns (for error messages)
                            case is_binary(Value) orelse is_list(Value) of
                                true ->
                                    ValueBin = ensure_binary(Value),
                                    %% Check for common secret patterns in value
                                    case re:run(ValueBin, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
                                        match ->
                                            maps:put(KeyBin, ~"[REDACTED]", Acc);
                                        nomatch ->
                                            maps:put(KeyBin, Value, Acc)
                                    end;
                                false ->
                                    maps:put(KeyBin, Value, Acc)
                            end
                    end
            end
    end, #{}, Context);
filter_pii(_) ->
    #{}.

%% Internal: Convert binary to lowercase (case-insensitive matching)
-spec binary_to_lowercase(binary()) -> binary().
binary_to_lowercase(Bin) when is_binary(Bin) ->
    << <<(case C of
        C when C >= $A, C =< $Z -> C + 32;
        C -> C
    end)>> || <<C>> <= Bin >>;
binary_to_lowercase(Other) ->
    ensure_binary(Other).

-spec level_to_binary(Level :: atom()) -> binary().
level_to_binary(error) -> ~"ERROR";
level_to_binary(warn) -> ~"WARN";
level_to_binary(info) -> ~"INFO";
level_to_binary(debug) -> ~"DEBUG";
level_to_binary(_) -> ~"INFO".

-spec write_jsonl(Json :: binary()) -> ok.
write_jsonl(Json) ->
    %% Check if console logging is enabled (e.g. for containers)
    case application:get_env(beamline_router, log_to_console, false) of
        true ->
            io:format(user, "~s~n", [Json]);
        false ->
            %% Try to use async writer if available
            case whereis(router_log_writer) of
                undefined ->
                    %% Fallback to sync write
                    write_jsonl_sync(Json);
                Pid when is_pid(Pid) ->
                    router_log_writer:log(Json)
            end
    end.

-spec write_jsonl_sync(Json :: binary()) -> ok.
write_jsonl_sync(Json) ->
    %% Get log file path from environment or use default
    LogDir = case application:get_env(beamline_router, log_dir, undefined) of
        undefined -> ".windsurf/reports";
        Dir -> Dir
    end,
    
    %% Ensure directory exists
    ok = filelib:ensure_dir(filename:join(LogDir, "dummy")),
    
    %% Log file: router_YYYY-MM-DD.jsonl
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    %% Append JSON line to file
    case file:write_file(LogFile, [Json, ~"\n"], [append, raw]) of
        ok -> ok;
        {error, _Reason} ->
            %% Fallback to stderr if file write fails
            io:format(standard_error, "~s~n", [Json]),
            ok
    end.

-spec date_string() -> string().
date_string() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]).

-spec ensure_binary(term()) -> binary().
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) ->
    %% Check if it's a valid iolist (string-like) vs a complex list (stack trace, etc.)
    case io_lib:printable_unicode_list(L) of
        true ->
            try
                iolist_to_binary(L)
            catch
                _:_ -> iolist_to_binary(io_lib:format("~p", [L]))
            end;
        false ->
            iolist_to_binary(io_lib:format("~p", [L]))
    end;
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(I) when is_integer(I) -> integer_to_binary(I);
ensure_binary(F) when is_float(F) -> float_to_binary(F);
ensure_binary(T) -> iolist_to_binary(io_lib:format("~p", [T])).

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
