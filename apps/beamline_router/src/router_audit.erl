-module(router_audit).

-doc "Audit Logging Module".
%% Provides audit trail for all policy and RBAC operations
%% CP1-ROUTER: Audit logging for Policy Enforcement

-export([log_policy_action/5, log_rbac_action/5, log_config_action/4]).
-export([log_decision/1]).
-export([get_audit_entries/3, get_client_ip/0]).
-export([get_audit_retention_days/0, clear_all_audit_entries/0]).
-export([get_table_size/0, get_table_memory/0, check_size_limit/0]).
-export([enforce_size_limit_if_needed/0, evict_oldest_entries/1, evict_expired_entries/1, cleanup/0, start_cleanup_timer/0]).

%% GDPR compliance: Default 90 days retention for audit logs
-spec get_audit_retention_days() -> integer().
get_audit_retention_days() ->
    application:get_env(beamline_router, audit_retention_days, 90).

%% ETS table for audit logs
-define(AUDIT_TABLE, audit_logs).

%% Audit entry record
-record(audit_entry, {
    ts :: integer(),
    tenant_id :: binary(),
    user_id :: binary(),
    action :: binary(),
    resource_type :: binary(),
    resource_id :: binary() | undefined,
    details :: map(),
    ip_address :: binary() | undefined,
    trace_id :: binary() | undefined
}).

-spec log_policy_action(binary(), binary(), binary(), binary() | string() | undefined, map()) -> ok.
log_policy_action(TenantId, UserId, Action, PolicyId0, Details) ->
    PolicyId = case PolicyId0 of
        undefined -> undefined;
        Bin when is_binary(Bin) -> Bin;
        Str when is_list(Str) -> iolist_to_binary(Str)
    end,
    Entry = #audit_entry{
        ts = erlang:system_time(millisecond),
        tenant_id = TenantId,
        user_id = UserId,
        action = Action,
        resource_type = ~"policy",
        resource_id = PolicyId,
        details = Details,
        ip_address = get_client_ip(),
        trace_id = maps:get(~"trace_id", Details, maps:get(trace_id, Details, undefined))
    },
    store_audit_entry(Entry).

-spec log_rbac_action(binary(), binary(), binary(), binary(), map()) -> ok.
log_rbac_action(TenantId, UserId, Action, ResourceId, Details) ->
    Entry = #audit_entry{
        ts = erlang:system_time(millisecond),
        tenant_id = TenantId,
        user_id = UserId,
        action = Action,
        resource_type = ~"rbac",
        resource_id = ResourceId,
        details = Details,
        ip_address = get_client_ip(),
        trace_id = maps:get(~"trace_id", Details, maps:get(trace_id, Details, undefined))
    },
    store_audit_entry(Entry).

-spec log_config_action(binary(), binary(), binary(), map()) -> ok.
log_config_action(TenantId, UserId, Action, Details) ->
    Entry = #audit_entry{
        ts = erlang:system_time(millisecond),
        tenant_id = TenantId,
        user_id = UserId,
        action = Action,
        resource_type = ~"config",
        resource_id = undefined,
        details = Details,
        ip_address = get_client_ip(),
        trace_id = maps:get(~"trace_id", Details, maps:get(trace_id, Details, undefined))
    },
    store_audit_entry(Entry).

%% Explanation format according to ROUTING_POLICY.md "Decision Explanation Format" section
%% Required fields: reason, provider_id, policy_id, policy_version, priority, steps, context
%% Logs in strict JSON format with PII filtering
%% 
%% Specification: docs/ROUTING_POLICY.md#decision-explanation-format
%% - reason: "sticky" | "weighted" | "fallback" | "retry"
%% - provider_id: selected provider identifier
%% - policy_id: policy identifier used
%% - policy_version: policy version (e.g., "1.0")
%% - priority: decision priority (25, 50, 100)
%% - steps: array of step-by-step explanation strings
%% - context: context object with tenant_id (required) and optional fields
-spec log_decision(map()) -> ok.
log_decision(Explanation) when is_map(Explanation) ->
    %% Extract required fields according to specification
    Context = maps:get(context, Explanation, #{}),
    TenantId = maps:get(~"tenant_id", Context, maps:get(tenant_id, Explanation, ~"unknown")),
    PolicyId = maps:get(policy_id, Explanation, ~"unknown"),
    ProviderId = maps:get(provider_id, Explanation, ~"unknown"),
    Reason = maps:get(reason, Explanation, ~"unknown"),
    PolicyVersion = maps:get(policy_version, Explanation, ~"1.0"),
    Priority = maps:get(priority, Explanation, 50),
    Steps = maps:get(steps, Explanation, []),
    
    %% Validate required fields (according to specification)
    %% reason must be one of: "sticky", "weighted", "fallback", "retry"
    ValidReason = case Reason of
        ~"sticky" -> ~"sticky";
        ~"weighted" -> ~"weighted";
        ~"fallback" -> ~"fallback";
        ~"retry" -> ~"retry";
        _ -> ~"unknown"
    end,
    
    %% priority must be one of: 25, 50, 100
    ValidPriority = case Priority of
        P when P =:= 25; P =:= 50; P =:= 100 -> P;
        _ -> 50  %% Default to 50 if invalid
    end,
    
    %% steps must be array (list in Erlang)
    ValidSteps = case Steps of
        StepsList when is_list(StepsList) -> StepsList;
        _ -> []
    end,
    
    %% Filter PII from context before logging
    FilteredContext = router_logger:filter_pii(Context),
    
    %% Normalize all values to ensure JSON serialization works
    NormalizedProviderId = ensure_binary(ProviderId),
    NormalizedPolicyId = ensure_binary(PolicyId),
    NormalizedPolicyVersion = ensure_binary(PolicyVersion),
    NormalizedTenantId = ensure_binary(TenantId),
    NormalizedSteps = normalize_steps(ValidSteps),
    
    %% Build structured log entry with all required fields (according to specification)
    %% Format: strict JSON, no sensitive data leakage
    LogContext = #{
        ~"provider_id" => NormalizedProviderId,
        ~"mechanism" => ValidReason,  %% sticky | weighted | fallback | retry
        ~"policy_id" => NormalizedPolicyId,
        ~"policy_version" => NormalizedPolicyVersion,
        ~"tenant_id" => NormalizedTenantId,
        ~"priority" => ValidPriority,
        ~"steps" => NormalizedSteps,
        ~"context" => FilteredContext
    },
    
    %% Log via router_logger for structured JSON logging with PII filtering
    router_logger:info(~"Routing decision", LogContext),
    
    %% Also create audit entry for audit trail (uses log_policy_action)
    Details = #{
        ~"provider_id" => NormalizedProviderId,
        ~"reason" => ValidReason,
        ~"mechanism" => ValidReason,  %% Explicit mechanism field
        ~"policy_version" => NormalizedPolicyVersion,
        ~"priority" => ValidPriority,
        ~"steps" => NormalizedSteps,
        ~"context" => FilteredContext
    },
    
    %% Use log_policy_action with "route" action for audit trail
    log_policy_action(NormalizedTenantId, ~"system", ~"route", NormalizedPolicyId, Details);
log_decision(_) ->
    %% Invalid explanation format, skip logging
    ok.

-spec get_audit_entries(binary(), integer(), integer()) -> [map()].
get_audit_entries(TenantId, Limit, Offset) ->
    try
        %% Ensure audit table exists
        ensure_audit_table(),
        
        %% Collect all entries for tenant
        Entries = ets:foldl(
            fun({_Key, Entry}, Acc) ->
                case Entry#audit_entry.tenant_id =:= TenantId of
                    true -> [Entry | Acc];
                    false -> Acc
                end
            end,
            [],
            ?AUDIT_TABLE
        ),
        
        %% Sort by timestamp (descending) and paginate
        SortedEntries = lists:sort(fun(E1, E2) -> E1#audit_entry.ts > E2#audit_entry.ts end, Entries),
        PaginatedEntries = lists:sublist(SortedEntries, Offset + 1, Limit),
        
        %% Convert to maps
        [audit_entry_to_map(Entry) || Entry <- PaginatedEntries]
    catch
        error:{badarg, _} ->
            %% Table not accessible - return empty list
            router_logger:error(~"Failed to get audit entries: table not accessible", #{
                ~"tenant_id" => TenantId
            }),
            [];
        Class:Reason ->
            %% Other errors - return empty list
            router_logger:error(~"Failed to get audit entries", #{
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            []
    end.

-spec get_client_ip() -> binary() | undefined.
get_client_ip() ->
    case erlang:get(client_ip) of
        undefined ->
            undefined;
        IP when is_binary(IP) ->
            IP;
        IP when is_list(IP) ->
            list_to_binary(IP);
        _ ->
            undefined
    end.

%% Internal functions

%% Ensure value is binary (for JSON serialization)
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(I) when is_integer(I) -> integer_to_binary(I);
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(_) -> ~"unknown".

%% Normalize steps list to ensure all elements are binary
normalize_steps(Steps) when is_list(Steps) ->
    lists:map(fun(Step) ->
        case Step of
            S when is_binary(S) -> S;
            S when is_list(S) -> list_to_binary(S);
            S when is_atom(S) -> atom_to_binary(S, utf8);
            S when is_integer(S) -> integer_to_binary(S);
            _ -> ~"unknown"
        end
    end, Steps);
normalize_steps(_) -> [].

%% Normalize details map for JSON serialization
normalize_details(Details) when is_map(Details) ->
    maps:fold(fun(K, V, Acc) ->
        KeyBin = ensure_binary(K),
        ValueBin = case V of
            VM when is_map(VM) -> normalize_details(VM);
            VL when is_list(VL) -> 
                case lists:all(fun(E) -> is_binary(E) orelse is_integer(E) orelse is_float(E) end, VL) of
                    true -> VL;
                    false -> [ensure_binary(E) || E <- VL]
                end;
            _ -> ensure_binary(V)
        end,
        maps:put(KeyBin, ValueBin, Acc)
    end, #{}, Details);
normalize_details(_) -> #{}.

%% Store audit entry
store_audit_entry(Entry) ->
    ensure_audit_table(),
    
    try
        %% Insert with timestamp as key for efficient range queries
        Key = {Entry#audit_entry.tenant_id, Entry#audit_entry.ts, erlang:make_ref()},
        ets:insert(?AUDIT_TABLE, {Key, Entry}),
        
        %% Emit telemetry event
        router_telemetry_helper:execute([router_audit, action], #{count => 1}, #{
            tenant_id => Entry#audit_entry.tenant_id,
            user_id => Entry#audit_entry.user_id,
            action => Entry#audit_entry.action,
            resource_type => Entry#audit_entry.resource_type,
            trace_id => Entry#audit_entry.trace_id
        }),
        
        %% Log to structured logger (normalize all values for JSON serialization)
        router_logger:info(~"Audit log entry", #{
            ~"tenant_id" => ensure_binary(Entry#audit_entry.tenant_id),
            ~"user_id" => ensure_binary(Entry#audit_entry.user_id),
            ~"action" => ensure_binary(Entry#audit_entry.action),
            ~"resource_type" => ensure_binary(Entry#audit_entry.resource_type),
            ~"resource_id" => ensure_binary(Entry#audit_entry.resource_id),
            ~"ip_address" => ensure_binary(Entry#audit_entry.ip_address),
            ~"trace_id" => ensure_binary(Entry#audit_entry.trace_id),
            ~"details" => normalize_details(Entry#audit_entry.details)
        }),
        
        ok
    catch
        error:{badarg, _} ->
            %% Table not accessible - log error but don't fail (normalize values)
            router_logger:error(~"Failed to store audit entry: table not accessible", #{
                ~"tenant_id" => ensure_binary(Entry#audit_entry.tenant_id),
                ~"user_id" => ensure_binary(Entry#audit_entry.user_id),
                ~"action" => ensure_binary(Entry#audit_entry.action)
            }),
            ok;
        Class:Reason ->
            %% Other errors - log and continue (normalize error for JSON serialization)
            ErrorStr = iolist_to_binary(io_lib:format("~p:~p", [Class, Reason])),
            router_logger:error(~"Failed to store audit entry", #{
                ~"error" => ErrorStr,
                ~"tenant_id" => ensure_binary(Entry#audit_entry.tenant_id)
            }),
            ok
    end.

%% @returns ok
-spec clear_all_audit_entries() -> ok.
clear_all_audit_entries() ->
    try
        ensure_audit_table(),
        ets:delete_all_objects(?AUDIT_TABLE),
        ok
    catch
        error:{badarg, _} ->
            %% Table not accessible - log but don't fail
            router_logger:error(~"Failed to clear audit entries: table not accessible", #{}),
            ok;
        Class:Reason ->
            %% Other errors - log but don't fail
            router_logger:error(~"Failed to clear audit entries", #{
                ~"error" => {Class, Reason}
            }),
            ok
    end.

%% Ensure audit table exists
ensure_audit_table() ->
    try
        case ets:info(?AUDIT_TABLE, name) of
            undefined ->
                ets:new(?AUDIT_TABLE, [
                    ordered_set,
                    named_table,
                    {keypos, 1},
                    public,
                    {read_concurrency, true}
                ]);
            _ ->
                ok
        end
    catch
        error:{badarg, _} ->
            %% Table name conflict - try to use existing table
            case ets:whereis(?AUDIT_TABLE) of
                undefined ->
                    %% Table doesn't exist, try to create with different approach
                    try ets:new(?AUDIT_TABLE, [
                        ordered_set,
                        named_table,
                        {keypos, 1},
                        public,
                        {read_concurrency, true}
                    ]) of
                        _ -> ok
                    catch
                        _:_ -> ok
                    end;
                _ ->
                    ok
            end;
        _:_ ->
            ok
    end.

%% Convert audit entry to map
audit_entry_to_map(#audit_entry{
    ts = Ts,
    tenant_id = TenantId,
    user_id = UserId,
    action = Action,
    resource_type = ResourceType,
    resource_id = ResourceId,
    details = Details,
    ip_address = IpAddress,
    trace_id = TraceId
}) ->
    Map = #{
        ~"ts" => Ts,
        ~"tenant_id" => TenantId,
        ~"user_id" => UserId,
        ~"action" => Action,
        ~"resource_type" => ResourceType,
        ~"details" => Details
    },
    
    Map1 = case ResourceId of
        undefined -> Map;
        _ -> maps:put(~"resource_id", ResourceId, Map)
    end,
    
    Map2 = case IpAddress of
        undefined -> Map1;
        _ -> maps:put(~"ip_address", IpAddress, Map1)
    end,
    
    case TraceId of
        undefined -> Map2;
        _ -> maps:put(~"trace_id", TraceId, Map2)
    end.

-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(?AUDIT_TABLE, size) of
        undefined -> undefined;
        Size -> Size
    end.

-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(?AUDIT_TABLE, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

-spec check_size_limit() ->
    {ok, integer()}
    | {error, exceeded, integer(), integer()}
    | {error, no_limit_configured}
    | {error, invalid_limit}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, audit_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(?AUDIT_TABLE, Limit);
        _ -> {error, invalid_limit}
    end.

%% Called during periodic cleanup
-spec enforce_size_limit_if_needed() -> ok.
enforce_size_limit_if_needed() ->
    MaxSize = application:get_env(beamline_router, audit_max_size, undefined),
    RetentionDays = get_audit_retention_days(),
    case MaxSize of
        undefined ->
            %% If no size limit, use retention-based cleanup
            _ = evict_expired_entries(RetentionDays),
            ok;
        Limit when is_integer(Limit), Limit > 0 ->
            %% First evict expired entries
            _ = evict_expired_entries(RetentionDays),
            %% Then check size limit
            case router_resource_monitor:check_table_size_limit(?AUDIT_TABLE, Limit) of
                {ok, _Size} -> ok;
                {error, exceeded, CurrentSize, Limit} ->
                    CountToEvict = CurrentSize - Limit,
                    EvictedCount = evict_oldest_entries(CountToEvict),
                    case EvictedCount > 0 of
                        true ->
                            router_logger:warn(~"Audit table size limit enforced", #{
                                ~"current_size" => CurrentSize,
                                ~"limit" => Limit,
                                ~"evicted" => EvictedCount
                            });
                        false ->
                            ok
                    end
            end;
        _ -> ok
    end.

-spec evict_oldest_entries(integer()) -> integer().
evict_oldest_entries(Count) when Count =< 0 -> 0;
evict_oldest_entries(Count) ->
    %% Get all entries sorted by timestamp (oldest first)
    AllEntries = ets:foldl(fun({Key, Entry}, Acc) -> [{Key, Entry} | Acc] end, [], ?AUDIT_TABLE),
    SortedEntries = lists:sort(fun({_K1, E1}, {_K2, E2}) ->
        E1#audit_entry.ts =< E2#audit_entry.ts
    end, AllEntries),
    
    %% Evict oldest N entries
    EntriesToEvict = lists:sublist(SortedEntries, Count),
    lists:foreach(fun({Key, _Entry}) ->
        ets:delete(?AUDIT_TABLE, Key)
    end, EntriesToEvict),
    length(EntriesToEvict).

-spec evict_expired_entries(integer()) -> integer().
evict_expired_entries(RetentionDays) ->
    RetentionMs = RetentionDays * 24 * 60 * 60 * 1000,
    Now = erlang:system_time(millisecond),
    CutoffTime = Now - RetentionMs,
    
    %% Match entries older than cutoff time
    Keys = ets:foldl(
        fun({Key, Entry}, Acc) ->
            case Entry#audit_entry.ts < CutoffTime of
                true -> [Key | Acc];
                false -> Acc
            end
        end,
        [],
        ?AUDIT_TABLE
    ),
    [ets:delete(?AUDIT_TABLE, Key) || Key <- Keys],
    length(Keys).

%% Protects ETS from unbounded growth
-spec cleanup() -> ok.
cleanup() ->
    %% Get table size before cleanup
    SizeBefore = case get_table_size() of
        undefined -> 0;
        Size -> Size
    end,
    
    %% Enforce size limit and retention policy
    enforce_size_limit_if_needed(),
    
    %% Get table size after cleanup
    SizeAfter = case get_table_size() of
        undefined -> 0;
        SizeAfterValue -> SizeAfterValue
    end,
    
    %% Emit metrics for table size monitoring
    telemetry:execute([router, audit, table_size], #{
        size => SizeAfter
    }, #{
        size_before => SizeBefore
    }),
    
    ok.

-spec start_cleanup_timer() -> ok.
start_cleanup_timer() ->
    CleanupInterval = application:get_env(beamline_router, audit_cleanup_interval_seconds, 3600),
    case timer:apply_interval(CleanupInterval * 1000, ?MODULE, cleanup, []) of
        {ok, _TimerRef} -> ok;
        {error, Reason} ->
            router_logger:error(~"Failed to start audit cleanup timer", #{
                ~"error" => Reason
            }),
            ok
    end.
