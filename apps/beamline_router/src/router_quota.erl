-module(router_quota).

-doc "Quota Management Module".
%% Provides quota checking and enforcement for policies
%% CP1-ROUTER: Quota management for Policy Enforcement

-export([check_policy_quota/1, check_policy_quota/2, check_rule_quota/2, check_provider_quota/2]).
-export([get_tenant_quota/1, set_tenant_quota/2, get_quota_usage/1]).
-export([get_default_quota/0, clear_all_quotas/0]).
-export([get_table_size/0, get_table_memory/0, check_size_limit/0]).

-ignore_xref([
  {router_quota, get_default_quota, 0},
  {router_quota, check_policy_quota, 1},
  {router_quota, check_policy_quota, 2},
  {router_quota, check_rule_quota, 2},
  {router_quota, check_provider_quota, 2},
  {router_quota, get_quota_usage, 1},
  {router_quota, get_tenant_quota, 1},
  {router_quota, set_tenant_quota, 2}
]).

-include("beamline_router.hrl").

%% Suppress warnings for internal functions that may be used in future or via timer

%% Get default quota from configuration
get_default_quota() ->
    DefaultQuota = application:get_env(beamline_router, default_quota, #{
        max_policies => 10,
        max_rules_per_policy => 50,
        max_providers_per_policy => 20
    }),
    #quota{
        tenant_id = ~"default",
        max_policies = maps:get(max_policies, DefaultQuota, 10),
        max_rules_per_policy = maps:get(max_rules_per_policy, DefaultQuota, 50),
        max_providers_per_policy = maps:get(max_providers_per_policy, DefaultQuota, 20)
    }.

%% Default quota limits
-define(DEFAULT_MAX_POLICIES, 10).
-define(DEFAULT_MAX_RULES_PER_POLICY, 50).
-define(DEFAULT_MAX_PROVIDERS_PER_POLICY, 20).

%% ETS table for quotas
-define(QUOTA_TABLE, tenant_quotas).

%% @param TenantId Tenant identifier (binary)
%% @param PolicyId Optional policy identifier (for future use, currently ignored)
%% @returns {ok, Remaining} | {error, quota_exceeded, MaxPolicies}
-spec check_policy_quota(binary()) -> {ok, integer()} | {error, quota_exceeded, integer()}.
-spec check_policy_quota(binary(), binary()) -> {ok, integer()} | {error, quota_exceeded, integer()}.
check_policy_quota(TenantId) ->
    check_policy_quota(TenantId, undefined).

check_policy_quota(TenantId, _PolicyId) ->
    Quota = get_tenant_quota(TenantId),
    CurrentCount = count_policies(TenantId),
    
    case CurrentCount < Quota#quota.max_policies of
        true ->
            Remaining = Quota#quota.max_policies - CurrentCount,
            {ok, Remaining};
        false ->
            {error, quota_exceeded, Quota#quota.max_policies}
    end.

-spec check_rule_quota(binary(), integer()) -> {ok, integer()} | {error, quota_exceeded, integer()}.
check_rule_quota(TenantId, RuleCount) ->
    Quota = get_tenant_quota(TenantId),
    
    case RuleCount < Quota#quota.max_rules_per_policy of
        true ->
            Remaining = Quota#quota.max_rules_per_policy - RuleCount,
            {ok, Remaining};
        false ->
            {error, quota_exceeded, Quota#quota.max_rules_per_policy}
    end.

-spec check_provider_quota(binary(), integer()) -> {ok, integer()} | {error, quota_exceeded, integer()}.
check_provider_quota(TenantId, ProviderCount) ->
    Quota = get_tenant_quota(TenantId),
    
    case ProviderCount < Quota#quota.max_providers_per_policy of
        true ->
            Remaining = Quota#quota.max_providers_per_policy - ProviderCount,
            {ok, Remaining};
        false ->
            {error, quota_exceeded, Quota#quota.max_providers_per_policy}
    end.

-spec get_tenant_quota(binary()) -> #quota{}.
get_tenant_quota(TenantId) ->
    ensure_quota_table(),
    
    case ets:lookup(?QUOTA_TABLE, TenantId) of
        [{TenantId, Quota}] ->
            Quota;
        [] ->
            %% Return default quota
            #quota{
                tenant_id = TenantId,
                max_policies = ?DEFAULT_MAX_POLICIES,
                max_rules_per_policy = ?DEFAULT_MAX_RULES_PER_POLICY,
                max_providers_per_policy = ?DEFAULT_MAX_PROVIDERS_PER_POLICY
            }
    end.

-spec set_tenant_quota(binary(), #quota{}) -> ok.
set_tenant_quota(TenantId, Quota) ->
    try
        ensure_quota_table(),
        ets:insert(?QUOTA_TABLE, {TenantId, Quota#quota{tenant_id = TenantId}}),
        ok
    catch
        error:{badarg, _} ->
            %% Table not accessible - log but don't fail
            router_logger:error(~"Failed to set tenant quota: table not accessible", #{
                ~"tenant_id" => TenantId
            }),
            ok;
        Class:Reason ->
            %% Other errors - log but don't fail
            router_logger:error(~"Failed to set tenant quota", #{
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            ok
    end.

-spec get_quota_usage(binary()) -> map().
get_quota_usage(TenantId) ->
    try
        Quota = get_tenant_quota(TenantId),
        CurrentPolicies = count_policies(TenantId),
        
        #{
            ~"tenant_id" => TenantId,
            ~"policies" => #{
                ~"current" => CurrentPolicies,
                ~"max" => Quota#quota.max_policies,
                ~"remaining" => max(0, Quota#quota.max_policies - CurrentPolicies)
            },
            ~"rules_per_policy" => #{
                ~"max" => Quota#quota.max_rules_per_policy
            },
            ~"providers_per_policy" => #{
                ~"max" => Quota#quota.max_providers_per_policy
            }
        }
    catch
        Class:Reason ->
            %% Return default quota usage on error
            router_logger:error(~"Failed to get quota usage", #{
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            DefaultQuota = get_default_quota(),
            #{
                ~"tenant_id" => TenantId,
                ~"policies" => #{
                    ~"current" => 0,
                    ~"max" => DefaultQuota#quota.max_policies,
                    ~"remaining" => DefaultQuota#quota.max_policies
                },
                ~"rules_per_policy" => #{
                    ~"max" => DefaultQuota#quota.max_rules_per_policy
                },
                ~"providers_per_policy" => #{
                    ~"max" => DefaultQuota#quota.max_providers_per_policy
                }
            }
    end.

%% Internal functions

%% Count policies for tenant
count_policies(TenantId) ->
    try
        case router_policy_store:list_policies(TenantId) of
            {ok, Policies} ->
                length(Policies);
            {error, _} ->
                0
        end
    catch
        _:_ ->
            %% Return 0 on any error (e.g., router_policy_store not available)
            0
    end.

%% @returns ok
-spec clear_all_quotas() -> ok.
clear_all_quotas() ->
    try
        ensure_quota_table(),
        ets:delete_all_objects(?QUOTA_TABLE),
        ok
    catch
        error:{badarg, _} ->
            %% Table not accessible - log but don't fail
            router_logger:error(~"Failed to clear quotas: table not accessible", #{}),
            ok;
        Class:Reason ->
            %% Other errors - log but don't fail
            router_logger:error(~"Failed to clear quotas", #{
                ~"error" => {Class, Reason}
            }),
            ok
    end.

%% Ensure quota table exists
ensure_quota_table() ->
    case ets:info(?QUOTA_TABLE, name) of
        undefined ->
            ets:new(?QUOTA_TABLE, [
                set,
                named_table,
                public,
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(?QUOTA_TABLE, size) of
        undefined -> undefined;
        Size -> Size
    end.

-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(?QUOTA_TABLE, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, quota_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(?QUOTA_TABLE, Limit);
        _ -> {error, invalid_limit}
    end.

%% Called during periodic cleanup
-spec enforce_size_limit_if_needed() -> ok.
enforce_size_limit_if_needed() ->
    MaxSize = application:get_env(beamline_router, quota_max_size, undefined),
    case MaxSize of
        undefined -> ok;
        Limit when is_integer(Limit), Limit > 0 ->
            case router_resource_monitor:check_table_size_limit(?QUOTA_TABLE, Limit) of
                {ok, _Size} -> ok;
                {error, exceeded, CurrentSize, Limit} ->
                    CountToEvict = CurrentSize - Limit,
                    EvictedCount = evict_oldest_quotas(CountToEvict),
                    case EvictedCount > 0 of
                        true ->
                            router_logger:warn(~"Quota table size limit enforced", #{
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

%% Internal: Evict oldest quota entries (by tenant_id, simple FIFO)
evict_oldest_quotas(Count) when Count =< 0 -> 0;
evict_oldest_quotas(Count) ->
    %% Get all quota entries
    AllEntries = ets:foldl(fun({TenantId, _Quota}, Acc) -> [TenantId | Acc] end, [], ?QUOTA_TABLE),
    %% Evict first N entries (simple FIFO)
    TenantsToEvict = lists:sublist(AllEntries, Count),
    lists:foreach(fun(TenantId) ->
        ets:delete(?QUOTA_TABLE, TenantId)
    end, TenantsToEvict),
    length(TenantsToEvict).

%% Protects ETS from unbounded growth
-spec cleanup() -> ok.
cleanup() ->
    %% Get table size before cleanup
    SizeBefore = case get_table_size() of
        undefined -> 0;
        Size -> Size
    end,
    
    %% Enforce size limit if needed
    enforce_size_limit_if_needed(),
    
    %% Get table size after cleanup
    SizeAfter = case get_table_size() of
        undefined -> 0;
        SizeAfterValue -> SizeAfterValue
    end,
    
    %% Emit metrics for table size monitoring
    telemetry:execute([router, quota, table_size], #{
        size => SizeAfter
    }, #{
        size_before => SizeBefore
    }),
    
    ok.

-spec start_cleanup_timer() -> ok.
start_cleanup_timer() ->
    CleanupInterval = application:get_env(beamline_router, quota_cleanup_interval_seconds, 300),
    case timer:apply_interval(CleanupInterval * 1000, ?MODULE, cleanup, []) of
        {ok, _TimerRef} -> ok;
        {error, Reason} ->
            router_logger:error(~"Failed to start quota cleanup timer", #{
                ~"error" => Reason
            }),
            ok
    end.
