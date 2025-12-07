%% @doc DEPRECATED: ACL module (legacy, not used in hot paths)
%%
%% ⚠️  **DEPRECATED / TECH DEBT** ⚠️
%%
%% This module is deprecated and should not be used in new code.
%% 
%% Migration: Use `router_tenant_validator.erl` instead (single source of truth for ACL decisions).
%% 
%% CP3/Pre-Release: Remove this module after ensuring no external dependencies.
%% 
%% Reference: apps/otp/router/docs/ACL_MODEL.md (deprecation notes)
%% Use `router_tenant_validator.erl` instead for tenant validation and ACL decisions.
%%
%% **Migration Path**:
%% - Replace `router_acl:allow/2` with `router_tenant_validator:validate_tenant/2`
%% - Replace `router_acl:load_policies/1` with `router_policy_store:upsert_policy/3`
%% - Replace `router_acl:audit/3` with `router_tenant_validator:emit_audit_event/3`
%%
%% **Status**: This module exists for backward compatibility only.
%% It is NOT used in any production code paths (result/ack/intake consumers use `router_tenant_validator`).
%%
%% **See**: `apps/otp/router/docs/ACL_MODEL.md` for formal ACL model specification.
%%
-module(router_acl).
-ignore_xref([
  router_acl,
  {router_acl, init, 0},
  {router_acl, load_policies, 1},
  {router_acl, load_policy, 3},
  {router_acl, allow, 2},
  {router_acl, allow, 3},
  {router_acl, deny, 2},
  {router_acl, deny, 3},
  {router_acl, audit, 3},
  {router_acl, audit, 4},
  {router_acl, metrics, 0}
]).
-export([
  init/0,
  load_policies/1,
  load_policy/3,
  allow/2,
  allow/3,
  deny/2,
  deny/3,
  audit/3,
  audit/4,
  metrics/0,
  get_table_size/0,
  get_table_memory/0,
  check_size_limit/0
]).

-include("beamline_router.hrl").

%% Policy format:
%% #{
%%   TenantId => #{
%%     Action => #{
%%       allowed => boolean(),
%%       conditions => map()  %% Optional conditions (e.g., time_window, ip_whitelist)
%%     }
%%   }
%% }
%%
%% Or simplified format (backward compatible):
%% #{
%%   {TenantId, Action} => boolean()
%% }

%% @doc Initialize ACL ETS table
init() ->
  case ets:info(router_acl) of
    undefined -> 
      ets:new(router_acl, [
        named_table, 
        public,
        {write_concurrency, true},
        {read_concurrency, true}
      ]);
    _ -> ok
  end,
  ok.

%% @doc Load policies map (bulk load)
%% Policy format: #{TenantId => #{Action => PolicyMap}} or #{{TenantId, Action} => boolean()}
load_policies(PoliciesMap) when is_map(PoliciesMap) ->
  init(),
  ets:insert(router_acl, {policies, PoliciesMap}),
  
  %% Count policies loaded
  PolicyCount = count_policies(PoliciesMap),
  telemetry:execute([router, acl, policies_loaded], #{count => PolicyCount}, #{}),
  
  router_logger:info(<<"ACL policies loaded">>, #{
    <<"policy_count">> => PolicyCount
  }),
  ok.

%% @doc Load single policy (incremental update)
%% Tenant: Tenant identifier (binary)
%% Action: Action identifier (atom or binary)
%% Policy: Policy map #{allowed => boolean(), conditions => map()} or boolean()
load_policy(Tenant, Action, Policy) when is_binary(Tenant) ->
  init(),
  case ets:lookup(router_acl, policies) of
    [{policies, PoliciesMap}] ->
      %% Update existing policies
      UpdatedMap = update_policy_map(PoliciesMap, Tenant, Action, Policy),
      ets:insert(router_acl, {policies, UpdatedMap}),
      telemetry:execute([router, acl, policy_updated], #{count => 1}, #{
        tenant => Tenant,
        action => Action
      });
    [] ->
      %% Create new policies map
      NewMap = create_policy_map(Tenant, Action, Policy),
      ets:insert(router_acl, {policies, NewMap}),
      telemetry:execute([router, acl, policy_created], #{count => 1}, #{
        tenant => Tenant,
        action => Action
      })
  end,
  ok.

%% @doc Check if action is allowed for tenant
%% Returns: {ok, allowed} | {error, denied}
allow(Tenant, Action) ->
  allow(Tenant, Action, #{}).

%% @doc Check if action is allowed for tenant with context
%% Context: Additional context for condition evaluation (e.g., #{ip => IP, time => Time})
allow(Tenant, Action, Context) when is_binary(Tenant) ->
  case acl_check(Tenant, Action, Context) of
    {ok, allowed} ->
      router_metrics:inc(router_acl_allowed_total),
      telemetry:execute([router, acl, allowed], #{count => 1}, #{
        tenant => Tenant,
        action => Action,
        context => Context
      }),
      {ok, allowed};
    {error, denied} ->
      deny(Tenant, Action, Context)
  end.

%% @doc Deny action for tenant
%% Returns: {error, denied}
deny(Tenant, Action) ->
  deny(Tenant, Action, #{}).

%% @doc Deny action for tenant with context
%% Returns: {error, denied}
deny(Tenant, Action, Context) when is_binary(Tenant) ->
  router_metrics:inc(router_acl_denied_total),
  audit(Tenant, Action, denied, Context),
  {error, denied}.

%% @doc Audit ACL decision
%% Result: allowed | denied
audit(Tenant, Action, Result) ->
  audit(Tenant, Action, Result, #{}).

%% @doc Audit ACL decision with context
%% Result: allowed | denied
%% Context: Additional context for audit
audit(Tenant, Action, Result, Context) when is_binary(Tenant) ->
  telemetry:execute([router, acl, audit], #{
    count => 1,
    result => Result
  }, #{
    tenant => Tenant,
    action => Action,
    context => Context,
    timestamp => erlang:system_time(millisecond)
  }),
  
  %% Log denied actions at WARN level
  case Result of
    denied ->
      router_logger:warn(<<"ACL denied action">>, #{
        <<"tenant">> => Tenant,
        <<"action">> => Action,
        <<"context">> => Context
      });
    allowed ->
      router_logger:debug(<<"ACL allowed action">>, #{
        <<"tenant">> => Tenant,
        <<"action">> => Action
      })
  end,
  ok.

%% @doc Get list of metrics exported by this module
metrics() -> [router_acl_denied_total, router_acl_allowed_total].

%% Internal functions

%% @doc Check ACL policy for tenant/action
%% Returns: {ok, allowed} | {error, denied}
acl_check(Tenant, Action, Context) ->
  case ets:lookup(router_acl, policies) of
    [{policies, PoliciesMap}] ->
      check_policy(PoliciesMap, Tenant, Action, Context);
    [] ->
      %% No policies loaded - default deny
      {error, denied}
  end.

%% @doc Check policy in map (supports both formats)
check_policy(PoliciesMap, Tenant, Action, Context) ->
  %% Try new format first: #{Tenant => #{Action => Policy}}
  case maps:get(Tenant, PoliciesMap, undefined) of
    undefined ->
      %% Try old format: #{{Tenant, Action} => boolean()}
      case maps:get({Tenant, Action}, PoliciesMap, undefined) of
        undefined ->
          {error, denied};
        Allowed when is_boolean(Allowed) ->
          case Allowed of
            true -> {ok, allowed};
            false -> {error, denied}
          end;
        PolicyMap when is_map(PolicyMap) ->
          evaluate_policy(PolicyMap, Context)
      end;
    TenantPolicies when is_map(TenantPolicies) ->
      case maps:get(Action, TenantPolicies, undefined) of
        undefined ->
          {error, denied};
        Allowed when is_boolean(Allowed) ->
          case Allowed of
            true -> {ok, allowed};
            false -> {error, denied}
          end;
        PolicyMap when is_map(PolicyMap) ->
          evaluate_policy(PolicyMap, Context)
      end
  end.

%% @doc Evaluate policy with conditions
%% PolicyMap: #{allowed => boolean(), conditions => map()}
evaluate_policy(PolicyMap, Context) ->
  Allowed = maps:get(allowed, PolicyMap, false),
  case Allowed of
    false ->
      {error, denied};
    true ->
      %% Check conditions if present
      case maps:get(conditions, PolicyMap, undefined) of
        undefined ->
          {ok, allowed};
        Conditions when is_map(Conditions) ->
          case evaluate_conditions(Conditions, Context) of
            true -> {ok, allowed};
            false -> {error, denied}
          end
      end
  end.

%% @doc Evaluate policy conditions
%% Conditions: #{time_window => #{start => Time, end => Time}, ip_whitelist => [IPs], ...}
%% Returns: true if all conditions pass, false otherwise
evaluate_conditions(Conditions, Context) ->
  %% Check time window if present
  TimeWindowOk = case maps:get(time_window, Conditions, undefined) of
    undefined -> true;
    #{start := Start, 'end' := End} ->
      Now = erlang:system_time(millisecond),
      Now >= Start andalso Now =< End;
    _ -> true
  end,
  
  case TimeWindowOk of
    false -> false;
    true ->
      %% Check IP whitelist if present
      IPWhitelistOk = case maps:get(ip_whitelist, Conditions, undefined) of
        undefined -> true;
        Whitelist when is_list(Whitelist) ->
          IP = maps:get(ip, Context, undefined),
          case IP of
            undefined -> false;
            _ -> lists:member(IP, Whitelist)
          end;
        _ -> true
      end,
      
      IPWhitelistOk
  end.

%% @doc Count policies in map
count_policies(PoliciesMap) ->
  maps:fold(fun
    ({_Tenant, _Action}, _Value, Acc) ->
      Acc + 1;
    (_Tenant, TenantPolicies, Acc) when is_map(TenantPolicies) ->
      Acc + maps:size(TenantPolicies);
    (_Key, _Value, Acc) ->
      Acc
  end, 0, PoliciesMap).

%% @doc Update policy map with new policy
update_policy_map(PoliciesMap, Tenant, Action, Policy) ->
  %% Try to get tenant policies
  case maps:get(Tenant, PoliciesMap, undefined) of
    undefined ->
      %% Create new tenant entry
      NewTenantPolicies = create_policy_map(Tenant, Action, Policy),
      maps:put(Tenant, maps:get(Action, NewTenantPolicies), PoliciesMap);
    TenantPolicies when is_map(TenantPolicies) ->
      %% Update existing tenant policies
      UpdatedTenantPolicies = maps:put(Action, normalize_policy(Policy), TenantPolicies),
      maps:put(Tenant, UpdatedTenantPolicies, PoliciesMap)
  end.

%% @doc Create new policy map
create_policy_map(Tenant, Action, Policy) ->
  #{Tenant => #{Action => normalize_policy(Policy)}}.

%% @doc Normalize policy to standard format
%% Input: boolean() | #{allowed => boolean(), conditions => map()}
%% Output: #{allowed => boolean(), conditions => map()}
normalize_policy(Policy) when is_boolean(Policy) ->
  #{allowed => Policy, conditions => #{}};
normalize_policy(Policy) when is_map(Policy) ->
  %% Ensure allowed field exists
  Allowed = maps:get(allowed, Policy, false),
  Conditions = maps:get(conditions, Policy, #{}),
  #{allowed => Allowed, conditions => Conditions}.

%% @doc Get current table size (number of entries)
-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(router_acl, size) of
        undefined -> undefined;
        Size -> Size
    end.

%% @doc Get current table memory usage in bytes
-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(router_acl, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

%% @doc Check if table size exceeds configured limit
-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, acl_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(router_acl, Limit);
        _ -> {error, invalid_limit}
    end.
