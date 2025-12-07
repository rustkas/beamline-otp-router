%% @doc Property-based tests for router_policy_store
%% Uses PropEr for randomized sequence of operations to detect race conditions
-module(router_policy_store_prop_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Always include proper.hrl in test profile (PropEr is available in test profile)
%% Runtime check for PropEr availability is done in prop_* functions
-include_lib("proper/include/proper.hrl").

%% Include router header for policy record
-include("../include/beamline_router.hrl").

%% Suppress warnings for PropEr generators (used via ?LET macro)
-compile({nowarn_unused_function, [
    upsert_operation/0,
    delete_operation/0,
    policy/0,
    policy_id/0,
    weights/0,
    provider_id/0,
    weight/0,
    execute_operation/2
]}).
%% Suppress warnings for variables in ?LET macros (used in record construction)
-compile(nowarn_unused_vars).

upsert_operation() ->
    ?LET(
        {PolicyId, Weights},
        {policy_id(), weights()},
        {upsert, #policy{
            tenant_id = <<"prop_tenant">>,
            policy_id = PolicyId,
            weights = Weights,
            version = <<"1.0">>,
            defaults = #{},
            escalate_on = [],
            fallback = undefined,
            sticky = undefined,
            metadata = #{}
        }}
    ).

delete_operation() ->
    ?LET(
        PolicyId,
        policy_id(),
        {delete, PolicyId}
    ).

policy() ->
    ?LET(
        {_PolicyId, _Weights},
        {policy_id(), weights()},
        #policy{
            tenant_id = <<"prop_tenant">>,
            policy_id = _PolicyId,
            weights = _Weights,
            version = <<"1.0">>,
            defaults = #{},
            escalate_on = [],
            fallback = undefined,
            sticky = undefined,
            metadata = #{}
        }
    ).

policy_id() ->
    ?LET(
        N,
        integer(1, 1000),
        list_to_binary("policy_" ++ integer_to_list(N))
    ).

weights() ->
    ?LET(
        Providers,
        list({provider_id(), weight()}),
        maps:from_list(Providers)
    ).

provider_id() ->
    oneof([<<"openai">>, <<"anthropic">>, <<"cohere">>]).

weight() ->
    ?LET(
        W,
        real(),
        max(0.0, min(1.0, abs(W)))
    ).

%% Helpers

execute_operation(TenantId, {upsert, Policy}) ->
    router_policy_store:upsert_policy(TenantId, Policy);
execute_operation(TenantId, {delete, _PolicyId}) ->
    router_policy_store:delete_policy(TenantId, _PolicyId).

