-module(router_test_policy_helpers).
-include_lib("common_test/include/ct.hrl").

-export([
    upsert_policy/2,
    create_test_policy_map/2
]).

upsert_policy(TenantId, PolicyMap) ->
    Id = maps:get(<<"id">>, PolicyMap, <<"test_policy">>),
    Policy = router_policy_store:parse_policy_map(TenantId, Id, PolicyMap),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    Policy.

create_test_policy_map(ProviderMap, Fallbacks) ->
    #{
        <<"version">> => <<"1.0">>,
        <<"providers">> => ProviderMap,
        <<"fallbacks">> => Fallbacks
    }.
