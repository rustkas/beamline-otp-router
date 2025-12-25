-module(router_policy_static).

-doc "Static Policy Provider for CP1".
%% Provides simple static policies for testing without database
-export([get_default_policy/1, get_policy/2]).

-include("beamline_router.hrl").

get_default_policy(TenantId) ->
    get_policy(TenantId, ~"default").

get_policy(_TenantId, _PolicyId) ->
    %% CP1: Simple static policy with weighted distribution
    #policy{
        tenant_id = ~"default_tenant",
        policy_id = ~"default",
        version = ~"1.0",
        defaults = #{
            ~"provider" => ~"openai"
        },
        weights = #{
            ~"openai" => 0.7,
            ~"anthropic" => 0.3
        },
        fallback = #{
            ~"provider" => ~"local_llm",
            ~"conditions" => [~"all_providers_failed", ~"timeout"]
        },
        sticky = #{
            ~"enabled" => true,
            ~"ttl_seconds" => 3600
        },
        metadata = #{
            ~"strategy" => ~"cp1_static",
            ~"description" => ~"CP1 static policy for testing"
        }
    }.

