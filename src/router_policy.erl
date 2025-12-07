%% @doc Policy Management
%% Loads routing policies directly (no in-process cache)
-module(router_policy).
-export([load_policy/2, parse_policy_json/1]).

-include("beamline_router.hrl").

-define(CACHE_TTL_SECONDS, 300).

%% @doc Load policy from ETS store (CP1) or database (CP2+)
%% CP1: Uses router_policy_store (ETS with fixtures)
%% CP2+: Falls back to database if ETS unavailable
load_policy(TenantId, PolicyId) ->
    %% CP1: Try ETS store first (fixtures-based)
    case router_policy_store:get_policy(TenantId, PolicyId) of
        {ok, Policy} ->
            {ok, Policy};
        {error, not_found} ->
            %% CP2+: Try database if ETS lookup fails
            load_from_database_or_static(TenantId, PolicyId)
    end.

%% Internal: Load from database with fallback to static
load_from_database_or_static(TenantId, PolicyId) ->
    %% CP1: Database may not be available (epgsql not loaded)
    %% Gracefully handle missing database connection
    try
        StartTime = erlang:system_time(microsecond),
        case router_db:load_policy(TenantId, PolicyId) of
            {ok, #{policy_json := PolicyJson, version := Version}} ->
                case parse_policy_json(PolicyJson) of
                    {ok, ParsedPolicy} ->
                        %% Use parse_policy_map from router_policy_store for consistent parsing
                        %% This ensures both JSON-DSL format and legacy format are supported
                        PolicyMap = maps:put(<<"version">>, Version, ParsedPolicy),
                        Policy = router_policy_store:parse_policy_map(TenantId, PolicyId, PolicyMap),
                        Latency = (erlang:system_time(microsecond) - StartTime) / 1000000.0,
                        router_metrics:emit_metric(policy_loads_total, #{count => 1}, #{
                            tenant_id => TenantId,
                            source => <<"database">>
                        }),
                        router_logger:info(<<"Policy loaded from database">>, #{
                            <<"tenant_id">> => TenantId,
                            <<"policy_id">> => PolicyId,
                            <<"version">> => Version,
                            <<"latency_ms">> => Latency * 1000
                        }),
                        {ok, Policy};
                    Error ->
                        router_logger:error(<<"Policy JSON parsing failed">>, #{
                            <<"tenant_id">> => TenantId,
                            <<"policy_id">> => PolicyId,
                            <<"error">> => Error
                        }),
                        Error
                end;
            _Error ->
                %% Database returned error
                {error, not_found}
        end
    catch
        error:undef ->
            %% CP1: Database module not available (epgsql not loaded)
            {error, not_found};
        _:_ ->
            %% Any other error
            {error, not_found}
    end.

%% No cache layer: policies are loaded directly; cache functions removed.

%% @doc Parse JSON policy
parse_policy_json(PolicyJson) when is_binary(PolicyJson) ->
    case jsx:decode(PolicyJson, [return_maps]) of
        Policy when is_map(Policy) ->
            {ok, Policy};
        Error ->
            {error, {invalid_json, Error}}
    end;
parse_policy_json(PolicyJson) when is_map(PolicyJson) ->
    {ok, PolicyJson};
parse_policy_json(_) ->
    {error, invalid_format}.
