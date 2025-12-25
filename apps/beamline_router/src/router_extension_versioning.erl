-module(router_extension_versioning).

-doc "Extension Versioning".
%% CP3: Version routing and selection for extensions
-export([lookup_with_version/2, select_version/2, get_versions/1]).

-ifdef(CP3).

-include("beamline_router.hrl").

%% Extension record is now defined in beamline_router.hrl
%% (moved from here to centralize common definitions)

-define(TELEMETRY_PREFIX, [router_extension_versioning]).

%% Returns: {ok, Extension} | {error, Reason}
-spec lookup_with_version(binary(), map()) -> {ok, #extension{}} | {error, term()}.
lookup_with_version(ExtensionId, Context) ->
    try
        %% Get all versions for this extension
        case get_versions(ExtensionId) of
            {ok, Versions} ->
                %% Select version based on routing rules
                case select_version(Versions, Context) of
                    {ok, SelectedVersion} ->
                        %% Load extension with selected version
                        load_extension_version(ExtensionId, SelectedVersion);
                    {error, no_version_match} ->
                        %% Fallback to default version (from extensions table)
                        router_extension_registry:lookup(ExtensionId);
                    {error, Reason} = Error ->
                        Error
                end;
            {error, Reason} = Error ->
                Error
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

%% Returns: {ok, Version} | {error, no_version_match}
-spec select_version([map()], map()) -> {ok, binary()} | {error, term()}.
select_version(Versions, Context) ->
    %% Filter enabled versions
    EnabledVersions = [V || V <- Versions, maps:get(enabled, V, true) =:= true],
    
    %% Try to match routing rules
    MatchingVersions = lists:filter(fun(Version) ->
        RoutingRules = maps:get(routing_rules, Version, #{}),
        match_routing_rules(RoutingRules, Context)
    end, EnabledVersions),
    
    case MatchingVersions of
        [] ->
            %% No matching version, try default (highest version or v1)
            case EnabledVersions of
                [] ->
                    {error, no_version_match};
                [DefaultVersion | _] ->
                    {ok, maps:get(version, DefaultVersion)}
            end;
        [MatchedVersion | _] ->
            {ok, maps:get(version, MatchedVersion)}
    end.

%% Returns: {ok, [VersionMap]} | {error, Reason}
-spec get_versions(binary()) -> {ok, [map()]} | {error, term()}.
get_versions(ExtensionId) ->
    case code:ensure_loaded(router_extension_registry_db) of
        {module, router_extension_registry_db} ->
            try
                case router_extension_registry_db:get_db_connection() of
                    Conn when is_pid(Conn) ->
                        Query = "SELECT id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at "
                                "FROM extension_versions WHERE id = $1 ORDER BY created_at DESC",
                        case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
                            {ok, _, Rows} ->
                                Versions = lists:map(fun(Row) ->
                                    {Id, Version, Subject, TimeoutMs, Retry, ConfigJson, MetadataJson, RoutingRulesJson, Enabled, CreatedAt} = Row,
                                    Config = case ConfigJson of
                                        null -> #{};
                                        Json when is_binary(Json) -> jsx:decode(Json, [return_maps]);
                                        Map when is_map(Map) -> Map;
                                        _ -> #{}
                                    end,
                                    Metadata = case MetadataJson of
                                        null -> #{};
                                        Json when is_binary(Json) -> jsx:decode(Json, [return_maps]);
                                        Map when is_map(Map) -> Map;
                                        _ -> #{}
                                    end,
                                    RoutingRules = case RoutingRulesJson of
                                        null -> #{};
                                        Json when is_binary(Json) -> jsx:decode(Json, [return_maps]);
                                        Map when is_map(Map) -> Map;
                                        _ -> #{}
                                    end,
                                    #{
                                        id => Id,
                                        version => Version,
                                        subject => Subject,
                                        timeout_ms => TimeoutMs,
                                        retry => Retry,
                                        config => Config,
                                        metadata => Metadata,
                                        routing_rules => RoutingRules,
                                        enabled => Enabled,
                                        created_at => CreatedAt
                                    }
                                end, Rows),
                                {ok, Versions};
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    _ ->
                        {error, database_not_available}
                end
            catch
                error:undef ->
                    {error, database_not_available};
                _:Reason ->
                    {error, Reason}
            end;
        _ -> {error, database_not_available}
    end.

%% Internal: Load extension with specific version
load_extension_version(ExtensionId, Version) ->
    case code:ensure_loaded(router_extension_registry_db) of
        {module, router_extension_registry_db} ->
            try
                case router_extension_registry_db:get_db_connection() of
                    Conn when is_pid(Conn) ->
                %% Get type from main extensions table
                TypeQuery = "SELECT type FROM extensions WHERE id = $1",
                TypeResult = case catch apply(epgsql, equery, [Conn, TypeQuery, [ExtensionId]]) of
                    {ok, _, [{Type}]} ->
                        {ok, Type};
                    {ok, _, []} ->
                        {error, extension_not_found};
                    {error, Reason} ->
                        {error, Reason}
                end,
                
                case TypeResult of
                    {ok, Type} ->
                        %% Get version details
                        Query = "SELECT id, version, subject, timeout_ms, retry, config, metadata "
                                "FROM extension_versions WHERE id = $1 AND version = $2 AND enabled = TRUE",
                        case catch apply(epgsql, equery, [Conn, Query, [ExtensionId, Version]]) of
                            {ok, _, [{Id, Ver, Subject, TimeoutMs, Retry, ConfigJson, MetadataJson}]} ->
                                Config = case ConfigJson of
                                    null -> #{};
                                    Json when is_binary(Json) -> jsx:decode(Json, [return_maps]);
                                    Map when is_map(Map) -> Map;
                                    _ -> #{}
                                end,
                                Metadata = case MetadataJson of
                                    null -> #{};
                                    Json when is_binary(Json) -> jsx:decode(Json, [return_maps]);
                                    Map when is_map(Map) -> Map;
                                    _ -> #{}
                                end,
                                Extension = #extension{
                                    id = ensure_binary(Id),
                                    type = ensure_binary(Type),
                                    subject = ensure_binary(Subject),
                                    timeout_ms = TimeoutMs,
                                    retry = Retry,
                                    enabled = true,
                                    config = Config,
                                    metadata = Metadata
                                },
                                {ok, Extension};
                            {ok, _, []} ->
                                {error, version_not_found};
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    {error, Reason} = Error ->
                        Error
                end;
            _ ->
                {error, database_not_available}
            end
        catch
            error:undef ->
                {error, database_not_available};
            _:Reason ->
                {error, Reason}
        end;
        _ -> {error, database_not_available}
    end.

%% Internal: Match routing rules against context
match_routing_rules(RoutingRules, Context) when map_size(RoutingRules) =:= 0 ->
    %% No routing rules, match by default
    true;
match_routing_rules(RoutingRules, Context) ->
    maps:fold(fun(Key, ExpectedValue, Acc) ->
        Acc andalso match_rule(Key, ExpectedValue, Context)
    end, true, RoutingRules).

%% Internal: Match single routing rule
%% Supports:
%%   - tenant_id: List of tenant IDs or single tenant ID
%%   - environment: List of environments (dev/stage/prod) or single environment
%%   - policy_id: List of policy IDs or single policy ID
%%   - Any other context key with list or exact match
match_rule(Key, ExpectedValue, Context) ->
    ContextValue = maps:get(Key, Context, undefined),
    case ExpectedValue of
        ValueList when is_list(ValueList) ->
            %% List of allowed values (e.g., ["tenant1", "tenant2"] or ["dev", "stage"])
            lists:member(ContextValue, ValueList);
        Value when is_binary(Value) orelse is_list(Value) ->
            %% Exact match (e.g., "tenant1" or "prod")
            ContextValue =:= ExpectedValue;
        _ ->
            false
    end.

%% Internal: Ensure binary value
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(_) -> <<"">>.

-else.

lookup_with_version(_ExtensionId, _Context) ->
    {error, cp3_disabled}.

select_version(_Versions, _Context) ->
    {error, cp3_disabled}.

get_versions(_ExtensionId) ->
    {error, cp3_disabled}.

-endif.
