%% @doc Extension Registry Database Integration
%% PostgreSQL integration for Extension Registry
%% CP2-LC: Database support with graceful fallback
-module(router_extension_registry_db).
-export([load_all_extensions/0, load_extension/1, update_health/2, log_audit/4, get_db_connection/0,
         load_extension_versions/1, load_extension_instances/1]).

-ignore_xref([
  {router_extension_registry_db, get_db_connection, 0},
  {router_extension_registry_db, load_all_extensions, 0},
  {router_extension_registry_db, load_extension, 1},
  {router_extension_registry_db, load_extension_versions, 1},
  {router_extension_registry_db, load_extension_instances, 1},
  {router_extension_registry_db, update_health, 2},
  {router_extension_registry_db, log_audit, 4}
]).

-ifdef(EPGSQL_AVAILABLE).
-define(HAVE_DB, true).
-else.
-define(HAVE_DB, false).
-endif.

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_extension_registry_db]).

%% @doc Load all enabled extensions from database
%% Returns: {ok, [Extension]} | {error, Reason}
-spec load_all_extensions() -> {ok, [#extension{}]} | {error, term()}.
load_all_extensions() ->
    case ?HAVE_DB of
        false ->
            {error, database_not_available};
        true ->
    try
        Conn = get_db_connection(),
        Query = "SELECT id, type, subject, timeout_ms, retry, enabled, version, "
                "config, metadata, updated_at "
                "FROM extensions "
                "WHERE enabled = TRUE "
                "ORDER BY id",
        case catch apply(epgsql, equery, [Conn, Query, []]) of
            {ok, _, Rows} ->
                Extensions = lists:map(fun(Row) ->
                    parse_extension_row(Row)
                end, Rows),
                router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [load_all_total], #{count => 1}, #{
                    extensions_count => length(Extensions)
                }),
                {ok, Extensions};
            {error, Reason} = Error ->
                router_logger:error(<<"Failed to load extensions from database">>, #{
                    <<"error">> => Reason
                }),
                Error
            end
    catch
        error:undef ->
            %% Database module not available (epgsql not loaded)
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
        end
    end.

%% @doc Load single extension by ID
%% Returns: {ok, Extension} | {error, not_found} | {error, Reason}
-spec load_extension(binary()) -> {ok, #extension{}} | {error, term()}.
load_extension(ExtensionId) ->
    case ?HAVE_DB of
        false -> {error, database_not_available};
        true ->
    try
        Conn = get_db_connection(),
        Query = "SELECT id, type, subject, timeout_ms, retry, enabled, version, "
                "config, metadata, updated_at "
                "FROM extensions "
                "WHERE id = $1 AND enabled = TRUE",
        case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
            {ok, _, [{Id, Type, Subject, TimeoutMs, Retry, Enabled, Version, ConfigJson, MetadataJson, UpdatedAt}]} ->
                Extension = parse_extension_row({Id, Type, Subject, TimeoutMs, Retry, Enabled, Version, ConfigJson, MetadataJson, UpdatedAt}),
                {ok, Extension};
            {ok, _, []} ->
                {error, not_found};
            {error, Reason} = Error ->
                router_logger:error(<<"Failed to load extension from database">>, #{
                    <<"extension_id">> => ExtensionId,
                    <<"error">> => Reason
                }),
                Error
        end
    catch
        error:undef ->
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
    end
    end.

%% @doc Update extension health metrics
%% Returns: ok | {error, Reason}
-spec update_health(binary(), map()) -> ok | {error, term()}.
update_health(ExtensionId, HealthData) ->
    case ?HAVE_DB of
        false -> {error, database_not_available};
        true ->
    try
        Conn = get_db_connection(),
        
        %% Build UPDATE query with only provided fields
        {Updates, Params} = build_health_updates(HealthData),
        
        case Updates of
            [] ->
                ok;  % No updates needed
            _ ->
                Query = "UPDATE extension_health SET " ++ string:join(Updates, ", ") ++ 
                        ", updated_at = CURRENT_TIMESTAMP WHERE extension_id = $" ++ 
                        integer_to_list(length(Params) + 1),
                AllParams = Params ++ [ExtensionId],
                case catch apply(epgsql, equery, [Conn, Query, AllParams]) of
                    {ok, _} ->
                        ok;
                    {error, Reason} = Error ->
                        router_logger:error(<<"Failed to update extension health">>, #{
                            <<"extension_id">> => ExtensionId,
                            <<"error">> => Reason
                        }),
                        Error
                end
        end
    catch
        error:undef ->
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
    end
    end.

%% @doc Log audit event for extension change
%% Returns: ok | {error, Reason}
-spec log_audit(binary(), binary(), map(), map()) -> ok | {error, term()}.
log_audit(ExtensionId, Action, OldValues, NewValues) ->
    case ?HAVE_DB of
        false -> {error, database_not_available};
        true ->
    try
        Conn = get_db_connection(),
        ChangedBy = maps:get(changed_by, NewValues, undefined),
        OldValuesJson = case OldValues of
            undefined -> null;
            _ -> jsx:encode(OldValues)
        end,
        NewValuesJson = case NewValues of
            undefined -> null;
            _ -> jsx:encode(NewValues)
        end,
        
        Query = "INSERT INTO extension_audit_log (extension_id, action, old_values, new_values, changed_by) "
                "VALUES ($1, $2, $3, $4, $5)",
        case catch apply(epgsql, equery, [Conn, Query, [ExtensionId, Action, OldValuesJson, NewValuesJson, ChangedBy]]) of
            {ok, _} ->
                ok;
            {error, Reason} = Error ->
            router_logger:error(<<"Failed to log extension audit">>, #{
                <<"extension_id">> => ExtensionId,
                <<"action">> => Action,
                <<"error">> => Reason
            }),
            Error
        end
    catch
        error:undef ->
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
        end
    end.

%% @doc Test database connection
%% Returns: ok | {error, Reason}
%% test_connection/0 removed (unused)

%% @doc Get database connection (exported for use by circuit breaker)
%% Uses process dictionary (simplified, same pattern as router_db.erl)
-spec get_db_connection() -> pid().
get_db_connection() ->
    case ?HAVE_DB of
        false -> throw({error, database_not_available});
        true ->
    case get(ext_registry_db_conn) of
        undefined ->
            %% Initialize connection
            Config = get_db_config(),
            Host = maps:get(host, Config, "localhost"),
            Port = maps:get(port, Config, 5432),
            Database = maps:get(database, Config, "beamline"),
            Username = maps:get(username, Config, "beamline"),
            Password = maps:get(password, Config, ""),
            
            case catch apply(epgsql, connect, [
                Host,
                Username,
                Password,
                [
                    {database, Database},
                    {port, Port}
                ]
            ]) of
                {ok, Conn} ->
                    put(ext_registry_db_conn, Conn),
                    Conn;
                {error, Reason} = Error ->
                    %% Security: Never log password in error messages
                    SanitizedReason = sanitize_error_for_logging(Reason),
                    router_logger:error(<<"Failed to connect to database">>, #{
                        <<"error">> => SanitizedReason,
                        <<"host">> => Host,
                        <<"database">> => Database,
                        <<"username">> => Username
                        %% Password intentionally omitted
                    }),
                    throw(Error)
                ;
                {'EXIT', _} ->
                    throw({error, database_not_available})
            end;
        Conn ->
            Conn
    end
    end.

%% Internal: Get database configuration from application environment
get_db_config() ->
    ExtensionRegistryConfig = application:get_env(beamline_router, extension_registry, []),
    #{
        host => proplists:get_value(db_host, ExtensionRegistryConfig, 
            os:getenv("EXTENSION_REGISTRY_DB_HOST", "localhost")),
        port => list_to_integer(
            proplists:get_value(db_port, ExtensionRegistryConfig,
                os:getenv("EXTENSION_REGISTRY_DB_PORT", "5432"))),
        database => proplists:get_value(db_name, ExtensionRegistryConfig,
            os:getenv("EXTENSION_REGISTRY_DB_NAME", "beamline")),
        username => proplists:get_value(db_user, ExtensionRegistryConfig,
            os:getenv("EXTENSION_REGISTRY_DB_USER", "beamline")),
        password => proplists:get_value(db_password, ExtensionRegistryConfig,
            os:getenv("EXTENSION_REGISTRY_DB_PASSWORD", ""))
    }.

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
            <<"[REDACTED: contains sensitive data]">>;
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.

%% Internal: Parse extension row from database
parse_extension_row({Id, Type, Subject, TimeoutMs, Retry, Enabled, _Version, ConfigJson, MetadataJson, _UpdatedAt}) ->
    Config = case ConfigJson of
        null -> #{};
        Json when is_binary(Json) -> jsx:decode(Json, [return_maps]);
        Map when is_map(Map) -> Map;
        _ -> #{}
    end,
    Metadata = case MetadataJson of
        null -> #{};
        MetadataJsonVal when is_binary(MetadataJsonVal) -> jsx:decode(MetadataJsonVal, [return_maps]);
        MetadataJsonVal when is_map(MetadataJsonVal) -> MetadataJsonVal;
        _ -> #{}
    end,
    
    #extension{
        id = ensure_binary(Id),
        type = ensure_binary(Type),
        subject = ensure_binary(Subject),
        timeout_ms = TimeoutMs,
        retry = Retry,
        enabled = Enabled,
        config = Config,
        metadata = Metadata
    }.

%% Internal: Build health update clauses and parameters
build_health_updates(HealthData) ->
    Fields = [
        {last_success, maps:get(last_success, HealthData, undefined)},
        {last_failure, maps:get(last_failure, HealthData, undefined)},
        {success_count, maps:get(success_count, HealthData, undefined)},
        {failure_count, maps:get(failure_count, HealthData, undefined)},
        {avg_latency_ms, maps:get(avg_latency_ms, HealthData, undefined)},
        {last_latency_ms, maps:get(last_latency_ms, HealthData, undefined)},
        {circuit_breaker_state, maps:get(circuit_breaker_state, HealthData, undefined)},
        {circuit_breaker_opened_at, maps:get(circuit_breaker_opened_at, HealthData, undefined)}
    ],
    {Updates, Params} = lists:foldl(fun
        ({_Key, undefined}, {UpdAcc, ParamAcc}) ->
            {UpdAcc, ParamAcc};
        ({Key, null}, {UpdAcc, ParamAcc}) ->
            UpdateClause = atom_to_list(Key) ++ " = NULL",
            {[UpdateClause | UpdAcc], ParamAcc};
        ({Key, Value}, {UpdAcc, ParamAcc}) ->
            UpdateClause = atom_to_list(Key) ++ " = $" ++ integer_to_list(length(ParamAcc) + 1),
            {[UpdateClause | UpdAcc], [Value | ParamAcc]}
    end, {[], []}, Fields),
    {lists:reverse(Updates), lists:reverse(Params)}.

%% @doc Load extension versions
%% Returns: {ok, [VersionMap]} | {error, Reason}
-spec load_extension_versions(binary()) -> {ok, [map()]} | {error, term()}.
load_extension_versions(ExtensionId) ->
    try
        Conn = get_db_connection(),
        Query = "SELECT id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at "
                "FROM extension_versions WHERE id = $1 ORDER BY created_at DESC",
        case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
            {ok, _, Rows} ->
                Versions = lists:map(fun(Row) ->
                    {Id, Version, Subject, TimeoutMs, Retry, ConfigJson, MetadataJson, RoutingRulesJson, Enabled, CreatedAt} = Row,
                    Config = case ConfigJson of
                        null -> #{};
                        ConfigJsonVal when is_binary(ConfigJsonVal) -> jsx:decode(ConfigJsonVal, [return_maps]);
                        ConfigJsonVal when is_map(ConfigJsonVal) -> ConfigJsonVal;
                        _ -> #{}
                    end,
                    Metadata = case MetadataJson of
                        null -> #{};
                        MetadataJsonVal when is_binary(MetadataJsonVal) -> jsx:decode(MetadataJsonVal, [return_maps]);
                        MetadataJsonVal when is_map(MetadataJsonVal) -> MetadataJsonVal;
                        _ -> #{}
                    end,
                    RoutingRules = case RoutingRulesJson of
                        null -> #{};
                        RoutingRulesJsonVal when is_binary(RoutingRulesJsonVal) -> jsx:decode(RoutingRulesJsonVal, [return_maps]);
                        RoutingRulesJsonVal when is_map(RoutingRulesJsonVal) -> RoutingRulesJsonVal;
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
                router_logger:error(<<"Failed to load extension versions">>, #{
                    <<"extension_id">> => ExtensionId,
                    <<"error">> => Reason
                }),
                {error, Reason};
            {'EXIT', _} ->
                {error, database_not_available};
            Other ->
                router_logger:error(<<"Unexpected error loading extension versions">>, #{
                    <<"extension_id">> => ExtensionId,
                    <<"error">> => Other
                }),
                {error, unexpected_error}
        end
    catch
        error:undef ->
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
        end.

%% @doc Load extension instances
%% Returns: {ok, [InstanceMap]} | {error, Reason}
-spec load_extension_instances(binary()) -> {ok, [map()]} | {error, term()}.
load_extension_instances(ExtensionId) ->
    try
        Conn = get_db_connection(),
        Query = "SELECT extension_id, instance_id, subject, weight, enabled, "
                "health_status, last_health_check, updated_at "
                "FROM extension_instances WHERE extension_id = $1 ORDER BY weight DESC, instance_id",
        case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
            {ok, _, []} ->
                {error, not_found};
            {ok, _, Rows} ->
                Instances = lists:map(fun(Row) ->
                    {ExtId, InstanceId, Subject, Weight, Enabled, HealthStatus, LastHealthCheck, UpdatedAt} = Row,
                    #{
                        extension_id => ExtId,
                        instance_id => InstanceId,
                        subject => Subject,
                        weight => Weight,
                        enabled => Enabled,
                        health_status => HealthStatus,
                        last_health_check => LastHealthCheck,
                        updated_at => UpdatedAt
                    }
                end, Rows),
                {ok, Instances};
            {error, Reason} = Error ->
            router_logger:error(<<"Failed to load extension instances">>, #{
                <<"extension_id">> => ExtensionId,
                <<"error">> => Reason
            }),
            Error
        end
    catch
        error:undef ->
            {error, database_not_available};
        _:_ ->
            {error, unexpected_error}
    end.

%% Internal: Ensure binary value
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(_) -> <<"">>.
