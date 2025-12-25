-module(router_extension_load_balancer).

-doc "Extension Load Balancer".
%% CP3: Load balancing between multiple instances of the same extension
-export([select_instance/2, get_instances/1, update_instance_health/3]).

-include("beamline_router.hrl").

-define(TELEMETRY_PREFIX, [router_extension_load_balancer]).

%% Returns: {ok, Subject} | {error, Reason}
-spec select_instance(binary(), map()) -> {ok, binary()} | {error, term()}.
select_instance(ExtensionId, _Context) ->
    case get_instances(ExtensionId) of
        {ok, Instances} ->
            HealthyInstances = [I || I <- Instances,
                maps:get(enabled, I, true) =:= true,
                maps:get(health_status, I, ~"unknown") =/= ~"unhealthy"],
            case HealthyInstances of
                [] ->
                    EnabledInstances = [I || I <- Instances, maps:get(enabled, I, true) =:= true],
                    case EnabledInstances of
                        [] -> {error, no_instances_available};
                        [Instance | _] -> {ok, maps:get(subject, Instance)}
                    end;
                _ ->
                    SelectedInstance = select_weighted_instance(HealthyInstances, ExtensionId),
                    {ok, maps:get(subject, SelectedInstance)}
            end;
        {error, not_found} ->
            case router_extension_registry:lookup(ExtensionId) of
                {ok, Extension} -> {ok, Extension#extension.subject};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% Returns: {ok, [InstanceMap]} | {error, Reason}
-spec get_instances(binary()) -> {ok, [map()]} | {error, term()}.
-ifdef(EPGSQL_AVAILABLE).
get_instances(ExtensionId) ->
    case router_extension_registry_db:get_db_connection() of
        Conn when is_pid(Conn) ->
            Query = "SELECT extension_id, instance_id, subject, weight, enabled, "
                    "health_status, last_health_check, updated_at "
                    "FROM extension_instances WHERE extension_id = $1 ORDER BY weight DESC, instance_id",
            case catch apply(epgsql, equery, [Conn, Query, [ExtensionId]]) of
                {ok, _, []} -> {error, not_found};
                {ok, _, Rows} when is_list(Rows) ->
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
                {error, Reason} -> {error, Reason};
                {'EXIT', _} -> {error, database_not_available};
                Other -> {error, Other}
            end;
        _ -> {error, database_not_available}
    end.
-else.
get_instances(_ExtensionId) ->
    {error, database_not_available}.
-endif.

%% Returns: ok | {error, Reason}
-spec update_instance_health(binary(), binary(), map()) -> ok | {error, term()}.
-ifdef(EPGSQL_AVAILABLE).
update_instance_health(ExtensionId, InstanceId, HealthData) when is_map(HealthData) ->
    case router_extension_registry_db:get_db_connection() of
        Conn when is_pid(Conn) ->
            HealthStatus = maps:get(health_status, HealthData, undefined),
            LastHealthCheck = maps:get(last_health_check, HealthData, calendar:universal_time()),

            {UpdateClauses, Params} = build_instance_health_updates(HealthStatus, LastHealthCheck),

            case UpdateClauses of
                [] ->
                    ok;
                _ ->
                    Query = "UPDATE extension_instances SET " ++ string:join(UpdateClauses, ", ") ++
                            ", updated_at = CURRENT_TIMESTAMP "
                            "WHERE extension_id = $" ++ integer_to_list(length(Params) + 1) ++
                            " AND instance_id = $" ++ integer_to_list(length(Params) + 2),
                    AllParams = Params ++ [ExtensionId, InstanceId],
                    case catch apply(epgsql, equery, [Conn, Query, AllParams]) of
                        {ok, _} -> ok;
                        {error, Reason} = Error ->
                            router_logger:error(~"Failed to update instance health", #{
                                ~"extension_id" => ExtensionId,
                                ~"instance_id" => InstanceId,
                                ~"error" => Reason
                            }),
                            Error
                    end
            end;
        _ -> {error, database_not_available}
    end.
-else.
update_instance_health(_ExtensionId, _InstanceId, _HealthData) ->
    {error, database_not_available}.
-endif.

%% Internal: Select instance using weighted round-robin
select_weighted_instance(Instances, ExtensionId) ->
    %% Calculate total weight
    TotalWeight = lists:sum([maps:get(weight, I, 100) || I <- Instances]),
    
    %% Use extension_id + timestamp for deterministic but distributed selection
    %% In production, could use consistent hashing based on tenant_id or request_id
    SelectionKey = erlang:phash2({ExtensionId, erlang:system_time(second)}),
    RandomValue = (SelectionKey rem 100) + 1,
    
    %% Select instance based on weight
    select_by_weight(Instances, TotalWeight, RandomValue).

%% Internal: Select instance by weight
select_by_weight([Instance | Rest], TotalWeight, RandomValue) ->
    Weight = maps:get(weight, Instance, 100),
    WeightPercent = (Weight * 100) / TotalWeight,
    
    case RandomValue =< trunc(WeightPercent) of
        true ->
            Instance;
        false ->
            select_by_weight(Rest, TotalWeight - Weight, RandomValue - trunc(WeightPercent))
    end;
select_by_weight([], _, _) ->
    %% Fallback to first instance (should not happen)
    throw(no_instance_selected).

%% Internal: Build instance health update clauses and parameters
%% Suppress unused warning when EPGSQL is not available

build_instance_health_updates(HealthStatus, LastHealthCheck) ->
    {Clauses, Params} = lists:foldl(fun
        ({_Key, undefined}, {ClauseAcc, ParamAcc}) ->
            {ClauseAcc, ParamAcc};
        ({health_status, Value}, {ClauseAcc, ParamAcc}) ->
            Clause = "health_status = $" ++ integer_to_list(length(ParamAcc) + 1),
            {[Clause | ClauseAcc], [Value | ParamAcc]};
        ({last_health_check, Value}, {ClauseAcc, ParamAcc}) ->
            Clause = "last_health_check = $" ++ integer_to_list(length(ParamAcc) + 1),
            {[Clause | ClauseAcc], [Value | ParamAcc]}
    end, {[], []}, [
        {health_status, HealthStatus},
        {last_health_check, LastHealthCheck}
    ]),
    {lists:reverse(Clauses), lists:reverse(Params)}.
