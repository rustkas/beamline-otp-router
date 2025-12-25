-module(router_gateway_backpressure).

-doc "Gateway Backpressure Integration Module".
%% Provides structure and helpers for Gateway → Router backpressure integration
%%
%% Features:
%% - Gateway backpressure status API
%% - Gateway notification helpers
%% - Backpressure status communication
%% - Gateway health check integration
%%
%% ⚠️ NOTE: Actual Gateway integration requires Gateway changes (external dependency)
%% This module provides structure and helpers for when Gateway integration is implemented
%%
%% @see INTEGRATION_GUIDE.md#gateway-backpressure For Gateway integration guidelines

-export([
    get_backpressure_status_for_gateway/1,
    notify_gateway_backpressure_status/2,
    build_gateway_backpressure_response/1,
    check_gateway_backpressure_health/0,
    get_all_subjects_backpressure_status/0,
    get_backpressure_status_by_tenant/1,
    get_backpressure_status_by_provider/1,
    register_gateway_endpoint/1,
    unregister_gateway_endpoint/1,
    get_registered_gateway_endpoints/0
]).

-include("beamline_router.hrl").

%% Returns status in Gateway-compatible format
-spec get_backpressure_status_for_gateway(binary()) -> map().
get_backpressure_status_for_gateway(Subject) ->
    %% Validate input
    case is_binary(Subject) andalso byte_size(Subject) > 0 andalso byte_size(Subject) < 1024 of
        false ->
            router_logger:error(~"Gateway backpressure status check failed: invalid subject", #{
                ~"subject" => Subject
            }),
            #{
                subject => Subject,
                status => ~"unknown",
                error => ~"invalid_subject",
                timestamp => erlang:system_time(millisecond)
            };
        true ->
            try
        %% Get detailed backpressure status
        DetailedStatus = router_intake_backpressure:get_detailed_backpressure_status(Subject),
        
        %% Format for Gateway
        Status = maps:get(status, DetailedStatus, backpressure_inactive),
        Metrics = maps:get(metrics, DetailedStatus, #{}),
        Thresholds = maps:get(thresholds, DetailedStatus, #{}),
        Policy = maps:get(policy, DetailedStatus, #{}),
        
        #{
            subject => Subject,
            status => format_status_for_gateway(Status),
            metrics => #{
                pending_messages => maps:get(pending_messages, Metrics, 0),
                latency_p95_ms => maps:get(latency_p95_ms, Metrics, 0),
                inflight_messages => maps:get(inflight_messages, Metrics, 0)
            },
            thresholds => #{
                queue_overload => maps:get(queue_overload, Thresholds, 1000),
                latency_overload_ms => maps:get(latency_overload_ms, Thresholds, 5000),
                inflight_overload => maps:get(inflight_overload, Thresholds, 500)
            },
            policy => #{
                retry_after_seconds => maps:get(retry_after_seconds, Policy, 30),
                max_retry_attempts => maps:get(max_retry_attempts, Policy, 3)
            },
            timestamp => erlang:system_time(millisecond)
        }
    catch
        Error:CatchReason ->
            router_logger:error(~"Failed to get gateway backpressure status", #{
                ~"subject" => Subject,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"gateway_backpressure_status_failed"
            }),
            #{
                subject => Subject,
                status => ~"unknown",
                error => error_to_binary(CatchReason),
                timestamp => erlang:system_time(millisecond)
            }
    end
    end.

-spec format_status_for_gateway(atom()) -> binary().
format_status_for_gateway(backpressure_active) -> ~"active";
format_status_for_gateway(backpressure_warning) -> ~"warning";
format_status_for_gateway(backpressure_inactive) -> ~"inactive";
format_status_for_gateway(_) -> ~"unknown".

%% GatewayEndpoint: Gateway endpoint URL or identifier
%% Status: Backpressure status map
-spec notify_gateway_backpressure_status(binary(), map()) -> {ok, map()} | {error, term()}.
notify_gateway_backpressure_status(GatewayEndpoint, Status) when is_binary(GatewayEndpoint), is_map(Status) ->
    %% Validate input
    case byte_size(GatewayEndpoint) > 0 andalso byte_size(GatewayEndpoint) < 2048 of
        false ->
            router_logger:error(~"Gateway backpressure notification failed: invalid endpoint", #{
                ~"endpoint" => GatewayEndpoint
            }),
            {error, invalid_endpoint};
        true ->
            try
        %% Build notification payload
        Notification = build_gateway_notification(Status),
        
        %% Store notification for tracking
        track_gateway_notification(GatewayEndpoint, Notification),
        
        %% Emit notification metric
        router_metrics:emit_metric(router_gateway_backpressure_notification_total, #{count => 1}, #{
            gateway_endpoint => GatewayEndpoint,
            status => maps:get(status, Status, ~"unknown")
        }),
        
        %% NOTE: Actual Gateway notification requires Gateway API call
        %% This is a stub that prepares the notification structure
                {ok, Notification}
            catch
                Error:CatchReason ->
                    router_logger:error(~"Gateway backpressure notification failed", #{
                        ~"endpoint" => GatewayEndpoint,
                        ~"error" => Error,
                        ~"reason" => sanitize_error_for_logging(CatchReason),
                        ~"event" => ~"gateway_notification_failed"
                    }),
                    {error, CatchReason}
            end
    end;
notify_gateway_backpressure_status(_GatewayEndpoint, _Status) ->
    router_logger:error(~"Gateway backpressure notification failed: invalid arguments", #{}),
    {error, invalid_arguments}.

-spec build_gateway_notification(map()) -> map().
build_gateway_notification(Status) ->
    #{
        type => ~"backpressure_status",
        status => maps:get(status, Status, ~"unknown"),
        subject => maps:get(subject, Status, ~"unknown"),
        metrics => maps:get(metrics, Status, #{}),
        timestamp => erlang:system_time(millisecond),
        retry_after_seconds => maps:get(retry_after_seconds, Status, 0)
    }.

-spec build_gateway_backpressure_response(binary()) -> map().
build_gateway_backpressure_response(Subject) ->
    try
        Status = get_backpressure_status_for_gateway(Subject),
        #{
            success => true,
            data => Status
        }
    catch
        _:Reason ->
            #{
                success => false,
                error => error_to_binary(Reason)
            }
    end.

%% Returns overall health status for Gateway integration
-spec check_gateway_backpressure_health() -> map().
check_gateway_backpressure_health() ->
    try
        %% Get all subjects with backpressure
        AllSubjects = get_all_backpressure_subjects(),
        
        %% Count by status
        StatusCounts = lists:foldl(fun(Subject, Acc) ->
            case router_intake_backpressure:get_backpressure_status(Subject) of
                backpressure_active ->
                    maps:update_with(active, fun(C) -> C + 1 end, 1, Acc);
                backpressure_warning ->
                    maps:update_with(warning, fun(C) -> C + 1 end, 1, Acc);
                backpressure_inactive ->
                    maps:update_with(inactive, fun(C) -> C + 1 end, 1, Acc);
                _ ->
                    Acc
            end
        end, #{active => 0, warning => 0, inactive => 0}, AllSubjects),
        
        #{
            healthy => maps:get(active, StatusCounts, 0) =:= 0,
            status_counts => StatusCounts,
            total_subjects => length(AllSubjects),
            timestamp => erlang:system_time(millisecond)
        }
    catch
        Error:CatchReason ->
            router_logger:error(~"Failed to check gateway backpressure health", #{
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"gateway_health_check_failed"
            }),
            #{
                healthy => false,
                error => error_to_binary(CatchReason),
                timestamp => erlang:system_time(millisecond)
            }
    end.

-spec get_all_backpressure_subjects() -> [binary()].
get_all_backpressure_subjects() ->
    try
        %% Get subjects from status history table
        Table = router_backpressure_status_history,
        case ets:whereis(Table) of
            undefined ->
                [];
            _Tid ->
                %% Get unique subjects
                Subjects = [Subject || {Subject, _, _} <- ets:tab2list(Table)],
                lists:usort(Subjects)
        end
    catch
        _:_ ->
            []
    end.

-spec track_gateway_notification(binary(), map()) -> ok.
track_gateway_notification(GatewayEndpoint, Notification) ->
    try
        Table = router_gateway_notifications,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, ordered_set, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        Entry = {
            erlang:system_time(millisecond),
            GatewayEndpoint,
            Notification
        },
        ets:insert(Table, Entry),
        ok
    catch
        Error:CatchReason ->
            router_logger:debug(~"Failed to track gateway notification", #{
                ~"endpoint" => GatewayEndpoint,
                ~"error" => Error,
                ~"reason" => sanitize_error_for_logging(CatchReason),
                ~"event" => ~"gateway_notification_tracking_failed"
            }),
            ok
    end.

-spec error_to_binary(term()) -> binary().
error_to_binary(Error) when is_binary(Error) -> Error;
error_to_binary(Error) when is_atom(Error) -> atom_to_binary(Error, utf8);
error_to_binary(Error) -> iolist_to_binary(io_lib:format("~p", [Error])).

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
            ~"[REDACTED: contains sensitive data]";
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.

-spec get_all_subjects_backpressure_status() -> list(map()).
get_all_subjects_backpressure_status() ->
    Subjects = get_all_backpressure_subjects(),
    lists:map(fun(Subject) ->
        get_backpressure_status_for_gateway(Subject)
    end, Subjects).

-spec get_backpressure_status_by_tenant(binary()) -> list(map()).
get_backpressure_status_by_tenant(TenantId) ->
    AllSubjects = get_all_backpressure_subjects(),
    FilteredSubjects = lists:filter(fun(Subject) ->
        case extract_tenant_from_subject(Subject) of
            TenantId -> true;
            _ -> false
        end
    end, AllSubjects),
    lists:map(fun(Subject) ->
        get_backpressure_status_for_gateway(Subject)
    end, FilteredSubjects).

-spec get_backpressure_status_by_provider(binary()) -> list(map()).
get_backpressure_status_by_provider(ProviderId) ->
    AllSubjects = get_all_backpressure_subjects(),
    FilteredSubjects = lists:filter(fun(Subject) ->
        case extract_provider_from_subject(Subject) of
            ProviderId -> true;
            _ -> false
        end
    end, AllSubjects),
    lists:map(fun(Subject) ->
        get_backpressure_status_for_gateway(Subject)
    end, FilteredSubjects).

-spec register_gateway_endpoint(binary()) -> {ok, binary()} | {error, term()}.
register_gateway_endpoint(Endpoint) when is_binary(Endpoint) ->
    %% Validate input
    case byte_size(Endpoint) > 0 andalso byte_size(Endpoint) < 2048 of
        false ->
            router_logger:error(~"Gateway endpoint registration failed: invalid endpoint", #{
                ~"endpoint" => Endpoint
            }),
            {error, invalid_endpoint};
        true ->
            try
        Table = router_gateway_endpoints,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, set, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        EndpointId = generate_endpoint_id(Endpoint),
        Entry = {EndpointId, Endpoint, erlang:system_time(millisecond)},
        ets:insert(Table, Entry),
        
        router_metrics:emit_metric(router_gateway_endpoint_registered_total, #{count => 1}, #{
            endpoint_id => EndpointId
        }),
        
                {ok, EndpointId}
            catch
                _:CatchReason ->
                    router_logger:error(~"Gateway endpoint registration failed", #{
                        ~"endpoint" => Endpoint,
                        ~"reason" => error_to_binary(CatchReason)
                    }),
                    {error, CatchReason}
            end
    end;
register_gateway_endpoint(_Endpoint) ->
    router_logger:error(~"Gateway endpoint registration failed: invalid argument", #{}),
    {error, invalid_argument}.

-spec unregister_gateway_endpoint(binary()) -> ok | {error, term()}.
unregister_gateway_endpoint(EndpointId) when is_binary(EndpointId) ->
    try
        Table = router_gateway_endpoints,
        case ets:whereis(Table) of
            undefined ->
                {error, not_found};
            _ ->
                case ets:lookup(Table, EndpointId) of
                    [] ->
                        {error, not_found};
                    _ ->
                        ets:delete(Table, EndpointId),
                        router_metrics:emit_metric(router_gateway_endpoint_unregistered_total, #{count => 1}, #{
                            endpoint_id => EndpointId
                        }),
                        ok
                end
        end
    catch
        _:CatchReason ->
            {error, CatchReason}
    end.

-spec get_registered_gateway_endpoints() -> list(map()).
get_registered_gateway_endpoints() ->
    try
        Table = router_gateway_endpoints,
        case ets:whereis(Table) of
            undefined ->
                [];
            _ ->
                Entries = ets:tab2list(Table),
                lists:map(fun({EndpointId, Endpoint, RegisteredAt}) ->
                    #{
                        endpoint_id => EndpointId,
                        endpoint => Endpoint,
                        registered_at => RegisteredAt
                    }
                end, Entries)
        end
    catch
        _:_ ->
            []
    end.

%% Helper: Extract tenant from subject
extract_tenant_from_subject(Subject) when is_binary(Subject) ->
    %% Subject format: "beamline.router.v1.decide.{tenant_id}.{provider_id}"
    Parts = binary:split(Subject, ~".", [global]),
    case length(Parts) >= 5 of
        true ->
            lists:nth(5, Parts);
        false ->
            undefined
    end;
extract_tenant_from_subject(_) ->
    undefined.

%% Helper: Extract provider from subject
extract_provider_from_subject(Subject) when is_binary(Subject) ->
    %% Subject format: "beamline.router.v1.decide.{tenant_id}.{provider_id}"
    Parts = binary:split(Subject, ~".", [global]),
    case length(Parts) >= 6 of
        true ->
            lists:nth(6, Parts);
        false ->
            undefined
    end;
extract_provider_from_subject(_) ->
    undefined.

%% Helper: Generate endpoint ID
generate_endpoint_id(Endpoint) ->
    Hash = crypto:hash(sha256, Endpoint),
    <<Id:16/binary, _/binary>> = base64:encode(Hash),
    Id.

