%% @doc NATS Admin Service Handlers
%% Handles admin endpoints via NATS request-reply
%% Subjects:
%%   - beamline.router.v1.admin.get_extension_health
%%   - beamline.router.v1.admin.get_circuit_breaker_states
%%   - beamline.router.v1.admin.dry_run_pipeline
%%   - beamline.router.v1.admin.get_pipeline_complexity
-module(router_admin_nats).

-export([handle_get_extension_health/1,
         handle_get_circuit_breaker_states/1,
         handle_dry_run_pipeline/1,
         handle_get_pipeline_complexity/1]).

-include("beamline_router.hrl").

%% @doc Handle get_extension_health NATS request
-spec handle_get_extension_health(binary()) -> {ok, binary()} | {error, term()}.
handle_get_extension_health(RequestJson) ->
    try
        %% Parse request (may contain api_key for auth)
        _RequestMap = case jsx:decode(RequestJson, [return_maps]) of
            Map when is_map(Map) -> Map;
            _ -> #{}
        end,
        
        %% Get extension health
        case router_extension_health:get_all_health() of
            {ok, HealthList} ->
                %% Convert to map keyed by extension_id for JSON response
                HealthMap = lists:foldl(fun(Health, Acc) ->
                    ExtId = maps:get(extension_id, Health),
                    %% Convert to status string
                    Status = case maps:get(success_rate, Health, 0.0) of
                        SR when SR >= 0.95 -> <<"healthy">>;
                        SR when SR >= 0.80 -> <<"degraded">>;
                        _ -> <<"unhealthy">>
                    end,
                    HealthWithStatus = maps:put(status, Status, Health),
                    maps:put(ExtId, HealthWithStatus, Acc)
                end, #{}, HealthList),
                
                Response = #{health => HealthMap},
                ResponseJson = jsx:encode(Response),
                {ok, ResponseJson};
            {error, Reason} ->
                ErrorResponse = #{
                    error => #{
                        code => <<"INTERNAL_ERROR">>,
                        message => list_to_binary(io_lib:format("Failed to get extension health: ~p", [Reason]))
                    }
                },
                {error, jsx:encode(ErrorResponse)}
        end
    catch
        ErrorClass:ErrorReason:ErrorStacktrace ->
            router_logger:error(<<"Error handling get_extension_health">>, #{
                <<"error">> => ErrorClass,
                <<"reason">> => ErrorReason,
                <<"stacktrace">> => ErrorStacktrace
            }),
            CatchErrorResponse = #{
                error => #{
                    code => <<"INTERNAL_ERROR">>,
                    message => <<"Internal server error">>
                }
            },
            {error, jsx:encode(CatchErrorResponse)}
    end.

%% @doc Handle get_circuit_breaker_states NATS request
-spec handle_get_circuit_breaker_states(binary()) -> {ok, binary()} | {error, term()}.
handle_get_circuit_breaker_states(RequestJson) ->
    try
        %% Parse request (may contain api_key for auth)
        _RequestMap = case jsx:decode(RequestJson, [return_maps]) of
            Map when is_map(Map) -> Map;
            _ -> #{}
        end,
        
        %% Get circuit breaker states
        case router_extension_circuit_breaker:get_all_circuit_states() of
            {ok, StatesMap} ->
                Response = #{states => StatesMap},
                ResponseJson = jsx:encode(Response),
                {ok, ResponseJson};
            {error, Reason} ->
                ErrorResponse = #{
                    error => #{
                        code => <<"INTERNAL_ERROR">>,
                        message => list_to_binary(io_lib:format("Failed to get circuit breaker states: ~p", [Reason]))
                    }
                },
                {error, jsx:encode(ErrorResponse)}
        end
    catch
        ErrorClass:ErrorReason:ErrorStacktrace ->
            router_logger:error(<<"Error handling get_circuit_breaker_states">>, #{
                <<"error">> => ErrorClass,
                <<"reason">> => ErrorReason,
                <<"stacktrace">> => ErrorStacktrace
            }),
            CatchErrorResponse = #{
                error => #{
                    code => <<"INTERNAL_ERROR">>,
                    message => <<"Internal server error">>
                }
            },
            {error, jsx:encode(CatchErrorResponse)}
    end.

%% @doc Handle dry_run_pipeline NATS request
-spec handle_dry_run_pipeline(binary()) -> {ok, binary()} | {error, term()}.
handle_dry_run_pipeline(RequestJson) ->
    try
        %% Parse request
        RequestMap = case jsx:decode(RequestJson, [return_maps]) of
            Map when is_map(Map) -> Map;
            _ -> throw({error, <<"Invalid request format">>})
        end,
        
        TenantId = maps:get(<<"tenant_id">>, RequestMap, undefined),
        PolicyId = maps:get(<<"policy_id">>, RequestMap, undefined),
        Payload = maps:get(<<"payload">>, RequestMap, undefined),
        
        if
            TenantId =:= undefined orelse PolicyId =:= undefined orelse Payload =:= undefined ->
                ErrorResponse = #{
                    ok => false,
                    error => #{
                        code => <<"INVALID_ARGUMENT">>,
                        message => <<"Missing required fields: tenant_id, policy_id, payload">>
                    }
                },
                {error, jsx:encode(ErrorResponse)};
            true ->
                %% Load policy
                case router_policy_store:get_policy(TenantId, PolicyId) of
                    {ok, Policy} ->
                        %% Create route request for dry-run
                        RouteRequest = #route_request{
                            message = Payload,
                            context = #{<<"tenant_id">> => TenantId, <<"dry_run">> => true}
                        },
                        Context = #{<<"tenant_id">> => TenantId, <<"dry_run">> => true},
                        
                        %% Execute pipeline (dry-run mode)
                        case router_decider:decide(RouteRequest, Policy, Context) of
                            {ok, Decision} ->
                                %% Extract executed extensions from decision metadata
                                ExecutedExtensions = maps:get(<<"executed_extensions">>, Decision#route_decision.metadata, []),
                                %% Format response (Decision is a route_decision record)
                                Response = #{
                                    ok => true,
                                    result => #{
                                        decision => #{
                                            provider_id => Decision#route_decision.provider_id,
                                            reason => Decision#route_decision.reason,
                                            priority => Decision#route_decision.priority
                                        },
                                        executed_extensions => ExecutedExtensions,
                                        final_payload => Payload
                                    }
                                },
                                ResponseJson = jsx:encode(Response),
                                {ok, ResponseJson};
                            {error, Reason} ->
                                ErrorResponse = #{
                                    ok => false,
                                    error => #{
                                        code => <<"PIPELINE_ERROR">>,
                                        message => list_to_binary(io_lib:format("Pipeline execution failed: ~p", [Reason]))
                                    }
                                },
                                {error, jsx:encode(ErrorResponse)}
                        end;
                    {error, not_found} ->
                        ErrorResponse = #{
                            ok => false,
                            error => #{
                                code => <<"POLICY_NOT_FOUND">>,
                                message => list_to_binary(io_lib:format("Policy '~s/~s' not found", [TenantId, PolicyId]))
                            }
                        },
                        {error, jsx:encode(ErrorResponse)};
                    {error, Reason} ->
                        ErrorResponse = #{
                            ok => false,
                            error => #{
                                code => <<"INTERNAL_ERROR">>,
                                message => list_to_binary(io_lib:format("Failed to load policy: ~p", [Reason]))
                            }
                        },
                        {error, jsx:encode(ErrorResponse)}
                end
        end
    catch
        {error, Msg} when is_binary(Msg) ->
            CatchErrorResponse = #{
                ok => false,
                error => #{
                    code => <<"INVALID_ARGUMENT">>,
                    message => Msg
                }
            },
            {error, jsx:encode(CatchErrorResponse)};
        ErrorClass:ErrorReason:ErrorStacktrace ->
            router_logger:error(<<"Error handling dry_run_pipeline">>, #{
                <<"error">> => ErrorClass,
                <<"reason">> => ErrorReason,
                <<"stacktrace">> => ErrorStacktrace
            }),
            CatchErrorResponse = #{
                ok => false,
                error => #{
                    code => <<"INTERNAL_ERROR">>,
                    message => <<"Internal server error">>
                }
            },
            {error, jsx:encode(CatchErrorResponse)}
    end.

%% @doc Handle get_pipeline_complexity NATS request
-spec handle_get_pipeline_complexity(binary()) -> {ok, binary()} | {error, term()}.
handle_get_pipeline_complexity(RequestJson) ->
    try
        %% Parse request
        RequestMap = case jsx:decode(RequestJson, [return_maps]) of
            Map when is_map(Map) -> Map;
            _ -> throw({error, <<"Invalid request format">>})
        end,
        
        TenantId = maps:get(<<"tenant_id">>, RequestMap, undefined),
        PolicyId = maps:get(<<"policy_id">>, RequestMap, undefined),
        
        if
            TenantId =:= undefined orelse PolicyId =:= undefined ->
                ErrorResponse = #{
                    error => #{
                        code => <<"INVALID_ARGUMENT">>,
                        message => <<"Missing required fields: tenant_id, policy_id">>
                    }
                },
                {error, jsx:encode(ErrorResponse)};
            true ->
                case router_policy_store:get_policy(TenantId, PolicyId) of
                    {ok, #policy{pre = Pre, validators = Validators, post = Post}} ->
                        ComplexityMap = router_decider:calculate_pipeline_complexity(Pre, Validators, Post),
                        ResponseJson = jsx:encode(ComplexityMap),
                        {ok, ResponseJson};
                    {error, not_found} ->
                        ErrorResponse = #{
                            error => #{
                                code => <<"POLICY_NOT_FOUND">>,
                                message => list_to_binary(io_lib:format("Policy '~s/~s' not found", [TenantId, PolicyId]))
                            }
                        },
                        {error, jsx:encode(ErrorResponse)};
                    {error, Reason} ->
                        ErrorResponse = #{
                            error => #{
                                code => <<"INTERNAL_ERROR">>,
                                message => list_to_binary(io_lib:format("Failed to load policy: ~p", [Reason]))
                            }
                        },
                        {error, jsx:encode(ErrorResponse)}
                end
        end
    catch
        {error, Msg} when is_binary(Msg) ->
            CatchErrorResponse = #{
                error => #{
                    code => <<"INVALID_ARGUMENT">>,
                    message => Msg
                }
            },
            {error, jsx:encode(CatchErrorResponse)};
        ErrorClass:ErrorReason:ErrorStacktrace ->
            router_logger:error(<<"Error handling get_pipeline_complexity">>, #{
                <<"error">> => ErrorClass,
                <<"reason">> => ErrorReason,
                <<"stacktrace">> => ErrorStacktrace
            }),
            CatchErrorResponse = #{
                error => #{
                    code => <<"INTERNAL_ERROR">>,
                    message => <<"Internal server error">>
                }
            },
            {error, jsx:encode(CatchErrorResponse)}
    end.

