%% @doc Configuration Validation Module
%%
%% Provides configuration validation, templates, and validation procedures.
%% Ensures all configuration is valid before deployment and runtime.
%%
%% @see DEPLOYMENT_GUIDE.md#configuration For configuration procedures
-module(router_config_validator).

-export([
    validate_config/0,
    validate_config/1,
    validate_config_value/2,
    get_config_template/0,
    get_config_template/1,
    check_required_config/0,
    check_config_compatibility/1
]).

-include("beamline_router.hrl").

%% @doc Validate all configuration
-spec validate_config() -> {ok, map()} | {error, term()}.
validate_config() ->
    validate_config(#{}).

%% @doc Validate configuration with options
%% @param Options Map with validation options
-spec validate_config(map()) -> {ok, map()} | {error, term()}.
validate_config(_) ->
    try
        %% Get all configuration
        AllConfig = get_all_config(),
        
        %% Validate required configuration
        RequiredCheck = check_required_config(),
        
        %% Validate each configuration value
        ValidationResults = maps:fold(fun
            (Key, Value, Acc) ->
                case validate_config_value(Key, Value) of
                    {ok, Valid} ->
                        maps:put(Key, #{valid => true, value => Valid}, Acc);
                    {error, Reason} ->
                        maps:put(Key, #{valid => false, error => Reason}, Acc)
                end
        end, #{}, AllConfig),
        
        %% Check if all validations passed
        AllValid = maps:fold(fun
            (_Key, Result, Acc) ->
                maps:get(valid, Result, false) andalso Acc
        end, true, ValidationResults),
        
        %% Check configuration compatibility
        CompatibilityCheck = check_config_compatibility(AllConfig),
        
        Report = #{
            required_config => RequiredCheck,
            validation_results => ValidationResults,
            compatibility => CompatibilityCheck,
            all_valid => AllValid andalso maps:get(passed, RequiredCheck, false) andalso
                        maps:get(passed, CompatibilityCheck, false),
            timestamp => erlang:system_time(second)
        },
        
        case maps:get(all_valid, Report, false) of
            true ->
                {ok, Report};
            false ->
                {error, {validation_failed, Report}}
        end
    catch
        Class:Reason:Stack ->
            {error, {validation_exception, Class, Reason, Stack}}
    end.

%% @doc Validate a specific configuration value
%% @param Key Atom configuration key
%% @param Value Configuration value
-spec validate_config_value(atom(), term()) -> {ok, term()} | {error, term()}.
validate_config_value(grpc_enabled, Value) when is_boolean(Value) ->
    {ok, Value};
validate_config_value(grpc_enabled, _) ->
    {error, must_be_boolean};

validate_config_value(grpc_port, Value) when is_integer(Value), Value > 0, Value =< 65535 ->
    {ok, Value};
validate_config_value(grpc_port, _) ->
    {error, must_be_port_number};

validate_config_value(admin_api_key, undefined) ->
    {ok, undefined};
validate_config_value(admin_api_key, Value) when is_binary(Value), byte_size(Value) >= 8 ->
    {ok, Value};
validate_config_value(admin_api_key, _) ->
    {error, must_be_binary_min_8_chars};

validate_config_value(nats_mode, Value) when Value =:= mock; Value =:= real ->
    {ok, Value};
validate_config_value(nats_mode, _) ->
    {error, must_be_mock_or_real};

validate_config_value(audit_retention_days, Value) when is_integer(Value), Value > 0 ->
    {ok, Value};
validate_config_value(audit_retention_days, _) ->
    {error, must_be_positive_integer};

validate_config_value(rbac_enabled, Value) when is_boolean(Value) ->
    {ok, Value};
validate_config_value(rbac_enabled, _) ->
    {error, must_be_boolean};

validate_config_value(tracing_enabled, Value) when is_boolean(Value) ->
    {ok, Value};
validate_config_value(tracing_enabled, _) ->
    {error, must_be_boolean};

validate_config_value(idempotency_enabled, Value) when is_boolean(Value) ->
    {ok, Value};
validate_config_value(idempotency_enabled, _) ->
    {error, must_be_boolean};

validate_config_value(telemetry_enabled, Value) when is_boolean(Value) ->
    {ok, Value};
validate_config_value(telemetry_enabled, _) ->
    {error, must_be_boolean};

validate_config_value(_Key, Value) ->
    %% Unknown key - allow it but don't validate
    {ok, Value}.

%% @doc Get configuration template
-spec get_config_template() -> map().
get_config_template() ->
    get_config_template(#{}).

%% @doc Get configuration template with options
%% @param Options Map with template options
-spec get_config_template(map()) -> map().
get_config_template(Options) ->
    BaseTemplate = #{
        grpc_enabled => false,
        grpc_port => 9000,
        admin_api_key => <<"CHANGE_ME">>,
        nats_mode => mock,
        audit_retention_days => 90,
        rbac_enabled => true,
        tracing_enabled => true,
        idempotency_enabled => true,
        telemetry_enabled => true,
        log_level => info,
        metrics_export_enabled => false
    },
    
    %% Apply options
    maps:merge(BaseTemplate, Options).

%% @doc Check required configuration
-spec check_required_config() -> map().
check_required_config() ->
    RequiredKeys = [
        grpc_enabled,
        nats_mode
    ],
    
    Results = lists:foldl(fun
        (Key, Acc) ->
            Value = application:get_env(beamline_router, Key, undefined),
            Present = Value =/= undefined,
            maps:put(Key, #{present => Present, value => Value}, Acc)
    end, #{}, RequiredKeys),
    
    AllPresent = maps:fold(fun
        (_Key, Result, Acc) ->
            maps:get(present, Result, false) andalso Acc
    end, true, Results),
    
    #{
        required_keys => RequiredKeys,
        results => Results,
        passed => AllPresent
    }.

%% @doc Check configuration compatibility
%% @param Config Map of configuration
-spec check_config_compatibility(map()) -> map().
check_config_compatibility(Config) ->
    try
        %% Check feature dependencies
        GrpcEnabled = maps:get(grpc_enabled, Config, false),
        AdminGrpcEnabled = maps:get(admin_grpc_enabled, Config, true),
        
        %% Admin gRPC requires gRPC to be enabled
        AdminGrpcCompatible = case AdminGrpcEnabled of
            true when GrpcEnabled =:= false ->
                false;
            _ ->
                true
        end,
        
        %% Check metrics dependencies
        MetricsEnabled = maps:get(metrics_export_enabled, Config, false),
        TelemetryEnabled = maps:get(telemetry_enabled, Config, true),
        
        %% Metrics export requires telemetry
        MetricsCompatible = case MetricsEnabled of
            true when TelemetryEnabled =:= false ->
                false;
            _ ->
                true
        end,
        
        %% Check CP2+ dependencies
        IdempotencyEnabled = maps:get(idempotency_enabled, Config, true),
        AckEnabled = maps:get(ack_enabled, Config, false),
        Cp2PlusAllowed = maps:get(cp2_plus_allowed, Config, false),
        
        %% CP2+ features require cp2_plus_allowed
        Cp2Compatible = case (IdempotencyEnabled orelse AckEnabled) of
            true when Cp2PlusAllowed =:= false ->
                false;
            _ ->
                true
        end,
        
        AllCompatible = AdminGrpcCompatible andalso MetricsCompatible andalso Cp2Compatible,
        
        #{
            admin_grpc => #{compatible => AdminGrpcCompatible},
            metrics => #{compatible => MetricsCompatible},
            cp2_features => #{compatible => Cp2Compatible},
            passed => AllCompatible
        }
    catch
        _:_ ->
            #{passed => false, error => compatibility_check_failed}
    end.

%% Internal: Get all configuration
-spec get_all_config() -> map().
get_all_config() ->
    %% Get all application environment variables
    AllEnv = application:get_all_env(beamline_router),
    maps:from_list(AllEnv).

