%% @doc Policy Validator
%% Validates policy JSON against JSON Schema
%% CP1: Basic validation using jsx for JSON parsing
-module(router_policy_validator).
-export([validate/1, validate_schema/1]).

-include("beamline_router.hrl").

%% @doc Validate policy structure
validate(PolicyMap) when is_map(PolicyMap) ->
    %% Check required fields
    case maps:is_key(<<"policy_id">>, PolicyMap) andalso
         maps:is_key(<<"tenant_id">>, PolicyMap) andalso
         maps:is_key(<<"weights">>, PolicyMap) of
        false ->
            {error, {invalid_policy, #{
                context => <<"Missing required fields: policy_id, tenant_id, or weights">>,
                missing_fields => get_missing_fields(PolicyMap)
            }}};
        true ->
            %% Validate policy_id
            PolicyId = maps:get(<<"policy_id">>, PolicyMap),
            case validate_policy_id(PolicyId) of
                {error, Reason} ->
                    {error, {invalid_policy, #{
                        context => <<"Invalid policy_id">>,
                        policy_id => PolicyId,
                        reason => Reason
                    }}};
                ok ->
                    %% Validate tenant_id
                    TenantId = maps:get(<<"tenant_id">>, PolicyMap),
                    case validate_tenant_id(TenantId) of
                        {error, Reason} ->
                            {error, {invalid_policy, #{
                                context => <<"Invalid tenant_id">>,
                                tenant_id => TenantId,
                                reason => Reason
                            }}};
                        ok ->
                            %% Validate weights
                            Weights = maps:get(<<"weights">>, PolicyMap),
                            case validate_weights(Weights) of
                                {error, Reason} ->
                                    {error, {invalid_policy, #{
                                        context => <<"Invalid weights">>,
                                        weights => Weights,
                                        reason => Reason
                                    }}};
                                ok ->
                                    %% Validate sticky if present
                                    case maps:get(<<"sticky">>, PolicyMap, undefined) of
                                        undefined ->
                                            ok;
                                        Sticky ->
                                            validate_sticky(Sticky, PolicyMap)
                                    end
                            end
                    end
            end
    end;
validate(_) ->
    {error, {invalid_policy, #{
        context => <<"Policy must be a map">>
    }}}.

%% @doc Validate against JSON Schema (stub for CP1)
%% Full JSON Schema validation requires additional library
validate_schema(PolicyMap) ->
    case validate(PolicyMap) of
        {error, _} = Err -> Err;
        ok ->
            case maybe_validate_jsonschema(PolicyMap) of
                ok -> ok;
                {error, Reasons} -> {error, {schema_validation_failed, Reasons}}
            end
    end.

maybe_validate_jsonschema(PolicyMap) ->
    try
        %% Load schema from priv/schemas/policy.schema.json
        SchemaBin = case code:priv_dir(beamline_router) of
            {error, bad_name} -> file:read_file(filename:join([code:lib_dir(beamline_router), "..", "priv", "schemas", "policy.schema.json"]));
            Dir -> file:read_file(filename:join([Dir, "schemas", "policy.schema.json"]))
        end,
        case SchemaBin of
            {ok, Bin} ->
                Schema = jsx:decode(Bin, [return_maps]),
                %% jesse returns ok | {error, [{Path, Reason}...]}
                case jesse:validate_with_schema(Schema, PolicyMap) of
                    ok -> ok;
                    {error, Reasons} -> {error, Reasons}
                end;
            {error, Reason} -> {error, {schema_read_failed, Reason}}
        end
    catch
        Class:CatchReason:Stack ->
            router_logger:error(<<"JSON Schema validation exception">>, #{
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(CatchReason),
                <<"stack_depth">> => length(Stack),
                <<"event">> => <<"schema_validation_exception">>
            }),
            {error, schema_exception}
    end.

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

%% Internal: Get missing required fields
get_missing_fields(PolicyMap) ->
    Required = [<<"policy_id">>, <<"tenant_id">>, <<"weights">>],
    Missing = [Field || Field <- Required, not maps:is_key(Field, PolicyMap)],
    Missing.

%% Internal: Validate policy_id
%% Uses router_security_validator for comprehensive security validation
validate_policy_id(<<>>) ->
    {error, <<"policy_id cannot be empty">>};
validate_policy_id(PolicyId) when is_binary(PolicyId) ->
    %% Use security validator if available, fallback to basic validation
    case erlang:function_exported(router_security_validator, validate_policy_id, 1) of
        true ->
            case router_security_validator:validate_policy_id(PolicyId) of
                {ok, _} -> ok;
                {error, Reason} ->
                    ReasonBin = case Reason of
                        policy_id_required -> <<"policy_id is required">>;
                        policy_id_empty -> <<"policy_id cannot be empty">>;
                        invalid_length -> <<"policy_id must be 1-255 characters">>;
                        invalid_format -> <<"policy_id contains invalid characters">>;
                        security_pattern_detected -> <<"policy_id contains security pattern">>;
                        _ -> <<"policy_id validation failed">>
                    end,
                    {error, ReasonBin}
            end;
        false ->
            %% Fallback: basic validation
            case byte_size(PolicyId) of
                Size when Size > 0, Size =< 255 ->
                    ok;
                _ ->
                    {error, <<"policy_id must be 1-255 characters">>}
            end
    end;
validate_policy_id(_) ->
    {error, <<"policy_id must be a string">>}.

%% Internal: Validate tenant_id
%% Uses router_security_validator for comprehensive security validation
validate_tenant_id(<<>>) ->
    {error, <<"tenant_id cannot be empty">>};
validate_tenant_id(TenantId) when is_binary(TenantId) ->
    %% Use security validator if available, fallback to basic validation
    case erlang:function_exported(router_security_validator, validate_tenant_id, 1) of
        true ->
            case router_security_validator:validate_tenant_id(TenantId) of
                {ok, _} -> ok;
                {error, Reason} ->
                    ReasonBin = case Reason of
                        tenant_id_required -> <<"tenant_id is required">>;
                        tenant_id_empty -> <<"tenant_id cannot be empty">>;
                        invalid_length -> <<"tenant_id must be 1-255 characters">>;
                        invalid_format -> <<"tenant_id contains invalid characters">>;
                        security_pattern_detected -> <<"tenant_id contains security pattern">>;
                        _ -> <<"tenant_id validation failed">>
                    end,
                    {error, ReasonBin}
            end;
        false ->
            %% Fallback: basic validation
            case byte_size(TenantId) of
                Size when Size > 0, Size =< 255 ->
                    ok;
                _ ->
                    {error, <<"tenant_id must be 1-255 characters">>}
            end
    end;
validate_tenant_id(_) ->
    {error, <<"tenant_id must be a string">>}.

%% Internal: Validate weights
validate_weights(Weights) when is_map(Weights) ->
    case map_size(Weights) of
        0 ->
            {error, <<"weights cannot be empty">>};
        _ ->
            validate_weights_values(Weights)
    end;
validate_weights(_) ->
    {error, <<"weights must be an object">>}.

%% Internal: Validate weight values
validate_weights_values(Weights) ->
    Providers = maps:keys(Weights),
    case validate_weight_values(Providers, Weights) of
        {error, _} = Error ->
            Error;
        ok ->
            %% Check total weight strictly greater than 0
            TotalWeight = lists:sum([maps:get(P, Weights) || P <- Providers]),
            if
                TotalWeight > 0.0 ->
                    ok;
                true ->
                    {error, <<"Total weight must be greater than 0">>}
            end
    end.

%% Internal: Validate individual weight values
validate_weight_values([Provider | Rest], Weights) ->
    Weight = maps:get(Provider, Weights),
    %% Enforce normalized weights in [0.0, 1.0]
    case is_number(Weight) andalso Weight >= 0.0 andalso Weight =< 1.0 of
        true ->
            validate_weight_values(Rest, Weights);
        false ->
            {error, #{
                provider => Provider,
                weight => Weight,
                reason => <<"Weight must be a number between 0.0 and 1.0">>
            }}
    end;
validate_weight_values([], _) ->
    ok.

%% Internal: Validate sticky configuration
validate_sticky(Sticky, _PolicyMap) when is_map(Sticky) ->
    case maps:get(<<"enabled">>, Sticky, undefined) of
        undefined ->
            {error, {invalid_policy, #{
                context => <<"sticky.enabled is required">>
            }}};
        Enabled when is_boolean(Enabled) ->
            case maps:get(<<"session_key">>, Sticky, <<"session_id">>) of
                SessionKey when is_binary(SessionKey) ->
                    case byte_size(SessionKey) of
                        Size when Size > 0, Size =< 255 ->
                            ok;
                        _ ->
                            {error, {invalid_policy, #{
                                context => <<"sticky.session_key must be 1-255 characters">>
                            }}}
                    end;
                _ ->
                    {error, {invalid_policy, #{
                        context => <<"sticky.session_key must be a string">>
                    }}}
            end;
        _ ->
            {error, {invalid_policy, #{
                context => <<"sticky.enabled must be a boolean">>
            }}}
    end;
validate_sticky(_, _) ->
    {error, {invalid_policy, #{
        context => <<"sticky must be an object">>
    }}}.
