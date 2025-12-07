%% @doc Intake Error Codes
%% Defines standardized error codes for Router intake validation failures
%% CP2+: Error code classification for intake validation layer
-module(router_intake_error_codes).

-export([error_code_to_string/1, error_code_severity/1, error_code_message/2]).

%% Error code types
-type error_code() :: 
    schema_validation_failed |
    version_unsupported |
    correlation_fields_invalid |
    tenant_forbidden |
    idempotency_violation |
    payload_too_small |
    internal_validation_error.

-export_type([error_code/0]).

%% @doc Convert error code to machine-readable string
-spec error_code_to_string(error_code()) -> binary().
error_code_to_string(schema_validation_failed) -> <<"SCHEMA_VALIDATION_FAILED">>;
error_code_to_string(version_unsupported) -> <<"VERSION_UNSUPPORTED">>;
error_code_to_string(correlation_fields_invalid) -> <<"CORRELATION_FIELDS_INVALID">>;
error_code_to_string(tenant_forbidden) -> <<"TENANT_FORBIDDEN">>;
error_code_to_string(idempotency_violation) -> <<"IDEMPOTENCY_VIOLATION">>;
error_code_to_string(payload_too_small) -> <<"PAYLOAD_TOO_SMALL">>;
error_code_to_string(internal_validation_error) -> <<"INTERNAL_VALIDATION_ERROR">>.

%% @doc Get severity for error code
-spec error_code_severity(error_code()) -> warn | error.
error_code_severity(idempotency_violation) -> warn;
error_code_severity(payload_too_small) -> warn;  %% Abuse detection - warn level
error_code_severity(_) -> error.

%% @doc Create human-readable error message
-spec error_code_message(error_code(), map()) -> binary().
error_code_message(schema_validation_failed, Context) ->
    Reason = maps:get(<<"reason">>, Context, maps:get(reason, Context, <<"unknown">>)),
    <<"Schema validation failed: ", Reason/binary>>;
error_code_message(version_unsupported, Context) ->
    Version = maps:get(<<"version">>, Context, maps:get(version, Context, <<"unknown">>)),
    Supported = maps:get(<<"supported_versions">>, Context, maps:get(supported_versions, Context, [<<"1">>])),
    SupportedList = case Supported of
        List when is_list(List) -> List;
        _ -> [<<"1">>]
    end,
    SupportedStr = string:join([binary_to_list(V) || V <- SupportedList], ", "),
    <<"Unsupported schema version: ", Version/binary, ", supported versions: [", (list_to_binary(SupportedStr))/binary, "]">>;
error_code_message(correlation_fields_invalid, Context) ->
    Reason = maps:get(<<"reason">>, Context, maps:get(reason, Context, <<"unknown">>)),
    <<"Correlation fields validation failed: ", Reason/binary>>;
error_code_message(tenant_forbidden, Context) ->
    Reason = maps:get(<<"reason">>, Context, maps:get(reason, Context, <<"unknown">>)),
    <<"Tenant validation failed: ", Reason/binary>>;
error_code_message(idempotency_violation, Context) ->
    Reason = maps:get(<<"reason">>, Context, maps:get(reason, Context, <<"unknown">>)),
    <<"Idempotency violation: ", Reason/binary>>;
error_code_message(payload_too_small, Context) ->
    PayloadSize = maps:get(<<"payload_size">>, Context, maps:get(payload_size, Context, <<"unknown">>)),
    MinPayloadSize = maps:get(<<"min_payload_size">>, Context, maps:get(min_payload_size, Context, <<"10">>)),
    PayloadSizeBin = case true of
        _ when is_binary(PayloadSize) -> PayloadSize;
        _ when is_integer(PayloadSize) -> integer_to_binary(PayloadSize);
        _ -> <<"unknown">>
    end,
    MinPayloadSizeBin = case true of
        _ when is_binary(MinPayloadSize) -> MinPayloadSize;
        _ when is_integer(MinPayloadSize) -> integer_to_binary(MinPayloadSize);
        _ -> <<"10">>
    end,
    <<"Payload is too small or empty: ", PayloadSizeBin/binary, " bytes, minimum: ", MinPayloadSizeBin/binary, " bytes">>;
error_code_message(internal_validation_error, Context) ->
    Reason = maps:get(<<"reason">>, Context, maps:get(reason, Context, <<"unknown">>)),
    <<"Internal validation error: ", Reason/binary>>.

