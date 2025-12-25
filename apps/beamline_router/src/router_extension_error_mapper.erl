-module(router_extension_error_mapper).

-doc "Extension Error Mapper".
%% Maps extension-related errors to Gateway-compatible error codes and messages
%% CP2-LC: Unified error mapping for extensions pipeline
-export([map_extension_error/1, map_extension_error_to_code/1]).

-include("beamline_router.hrl").

%% Input: {error, {Reason, Metadata}} where Reason is extension error atom
%% Returns: {ErrorCode, ErrorMessage, Details}
%% ErrorCode: binary() - Gateway-compatible error code
%% ErrorMessage: binary() - Human-readable error message
%% Details: map() - Additional error details
-spec map_extension_error({error, {atom(), map()}}) -> {binary(), binary(), map()}.
map_extension_error({error, {Reason, Metadata}}) ->
    ErrorCode = map_extension_error_to_code(Reason),
    ErrorMessage = map_extension_error_to_message(Reason, Metadata),
    Details = build_error_details(Reason, Metadata),
    {ErrorCode, ErrorMessage, Details}.

%% Returns: binary() - Gateway-compatible error code
-spec map_extension_error_to_code(atom()) -> binary().
map_extension_error_to_code(extension_not_found) ->
    ~"extension_not_found";
map_extension_error_to_code(extension_timeout) ->
    ~"extension_timeout";
map_extension_error_to_code(validator_blocked) ->
    ~"validator_blocked";
map_extension_error_to_code(post_processor_failed) ->
    ~"post_processor_failed";
map_extension_error_to_code(extension_circuit_open) ->
    ~"extension_unavailable";
map_extension_error_to_code(extension_invocation_error) ->
    ~"extension_error";
map_extension_error_to_code(extension_max_retries_exceeded) ->
    ~"extension_timeout";
map_extension_error_to_code(extension_registry_error) ->
    ~"extension_error";
map_extension_error_to_code(extension_load_balancer_error) ->
    ~"extension_error";
map_extension_error_to_code(pipeline_too_deep) ->
    ~"invalid_request";
map_extension_error_to_code(too_many_pre_processors) ->
    ~"invalid_request";
map_extension_error_to_code(too_many_validators) ->
    ~"invalid_request";
map_extension_error_to_code(too_many_post_processors) ->
    ~"invalid_request";
map_extension_error_to_code(_UnknownReason) ->
    ~"extension_error".

%% Returns: binary() - Human-readable error message
-spec map_extension_error_to_message(atom(), map()) -> binary().
map_extension_error_to_message(extension_not_found, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    <<"Extension not found: ", ExtensionId/binary>>;
map_extension_error_to_message(extension_timeout, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    Retries = maps:get(retries, Metadata, 0),
    case Retries > 0 of
        true ->
            RetriesBin = integer_to_binary(Retries),
            <<"Extension timeout after ", RetriesBin/binary, " retries: ", ExtensionId/binary>>;
        false ->
            <<"Extension timeout: ", ExtensionId/binary>>
    end;
map_extension_error_to_message(validator_blocked, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    Reason = maps:get(reason, Metadata, ~"Validation failed"),
    <<"Validator blocked request: ", ExtensionId/binary, " - ", Reason/binary>>;
map_extension_error_to_message(post_processor_failed, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    Reason = maps:get(reason, Metadata, ~"Post-processing failed"),
    <<"Post-processor failed: ", ExtensionId/binary, " - ", Reason/binary>>;
map_extension_error_to_message(extension_circuit_open, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    <<"Extension unavailable (circuit breaker open): ", ExtensionId/binary>>;
map_extension_error_to_message(extension_invocation_error, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    Reason = maps:get(reason, Metadata, ~"Invocation failed"),
    <<"Extension invocation error: ", ExtensionId/binary, " - ", Reason/binary>>;
map_extension_error_to_message(extension_max_retries_exceeded, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    MaxRetries = maps:get(max_retries, Metadata, 0),
    MaxRetriesBin = integer_to_binary(MaxRetries),
    <<"Extension max retries exceeded (", MaxRetriesBin/binary, "): ", ExtensionId/binary>>;
map_extension_error_to_message(extension_registry_error, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    Reason = maps:get(reason, Metadata, ~"Registry error"),
    <<"Extension registry error: ", ExtensionId/binary, " - ", Reason/binary>>;
map_extension_error_to_message(extension_load_balancer_error, Metadata) ->
    ExtensionId = maps:get(extension_id, Metadata, ~"unknown"),
    Reason = maps:get(reason, Metadata, ~"Load balancer error"),
    <<"Extension load balancer error: ", ExtensionId/binary, " - ", Reason/binary>>;
map_extension_error_to_message(pipeline_too_deep, Metadata) ->
    Depth = maps:get(depth, Metadata, 0),
    MaxDepth = maps:get(max_depth, Metadata, 10),
    DepthBin = integer_to_binary(Depth),
    MaxDepthBin = integer_to_binary(MaxDepth),
    <<"Extension pipeline too deep: ", DepthBin/binary, " > ", MaxDepthBin/binary>>;
map_extension_error_to_message(too_many_pre_processors, Metadata) ->
    Count = maps:get(count, Metadata, 0),
    MaxCount = maps:get(max_count, Metadata, 5),
    CountBin = integer_to_binary(Count),
    MaxCountBin = integer_to_binary(MaxCount),
    <<"Too many pre-processors: ", CountBin/binary, " > ", MaxCountBin/binary>>;
map_extension_error_to_message(too_many_validators, Metadata) ->
    Count = maps:get(count, Metadata, 0),
    MaxCount = maps:get(max_count, Metadata, 5),
    CountBin = integer_to_binary(Count),
    MaxCountBin = integer_to_binary(MaxCount),
    <<"Too many validators: ", CountBin/binary, " > ", MaxCountBin/binary>>;
map_extension_error_to_message(too_many_post_processors, Metadata) ->
    Count = maps:get(count, Metadata, 0),
    MaxCount = maps:get(max_count, Metadata, 5),
    CountBin = integer_to_binary(Count),
    MaxCountBin = integer_to_binary(MaxCount),
    <<"Too many post-processors: ", CountBin/binary, " > ", MaxCountBin/binary>>;
map_extension_error_to_message(_UnknownReason, Metadata) ->
    Reason = maps:get(reason, Metadata, ~"Unknown extension error"),
    <<"Extension error: ", Reason/binary>>.

%% Internal: Build error details map
-spec build_error_details(atom(), map()) -> map().
build_error_details(Reason, Metadata) ->
    %% Extract relevant fields from metadata
    Details = maps:with([
        extension_id,
        policy_id,
        tenant_id,
        retries,
        max_retries,
        timeout_ms,
        depth,
        max_depth,
        count,
        max_count,
        context
    ], Metadata),
    
    %% Add error type
    DetailsWithType = maps:put(~"error_type", atom_to_binary(Reason, utf8), Details),
    
    %% Add timestamp (ISO 8601 format)
    Timestamp = format_timestamp(erlang:system_time(second)),
    maps:put(~"timestamp", Timestamp, DetailsWithType).

%% Internal: Format timestamp as ISO 8601 string
-spec format_timestamp(integer()) -> binary().
format_timestamp(UnixSeconds) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(UnixSeconds + 62167219200),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Year, Month, Day, Hour, Min, Sec])).

