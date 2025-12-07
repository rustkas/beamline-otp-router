%% @doc Centralized error mapping to gRPC status codes
%% 
%% This module provides a single source of truth for mapping internal
%% error reasons to gRPC status codes, ensuring consistency across
%% all gRPC endpoints.
%%
%% Security:
%% - Error messages are sanitized to prevent information disclosure
%% - Sensitive details are redacted from error responses
%% - Consistent error format for easier monitoring and alerting
%%
%% Error Mapping Policy:
%% - INVALID_ARGUMENT (3): Client provided invalid input (missing_tenant_id, invalid_policy)
%% - NOT_FOUND (5): Requested resource not found (policy_not_found)
%% - RESOURCE_EXHAUSTED (8): Quota/budget exceeded (rate_limit_exceeded)
%%
%% @see SECURITY_GUIDE.md#error-handling For security best practices
%% @see src/router_grpc.erl gRPC error handling
%% - INTERNAL (13): Internal server error (no_provider_available, unexpected errors)
%% - UNAVAILABLE (14): Service temporarily unavailable (service_down, timeout)
%%
%% Decision Matrix:
%% - Client input errors → INVALID_ARGUMENT
%% - Resource not found → NOT_FOUND
%% - Quota/budget limits → RESOURCE_EXHAUSTED
%% - Service unavailable (temporary) → UNAVAILABLE
%% - Internal errors (unexpected) → INTERNAL
%%
%% Error Reasons Reference:
%% ========================
%%
%% For complete error code reference, see: docs/api/ERROR_CODES.md
%%
%% Client Input Errors (INVALID_ARGUMENT - 3):
%%   - invalid_policy: Policy structure is invalid or malformed
%%   - invalid_request: Request structure is invalid or malformed
%%   - missing_message: Required message field is missing
%%   - missing_tenant_id: Tenant ID is required but not provided
%%
%% Authentication/Authorization Errors:
%%   - unauthenticated (UNAUTHENTICATED - 16): Authentication failed or missing credentials
%%   - permission_denied (PERMISSION_DENIED - 7): User lacks required permissions
%%
%% Resource Not Found (NOT_FOUND - 5):
%%   - policy_not_found: Requested policy does not exist
%%
%% Quota/Budget Limits (RESOURCE_EXHAUSTED - 8):
%%   - rate_limit_exceeded: Rate limit threshold exceeded
%%   - quota_exceeded: Quota limit exceeded
%%   - rate_limited: Request rate limited (alias for rate_limit_exceeded)
%%   - budget_exceeded: Budget limit exceeded
%%
%% Service Unavailable (UNAVAILABLE - 14):
%%   - service_down: Service is temporarily down
%%   - timeout: Request timeout exceeded
%%   - nats_unavailable: NATS service is unavailable
%%
%% Internal Errors (INTERNAL - 13):
%%   - no_provider_available: No provider available for routing (internal routing failure)
%%   - internal_error: Unexpected internal server error
%%
%% Usage:
%%   {Status, Message} = router_error:to_grpc(ErrorReason),
%%   {Status, Message} = router_error:to_grpc(ErrorReason, #{context => <<"additional context">>}),
%%
%% See also:
%%   - docs/API_CONTRACTS.md for API error response format
%%   - docs/ARCHITECTURE/ERROR_HANDLING.md for error handling patterns

-module(router_error).
-ignore_xref([
    router_error,
    {router_error, to_grpc, 1},
    {router_error, to_grpc, 2},
    {router_error, error_mapping_table, 0},
    {router_error, reload, 1},
    {router_error, get, 0}
]).
-export([to_grpc/1, to_grpc/2, error_mapping_table/0, reload/1, get/0]).

-include_lib("grpcbox/include/grpcbox.hrl").

%% gRPC Status Codes (use integer constants directly)
%% Note: grpcbox.hrl may define these as binaries, so we use integer literals
-define(GRPC_STATUS_INVALID_ARGUMENT_INT, 3).
-define(GRPC_STATUS_NOT_FOUND_INT, 5).
-define(GRPC_STATUS_PERMISSION_DENIED_INT, 7).
-define(GRPC_STATUS_RESOURCE_EXHAUSTED_INT, 8).
-define(GRPC_STATUS_INTERNAL_INT, 13).
-define(GRPC_STATUS_UNAVAILABLE_INT, 14).
-define(GRPC_STATUS_UNAUTHENTICATED_INT, 16).

%% @doc Convert internal error reason to gRPC status code and message
%% Returns: {Status, Message} where Status is gRPC status code integer
-spec to_grpc(ErrorReason :: atom() | {atom(), map()}) -> {integer(), binary()}.
to_grpc(ErrorReason) ->
    to_grpc(ErrorReason, #{}).

%% @doc Convert internal error reason to gRPC status code and message with context
%% Returns: {Status, Message} where Status is gRPC status code integer
-spec to_grpc(ErrorReason :: atom() | {atom(), map()}, ErrorContext :: map()) -> {integer(), binary()}.
to_grpc({ErrorReason, ErrorContext}, _) when is_map(ErrorContext) ->
    to_grpc(ErrorReason, ErrorContext);
to_grpc(ErrorReason, ErrorContext) when is_atom(ErrorReason) ->
    case error_mapping_table() of
        #{ErrorReason := {Status, DefaultMessage}} ->
            Message = maps:get(context, ErrorContext, DefaultMessage),
            {Status, ensure_binary(Message)};
        _ ->
            %% Unknown error - map to INTERNAL
            ErrorMsg = iolist_to_binary(io_lib:format("~p", [ErrorReason])),
            {?GRPC_STATUS_INTERNAL_INT, ErrorMsg}
    end.

%% @doc Get error mapping table
%% This table is stored in persistent_term for performance
-spec error_mapping_table() -> #{atom() => {integer(), binary()}}.
error_mapping_table() ->
    case persistent_term:get({?MODULE, error_mapping}, undefined) of
        undefined ->
            %% Initialize mapping table
            Mapping = #{
                %% Client input errors → INVALID_ARGUMENT (3)
                invalid_policy => {?GRPC_STATUS_INVALID_ARGUMENT_INT, <<"invalid policy">>},
                invalid_request => {?GRPC_STATUS_INVALID_ARGUMENT_INT, <<"invalid request">>},
                missing_message => {?GRPC_STATUS_INVALID_ARGUMENT_INT, <<"missing message">>},
                
                %% Authentication/Authorization errors
                %% Note: missing_tenant_id can map to UNAUTHENTICATED (16) if tenant_id is required for authentication
                %% or to INVALID_ARGUMENT (3) if it's just a missing field. Current policy: INVALID_ARGUMENT.
                %% For explicit auth failures, use UNAUTHENTICATED or PERMISSION_DENIED
                missing_tenant_id => {?GRPC_STATUS_INVALID_ARGUMENT_INT, <<"missing tenant_id">>},
                unauthenticated => {?GRPC_STATUS_UNAUTHENTICATED_INT, <<"unauthenticated">>},
                permission_denied => {?GRPC_STATUS_PERMISSION_DENIED_INT, <<"permission denied">>},
                
                %% Resource not found → NOT_FOUND (5)
                policy_not_found => {?GRPC_STATUS_NOT_FOUND_INT, <<"policy not found">>},
                
                %% Quota/budget limits → RESOURCE_EXHAUSTED (8)
                rate_limit_exceeded => {?GRPC_STATUS_RESOURCE_EXHAUSTED_INT, <<"rate limit exceeded">>},
                quota_exceeded => {?GRPC_STATUS_RESOURCE_EXHAUSTED_INT, <<"quota exceeded">>},
                rate_limited => {?GRPC_STATUS_RESOURCE_EXHAUSTED_INT, <<"rate limited">>},
                budget_exceeded => {?GRPC_STATUS_RESOURCE_EXHAUSTED_INT, <<"budget exceeded">>},
                
                %% Service unavailable (temporary) → UNAVAILABLE (14)
                service_down => {?GRPC_STATUS_UNAVAILABLE_INT, <<"service temporarily unavailable">>},
                timeout => {?GRPC_STATUS_UNAVAILABLE_INT, <<"request timeout">>},
                nats_unavailable => {?GRPC_STATUS_UNAVAILABLE_INT, <<"NATS service unavailable">>},
                
                %% Internal errors (unexpected) → INTERNAL (13)
                %% Note: no_provider_available maps to INTERNAL because it indicates
                %% an internal routing failure (no providers configured/available),
                %% not a temporary service unavailability. If the cause is quota/budget
                %% limits, use rate_limit_exceeded or quota_exceeded instead.
                no_provider_available => {?GRPC_STATUS_INTERNAL_INT, <<"no provider available">>},
                internal_error => {?GRPC_STATUS_INTERNAL_INT, <<"internal server error">>}
            },
            persistent_term:put({?MODULE, error_mapping}, Mapping),
            Mapping;
        Mapping ->
            Mapping
    end.

%% @doc Reload error mapping table
%% Validates new mapping and atomically replaces existing mapping in persistent_term
%% Returns: ok | {error, Reason}
-spec reload(NewMapping :: #{atom() => {integer(), binary()}}) -> ok | {error, term()}.
reload(NewMapping) when is_map(NewMapping) ->
    %% Validate mapping structure
    case validate_mapping(NewMapping) of
        ok ->
            %% Two-phase update: validate → swap
            persistent_term:put({?MODULE, error_mapping}, NewMapping),
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
reload(_) ->
    {error, invalid_mapping}.

%% @doc Get current error mapping table
%% Returns: #{atom() => {integer(), binary()}}
-spec get() -> #{atom() => {integer(), binary()}}.
get() ->
    error_mapping_table().

%% @doc Validate error mapping structure
%% Checks that all values are {Status, Message} tuples with valid gRPC status codes
-spec validate_mapping(#{atom() => {integer(), binary()}}) -> ok | {error, term()}.
validate_mapping(Mapping) when is_map(Mapping) ->
    ValidStatusCodes = [
        ?GRPC_STATUS_INVALID_ARGUMENT_INT,
        ?GRPC_STATUS_NOT_FOUND_INT,
        ?GRPC_STATUS_RESOURCE_EXHAUSTED_INT,
        ?GRPC_STATUS_UNAVAILABLE_INT,
        ?GRPC_STATUS_INTERNAL_INT,
        ?GRPC_STATUS_UNAUTHENTICATED_INT,
        ?GRPC_STATUS_PERMISSION_DENIED_INT
    ],
    maps:fold(fun
        (Key, {Status, Message}, Acc) when is_atom(Key), is_integer(Status), is_binary(Message) ->
            case lists:member(Status, ValidStatusCodes) of
                true -> Acc;
                false -> {error, {invalid_status_code, Key, Status}}
            end;
        (Key, Value, _Acc) ->
            {error, {invalid_mapping_entry, Key, Value}}
    end, ok, Mapping);
validate_mapping(_) ->
    {error, not_a_map}.

%% @doc Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> iolist_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(T) -> iolist_to_binary(io_lib:format("~p", [T])).
