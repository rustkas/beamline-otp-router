%% @doc Error Handling Pattern Template
%%
%% This is a template for implementing standardized error handling in router modules.
%% Copy this file and customize it for your module.
%%
%% Pattern: Standardized error handling with consistent return values and error mapping
%%
%% Error Handling Principles:
%% 1. Always return {error, Reason} for errors
%% 2. Use router_error:to_grpc/1 for gRPC error mapping
%% 3. Log errors with router_logger
%% 4. Sanitize error messages to prevent information disclosure
%% 5. Use try-catch for error recovery where appropriate
%%
%% Reference implementations:
%% - router_error.erl (error mapping to gRPC status codes)
%% - router_rbac.erl (error handling with try-catch)
%% - router_core.erl (error normalization)
%%
%% @see DESIGN_PATTERNS.md#error-handling For error handling pattern documentation
%%
%% ⚠️ TEMPLATE FILE - DO NOT USE DIRECTLY
%% Copy this file and customize for your module.
-module(router_error_handling_template).

-export([
    example_function/1,
    example_function_with_error_mapping/1,
    example_function_with_recovery/1
]).

-include("beamline_router.hrl").

%% ============================================================================
%% Error Handling Examples
%% ============================================================================

%% @doc Example: Basic error handling with {error, Reason}
-spec example_function(term()) -> {ok, term()} | {error, atom()}.
example_function(Input) ->
    try
        %% Validate input
        case validate_input(Input) of
            ok ->
                %% Process input
                Result = process_input(Input),
                {ok, Result};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:CatchReason:Stack ->
            %% Log error with context
            router_logger:error(<<"Example function error">>, #{
                <<"error">> => {Class, CatchReason},
                <<"stack">> => iolist_to_binary(io_lib:format("~p", [Stack])),
                <<"input">> => sanitize_input(Input)
            }),
            {error, internal_error}
    end.

%% @doc Example: Error handling with gRPC error mapping
-spec example_function_with_error_mapping(term()) -> {ok, term()} | {error, {integer(), binary()}}.
example_function_with_error_mapping(Input) ->
    case example_function(Input) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            %% Map error to gRPC status code
            {Status, Message} = router_error:to_grpc(Reason),
            {error, {Status, Message}}
    end.

%% @doc Example: Error handling with recovery
-spec example_function_with_recovery(term()) -> {ok, term()} | {error, atom()}.
example_function_with_recovery(Input) ->
    try
        %% Attempt primary operation
        case primary_operation(Input) of
            {ok, Result} ->
                {ok, Result};
            {error, Reason} ->
                %% Attempt recovery
                case recover_from_error(Reason, Input) of
                    {ok, RecoveredResult} ->
                        router_logger:info(<<"Recovered from error">>, #{
                            <<"error">> => Reason,
                            <<"recovery">> => success
                        }),
                        {ok, RecoveredResult};
                    {error, RecoveryError} ->
                        router_logger:error(<<"Recovery failed">>, #{
                            <<"original_error">> => Reason,
                            <<"recovery_error">> => RecoveryError
                        }),
                        {error, RecoveryError}
                end
        end
    catch
        Class:CatchReason:Stack ->
            %% Log unexpected errors
            router_logger:error(<<"Unexpected error in example function">>, #{
                <<"error">> => {Class, CatchReason},
                <<"stack">> => iolist_to_binary(io_lib:format("~p", [Stack]))
            }),
            {error, internal_error}
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

-spec validate_input(term()) -> ok | {error, atom()}.
validate_input(Input) ->
    case Input of
        undefined ->
            {error, invalid_input};
        _ ->
            ok
    end.

-spec process_input(term()) -> term().
process_input(Input) ->
    Input.

-spec sanitize_input(term()) -> binary().
sanitize_input(Input) ->
    %% Sanitize input to prevent information disclosure
    case Input of
        Map when is_map(Map) ->
            %% Remove sensitive fields
            Sanitized = maps:remove(<<"password">>, maps:remove(<<"api_key">>, Map)),
            iolist_to_binary(io_lib:format("~p", [Sanitized]));
        _ ->
            iolist_to_binary(io_lib:format("~p", [Input]))
    end.

-spec primary_operation(term()) -> {ok, term()} | {error, atom()}.
primary_operation(Input) ->
    {ok, Input}.

-spec recover_from_error(atom(), term()) -> {ok, term()} | {error, atom()}.
recover_from_error(Reason, Input) ->
    case Reason of
        timeout ->
            %% Retry with longer timeout
            {ok, Input};
        _ ->
            {error, recovery_failed}
    end.

