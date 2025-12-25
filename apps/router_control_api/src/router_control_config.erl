%% Control API configuration helpers.
-module(router_control_config).

-export([
    control_mode_enabled/0,
    control_tenant_id/0,
    control_token/0,
    control_subject_prefix/0,
    control_alias_prefix/0,
    control_alias_enabled/0
]).

-define(DEFAULT_TENANT_ID, <<"local">>).

-spec control_mode_enabled() -> boolean().
control_mode_enabled() ->
    case getenv_bool("ROUTER_CONTROL_MODE") of
        {ok, Value} -> Value;
        error -> application:get_env(beamline_router, control_mode, false)
    end.

-spec control_tenant_id() -> binary().
control_tenant_id() ->
    case getenv_bin("ROUTER_CONTROL_TENANT_ID") of
        {ok, TenantId} -> TenantId;
        error ->
            case application:get_env(beamline_router, control_tenant_id, undefined) of
                undefined -> ?DEFAULT_TENANT_ID;
                Value -> normalize_bin(Value, ?DEFAULT_TENANT_ID)
            end
    end.

-spec control_token() -> binary() | undefined.
control_token() ->
    case getenv_bin("ROUTER_CONTROL_TOKEN") of
        {ok, Token} -> Token;
        error ->
            case application:get_env(beamline_router, control_token, undefined) of
                undefined -> undefined;
                Value -> normalize_bin(Value, undefined)
            end
    end.

%% ------------------------------------------------------------------
%% Subject namespace helpers
%% ------------------------------------------------------------------

control_subject_prefix() ->
    <<"beamline.router.control.v1">>.

control_alias_prefix() ->
    binary:replace(control_subject_prefix(), <<"control">>, <<"ide">>, [global]).

control_alias_enabled() ->
    case getenv_bool("ROUTER_CONTROL_SUBJECT_ALIAS") of
        {ok, Value} -> Value;
        error -> false
    end.

%% ------------------------------------------------------------------
%% Internal helpers
%% ------------------------------------------------------------------

-spec getenv_bool(string()) -> {ok, boolean()} | error.
getenv_bool(Key) ->
    case os:getenv(Key) of
        false -> error;
        "" -> error;
        Value ->
            case string:lowercase(Value) of
                "true" -> {ok, true};
                "1" -> {ok, true};
                "false" -> {ok, false};
                "0" -> {ok, false};
                _ -> error
            end
    end.

-spec getenv_bin(string()) -> {ok, binary()} | error.
getenv_bin(Key) ->
    case os:getenv(Key) of
        false -> error;
        "" -> error;
        Value -> {ok, list_to_binary(Value)}
    end.

-spec normalize_bin(term(), binary() | undefined) -> binary() | undefined.
normalize_bin(Value, _Default) when is_binary(Value) ->
    Value;
normalize_bin(Value, _Default) when is_list(Value) ->
    list_to_binary(Value);
normalize_bin(_, Default) ->
    Default.
