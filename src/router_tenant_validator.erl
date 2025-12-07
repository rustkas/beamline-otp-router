%% @doc Tenant ID Validator
%% Validates tenant_id against allowlist and policy registry, emits audit events
-module(router_tenant_validator).
-export([validate_tenant/2]).
-export([emit_audit_event/3]).

-ignore_xref([{router_tenant_validator, emit_audit_event, 3}]).

-include("beamline_router.hrl").

-define(AUDIT_TELEMETRY_PREFIX, [router_tenant_validator, audit]).

%% @doc Validate tenant_id against allowlist and policy registry
%% Returns: {ok, TenantId} | {error, Reason, Context}
-spec validate_tenant(binary() | undefined, map()) -> {ok, binary()} | {error, atom(), map()}.
validate_tenant(undefined, Context) ->
    emit_audit_event(tenant_missing, undefined, Context),
    {error, tenant_missing, maps:merge(Context, #{reason => <<"tenant_id is required">>})};
validate_tenant(<<>>, Context) ->
    emit_audit_event(tenant_empty, <<>>, Context),
    {error, tenant_empty, maps:merge(Context, #{reason => <<"tenant_id cannot be empty">>})};
validate_tenant(TenantId, Context) when is_binary(TenantId) ->
    %% First check allowlist (if configured)
    case validate_tenant_allowlist(TenantId) of
        false ->
            AuditContext = maps:merge(Context, #{
                tenant_id => TenantId,
                reason => <<"tenant_id not in allowlist">>,
                check_type => allowlist
            }),
            emit_audit_event(tenant_not_allowed, TenantId, AuditContext),
            {error, tenant_not_allowed, AuditContext};
        true ->
            %% Then check policy registry (if policy exists, tenant is valid)
            case validate_tenant_policy(TenantId) of
                false ->
                    %% Policy not found - this might be acceptable depending on configuration
                    %% For now, we allow it but emit audit event
                    AuditContext = maps:merge(Context, #{
                        tenant_id => TenantId,
                        reason => <<"no policy found for tenant_id">>,
                        check_type => policy_registry
                    }),
                    emit_audit_event(tenant_policy_not_found, TenantId, AuditContext),
                    {ok, TenantId};  %% Allow but audit
                true ->
                    {ok, TenantId}
            end
    end;
validate_tenant(TenantId, Context) ->
    AuditContext = maps:merge(Context, #{
        tenant_id => TenantId,
        reason => <<"tenant_id must be a binary">>,
        check_type => format
    }),
    emit_audit_event(tenant_invalid_format, TenantId, AuditContext),
    {error, tenant_invalid_format, AuditContext}.

%% @doc Validate tenant_id against allowlist
%% Returns: true if allowed, false if not allowed or allowlist not configured
-spec validate_tenant_allowlist(binary()) -> boolean().
validate_tenant_allowlist(TenantId) ->
    %% Check result/ACK specific allowlist first
    case application:get_env(beamline_router, result_ack_allowed_tenants, undefined) of
        undefined ->
            %% Fall back to general CAF allowlist
            check_tenant_against_allowlist(TenantId, caf_push_assignment_allowed_tenants);
        AllowedTenants ->
            check_tenant_against_allowlist_internal(TenantId, AllowedTenants)
    end.

%% @doc Validate tenant_id against policy registry
%% Returns: true if policy exists, false if not found
-spec validate_tenant_policy(binary()) -> boolean().
validate_tenant_policy(TenantId) ->
    %% Check if policy exists for this tenant (using default policy)
    case router_policy_store:load_policy(TenantId, <<"default">>) of
        {ok, _Policy} -> true;
        {error, not_found} -> false;
        {error, _Reason} -> false  %% Treat errors as not found
    end.

%% @doc Emit audit event for tenant validation
-spec emit_audit_event(atom(), binary() | undefined, map()) -> ok.
emit_audit_event(EventType, TenantId, Context) ->
    Metadata = maps:merge(Context, #{
        event_type => EventType,
        tenant_id => TenantId,
        timestamp => erlang:system_time(millisecond)
    }),
    
    %% Emit telemetry event
    router_telemetry_helper:execute(?AUDIT_TELEMETRY_PREFIX, #{count => 1}, Metadata),
    
    %% Emit counter for audit events
    emit_counter(router_tenant_audit_total, Metadata),
    
    %% Log audit event
    router_logger:warn(<<"Tenant validation audit event">>, Metadata).

%% Internal: Check tenant against allowlist (general)
-spec check_tenant_against_allowlist(binary(), atom()) -> boolean().
check_tenant_against_allowlist(TenantId, ConfigKey) ->
    case application:get_env(beamline_router, ConfigKey, undefined) of
        undefined -> true;  %% No allowlist = allow all
        AllowedTenants -> check_tenant_against_allowlist_internal(TenantId, AllowedTenants)
    end.

%% Internal: Check tenant against allowlist (internal)
-spec check_tenant_against_allowlist_internal(binary(), list() | map()) -> boolean().
check_tenant_against_allowlist_internal(TenantId, AllowedTenants) when is_list(AllowedTenants) ->
    %% Normalize TenantId to binary for comparison
    TenantIdBin = case TenantId of
        Bin when is_binary(Bin) -> Bin;
        TenantStr when is_list(TenantStr) -> list_to_binary(TenantStr);
        _ -> TenantId
    end,
    %% Check if any element in allowlist matches (supporting both binary and string)
    lists:any(fun(Allowed) ->
        case Allowed of
            AllowedBin when is_binary(AllowedBin) -> AllowedBin =:= TenantIdBin;
            AllowedStr when is_list(AllowedStr) -> list_to_binary(AllowedStr) =:= TenantIdBin;
            _ -> Allowed =:= TenantId
        end
    end, AllowedTenants);
check_tenant_against_allowlist_internal(TenantId, AllowedTenants) when is_map(AllowedTenants) ->
    %% Normalize TenantId to binary for map key lookup
    TenantIdBin = case TenantId of
        Bin when is_binary(Bin) -> Bin;
        TenantStr when is_list(TenantStr) -> list_to_binary(TenantStr);
        _ -> TenantId
    end,
    maps:is_key(TenantIdBin, AllowedTenants) orelse
    (is_list(TenantId) andalso maps:is_key(list_to_binary(TenantId), AllowedTenants));
check_tenant_against_allowlist_internal(_TenantId, _AllowedTenants) ->
    false.

%% Internal: Emit counter
-spec emit_counter(atom(), map()) -> ok.
emit_counter(MetricName, Metadata) ->
    router_telemetry_helper:execute(
        [router_tenant_validator, MetricName],
        #{count => 1},
        Metadata
    ).
