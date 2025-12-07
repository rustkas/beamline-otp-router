%% @doc Permissions Module
%% Provides permission checking utilities for RBAC
%% CP1-ROUTER: Permission checks for Policy Enforcement
-module(router_permissions).
-ignore_xref([
    router_permissions,
    {router_permissions, check_policy_access, 4},
    {router_permissions, check_policy_write, 4},
    {router_permissions, check_policy_delete, 4},
    {router_permissions, check_config_access, 3},
    {router_permissions, validate_permission, 3},
    {router_permissions, get_permission_string, 2}
]).

-export([check_policy_access/4, check_policy_write/4, check_policy_delete/4, check_config_access/3]).
-export([validate_permission/3, get_permission_string/2]).

-include("beamline_router.hrl").

%% @doc Check if user can read policy
-spec check_policy_access(binary(), binary(), binary(), map()) -> boolean().
check_policy_access(UserId, TenantId, _PolicyId, Context) ->
    try
        router_rbac:can_access(UserId, TenantId, <<"read">>, <<"policy">>, Context)
    catch
        _:_ ->
            %% On any error, deny access (fail closed for security)
            false
    end.

%% @doc Check if user can write (create/update) policy
-spec check_policy_write(binary(), binary(), binary(), map()) -> boolean().
check_policy_write(UserId, TenantId, _PolicyId, Context) ->
    try
        %% Check write permission
        HasWrite = router_rbac:can_access(UserId, TenantId, <<"write">>, <<"policy">>, Context),
        
        %% Additional check: can edit own policies
        case HasWrite of
            true ->
                true;
            false ->
                %% Check if user created the policy (conditional permission)
                CreatedBy = maps:get(<<"created_by">>, Context, undefined),
                case CreatedBy of
                    UserId when UserId =/= undefined ->
                        %% User can edit own policies even without write permission
                        true;
                    _ ->
                        false
                end
        end
    catch
        _:_ ->
            %% On any error, deny access (fail closed for security)
            false
    end.

%% @doc Check if user can delete policy
-spec check_policy_delete(binary(), binary(), binary(), map()) -> boolean().
check_policy_delete(UserId, TenantId, _PolicyId, Context) ->
    try
        %% Check delete permission
        HasDelete = router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"policy">>, Context),
        
        %% Additional check: admins can always delete
        case HasDelete of
            true ->
                true;
            false ->
                router_rbac:is_admin(UserId, TenantId)
        end
    catch
        _:_ ->
            %% On any error, deny access (fail closed for security)
            false
    end.

%% @doc Check if user can access config (admin only)
-spec check_config_access(binary(), binary(), map()) -> boolean().
check_config_access(UserId, TenantId, Context) ->
    try
        router_rbac:can_access(UserId, TenantId, <<"admin">>, <<"config">>, Context)
    catch
        _:_ ->
            %% On any error, deny access (fail closed for security)
            false
    end.

%% @doc Validate permission format
-spec validate_permission(binary(), binary(), map()) -> {ok, map()} | {error, binary()}.
validate_permission(Action, Resource, Context) ->
    %% Validate action
    ValidActions = [<<"read">>, <<"write">>, <<"delete">>, <<"admin">>],
    case lists:member(Action, ValidActions) of
        false ->
            {error, <<"Invalid action: ", Action/binary>>};
        true ->
            %% Validate resource
            ValidResources = [<<"policy">>, <<"config">>, <<"metrics">>],
            case lists:member(Resource, ValidResources) of
                false ->
                    {error, <<"Invalid resource: ", Resource/binary>>};
                true ->
                    %% Build permission map
                    Permission = #{
                        <<"action">> => Action,
                        <<"resource">> => Resource,
                        <<"context">> => Context
                    },
                    {ok, Permission}
            end
    end.

%% @doc Get permission string for logging
-spec get_permission_string(binary(), binary()) -> binary().
get_permission_string(Action, Resource) ->
    <<Action/binary, ":", Resource/binary>>.
