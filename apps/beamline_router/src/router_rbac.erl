-module(router_rbac).

-doc "RBAC (Role-Based Access Control) Module".
%%
%% Provides role-based access control for Router policies and resources.
%% Implements role-based permissions with support for conditional permissions.
%%
%% Security:
%% - All access checks go through this module
%% - Supports role-based and resource-level authorization
%% - Permission caching for performance
%% - Audit logging for security events
%%
%% Roles:
%% - admin: Full access to all resources
%% - operator: Read/write access to policies and config
%% - viewer: Read-only access
%%
%% @see SECURITY_GUIDE.md#access-control For security best practices
%% @see DEVELOPER_GUIDE.md For development workflow
%% @see src/router_grpc.erl gRPC API with RBAC integration
-behaviour(gen_server).

%% NOTE: Internal helper functions are used in complex contexts (try-catch, nested case)
%% The compiler may not detect all usages statically, but they are all called.
%% Suppress warnings for these internal functions that are definitely used:

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([can_access/5, can_access/4]).
-export([is_admin/2]).
-export([handle_rbac_error/1]).
-export([reset/0]).
%% RBAC API functions (for future use or external API)
-export([assign_role/3, revoke_role/3, has_permission/4, is_operator/2, is_viewer/2]).
-ignore_xref([
    {router_rbac, start_link, 0},
    {router_rbac, can_access, 4},
    {router_rbac, can_access, 5},
    {router_rbac, handle_rbac_error, 1}
]).

-include("beamline_router.hrl").

%% ETS tables for RBAC data
-define(ROLES_TABLE, rbac_roles).
-define(USER_ROLES_TABLE, rbac_user_roles).
-define(PERMISSIONS_TABLE, rbac_permissions).

%% Default roles
-define(ROLE_ADMIN, ~"admin").
-define(ROLE_OPERATOR, ~"operator").
-define(ROLE_VIEWER, ~"viewer").

%% Actions
-define(ACTION_READ, ~"read").
-define(ACTION_WRITE, ~"write").
-define(ACTION_DELETE, ~"delete").
-define(ACTION_ADMIN, ~"admin").

%% Resources
-define(RESOURCE_POLICY, ~"policy").
-define(RESOURCE_CONFIG, ~"config").
-define(RESOURCE_METRICS, ~"metrics").
-define(RESOURCE_EXTENSION_REGISTRY, ~"extension_registry").

-record(state, {
    roles_table :: ets:tid(),
    user_roles_table :: ets:tid(),
    permissions_table :: ets:tid(),
    permission_cache :: ets:tid() | undefined,
    rbac_enabled :: boolean(),
    cache_ttl_seconds :: integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Returns table reference (atom for named_table)
ensure_ets_table(TableName, Options) ->
    %% First check if table already exists
    case ets:whereis(TableName) of
        undefined ->
            %% Table doesn't exist, create it
            ets:new(TableName, Options);
        _Tid ->
            %% Table already exists, use its name
            TableName
    end.

%% Pattern: init/1 → do_init/1 for safe reset/lifecycle pattern
init([]) ->
    do_init([]).

%% This separation allows safe reset without restarting the gen_server
-spec do_init(list()) -> {ok, #state{}}.
do_init(_Opts) ->
    %% Create ETS tables for RBAC data
    %% Try to create table, if it already exists (badarg), use existing one
    RolesTable = ensure_ets_table(?ROLES_TABLE, [
        set,
        named_table,
        {keypos, 1},
        public,
        {read_concurrency, true}
    ]),
    
    %% For bag table, use public access in test mode to allow test process access
    %% In production, protected is fine since only router_rbac process accesses it
    RbacTestMode = application:get_env(beamline_router, rbac_test_mode, false),
    UserRolesTableOptions = [
        bag,
        named_table,
        {keypos, 1},
        case RbacTestMode of
            true -> public;
            false -> protected
        end,
        {read_concurrency, true}
    ],
    %% Try to create table, handling name conflicts
    %% Fix ETS cleanup issue: If table name is registered but table doesn't exist,
    %% we need to wait for the name to be released or force cleanup
    UserRolesTable = case catch ets:new(?USER_ROLES_TABLE, UserRolesTableOptions) of
        {'EXIT', {badarg, _}} ->
            %% Name is taken, check if table actually exists
            case ets:whereis(?USER_ROLES_TABLE) of
                undefined ->
                    %% Name is registered but table doesn't exist - ETS cleanup issue
                    %% Try to force cleanup by attempting to delete the table name
                    catch ets:delete(?USER_ROLES_TABLE),
                    timer:sleep(100),  %% Brief wait for name release
                    
                    %% Try to create again
                    case catch ets:new(?USER_ROLES_TABLE, UserRolesTableOptions) of
                        {'EXIT', {badarg, Reason2}} ->
                            %% Still failing, likely name stuck. Fallback to unique name.
                            UniqueName = list_to_atom("rbac_user_roles_" ++ integer_to_list(erlang:system_time(microsecond))),
                            router_logger:warn(~"Name collision for rbac_user_roles, falling back to unique name", #{
                                ~"original" => ?USER_ROLES_TABLE,
                                ~"new" => UniqueName,
                                ~"reason" => iolist_to_binary(io_lib:format("~p", [Reason2]))
                            }),
                            ets:new(UniqueName, UserRolesTableOptions);
                        {'EXIT', Reason} ->
                            exit({ets_create_failed, ?USER_ROLES_TABLE, Reason});
                        Tid ->
                            Tid
                    end;
                _Tid ->
                    %% Table exists, use it
                    ?USER_ROLES_TABLE
            end;
        {'EXIT', Reason} ->
            exit({ets_create_failed, ?USER_ROLES_TABLE, Reason});
        Tid ->
            %% Successfully created
            Tid
    end,
    
    PermissionsTable = ensure_ets_table(?PERMISSIONS_TABLE, [
        set,
        named_table,
        {keypos, 1},
        public,
        {read_concurrency, true}
    ]),
    
    %% Initialize default roles and permissions only if table is empty
    %% RolesTable can be either TID (integer) or table name (atom) for named_table
    %% Check if table is accessible before trying to use it
    case catch ets:info(RolesTable) of
        undefined ->
            %% Table not accessible, skip initialization
            router_logger:warn(~"RBAC init: roles table not accessible", #{
                ~"event" => ~"rbac_init_skip"
            });
        {'EXIT', _} ->
            %% Info failed, skip initialization
            router_logger:warn(~"RBAC init: roles table info failed", #{
                ~"event" => ~"rbac_init_skip"
            });
        _Info when is_list(_Info) ->
            %% Table is accessible, check if empty and initialize
            case ets:first(RolesTable) of
                '$end_of_table' ->
                    %% Table is empty, initialize
                    case initialize_default_roles(RolesTable, PermissionsTable) of
                        ok ->
                            ok;
                        {error, InitReason} ->
                            router_logger:error(~"RBAC init: failed to initialize default roles", #{
                                ~"event" => ~"rbac_init_failed",
                                ~"reason" => iolist_to_binary(io_lib:format("~p", [InitReason]))
                            })
                    end;
                _ ->
                    %% Table has data, skip initialization
                    ok
            end
    end,
    
    %% Get configuration
    RbacEnabled = application:get_env(beamline_router, rbac_enabled, true),
    CacheTtlSeconds = application:get_env(beamline_router, rbac_cache_ttl_seconds, 300),
    
    %% Create permission cache if enabled
    PermissionCache = case RbacEnabled of
        true ->
            ensure_ets_table(permission_cache, [
                set,
                named_table,
                {keypos, 1},
                protected,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        false ->
            undefined
    end,
    
    State = #state{
        roles_table = RolesTable,
        user_roles_table = UserRolesTable,
        permissions_table = PermissionsTable,
        permission_cache = PermissionCache,
        rbac_enabled = RbacEnabled,
        cache_ttl_seconds = CacheTtlSeconds
    },
    
    {ok, State}.

-spec can_access(binary(), binary(), binary(), binary(), map()) -> boolean().
can_access(UserId, TenantId, Action, Resource, Context) ->
    try
        gen_server:call(?MODULE, {can_access, UserId, TenantId, Action, Resource, Context}, 5000)
    catch
        exit:{noproc, _} ->
            %% RBAC server not running - deny access (fail closed for security)
            router_logger:error(~"RBAC server not running, denying access", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"action" => Action,
                ~"resource" => Resource
            }),
            false;
        exit:{timeout, _} ->
            %% Timeout - deny access (fail closed for security)
            router_logger:error(~"RBAC check timeout, denying access", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId
            }),
            false;
        Class:Reason ->
            %% Other errors - deny access (fail closed for security)
            router_logger:error(~"RBAC check error, denying access", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            false
    end.

-spec can_access(binary(), binary(), binary(), binary()) -> boolean().
can_access(UserId, TenantId, Action, Resource) ->
    can_access(UserId, TenantId, Action, Resource, #{}).

-spec assign_role(binary(), binary(), binary()) -> ok | {error, term()}.
assign_role(UserId, TenantId, RoleId) ->
    try
        gen_server:call(?MODULE, {assign_role, UserId, TenantId, RoleId}, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(~"RBAC server not running", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"role_id" => RoleId
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(~"RBAC assign_role timeout", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(~"RBAC assign_role error", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

-spec revoke_role(binary(), binary(), binary()) -> ok | {error, term()}.
revoke_role(UserId, TenantId, RoleId) ->
    try
        gen_server:call(?MODULE, {revoke_role, UserId, TenantId, RoleId}, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(~"RBAC server not running", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"role_id" => RoleId
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(~"RBAC revoke_role timeout", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(~"RBAC revoke_role error", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

-spec get_user_roles(binary(), binary()) -> [binary()].
get_user_roles(UserId, TenantId) ->
    try
        gen_server:call(?MODULE, {get_user_roles, UserId, TenantId}, 5000)
    catch
        exit:{noproc, _} ->
            %% RBAC server not running - return empty list (graceful degradation)
            router_logger:error(~"RBAC server not running, returning empty roles", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId
            }),
            [];
        exit:{timeout, _} ->
            %% Timeout - return empty list (graceful degradation)
            router_logger:error(~"RBAC get_user_roles timeout, returning empty roles", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId
            }),
            [];
        Class:Reason ->
            %% Other errors - return empty list (graceful degradation)
            router_logger:error(~"RBAC get_user_roles error, returning empty roles", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            []
    end.

-spec has_permission(binary(), binary(), binary(), binary()) -> boolean().
has_permission(UserId, TenantId, Action, Resource) ->
    can_access(UserId, TenantId, Action, Resource).

-spec is_admin(binary(), binary()) -> boolean().
is_admin(UserId, TenantId) ->
    try
        Roles = get_user_roles(UserId, TenantId),
        lists:member(?ROLE_ADMIN, Roles)
    catch
        _:_ ->
            %% On any error, return false (fail closed for security)
            false
    end.

-spec is_operator(binary(), binary()) -> boolean().
is_operator(UserId, TenantId) ->
    try
        Roles = get_user_roles(UserId, TenantId),
        lists:member(?ROLE_OPERATOR, Roles)
    catch
        _:_ ->
            %% On any error, return false (fail closed for security)
            false
    end.

-spec is_viewer(binary(), binary()) -> boolean().
is_viewer(UserId, TenantId) ->
    try
        Roles = get_user_roles(UserId, TenantId),
        lists:member(?ROLE_VIEWER, Roles)
    catch
        _:_ ->
            %% On any error, return false (fail closed for security)
            false
    end.



%% Safe reset via handle_call(reset_all, ...) - clears ETS tables but keeps process alive
%% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
-spec reset() -> ok | {error, term()}.
reset() ->
    try
        gen_server:call(?MODULE, reset_all, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(~"RBAC server not running for reset", #{
                ~"event" => ~"rbac_reset"
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(~"RBAC reset timeout", #{
                ~"event" => ~"rbac_reset"
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(~"RBAC reset error", #{
                ~"event" => ~"rbac_reset",
                ~"error" => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

%% gen_server callbacks

handle_call({can_access, UserId, TenantId, Action, Resource, Context}, _From, State) ->
    try
        %% Validate input - fail-closed on invalid input
        case validate_user_input(UserId, TenantId) of
            {error, _Reason} ->
                %% Invalid input: deny access (fail-closed for security)
                {reply, false, State};
            ok ->
                %% Check if RBAC is enabled
                case State#state.rbac_enabled of
                    false ->
                        {reply, true, State};  %% Allow all if RBAC disabled
                    true ->
                        Result = check_access(UserId, TenantId, Action, Resource, Context, State),
                        %% Emit telemetry
                        router_telemetry_helper:execute([router_rbac, check_total], #{count => 1}, #{
                            user_id => UserId,
                            tenant_id => TenantId,
                            action => Action,
                            resource => Resource,
                            allowed => Result
                        }),
                        {reply, Result, State}
                end
        end
    catch
        Class:Error:Stacktrace ->
            %% CRITICAL: Always return boolean for can_access (fail-closed for security)
            %% Log the error for debugging, but deny access instead of returning error tuple
            router_logger:error(~"RBAC error in can_access, denying access (fail-closed)", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"error" => iolist_to_binary(io_lib:format("~p", [Error])),
                ~"class" => iolist_to_binary(io_lib:format("~p", [Class])),
                ~"stacktrace" => iolist_to_binary(io_lib:format("~p", [Stacktrace]))
            }),
            {reply, false, State}
    end;

handle_call({assign_role, UserId, TenantId, RoleId}, _From, State) ->
    try
        %% Validate input
        case validate_user_input(UserId, TenantId) of
            {error, Reason} ->
                {reply, {error, Reason}, State};
            ok ->
                %% Validate RoleId
                case validate_role_id(RoleId) of
                    {error, Reason} ->
                        {reply, {error, Reason}, State};
                    ok ->
                        Result = do_assign_role(UserId, TenantId, RoleId, State),
                        {reply, Result, State}
                end
        end
    catch
        Class:Error:Stacktrace ->
            ErrorResult = handle_rbac_error({Class, Error}),
            router_logger:error(~"RBAC error in assign_role", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"role_id" => RoleId,
                ~"error" => iolist_to_binary(io_lib:format("~p", [Error])),
                ~"stacktrace" => iolist_to_binary(io_lib:format("~p", [Stacktrace]))
            }),
            {reply, ErrorResult, State}
    end;

handle_call({revoke_role, UserId, TenantId, RoleId}, _From, State) ->
    try
        %% Validate input
        case validate_user_input(UserId, TenantId) of
            {error, Reason} ->
                {reply, {error, Reason}, State};
            ok ->
                %% Validate RoleId
                case validate_role_id(RoleId) of
                    {error, Reason} ->
                        {reply, {error, Reason}, State};
                    ok ->
                        Result = do_revoke_role(UserId, TenantId, RoleId, State),
                        {reply, Result, State}
                end
        end
    catch
        Class:Error:Stacktrace ->
            ErrorResult = handle_rbac_error({Class, Error}),
            router_logger:error(~"RBAC error in revoke_role", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"role_id" => RoleId,
                ~"error" => iolist_to_binary(io_lib:format("~p", [Error])),
                ~"stacktrace" => iolist_to_binary(io_lib:format("~p", [Stacktrace]))
            }),
            {reply, ErrorResult, State}
    end;

handle_call({get_user_roles, UserId, TenantId}, _From, State) ->
    try
        %% Validate input
        case validate_user_input(UserId, TenantId) of
            {error, Reason} ->
                {reply, {error, Reason}, State};
            ok ->
                Roles = do_get_user_roles(UserId, TenantId, State),
                {reply, Roles, State}
        end
    catch
        Class:Error:Stacktrace ->
            ErrorResult = handle_rbac_error({Class, Error}),
            router_logger:error(~"RBAC error in get_user_roles", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"error" => iolist_to_binary(io_lib:format("~p", [Error])),
                ~"stacktrace" => iolist_to_binary(io_lib:format("~p", [Stacktrace]))
            }),
            {reply, ErrorResult, State}
    end;

handle_call(reset_all, _From, State) ->
    %% Safe reset: clear all ETS tables but keep process and tables alive
    %% This is called from test utilities, should not kill the process
    %% Pattern: Reuse do_init/1 to reinitialize tables
    %% Fix ETS cleanup issue: Properly handle table deletion and recreation
    try
        %% Clear all ETS tables - use safe deletion that handles cleanup issues
        SafeDeleteAllObjects = fun(Table) ->
            try
                case catch ets:info(Table) of
                    undefined -> ok;
                    {'EXIT', _} -> ok;
                    _Info when is_list(_Info) ->
                        %% Table exists and is accessible
                        ets:delete_all_objects(Table),
                        ok;
                    _ -> ok
                end
            catch
                _:_ -> ok
            end
        end,
        
        SafeDeleteAllObjects(State#state.roles_table),
        SafeDeleteAllObjects(State#state.user_roles_table),
        SafeDeleteAllObjects(State#state.permissions_table),
        case State#state.permission_cache of
            undefined -> ok;
            CacheTable -> SafeDeleteAllObjects(CacheTable)
        end,
        
        %% Verify tables are still accessible after cleanup
        %% If any table became inaccessible, we need to recreate it
        VerifyTable = fun(Table, TableName) ->
            case catch ets:info(Table) of
                undefined ->
                    %% Table was deleted - this is the cleanup issue
                    %% For named_table, we can't easily recreate, so log and continue
                    router_logger:warn(~"RBAC table became inaccessible after reset", #{
                        ~"table" => TableName,
                        ~"event" => ~"rbac_table_inaccessible"
                    }),
                    false;
                {'EXIT', _} ->
                    %% Table access error
                    router_logger:warn(~"RBAC table access error after reset", #{
                        ~"table" => TableName,
                        ~"event" => ~"rbac_table_access_error"
                    }),
                    false;
                _Info when is_list(_Info) ->
                    %% Table is accessible
                    true;
                _ -> false
            end
        end,
        
        RolesTableOk = VerifyTable(State#state.roles_table, ~"roles_table"),
        UserRolesTableOk = VerifyTable(State#state.user_roles_table, ~"user_roles_table"),
        PermissionsTableOk = VerifyTable(State#state.permissions_table, ~"permissions_table"),
        
        %% Reinitialize default roles and permissions only if tables are accessible
        %% FIX: Always reinitialize after reset - no check for empty table needed
        %% The previous check "ets:first(...) =/= '$end_of_table'" was wrong because:
        %% 1. After delete_all_objects, table IS empty - check was redundant
        %% 2. The "_ -> ok" branch silently skipped initialization on any race/error
        if RolesTableOk andalso PermissionsTableOk ->
            case initialize_default_roles(State#state.roles_table, State#state.permissions_table) of
                ok ->
                    ok;
                InitError ->
                    router_logger:error(~"RBAC reset_all: failed to initialize default roles", #{
                        ~"event" => ~"rbac_reset_all_init_failed",
                        ~"error" => iolist_to_binary(io_lib:format("~p", [InitError]))
                    })
            end;
        true ->
            %% Some tables are inaccessible - can't reinitialize
            router_logger:warn(~"RBAC reset_all: cannot reinitialize - tables inaccessible", #{
                ~"event" => ~"rbac_reset_all_partial",
                ~"roles_table_ok" => RolesTableOk,
                ~"user_roles_table_ok" => UserRolesTableOk,
                ~"permissions_table_ok" => PermissionsTableOk
            })
        end,
        
        router_logger:info(~"RBAC reset_all: tables cleared and reinitialized", #{
            ~"event" => ~"rbac_reset_all",
            ~"tables_accessible" => RolesTableOk andalso UserRolesTableOk andalso PermissionsTableOk
        }),
        {reply, ok, State}
    catch
        Error:Reason ->
            router_logger:error(~"RBAC reset_all failed", #{
                ~"event" => ~"rbac_reset_all",
                ~"error" => iolist_to_binary(io_lib:format("~p", [Error])),
                ~"reason" => iolist_to_binary(io_lib:format("~p", [Reason]))
            }),
            {reply, {error, {Error, Reason}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% Check access with roles and permissions (with caching)
check_access(UserId, TenantId, Action, Resource, Context, State) ->
    %% Get user permissions (with caching if enabled)
    UserPermissions = get_user_permissions_cached(UserId, TenantId, State),
    
    %% Build permission key
    PermissionKey = {Action, Resource},
    
    %% Check if user has permission
    HasPermission = sets:is_element(PermissionKey, UserPermissions),
    
    %% Check conditional permissions (e.g., can edit own policies)
    ConditionalAllowed = check_conditional_permissions_for_user(UserId, Action, Resource, Context, State),
    
    HasPermission orelse ConditionalAllowed.

%% Get user permissions with caching
get_user_permissions_cached(UserId, TenantId, State) ->
    case State#state.permission_cache of
        undefined ->
            %% Cache disabled, get permissions directly
            get_user_permissions(UserId, TenantId, State);
        CacheTable ->
            try
                %% Check cache first
                CacheKey = {UserId, TenantId},
                Now = erlang:system_time(second),
                case ets:lookup(CacheTable, CacheKey) of
                    [{CacheKey, Permissions, ExpiresAt}] when ExpiresAt > Now ->
                        %% Cache hit, return cached permissions
                        Permissions;
                    _ ->
                        %% Cache miss or expired, fetch and cache
                        Permissions = get_user_permissions(UserId, TenantId, State),
                        ExpiresAt = Now + State#state.cache_ttl_seconds,
                        ets:insert(CacheTable, {CacheKey, Permissions, ExpiresAt}),
                        Permissions
                end
            catch
                error:{badarg, _} ->
                    %% Cache table not accessible - fallback to direct lookup
                    get_user_permissions(UserId, TenantId, State);
                Class:Reason ->
                    %% Other errors - fallback to direct lookup
                    router_logger:error(~"Permission cache error, using direct lookup", #{
                        ~"user_id" => UserId,
                        ~"tenant_id" => TenantId,
                        ~"error" => {Class, Reason}
                    }),
                    get_user_permissions(UserId, TenantId, State)
            end
    end.

%% Get user permissions from roles (without caching)
get_user_permissions(UserId, TenantId, State) ->
    try
        %% Get user roles
        UserRoles = do_get_user_roles(UserId, TenantId, State),
        RolesTable = State#state.roles_table,
        
        %% Collect all permissions from roles
        Permissions = lists:foldl(
            fun(RoleId, Acc) ->
                try
                    case ets:lookup(RolesTable, RoleId) of
                        [{RoleId, RolePermissions}] ->
                            %% Add role permissions to accumulator
                            lists:foldl(
                                fun({Action, Resource}, Acc2) ->
                                    sets:add_element({Action, Resource}, Acc2)
                                end,
                                Acc,
                                RolePermissions
                            );
                        [] ->
                            Acc
                    end
                catch
                    error:{badarg, _} ->
                        %% Table not accessible - skip this role
                        Acc;
                    _:_ ->
                        %% Other errors - skip this role
                        Acc
                end
            end,
            sets:new(),
            UserRoles
        ),
        Permissions
    catch
        _:_ ->
            %% Return empty permissions set on any error
            sets:new()
    end.

%%  Note: Role-level permission checking is implemented inline in check_access/6
%% for better performance. Future extensions can add conditional logic here if needed.

%% Check conditional permissions for user (e.g., can edit own policies)
check_conditional_permissions_for_user(UserId, Action, Resource, Context, _State) ->
    case {Action, Resource} of
        {?ACTION_WRITE, ?RESOURCE_POLICY} ->
            %% Check if user created the policy (conditional permission)
            CreatedBy = maps:get(~"created_by", Context, undefined),
            case CreatedBy of
                UserId when UserId =/= undefined ->
                    %% User can edit own policies even without write permission
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% Assign role to user
do_assign_role(UserId, TenantId, RoleId, State) ->
    Key = {UserId, TenantId, RoleId},
    UserRolesTable = State#state.user_roles_table,
    %% Try to use table directly - catch handles table accessibility issues
    try
        case ets:lookup(UserRolesTable, Key) of
            [] ->
                ets:insert(UserRolesTable, {Key, RoleId}),
                ok;
            [_] ->
                {error, role_already_assigned}
        end
    catch
        error:{badarg, _} ->
            %% Table not accessible (doesn't exist or name conflict)
            {error, table_not_accessible};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% Revoke role from user
do_revoke_role(UserId, TenantId, RoleId, State) ->
    Key = {UserId, TenantId, RoleId},
    UserRolesTable = State#state.user_roles_table,
    try
        case ets:delete(UserRolesTable, Key) of
            true ->
                ok;
            false ->
                {error, role_not_found}
        end
    catch
        error:{badarg, _} ->
            %% Table not accessible
            {error, table_not_accessible};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% Get user roles
do_get_user_roles(UserId, TenantId, State) ->
    UserRolesTable = State#state.user_roles_table,
    try
        %% Match all roles for user in tenant
        Pattern = {{UserId, TenantId, '$1'}, '$2'},
        Matches = ets:match(UserRolesTable, Pattern),
        [RoleId || [RoleId, _] <- Matches]
    catch
        error:{badarg, _} ->
            %% Table not accessible
            [];
        Class:Reason ->
            router_logger:error(~"Failed to get user roles", #{
                ~"user_id" => UserId,
                ~"tenant_id" => TenantId,
                ~"error" => {Class, Reason}
            }),
            []
    end.

%% Initialize default roles and permissions
%% Returns ok on success, {error, Reason} on failure
-spec initialize_default_roles(ets:tid() | atom(), ets:tid() | atom()) -> ok | {error, term()}.
initialize_default_roles(RolesTable, _PermissionsTable) ->
    try
        %% Admin role: full access
        AdminPermissions = [
            {?ACTION_READ, ?RESOURCE_POLICY},
            {?ACTION_WRITE, ?RESOURCE_POLICY},
            {?ACTION_DELETE, ?RESOURCE_POLICY},
            {?ACTION_ADMIN, ?RESOURCE_CONFIG},
            {?ACTION_READ, ?RESOURCE_METRICS}
        ],
        true = ets:insert(RolesTable, {?ROLE_ADMIN, AdminPermissions}),
        
        %% Operator role: read/write policies, no admin
        OperatorPermissions = [
            {?ACTION_READ, ?RESOURCE_POLICY},
            {?ACTION_WRITE, ?RESOURCE_POLICY},
            {?ACTION_READ, ?RESOURCE_METRICS},
            {?ACTION_READ, ?RESOURCE_EXTENSION_REGISTRY},
            {?ACTION_WRITE, ?RESOURCE_EXTENSION_REGISTRY}
        ],
        true = ets:insert(RolesTable, {?ROLE_OPERATOR, OperatorPermissions}),
        
        %% Viewer role: read only
        ViewerPermissions = [
            {?ACTION_READ, ?RESOURCE_POLICY},
            {?ACTION_READ, ?RESOURCE_METRICS},
            {?ACTION_READ, ?RESOURCE_EXTENSION_REGISTRY}
        ],
        true = ets:insert(RolesTable, {?ROLE_VIEWER, ViewerPermissions}),
        
        ok
    catch
        error:{badarg, BadargReason} ->
            %% Table not accessible - return error
            router_logger:error(~"Failed to initialize default roles: table not accessible", #{
                ~"reason" => iolist_to_binary(io_lib:format("~p", [BadargReason]))
            }),
            {error, {table_not_accessible, BadargReason}};
        Class:Reason ->
            %% Other errors - return error
            router_logger:error(~"Failed to initialize default roles", #{
                ~"class" => iolist_to_binary(io_lib:format("~p", [Class])),
                ~"reason" => iolist_to_binary(io_lib:format("~p", [Reason]))
            }),
            {error, {Class, Reason}}
    end.

-spec handle_rbac_error(term()) -> {error, atom()}.
handle_rbac_error(Error) ->
    case Error of
        {timeout, _} ->
            {error, service_unavailable};
        {database_timeout, _} ->
            {error, service_unavailable};
        {invalid_permission, _} ->
            {error, forbidden};
        {quota_exceeded, _} ->
            {error, resource_exhausted};
        {invalid_input, _} ->
            {error, invalid_argument};
        {not_found, _} ->
            {error, not_found};
        _ ->
            {error, internal}
    end.

%% Uses router_security_validator for comprehensive security validation
-spec validate_user_input(binary() | undefined, binary() | undefined) -> ok | {error, invalid_input}.
validate_user_input(UserId, TenantId) ->
    %% Use security validator if available, fallback to basic validation
    case erlang:function_exported(router_security_validator, validate_user_id, 1) andalso
         erlang:function_exported(router_security_validator, validate_tenant_id, 1) of
        true ->
            %% Validate UserId
            UserIdResult = case UserId of
                undefined -> {error, user_id_required};
                _ -> router_security_validator:validate_user_id(UserId)
            end,
            %% Validate TenantId
            TenantIdResult = case TenantId of
                undefined -> {error, tenant_id_required};
                _ -> router_security_validator:validate_tenant_id(TenantId)
            end,
            %% Both must be valid
            case {UserIdResult, TenantIdResult} of
                {{ok, _}, {ok, _}} -> ok;
                _ -> {error, invalid_input}
            end;
        false ->
            %% Fallback: basic validation
            IsValidUserId = case UserId of
                undefined -> false;
                U when is_binary(U) ->
                    case re:run(U, "^[a-zA-Z0-9_-]{1,64}$", [{capture, none}]) of
                        match -> true;
                        _ -> false
                    end;
                _ -> false
            end,
            
            IsValidTenantId = case TenantId of
                undefined -> false;
                T when is_binary(T) ->
                    case re:run(T, "^[a-zA-Z0-9_-]{1,64}$", [{capture, none}]) of
                        match -> true;
                        _ -> false
                    end;
                _ -> false
            end,
            
            case {IsValidUserId, IsValidTenantId} of
                {true, true} -> ok;
                _ -> {error, invalid_input}
            end
    end.

%% Uses router_security_validator for comprehensive security validation
-spec validate_role_id(binary() | undefined) -> ok | {error, invalid_input}.
validate_role_id(RoleId) ->
    %% Use security validator if available, fallback to basic validation
    case erlang:function_exported(router_security_validator, validate_role_id, 1) of
        true ->
            case router_security_validator:validate_role_id(RoleId) of
                {ok, _} ->
                    %% Also check if it's a standard role
                    ValidRoles = [?ROLE_ADMIN, ?ROLE_OPERATOR, ?ROLE_VIEWER],
                    case lists:member(RoleId, ValidRoles) of
                        true -> ok;
                        false -> ok  %% Allow custom roles with valid format
                    end;
                {error, _} -> {error, invalid_input}
            end;
        false ->
            %% Fallback: basic validation
            case RoleId of
                undefined -> {error, invalid_input};
                R when is_binary(R) ->
                    %% Valid roles: admin, operator, viewer
                    ValidRoles = [?ROLE_ADMIN, ?ROLE_OPERATOR, ?ROLE_VIEWER],
                    case lists:member(R, ValidRoles) of
                        true -> ok;
                        false ->
                            %% Also check format: alphanumeric, underscore, hyphen, 1-64 chars
                            case re:run(R, "^[a-zA-Z0-9_-]{1,64}$", [{capture, none}]) of
                                match -> ok;  %% Allow custom roles with valid format
                                _ -> {error, invalid_input}
                            end
                    end;
                _ -> {error, invalid_input}
            end
    end.
