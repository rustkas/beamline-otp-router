%% @doc PostgreSQL Database Integration
%% Manages database connection pool and queries
-module(router_db).
-export([load_policy/2, query/2]).

-ifdef(EPGSQL_AVAILABLE).
-define(HAVE_DB, true).
-else.
-define(HAVE_DB, false).
-endif.

-include("beamline_router.hrl").

%% State record (kept for future use with gen_server)
%% -record(state, {
%%     pool :: pid(),
%%     config :: map()
%% }).

%% @doc Initialize database connection pool
%% Removed init/1 (unused); connection management is handled elsewhere

%% @doc Load policy from database
%% Compile-time guarded definitions to avoid xref warnings
-ifdef(EPGSQL_AVAILABLE).
load_policy(TenantId, PolicyId) ->
    Conn = get(db_conn),
    Query = "SELECT policy_json, version, updated_at FROM beamline.policies "
            "WHERE tenant_id = $1 AND policy_id = $2 AND is_active = TRUE",
    case catch epgsql:equery(Conn, Query, [TenantId, PolicyId]) of
        {ok, _, [{PolicyJson, Version, UpdatedAt}]} ->
            {ok, #{policy_json => PolicyJson, version => Version, updated_at => UpdatedAt}};
        {ok, _, []} ->
            {error, not_found};
        {'EXIT', _} ->
            {error, database_not_available};
        Error ->
            {error, Error}
    end.
-else.
load_policy(_TenantId, _PolicyId) ->
    {error, database_not_available}.
-endif.

%% @doc Execute generic query
-spec query(binary() | string(), list()) -> {error, database_not_available}.
query(_Query, _Params) ->
    {error, database_not_available}.
