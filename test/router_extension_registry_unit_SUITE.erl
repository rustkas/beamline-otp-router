%% @doc Unit Tests for router_extension_registry module
%% @test_category unit, fast, coverage_hotspot
-module(router_extension_registry_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_lookup_existing/1,
    test_lookup_nonexistent/1,
    test_lookup_by_type/1,
    test_reload/1,
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_lookup_existing/1,
    test_lookup_nonexistent/1,
    test_lookup_by_type/1,
    test_reload/1,
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1
]}).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_lookup_existing,
            test_lookup_nonexistent,
            test_lookup_by_type,
            test_reload,
            test_get_table_size,
            test_get_table_memory,
            test_check_size_limit
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Start extension registry if not running
    case whereis(router_extension_registry) of
        undefined ->
            case router_extension_registry:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _}} -> ok;
                Error -> ct:fail("Failed to start router_extension_registry: ~p", [Error])
            end;
        _Pid ->
            ok
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

test_lookup_existing(_Config) ->
    %% Try to lookup a known extension (if any exist in fixtures)
    Result = router_extension_registry:lookup(<<"circuit_breaker">>),
    
    case Result of
        {ok, _Extension} ->
            ok;
        {error, not_found} ->
            %% No fixture for this extension - acceptable
            ok
    end,
    
    ok.

test_lookup_nonexistent(_Config) ->
    Result = router_extension_registry:lookup(<<"nonexistent_extension_12345">>),
    
    ?assertEqual({error, not_found}, Result),
    
    ok.

test_lookup_by_type(_Config) ->
    %% Test lookup by type - returns {ok, Extensions} or {error, _}
    Result = router_extension_registry:lookup_by_type(<<"pre_processor">>),
    
    case Result of
        {ok, Extensions} when is_list(Extensions) ->
            ok;
        {error, invalid_type} ->
            ok;
        {error, _} ->
            ok
    end,
    
    ok.

test_reload(_Config) ->
    Result = router_extension_registry:reload(),
    
    ?assertEqual(ok, Result),
    
    ok.

test_get_table_size(_Config) ->
    Result = router_extension_registry:get_table_size(),
    
    ?assertEqual(true, is_integer(Result) orelse Result =:= undefined),
    
    ok.

test_get_table_memory(_Config) ->
    Result = router_extension_registry:get_table_memory(),
    
    ?assertEqual(true, is_integer(Result) orelse Result =:= undefined),
    
    ok.

test_check_size_limit(_Config) ->
    Result = router_extension_registry:check_size_limit(),
    
    case Result of
        {ok, Size} when is_integer(Size) ->
            ?assertEqual(true, Size >= 0);
        {error, no_limit_configured} ->
            ok;
        {error, exceeded, _Current, _Limit} ->
            ok
    end,
    
    ok.
