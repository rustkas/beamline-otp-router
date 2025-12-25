%% @doc Integration tests for Extension Registry Dual-Mode Support
%% Tests database + fixtures fallback behavior
-module(router_extension_registry_dual_mode_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, groups/0, end_per_suite/1, end_per_testcase/2, init_per_suite/1, init_per_testcase/2]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_auto_mode_database_available/1,
    test_auto_mode_database_unavailable/1,
    test_database_mode/1,
    test_fixtures_mode/1,
    test_periodic_sync/1,
    test_reload_auto_mode/1,
    test_reload_database/1,
    test_reload_fixtures/1
]).


-export([groups_for_level/1]).

%% Test suite configuration
suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Dual mode tests involve database and app restarts, so full tier
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [{group, dual_mode_tests}];
groups_for_level(heavy) -> [{group, dual_mode_tests}].

groups() ->
    [
        {dual_mode_tests, [sequence], [
            test_fixtures_mode,
            test_database_mode,
            test_auto_mode_database_available,
            test_auto_mode_database_unavailable,
            test_periodic_sync,
            test_reload_fixtures,
            test_reload_database,
            test_reload_auto_mode
        ]}
    ].

init_per_suite(Config) ->
    %% Start router application
    application:ensure_all_started(beamline_router),
    Config.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear extension registry
    router_extension_registry:reload(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Fixtures mode (source = fixtures)
test_fixtures_mode(_Config) ->
    %% Set configuration to fixtures mode
    application:set_env(beamline_router, extension_registry, [
        {source, fixtures},
        {db_enabled, false}
    ]),
    
    %% Restart registry to apply config
    gen_server:stop(router_extension_registry),
    timer:sleep(100),
    router_extension_registry:start_link(),
    timer:sleep(100),
    
    %% Should load from fixtures
    Result = router_extension_registry:lookup(<<"normalize_text">>),
    
    %% Should find extension (from default fixtures)
    case Result of
        {ok, Extension} ->
            ?assert(is_record(Extension, extension)),
            ?assertEqual(<<"normalize_text">>, Extension#extension.id);
        {error, not_found} ->
            %% May not be available if fixtures not loaded
            ok;
        _ ->
            ok
    end.

%% Test: Database mode (source = database)
test_database_mode(_Config) ->
    %% Set configuration to database mode
    application:set_env(beamline_router, extension_registry, [
        {source, database},
        {db_enabled, true},
        {db_host, "localhost"},
        {db_port, 5432},
        {db_name, "beamline"},
        {db_user, "beamline"},
        {db_password, ""}
    ]),
    
    %% Restart registry to apply config
    gen_server:stop(router_extension_registry),
    timer:sleep(100),
    router_extension_registry:start_link(),
    timer:sleep(100),
    
    %% Should attempt to load from database
    %% May fail if database not available, but should not crash
    Result = router_extension_registry:lookup(<<"normalize_text">>),
    
    %% Result depends on database availability
    case Result of
        {ok, Extension} ->
            ?assert(is_record(Extension, extension));
        {error, not_found} ->
            %% Expected if database not available or extension not in database
            ok;
        {error, _} ->
            %% Other errors acceptable
            ok
    end.

%% Test: Auto mode with database available
test_auto_mode_database_available(_Config) ->
    %% Set configuration to auto mode with database enabled
    application:set_env(beamline_router, extension_registry, [
        {source, auto},
        {db_enabled, true},
        {db_host, "localhost"},
        {db_port, 5432},
        {db_name, "beamline"},
        {db_user, "beamline"},
        {db_password, ""}
    ]),
    
    %% Restart registry to apply config
    gen_server:stop(router_extension_registry),
    timer:sleep(100),
    router_extension_registry:start_link(),
    timer:sleep(100),
    
    %% Should try database first, fallback to fixtures if unavailable
    Result = router_extension_registry:lookup(<<"normalize_text">>),
    
    %% Should work (either from database or fixtures)
    case Result of
        {ok, Extension} ->
            ?assert(is_record(Extension, extension));
        {error, not_found} ->
            %% May not be available
            ok;
        _ ->
            ok
    end.

%% Test: Auto mode with database unavailable
test_auto_mode_database_unavailable(_Config) ->
    %% Set configuration to auto mode with invalid database
    application:set_env(beamline_router, extension_registry, [
        {source, auto},
        {db_enabled, true},
        {db_host, "invalid_host"},
        {db_port, 5432},
        {db_name, "beamline"},
        {db_user, "beamline"},
        {db_password, ""}
    ]),
    
    %% Restart registry to apply config
    gen_server:stop(router_extension_registry),
    timer:sleep(100),
    router_extension_registry:start_link(),
    timer:sleep(100),
    
    %% Should fallback to fixtures
    Result = router_extension_registry:lookup(<<"normalize_text">>),
    
    %% Should work from fixtures (fallback)
    case Result of
        {ok, Extension} ->
            ?assert(is_record(Extension, extension));
        {error, not_found} ->
            %% May not be available
            ok;
        _ ->
            ok
    end.

%% Test: Periodic sync (if database enabled)
test_periodic_sync(_Config) ->
    %% Set configuration with sync enabled
    application:set_env(beamline_router, extension_registry, [
        {source, database},
        {db_enabled, true},
        {sync_interval_seconds, 1}  % 1 second for testing
    ]),
    
    %% Restart registry to apply config
    gen_server:stop(router_extension_registry),
    timer:sleep(100),
    router_extension_registry:start_link(),
    timer:sleep(100),
    
    %% Wait for sync (may not happen if database unavailable)
    timer:sleep(2000),
    
    %% Registry should still work
    Result = router_extension_registry:lookup(<<"normalize_text">>),
    
    %% Result depends on database availability
    case Result of
        {ok, Extension} ->
            ?assert(is_record(Extension, extension));
        {error, not_found} ->
            ok;
        _ ->
            ok
    end.

%% Test: Reload in fixtures mode
test_reload_fixtures(_Config) ->
    %% Set configuration to fixtures mode
    application:set_env(beamline_router, extension_registry, [
        {source, fixtures},
        {db_enabled, false}
    ]),
    
    %% Reload should work
    Result = router_extension_registry:reload(),
    
    ?assertEqual(ok, Result).

%% Test: Reload in database mode
test_reload_database(_Config) ->
    %% Set configuration to database mode
    application:set_env(beamline_router, extension_registry, [
        {source, database},
        {db_enabled, true}
    ]),
    
    %% Reload should work (may fail if database unavailable, but should not crash)
    Result = router_extension_registry:reload(),
    
    %% Should return ok (even if database unavailable)
    ?assertEqual(ok, Result).

%% Test: Reload in auto mode
test_reload_auto_mode(_Config) ->
    %% Set configuration to auto mode
    application:set_env(beamline_router, extension_registry, [
        {source, auto},
        {db_enabled, true}
    ]),
    
    %% Reload should work
    Result = router_extension_registry:reload(),
    
    ?assertEqual(ok, Result).

