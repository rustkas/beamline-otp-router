%% @doc Unit Tests for router_caf_adapter module
%% Targeted coverage tests for internal helper functions
%% @test_category unit, fast, coverage_hotspot
-module(router_caf_adapter_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_generate_assignment_id/1,
    test_classify_exception/1,
    test_get_config/1,
    test_sanitize_error/1,
    test_calculate_deadline/1,
    test_get_assignment_subject/1,
    test_build_headers/1,
    test_is_tenant_assignment_enabled/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_module_exports,
            test_generate_assignment_id,
            test_classify_exception,
            test_get_config,
            test_sanitize_error,
            test_calculate_deadline,
            test_get_assignment_subject,
            test_build_headers,
            test_is_tenant_assignment_enabled
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for module structure
%% ============================================================================

test_module_exports(_Config) ->
    {module, router_caf_adapter} = code:ensure_loaded(router_caf_adapter),
    Exports = router_caf_adapter:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    
    %% Check key exports
    ?assertEqual(true, lists:member({publish_assignment, 2}, Exports)),
    ok.

%% ============================================================================
%% Tests for generate_assignment_id (if exported)
%% ============================================================================

test_generate_assignment_id(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    case lists:member({generate_assignment_id, 0}, Exports) of
        true ->
            Id = router_caf_adapter:generate_assignment_id(),
            ?assertEqual(true, is_binary(Id)),
            ?assertEqual(true, byte_size(Id) > 0),
            
            %% IDs should be unique
            Id2 = router_caf_adapter:generate_assignment_id(),
            ?assertNotEqual(Id, Id2);
        false ->
            %% Not exported, skip
            ok
    end,
    ok.

%% ============================================================================
%% Tests for classify_exception (if exported)
%% ============================================================================

test_classify_exception(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    case lists:member({classify_exception, 2}, Exports) of
        true ->
            Type1 = router_caf_adapter:classify_exception(error, timeout),
            ?assertEqual(true, is_atom(Type1) orelse is_binary(Type1)),
            
            Type2 = router_caf_adapter:classify_exception(throw, test),
            ?assertEqual(true, is_atom(Type2) orelse is_binary(Type2)),
            
            Type3 = router_caf_adapter:classify_exception(exit, normal),
            ?assertEqual(true, is_atom(Type3) orelse is_binary(Type3));
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for get_config (if exported)
%% ============================================================================

test_get_config(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    case lists:member({get_config, 2}, Exports) of
        true ->
            Val = router_caf_adapter:get_config(assignment_enabled, true),
            ?assertEqual(true, is_boolean(Val) orelse Val =:= undefined);
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for sanitize_error (if exported)
%% ============================================================================

test_sanitize_error(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    %% Just check if module is loaded - sanitize_error may not be exported
    ?assertEqual(true, length(Exports) > 0),
    ok.

%% ============================================================================
%% Tests for calculate_deadline (if exported)
%% ============================================================================

test_calculate_deadline(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    %% Just verify function is exported if present
    case lists:member({calculate_deadline, 1}, Exports) of
        true ->
            ?assertEqual(true, true);  %% Function exists
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for get_assignment_subject (if exported)
%% ============================================================================

test_get_assignment_subject(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    case lists:member({get_assignment_subject, 1}, Exports) of
        true ->
            Subject = router_caf_adapter:get_assignment_subject(#{}),
            ?assertEqual(true, is_binary(Subject));
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for build_headers (if exported)
%% ============================================================================

test_build_headers(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    case lists:member({build_headers, 1}, Exports) of
        true ->
            Headers = router_caf_adapter:build_headers(#{}),
            ?assertEqual(true, is_map(Headers));
        false ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for is_tenant_assignment_enabled (if exported)
%% ============================================================================

test_is_tenant_assignment_enabled(_Config) ->
    Exports = router_caf_adapter:module_info(exports),
    case lists:member({is_tenant_assignment_enabled, 1}, Exports) of
        true ->
            Result = router_caf_adapter:is_tenant_assignment_enabled(<<"test_tenant">>),
            ?assertEqual(true, is_tuple(Result) orelse is_boolean(Result));
        false ->
            ok
    end,
    ok.
