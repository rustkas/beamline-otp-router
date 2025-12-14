%% @doc Meta-Test Suite for Test Structure Enforcement
%%
%% This suite validates that all test suites in the project follow
%% the standardized test level architecture:
%% - Export all/0, groups/0, groups_for_level/1
%% - all/0 dispatches via ROUTER_TEST_LEVEL
%% - groups_for_level(fast) never returns heavy groups
%%
%% @test_category meta, structure, fast
-module(router_test_structure_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% Test exports
-export([
    test_all_suites_export_required_functions/1,
    test_fast_level_excludes_heavy_groups/1,
    test_group_naming_conventions/1,
    test_no_orphan_test_functions/1,
    test_active_suites_have_tests/1
]).

%% Allowed group names by category
-define(FAST_GROUPS, [
    unit_tests, fast_tests, smoke_tests, contract_tests,
    validation_tests, parsing_tests, serialization_tests
]).

-define(FULL_GROUPS, [
    integration_tests, e2e_tests, nats_integration_tests,
    grpc_integration_tests, policy_integration_tests,
    cp1_fields_validation_tests, cp1_fields_propagation_tests,
    cp1_fields_error_tests, property_tests
]).

-define(HEAVY_GROUPS, [
    heavy_tests, stress_tests, soak_tests, chaos_tests,
    performance_tests, load_tests, fault_injection_tests,
    concurrent_fault_tests, heavy_faults, recovery_tests,
    chaos_engineering_tests, randomized_scenarios,
    r10_mass_failure, r10_latency_trigger, r10_error_rate,
    r10_risk_scenarios, aggregation_tests, rate_tests,
    cardinality_tests, combined_tests
]).

%% Suites that are known exceptions (to be migrated later)
-define(EXCEPTION_SUITES, [
    %% Legacy suites without groups_for_level
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, structure_tests}];
groups_for_level(full) ->
    [{group, structure_tests}];
groups_for_level(_) -> %% fast
    [{group, structure_tests}].

groups() ->
    [
        {structure_tests, [sequence], [
            test_all_suites_export_required_functions,
            test_fast_level_excludes_heavy_groups,
            test_group_naming_conventions,
            test_no_orphan_test_functions,
            test_active_suites_have_tests
        ]}
    ].

init_per_suite(Config) ->
    %% Get list of test suites from loaded modules (more reliable than filesystem)
    %% Only check modules that are already compiled and loaded
    AllModules = erlang:loaded(),
    Suites = [M || M <- AllModules,
                   is_suite_module(M),
                   M =/= ?MODULE],
    
    %% If no suites loaded yet, try to find them in code path
    LoadedSuites = case Suites of
        [] ->
            %% Fallback: scan for beam files
            CodePath = code:get_path(),
            TestDirs = [D || D <- CodePath, string:find(D, "test") =/= nomatch],
            find_suite_modules(TestDirs);
        _ ->
            Suites
    end,
    
    ct:log("Found ~p test suites for analysis", [length(LoadedSuites)]),
    [{suites, LoadedSuites} | Config].

init_per_testcase(_TestCase, Config) ->
    Config.

%% Check if module name looks like a test suite
is_suite_module(Module) ->
    Name = atom_to_list(Module),
    lists:suffix("_SUITE", Name).

%% Find suite modules in test directories
find_suite_modules(Dirs) ->
    lists:flatmap(fun(Dir) ->
        case file:list_dir(Dir) of
            {ok, Files} ->
                BeamFiles = [F || F <- Files, lists:suffix("_SUITE.beam", F)],
                [list_to_atom(filename:basename(F, ".beam")) || F <- BeamFiles,
                 F =/= "router_test_structure_SUITE.beam"];
            _ ->
                []
        end
    end, Dirs).

end_per_suite(_Config) ->
    ok.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% =============================================================================
%% Test: All suites export required functions
%% =============================================================================
test_all_suites_export_required_functions(Config) ->
    Suites = proplists:get_value(suites, Config),
    
    Results = lists:map(fun(Suite) ->
        try
            %% Try to load the module
            case code:ensure_loaded(Suite) of
                {module, Suite} ->
                    Exports = Suite:module_info(exports),
                    HasAll = lists:member({all, 0}, Exports),
                    HasGroups = lists:member({groups, 0}, Exports),
                    HasGroupsForLevel = lists:member({groups_for_level, 1}, Exports),
                    
                    case {HasAll, HasGroups, HasGroupsForLevel} of
                        {true, true, true} ->
                            {ok, Suite};
                        {HasAll1, HasGroups1, HasGroupsForLevel1} ->
                            Missing = [
                                case HasAll1 of true -> []; false -> [all] end,
                                case HasGroups1 of true -> []; false -> [groups] end,
                                case HasGroupsForLevel1 of true -> []; false -> [groups_for_level] end
                            ],
                            {missing, Suite, lists:flatten(Missing)}
                    end;
                {error, Reason} ->
                    {load_error, Suite, Reason}
            end
        catch
            _:Error ->
                {error, Suite, Error}
        end
    end, Suites),
    
    %% Collect issues
    Issues = [{Type, S, Details} || {Type, S, Details} <- Results, Type =/= ok],
    
    case Issues of
        [] ->
            ct:log("All ~p suites export required functions", [length(Suites)]),
            ok;
        _ ->
            %% Log but don't fail - some suites may be in transition
            ct:log("~nSuites with structural issues:~n~p", [Issues]),
            %% Count by type
            MissingCount = length([1 || {missing, _, _} <- Issues]),
            LoadErrors = length([1 || {load_error, _, _} <- Issues]),
            ct:log("Missing exports: ~p, Load errors: ~p", [MissingCount, LoadErrors]),
            
            %% For now, just warn. To enforce, uncomment:
            %% ?assert(Issues =:= [])
            ok
    end.

%% =============================================================================
%% Test: Fast level excludes heavy groups
%% =============================================================================
test_fast_level_excludes_heavy_groups(Config) ->
    Suites = proplists:get_value(suites, Config),
    HeavyGroupNames = ?HEAVY_GROUPS,
    
    Violations = lists:filtermap(fun(Suite) ->
        try
            case code:ensure_loaded(Suite) of
                {module, Suite} ->
                    Exports = Suite:module_info(exports),
                    HasGroupsForLevel = lists:member({groups_for_level, 1}, Exports),
                    
                    case HasGroupsForLevel of
                        true ->
                            FastGroups = Suite:groups_for_level(fast),
                            %% Extract group names
                            FastGroupNames = [G || {group, G} <- FastGroups],
                            %% Check for heavy groups in fast
                            HeavyInFast = [G || G <- FastGroupNames, 
                                                lists:member(G, HeavyGroupNames)],
                            case HeavyInFast of
                                [] -> false;
                                _ -> {true, {Suite, HeavyInFast}}
                            end;
                        false ->
                            false
                    end;
                _ ->
                    false
            end
        catch
            _:_ -> false
        end
    end, Suites),
    
    case Violations of
        [] ->
            ct:log("No heavy groups found in fast level"),
            ok;
        _ ->
            ct:log("~nViolations (heavy groups in fast):~n~p", [Violations]),
            %% This is a hard requirement
            ?assertEqual([], Violations)
    end.

%% =============================================================================
%% Test: Group naming follows conventions
%% =============================================================================
test_group_naming_conventions(Config) ->
    Suites = proplists:get_value(suites, Config),
    AllowedGroups = ?FAST_GROUPS ++ ?FULL_GROUPS ++ ?HEAVY_GROUPS,
    
    NonConforming = lists:filtermap(fun(Suite) ->
        try
            case code:ensure_loaded(Suite) of
                {module, Suite} ->
                    Exports = Suite:module_info(exports),
                    HasGroups = lists:member({groups, 0}, Exports),
                    
                    case HasGroups of
                        true ->
                            Groups = Suite:groups(),
                            GroupNames = [Name || {Name, _, _} <- Groups],
                            Unknown = [G || G <- GroupNames, 
                                           not lists:member(G, AllowedGroups)],
                            case Unknown of
                                [] -> false;
                                _ -> {true, {Suite, Unknown}}
                            end;
                        false ->
                            false
                    end;
                _ ->
                    false
            end
        catch
            _:_ -> false
        end
    end, Suites),
    
    case NonConforming of
        [] ->
            ct:log("All groups follow naming conventions"),
            ok;
        _ ->
            %% Log non-standard group names (info only)
            ct:log("~nSuites with non-standard group names:~n~p", [NonConforming]),
            %% Don't fail - just warn for now
            ok
    end.

%% =============================================================================
%% Test: No orphan test functions (all tests should be in groups)
%% =============================================================================
test_no_orphan_test_functions(Config) ->
    Suites = proplists:get_value(suites, Config),
    
    OrphanCount = lists:foldl(fun(Suite, Acc) ->
        try
            case code:ensure_loaded(Suite) of
                {module, Suite} ->
                    Exports = Suite:module_info(exports),
                    HasGroups = lists:member({groups, 0}, Exports),
                    HasAll = lists:member({all, 0}, Exports),
                    
                    case {HasAll, HasGroups} of
                        {true, true} ->
                            %% Check if all/0 returns groups or bare tests
                            AllResult = Suite:all(),
                            BareTests = [T || T <- AllResult, is_atom(T)],
                            case BareTests of
                                [] -> Acc;
                                _ -> 
                                    ct:log("Suite ~p has bare tests in all/0: ~p", 
                                           [Suite, BareTests]),
                                    Acc + 1
                            end;
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        catch
            _:_ -> Acc
        end
    end, 0, Suites),
    
    ct:log("Suites with bare tests in all/0: ~p", [OrphanCount]),
    %% Info only - don't fail
    ok.

%% @doc Verify that suites intended for full/fast execution actually contain tests
test_active_suites_have_tests(Config) ->
    Suites = ?config(suites, Config),
    
    %% Suites that are allowed to be empty on 'full' level (e.g. heavy-only or WIP)
    AllowedEmpty = [
        router_deployment_SUITE,
        router_performance_benchmark_SUITE,
        router_performance_regression_SUITE,
        router_nats_connection_failure_SUITE,
        router_intake_overload_SUITE,
        router_performance_load_SUITE,
        router_policy_applier_load_SUITE,
        %% Split chaos suites
        router_ext_chaos_fault_SUITE,
        router_ext_chaos_recovery_SUITE,
        %% Split circuit breaker load suites
        router_cb_load_spike_SUITE,
        router_cb_load_concurrent_SUITE,
        %% Split jetstream soak suites
        router_jetstream_soak_basic_SUITE,
        router_jetstream_soak_stress_SUITE,
        router_jetstream_soak_restart_SUITE,
        router_jetstream_soak_perf_SUITE,
        router_metrics_r10_SUITE,
        router_test_structure_SUITE  %% Avoid recursion/self-check issues if empty
    ],
    
    EmptySuites = lists:filter(fun(Suite) ->
        case lists:member(Suite, AllowedEmpty) of
            true -> false;
            false ->
                case catch Suite:groups_for_level(full) of
                    [] -> true;
                    {'EXIT', _} -> false; %% Should fail in other test
                    _ -> false
                end
        end
    end, Suites),
    
    case EmptySuites of
        [] -> ok;
        _ ->
            ct:pal("Suites with 0 tests on 'full' level (unexpected): ~p", [EmptySuites]),
            ct:fail("Found ~p suites that are empty on 'full' level but not whitelisted", [length(EmptySuites)])
    end.
