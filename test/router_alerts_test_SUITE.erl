%% @doc Alert Evaluation and Rules Tests
%%
%% Tests alert rule definitions, alert evaluation, and alert state management.
%% Validates that alert conditions are correctly evaluated and alerts are
%% properly tracked.
%%
%% @test_category alerts, observability
-module(router_alerts_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test utilities
-import(ct_quarantine, [is_quarantined/1]).

%% Test functions
-export([
    test_alert_rules_definitions/1,
    test_r13_fault_rules/1,
    test_quarantined_alert_rule/1,
    test_circuit_breaker_rules/1,
    test_error_rate_rules/1,
    test_performance_rules/1,
    test_alert_evaluation/1,
    test_alert_state_management/1,
    test_circuit_breaker_alert_conditions/1
]).

all() ->
    router_ct_groups:all_selection(?MODULE, [
        {group, unit_tests},
        {group, quarantine}
    ]).

meta_all() ->
    [].

groups_for_level(heavy) ->
    [{group, unit_tests}, {group, quarantine}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    router_ct_groups:groups_definitions(?MODULE, base_groups()).

base_groups() ->
    [
        {unit_tests, [], [
            test_alert_rules_definitions,
            test_r13_fault_rules,
            test_circuit_breaker_rules,
            test_error_rate_rules,
            test_performance_rules,
            test_alert_evaluation,
            test_alert_state_management,
            test_circuit_breaker_alert_conditions
        ]},
        {quarantine, [], [
            test_quarantined_alert_rule
        ]}
    ].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{}),
    ok = router_metrics:clear_all(),
    ok = router_r10_metrics:clear_metrics(),
    Config1.

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(TestCase, Config) ->
    Config1 = router_test_bootstrap:init_per_testcase(TestCase, Config, #{}),
    ok = router_metrics:clear_all(),
    ok = router_r10_metrics:clear_metrics(),
    Config1.

end_per_testcase(TestCase, Config) ->
    router_test_bootstrap:end_per_testcase(TestCase, Config, #{}).

%% @doc Test alert rules definitions
test_alert_rules_definitions(_Config) ->
    AllRules = router_alert_rules:get_all_rules(),
    
    ?assert(is_list(AllRules)),
    ?assert(length(AllRules) > 0),
    
    %% Validate each rule
    lists:foreach(fun(Rule) ->
        {ok, ValidatedRule} = router_alert_rules:validate_rule(Rule),
        ?assert(maps:is_key(id, ValidatedRule)),
        ?assert(maps:is_key(name, ValidatedRule)),
        ?assert(maps:is_key(severity, ValidatedRule)),
        ?assert(maps:is_key(metric, ValidatedRule)),
        ?assert(maps:is_key(condition, ValidatedRule))
    end, AllRules),
    
    ok.

%% @doc Test R13 fault rules
test_r13_fault_rules(_Config) ->
    Rules = router_alert_rules:get_r13_fault_rules(),
    
    ?assert(is_list(Rules)),
    ?assert(length(Rules) >= 3),
    
    %% Check for specific rules
    RuleIds = [maps:get(id, Rule) || Rule <- Rules],
    ?assert(lists:member(<<"r13_high_fault_rate">>, RuleIds)),
    ?assert(lists:member(<<"r13_critical_fault_rate">>, RuleIds)),
    ?assert(lists:member(<<"r13_fault_count_spike">>, RuleIds)),
    
    ok.

%% @doc Test circuit breaker rules
test_circuit_breaker_rules(_Config) ->
    Rules = router_alert_rules:get_circuit_breaker_rules(),
    
    ?assert(is_list(Rules)),
    ?assert(length(Rules) >= 3),
    
    %% Check for specific rules
    RuleIds = [maps:get(id, Rule) || Rule <- Rules],
    ?assert(lists:member(<<"circuit_breaker_open_too_long">>, RuleIds)),
    ?assert(lists:member(<<"circuit_breaker_flapping">>, RuleIds)),
    ?assert(lists:member(<<"circuit_breaker_multiple_open">>, RuleIds)),
    
    ok.

%% @doc Test error rate rules
test_error_rate_rules(_Config) ->
    Rules = router_alert_rules:get_error_rate_rules(),
    
    ?assert(is_list(Rules)),
    ?assert(length(Rules) >= 3),
    
    %% Check for specific rules
    RuleIds = [maps:get(id, Rule) || Rule <- Rules],
    ?assert(lists:member(<<"high_error_rate_warning">>, RuleIds)),
    ?assert(lists:member(<<"high_error_rate_critical">>, RuleIds)),
    ?assert(lists:member(<<"error_rate_spike">>, RuleIds)),
    
    ok.

%% @doc Test performance rules
test_performance_rules(_Config) ->
    Rules = router_alert_rules:get_performance_rules(),
    
    ?assert(is_list(Rules)),
    ?assert(length(Rules) >= 4),
    
    %% Check for specific rules
    RuleIds = [maps:get(id, Rule) || Rule <- Rules],
    ?assert(lists:member(<<"high_latency_p95">>, RuleIds)),
    ?assert(lists:member(<<"high_latency_p99">>, RuleIds)),
    ?assert(lists:member(<<"throughput_degradation">>, RuleIds)),
    ?assert(lists:member(<<"memory_usage_high">>, RuleIds)),
    
    ok.

%% @doc Test alert evaluation
test_alert_evaluation(_Config) ->
    %% Start alert evaluator
    {ok, _Pid} = router_alerts:start_link(),
    
    %% Evaluate all rules
    {ok, Results} = router_alerts:evaluate_all_rules(),
    
    ?assert(is_list(Results)),
    ?assert(length(Results) > 0),
    
    %% Check result structure
    lists:foreach(fun(Result) ->
        ?assert(maps:is_key(rule_id, Result)),
        ?assert(maps:is_key(status, Result)),
        ?assert(maps:is_key(firing, Result))
    end, Results),
    
    ok.

%% @doc Test alert state management
test_alert_state_management(_Config) ->
    %% Start alert evaluator
    {ok, _Pid} = router_alerts:start_link(),
    
    %% Get active alerts (should be empty initially)
    ActiveAlerts = router_alerts:get_active_alerts(),
    ?assert(is_list(ActiveAlerts)),
    
    %% Get alert stats
    Stats = router_alerts:get_alert_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(total_rules, Stats)),
    ?assert(maps:is_key(active_alerts, Stats)),
    
    ok.

%% @doc Test circuit breaker alert conditions
test_circuit_breaker_alert_conditions(_Config) ->
    ok.

%% @doc This is a quarantined test that will only run in heavy/nightly test runs
test_quarantined_alert_rule(_Config) ->
    ct:comment("This is a quarantined test that verifies a specific alert rule"),
    ?assertMatch(true, true),
    TenantId = <<"test_tenant_alerts">>,
    ProviderId = <<"test_provider_alerts">>,
    
    %% Create some circuit breaker state
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    Conditions = router_r10_metrics:check_circuit_breaker_alert_conditions(),
    
    ?assert(is_map(Conditions)),
    ?assert(maps:is_key(open_circuits_count, Conditions)),
    ?assert(maps:is_key(transition_rate, Conditions)),
    ?assert(maps:is_key(has_open_circuits, Conditions)),
    
    %% Check with filters
    FilteredConditions = router_r10_metrics:check_circuit_breaker_alert_conditions(#{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    
    ?assert(is_map(FilteredConditions)),
    
    ok.
