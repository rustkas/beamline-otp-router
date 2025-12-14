%% @doc Dashboard Configuration and Data Tests
%%
%% Tests dashboard configuration, data aggregation, and query functions.
%% Validates that dashboard modules provide correct data structures and
%% that dashboard queries return expected data formats.
%%
%% @test_category dashboard, observability
-module(router_dashboard_test_SUITE).

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

%% Test functions
-export([
    test_r10_dashboard_config/1,
    test_performance_dashboard_config/1,
    test_health_dashboard_config/1,
    test_trigger_reason_dashboard_config/1,
    test_r10_dashboard_data/1,
    test_trigger_reason_distribution/1,
    test_circuit_state_summary/1,
    test_dashboard_aggregation/1
]).

all() ->
    [].

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_r10_dashboard_config,
            test_performance_dashboard_config,
            test_health_dashboard_config,
            test_trigger_reason_dashboard_config,
            test_r10_dashboard_data,
            test_trigger_reason_distribution,
            test_circuit_state_summary,
            test_dashboard_aggregation
        ]}
    ].

init_per_suite(Config) ->
    ok = router_suite_helpers:start_router_suite(),
    ok = router_r10_metrics:clear_metrics(),
    Config.

end_per_suite(_Config) ->
    router_suite_helpers:stop_router_suite(),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = router_test_utils:ensure_circuit_breaker_alive(),
    ok = router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test R10 dashboard configuration
test_r10_dashboard_config(_Config) ->
    Config = router_dashboard_config:get_r10_dashboard_config(),
    
    ?assert(is_map(Config)),
    ?assert(maps:is_key(title, Config)),
    ?assert(maps:is_key(panels, Config)),
    ?assert(maps:is_key(variables, Config)),
    
    Panels = maps:get(panels, Config),
    ?assert(is_list(Panels)),
    ?assert(length(Panels) > 0),
    
    Variables = maps:get(variables, Config),
    ?assert(is_list(Variables)),
    ?assert(length(Variables) > 0),
    
    ok.

%% @doc Test performance dashboard configuration
test_performance_dashboard_config(_Config) ->
    Config = router_dashboard_config:get_performance_dashboard_config(),
    
    ?assert(is_map(Config)),
    ?assert(maps:is_key(title, Config)),
    ?assert(maps:is_key(panels, Config)),
    
    Panels = maps:get(panels, Config),
    ?assert(is_list(Panels)),
    ?assert(length(Panels) > 0),
    
    ok.

%% @doc Test health dashboard configuration
test_health_dashboard_config(_Config) ->
    Config = router_dashboard_config:get_health_dashboard_config(),
    
    ?assert(is_map(Config)),
    ?assert(maps:is_key(title, Config)),
    ?assert(maps:is_key(panels, Config)),
    
    Panels = maps:get(panels, Config),
    ?assert(is_list(Panels)),
    ?assert(length(Panels) > 0),
    
    ok.

%% @doc Test trigger reason dashboard configuration
test_trigger_reason_dashboard_config(_Config) ->
    Config = router_dashboard_config:get_trigger_reason_dashboard_config(),
    
    ?assert(is_map(Config)),
    ?assert(maps:is_key(title, Config)),
    ?assert(maps:is_key(panels, Config)),
    
    Panels = maps:get(panels, Config),
    ?assert(is_list(Panels)),
    ?assert(length(Panels) >= 2),  %% At least pie and bar charts
    
    %% Verify pie chart panel exists
    PiePanel = find_panel_by_id(1, Panels),
    ?assert(PiePanel =/= false),
    
    %% Verify bar chart panel exists
    BarPanel = find_panel_by_id(2, Panels),
    ?assert(BarPanel =/= false),
    
    ok.

find_panel_by_id(Id, Panels) ->
    case [P || P <- Panels, maps:get(id, P) =:= Id] of
        [Panel | _] -> Panel;
        [] -> false
    end.

%% @doc Test R10 dashboard data aggregation
test_r10_dashboard_data(_Config) ->
    TenantId = <<"test_tenant_dashboard">>,
    ProviderId = <<"test_provider_dashboard">>,
    
    %% Create some test metrics
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    %% Get dashboard data
    Data = router_dashboard_data:get_r10_dashboard_data(),
    
    ?assert(is_map(Data)),
    ?assert(maps:is_key(circuit_states, Data)),
    ?assert(maps:is_key(state_transitions, Data)),
    ?assert(maps:is_key(trigger_reasons, Data)),
    
    ok.

%% @doc Test trigger reason distribution
test_trigger_reason_distribution(_Config) ->
    TenantId = <<"test_tenant_dist">>,
    ProviderId = <<"test_provider_dist">>,
    
    %% Trigger circuit breaker with failures
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 10)),
    
    %% Get trigger reason distribution
    Distribution = router_r10_metrics:get_trigger_reason_distribution(),
    
    ?assert(is_map(Distribution)),
    
    %% Get filtered distribution
    Filtered = router_r10_metrics:get_trigger_reason_distribution(#{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    
    ?assert(is_map(Filtered)),
    
    ok.

%% @doc Test circuit state summary
test_circuit_state_summary(_Config) ->
    TenantId = <<"test_tenant_summary">>,
    ProviderId = <<"test_provider_summary">>,
    
    %% Create circuit breaker state
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    %% Get state summary
    Summary = router_r10_metrics:get_circuit_state_summary(),
    
    ?assert(is_map(Summary)),
    
    %% Get filtered summary
    Filtered = router_r10_metrics:get_circuit_state_summary(#{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    
    ?assert(is_map(Filtered)),
    
    ok.

%% @doc Test dashboard aggregation functions
test_dashboard_aggregation(_Config) ->
    TenantId = <<"test_tenant_agg">>,
    ProviderId = <<"test_provider_agg">>,
    
    %% Create some metrics
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    %% Test get_metrics_for_dashboard
    Metrics = router_metrics:get_metrics_for_dashboard(router_circuit_breaker_state),
    ?assert(is_map(Metrics)),
    
    %% Test aggregate_metrics_by_label
    Aggregated = router_metrics:aggregate_metrics_by_label(
        router_circuit_breaker_trigger_reason,
        reason
    ),
    ?assert(is_map(Aggregated)),
    
    %% Test get_metric_time_series
    TimeSeries = router_metrics:get_metric_time_series(router_circuit_breaker_state, 3600),
    ?assert(is_list(TimeSeries)),
    
    ok.
