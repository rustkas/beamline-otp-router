%% @doc Dashboard Configuration and Metadata
%%
%% Provides dashboard configuration, panel definitions, and metadata
%% for Grafana dashboards. This module serves as the source of truth
%% for dashboard structure and panel queries.
%%
%% Dashboards:
%% - R10 Circuit Breaker Dashboard
%% - Router Performance Dashboard
%% - Router Health Dashboard
%% - Trigger Reason Dashboard
%%
%% @see router_dashboard_data.erl For dashboard data aggregation
-module(router_dashboard_config).

-export([
    get_r10_dashboard_config/0,
    get_performance_dashboard_config/0,
    get_health_dashboard_config/0,
    get_trigger_reason_dashboard_config/0,
    get_dashboard_panels/1,
    get_dashboard_variables/1
]).

-include("beamline_router.hrl").

%% ============================================================================
%% Dashboard Configurations
%% ============================================================================

%% @doc Get R10 Circuit Breaker dashboard configuration
%% Returns dashboard metadata including panels, variables, and queries
-spec get_r10_dashboard_config() -> map().
get_r10_dashboard_config() ->
    #{
        title => <<"R10 Circuit Breaker Dashboard">>,
        description => <<"Circuit breaker state, transitions, and trigger reasons">>,
        panels => get_r10_panels(),
        variables => get_r10_variables(),
        refresh_interval => <<"30s">>,
        time_range => <<"now-1h">>
    }.

%% @doc Get Router Performance dashboard configuration
-spec get_performance_dashboard_config() -> map().
get_performance_dashboard_config() ->
    #{
        title => <<"Router Performance Dashboard">>,
        description => <<"Request throughput, latency, and performance metrics">>,
        panels => get_performance_panels(),
        variables => get_performance_variables(),
        refresh_interval => <<"30s">>,
        time_range => <<"now-1h">>
    }.

%% @doc Get Router Health dashboard configuration
-spec get_health_dashboard_config() -> map().
get_health_dashboard_config() ->
    #{
        title => <<"Router Health Dashboard">>,
        description => <<"System health, resource usage, and component status">>,
        panels => get_health_panels(),
        variables => get_health_variables(),
        refresh_interval => <<"30s">>,
        time_range => <<"now-1h">>
    }.

%% @doc Get Trigger Reason dashboard configuration
-spec get_trigger_reason_dashboard_config() -> map().
get_trigger_reason_dashboard_config() ->
    #{
        title => <<"Circuit Breaker Trigger Reasons">>,
        description => <<"Distribution of circuit breaker trigger reasons">>,
        panels => get_trigger_reason_panels(),
        variables => get_trigger_reason_variables(),
        refresh_interval => <<"30s">>,
        time_range => <<"now-1h">>
    }.

%% @doc Get dashboard panels by dashboard type
-spec get_dashboard_panels(atom()) -> list(map()).
get_dashboard_panels(r10) ->
    get_r10_panels();
get_dashboard_panels(performance) ->
    get_performance_panels();
get_dashboard_panels(health) ->
    get_health_panels();
get_dashboard_panels(trigger_reason) ->
    get_trigger_reason_panels();
get_dashboard_panels(_) ->
    [].

%% @doc Get dashboard variables by dashboard type
-spec get_dashboard_variables(atom()) -> list(map()).
get_dashboard_variables(r10) ->
    get_r10_variables();
get_dashboard_variables(performance) ->
    get_performance_variables();
get_dashboard_variables(health) ->
    get_health_variables();
get_dashboard_variables(trigger_reason) ->
    get_trigger_reason_variables();
get_dashboard_variables(_) ->
    [].

%% ============================================================================
%% R10 Dashboard Panels
%% ============================================================================

get_r10_panels() ->
    [
        #{
            id => 1,
            title => <<"Circuit Breaker State">>,
            type => <<"gauge">>,
            metric => router_circuit_breaker_state,
            description => <<"Current circuit breaker state per tenant/provider">>
        },
        #{
            id => 2,
            title => <<"State Transitions">>,
            type => <<"graph">>,
            metric => router_circuit_breaker_state_transitions_total,
            description => <<"Circuit breaker state transition rates over time">>
        },
        #{
            id => 3,
            title => <<"Trigger Reasons">>,
            type => <<"piechart">>,
            metric => router_circuit_breaker_trigger_reason,
            description => <<"Distribution of trigger reasons">>
        },
        #{
            id => 4,
            title => <<"Error Rate">>,
            type => <<"graph">>,
            metric => router_circuit_breaker_error_rate,
            description => <<"Sliding window error rate over time">>
        },
        #{
            id => 5,
            title => <<"Timeout Remaining">>,
            type => <<"graph">>,
            metric => router_circuit_breaker_timeout_remaining_ms,
            description => <<"Time until half-open transition">>
        },
        #{
            id => 6,
            title => <<"Open Circuits">>,
            type => <<"stat">>,
            metric => router_circuit_breaker_state,
            description => <<"Count of circuits in open state">>
        }
    ].

get_r10_variables() ->
    [
        #{
            name => <<"tenant_id">>,
            type => <<"query">>,
            query => <<"label_values(router_circuit_breaker_state, tenant_id)">>,
            current => #{value => <<"$__all">>},
            options => []
        },
        #{
            name => <<"provider_id">>,
            type => <<"query">>,
            query => <<"label_values(router_circuit_breaker_state, provider_id)">>,
            current => #{value => <<"$__all">>},
            options => []
        }
    ].

%% ============================================================================
%% Performance Dashboard Panels
%% ============================================================================

get_performance_panels() ->
    [
        #{
            id => 1,
            title => <<"Request Throughput">>,
            type => <<"graph">>,
            metric => router_requests_total,
            description => <<"Requests per second over time">>
        },
        #{
            id => 2,
            title => <<"Request Latency (P95)">>,
            type => <<"graph">>,
            metric => router_request_duration_seconds,
            description => <<"P95 latency over time">>
        },
        #{
            id => 3,
            title => <<"Request Latency (P99)">>,
            type => <<"graph">>,
            metric => router_request_duration_seconds,
            description => <<"P99 latency over time">>
        },
        #{
            id => 4,
            title => <<"Error Rate">>,
            type => <<"graph">>,
            metric => router_request_errors_total,
            description => <<"Error rate percentage over time">>
        },
        #{
            id => 5,
            title => <<"Active Requests">>,
            type => <<"stat">>,
            metric => router_requests_active,
            description => <<"Currently active requests">>
        }
    ].

get_performance_variables() ->
    [
        #{
            name => <<"tenant_id">>,
            type => <<"query">>,
            query => <<"label_values(router_requests_total, tenant_id)">>,
            current => #{value => <<"$__all">>},
            options => []
        }
    ].

%% ============================================================================
%% Health Dashboard Panels
%% ============================================================================

get_health_panels() ->
    [
        #{
            id => 1,
            title => <<"System Health Status">>,
            type => <<"stat">>,
            metric => router_health_status,
            description => <<"Overall system health (0=healthy, 1=degraded, 2=unhealthy)">>
        },
        #{
            id => 2,
            title => <<"Memory Usage">>,
            type => <<"graph">>,
            metric => router_memory_usage_bytes,
            description => <<"Memory usage over time">>
        },
        #{
            id => 3,
            title => <<"Process Count">>,
            type => <<"graph">>,
            metric => router_process_count,
            description => <<"Erlang process count over time">>
        },
        #{
            id => 4,
            title => <<"ETS Table Size">>,
            type => <<"graph">>,
            metric => router_ets_table_size,
            description => <<"ETS table sizes over time">>
        },
        #{
            id => 5,
            title => <<"Component Health">>,
            type => <<"table">>,
            metric => router_component_health,
            description => <<"Health status of individual components">>
        }
    ].

get_health_variables() ->
    [
        #{
            name => <<"component">>,
            type => <<"query">>,
            query => <<"label_values(router_component_health, component)">>,
            current => #{value => <<"$__all">>},
            options => []
        }
    ].

%% ============================================================================
%% Trigger Reason Dashboard Panels
%% ============================================================================

get_trigger_reason_panels() ->
    [
        #{
            id => 1,
            title => <<"Trigger Reason Distribution (Pie)">>,
            type => <<"piechart">>,
            metric => router_circuit_breaker_trigger_reason,
            description => <<"Pie chart showing distribution of trigger reasons">>
        },
        #{
            id => 2,
            title => <<"Trigger Reason Distribution (Bar)">>,
            type => <<"barchart">>,
            metric => router_circuit_breaker_trigger_reason,
            description => <<"Bar chart showing trigger reason counts">>
        },
        #{
            id => 3,
            title => <<"Trigger Reason Over Time">>,
            type => <<"graph">>,
            metric => router_circuit_breaker_trigger_reason,
            description => <<"Trigger reason counts over time">>
        },
        #{
            id => 4,
            title => <<"Trigger Reason by Tenant/Provider">>,
            type => <<"table">>,
            metric => router_circuit_breaker_trigger_reason,
            description => <<"Trigger reason breakdown by tenant and provider">>
        }
    ].

get_trigger_reason_variables() ->
    [
        #{
            name => <<"tenant_id">>,
            type => <<"query">>,
            query => <<"label_values(router_circuit_breaker_trigger_reason, tenant_id)">>,
            current => #{value => <<"$__all">>},
            options => []
        },
        #{
            name => <<"provider_id">>,
            type => <<"query">>,
            query => <<"label_values(router_circuit_breaker_trigger_reason, provider_id)">>,
            current => #{value => <<"$__all">>},
            options => []
        },
        #{
            name => <<"reason">>,
            type => <<"query">>,
            query => <<"label_values(router_circuit_breaker_trigger_reason, reason)">>,
            current => #{value => <<"$__all">>},
            options => []
        }
    ].

