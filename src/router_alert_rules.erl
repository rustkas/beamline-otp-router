%% @doc Alert Rules Definitions
%%
%% Defines alert rules for:
%% - R13 metrics under faults
%% - Circuit breaker state changes
%% - High error rates
%% - Performance degradation
%%
%% Alert rules are evaluated periodically and trigger alerts when conditions are met.
%%
%% @see router_alerts.erl For alert evaluation and state management
-module(router_alert_rules).

-export([
    get_all_rules/0,
    get_rule/1,
    get_r13_fault_rules/0,
    get_circuit_breaker_rules/0,
    get_error_rate_rules/0,
    get_performance_rules/0,
    validate_rule/1
]).

-include("beamline_router.hrl").

%% ============================================================================
%% Alert Rule Definitions
%% ============================================================================

%% @doc Get all alert rules
-spec get_all_rules() -> list(map()).
get_all_rules() ->
    get_r13_fault_rules() ++
    get_circuit_breaker_rules() ++
    get_error_rate_rules() ++
    get_performance_rules().

%% @doc Get alert rule by ID
-spec get_rule(binary()) -> {ok, map()} | {error, not_found}.
get_rule(RuleId) ->
    AllRules = get_all_rules(),
    case lists:keyfind(RuleId, id, AllRules) of
        false ->
            {error, not_found};
        Rule ->
            {ok, Rule}
    end.

%% @doc Get R13 metrics under faults alert rules
-spec get_r13_fault_rules() -> list(map()).
get_r13_fault_rules() ->
    [
        #{
            id => <<"r13_high_fault_rate">>,
            name => <<"R13 High Fault Rate">>,
            description => <<"Fault rate exceeds threshold for extended period">>,
            severity => <<"warning">>,
            metric => router_fault_rate,
            condition => #{
                type => threshold,
                operator => greater_than,
                threshold => 0.1,  %% 10% fault rate
                duration_seconds => 300  %% 5 minutes
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"r13_critical_fault_rate">>,
            name => <<"R13 Critical Fault Rate">>,
            description => <<"Fault rate exceeds critical threshold">>,
            severity => <<"critical">>,
            metric => router_fault_rate,
            condition => #{
                type => threshold,
                operator => greater_than,
                threshold => 0.5,  %% 50% fault rate
                duration_seconds => 60  %% 1 minute
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"r13_fault_count_spike">>,
            name => <<"R13 Fault Count Spike">>,
            description => <<"Sudden spike in fault count">>,
            severity => <<"warning">>,
            metric => router_faults_total,
            condition => #{
                type => rate_of_change,
                operator => greater_than,
                threshold => 100,  %% 100 faults per minute
                duration_seconds => 60
            },
            labels => #{},
            enabled => true
        }
    ].

%% @doc Get circuit breaker state change alert rules
-spec get_circuit_breaker_rules() -> list(map()).
get_circuit_breaker_rules() ->
    [
        #{
            id => <<"circuit_breaker_open_too_long">>,
            name => <<"Circuit Breaker Open Too Long">>,
            description => <<"Circuit breaker has been open for extended period">>,
            severity => <<"warning">>,
            metric => router_circuit_breaker_state,
            condition => #{
                type => state_duration,
                state => open,
                duration_seconds => 300  %% 5 minutes
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"circuit_breaker_flapping">>,
            name => <<"Circuit Breaker Flapping">>,
            description => <<"Circuit breaker transitioning too frequently">>,
            severity => <<"warning">>,
            metric => router_circuit_breaker_state_transitions_total,
            condition => #{
                type => rate_of_change,
                operator => greater_than,
                threshold => 10,  %% 10 transitions per 5 minutes
                duration_seconds => 300
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"circuit_breaker_multiple_open">>,
            name => <<"Multiple Circuits Open">>,
            description => <<"Multiple circuit breakers are open simultaneously">>,
            severity => <<"critical">>,
            metric => router_circuit_breaker_state,
            condition => #{
                type => count,
                operator => greater_than,
                threshold => 5,  %% More than 5 circuits open
                duration_seconds => 60
            },
            labels => #{state => open},
            enabled => true
        }
    ].

%% @doc Get high error rate alert rules
-spec get_error_rate_rules() -> list(map()).
get_error_rate_rules() ->
    [
        #{
            id => <<"high_error_rate_warning">>,
            name => <<"High Error Rate Warning">>,
            description => <<"Error rate exceeds warning threshold">>,
            severity => <<"warning">>,
            metric => router_request_errors_total,
            condition => #{
                type => rate,
                operator => greater_than,
                threshold => 0.05,  %% 5% error rate
                duration_seconds => 300  %% 5 minutes
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"high_error_rate_critical">>,
            name => <<"High Error Rate Critical">>,
            description => <<"Error rate exceeds critical threshold">>,
            severity => <<"critical">>,
            metric => router_request_errors_total,
            condition => #{
                type => rate,
                operator => greater_than,
                threshold => 0.2,  %% 20% error rate
                duration_seconds => 60  %% 1 minute
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"error_rate_spike">>,
            name => <<"Error Rate Spike">>,
            description => <<"Sudden spike in error rate">>,
            severity => <<"warning">>,
            metric => router_request_errors_total,
            condition => #{
                type => rate_of_change,
                operator => greater_than,
                threshold => 0.1,  %% 10% increase per minute
                duration_seconds => 60
            },
            labels => #{},
            enabled => true
        }
    ].

%% @doc Get performance degradation alert rules
-spec get_performance_rules() -> list(map()).
get_performance_rules() ->
    [
        #{
            id => <<"high_latency_p95">>,
            name => <<"High Latency P95">>,
            description => <<"P95 latency exceeds threshold">>,
            severity => <<"warning">>,
            metric => router_request_duration_seconds,
            condition => #{
                type => percentile,
                percentile => 95,
                operator => greater_than,
                threshold => 1.0,  %% 1 second
                duration_seconds => 300  %% 5 minutes
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"high_latency_p99">>,
            name => <<"High Latency P99">>,
            description => <<"P99 latency exceeds threshold">>,
            severity => <<"critical">>,
            metric => router_request_duration_seconds,
            condition => #{
                type => percentile,
                percentile => 99,
                operator => greater_than,
                threshold => 2.0,  %% 2 seconds
                duration_seconds => 300  %% 5 minutes
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"throughput_degradation">>,
            name => <<"Throughput Degradation">>,
            description => <<"Request throughput has degraded significantly">>,
            severity => <<"warning">>,
            metric => router_requests_total,
            condition => #{
                type => rate_of_change,
                operator => less_than,
                threshold => -0.3,  %% 30% decrease
                duration_seconds => 300  %% 5 minutes
            },
            labels => #{},
            enabled => true
        },
        #{
            id => <<"memory_usage_high">>,
            name => <<"High Memory Usage">>,
            description => <<"Memory usage exceeds threshold">>,
            severity => <<"warning">>,
            metric => router_memory_usage_bytes,
            condition => #{
                type => threshold,
                operator => greater_than,
                threshold => 1073741824,  %% 1 GB
                duration_seconds => 300
            },
            labels => #{},
            enabled => true
        }
    ].

%% @doc Validate alert rule structure
-spec validate_rule(map()) -> {ok, map()} | {error, term()}.
validate_rule(Rule) ->
    RequiredFields = [id, name, description, severity, metric, condition, enabled],
    MissingFields = lists:filter(fun(Field) ->
        not maps:is_key(Field, Rule)
    end, RequiredFields),
    
    case MissingFields of
        [] ->
            %% Validate severity
            ValidSeverities = [<<"info">>, <<"warning">>, <<"critical">>],
            Severity = maps:get(severity, Rule),
            case lists:member(Severity, ValidSeverities) of
                true ->
                    {ok, Rule};
                false ->
                    {error, {invalid_severity, Severity}}
            end;
        _ ->
            {error, {missing_fields, MissingFields}}
    end.

