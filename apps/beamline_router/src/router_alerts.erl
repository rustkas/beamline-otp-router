-module(router_alerts).

-doc "Alert Evaluation and State Management".
%%
%% Evaluates alert rules against current metrics and manages alert state.
%% Provides functions to:
%% - Evaluate alert conditions
%% - Track alert state (firing, resolved)
%% - Query active alerts
%% - Manage alert history
%%
%% @see router_alert_rules.erl For alert rule definitions

-behaviour(gen_server).

-export([
    start_link/0,
    evaluate_all_rules/0,
    evaluate_rule/1,
    get_active_alerts/0,
    get_alert_history/1,
    resolve_alert/1,
    get_alert_stats/0,
    %% Test/Management functions
    reset/0,
    add_rule/1,
    record_metric/2,
    list_rules/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("beamline_router.hrl").

-record(state, {
    active_alerts = #{} :: map(),  %% RuleId -> AlertState
    alert_history = [] :: list(),  %% List of alert events
    last_evaluation = undefined :: integer() | undefined
}).

-record(alert_state, {
    rule_id :: binary(),
    rule :: map(),
    status :: firing | resolved,
    first_fired :: integer(),
    last_fired :: integer(),
    last_resolved :: integer() | undefined,
    fire_count :: integer()
}).

%% ============================================================================
%% Public API
%% ============================================================================

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec evaluate_all_rules() -> {ok, list(map())}.
evaluate_all_rules() ->
    gen_server:call(?MODULE, evaluate_all_rules).

-spec evaluate_rule(binary()) -> {ok, map()} | {error, term()}.
evaluate_rule(RuleId) ->
    gen_server:call(?MODULE, {evaluate_rule, RuleId}).

-spec get_active_alerts() -> list(map()).
get_active_alerts() ->
    gen_server:call(?MODULE, get_active_alerts).

-spec get_alert_history(binary()) -> list(map()).
get_alert_history(RuleId) ->
    gen_server:call(?MODULE, {get_alert_history, RuleId}).

-spec resolve_alert(binary()) -> ok | {error, term()}.
resolve_alert(RuleId) ->
    gen_server:call(?MODULE, {resolve_alert, RuleId}).

-spec get_alert_stats() -> map().
get_alert_stats() ->
    gen_server:call(?MODULE, get_alert_stats).

%% Test/Management functions

-spec reset() -> ok.
reset() ->
    gen_server:call(?MODULE, reset).

-spec add_rule(#alert_rule{}) -> ok | {error, term()}.
add_rule(Rule) when is_record(Rule, alert_rule) ->
    gen_server:call(?MODULE, {add_rule, Rule}).

-spec record_metric(atom(), number()) -> ok.
record_metric(Metric, Value) when is_atom(Metric), is_number(Value) ->
    gen_server:call(?MODULE, {record_metric, Metric, Value}).

-spec list_rules() -> list(#alert_rule{}).
list_rules() ->
    gen_server:call(?MODULE, list_rules).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

init([]) ->
    %% Schedule periodic evaluation
    schedule_evaluation(),
    {ok, #state{}}.

handle_call(evaluate_all_rules, _From, State) ->
    Rules = router_alert_rules:get_all_rules(),
    Results = lists:map(fun(Rule) ->
        evaluate_rule_internal(Rule, State)
    end, Rules),
    NewState = update_alert_state(Results, State),
    {reply, {ok, Results}, NewState};

handle_call({evaluate_rule, RuleId}, _From, State) ->
    case router_alert_rules:get_rule(RuleId) of
        {ok, Rule} ->
            Result = evaluate_rule_internal(Rule, State),
            NewState = update_alert_state([Result], State),
            {reply, {ok, Result}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_active_alerts, _From, State) ->
    ActiveAlerts = maps:fold(fun(_, AlertState, Acc) ->
        case AlertState#alert_state.status of
            firing ->
                [alert_state_to_map(AlertState) | Acc];
            resolved ->
                Acc
        end
    end, [], State#state.active_alerts),
    {reply, ActiveAlerts, State};

handle_call({get_alert_history, RuleId}, _From, State) ->
    History = lists:filter(fun(Event) ->
        maps:get(rule_id, Event, undefined) =:= RuleId
    end, State#state.alert_history),
    {reply, History, State};

handle_call({resolve_alert, RuleId}, _From, State) ->
    case maps:get(RuleId, State#state.active_alerts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        AlertState ->
            NewAlertState = AlertState#alert_state{
                status = resolved,
                last_resolved = erlang:system_time(second)
            },
            NewActiveAlerts = maps:put(RuleId, NewAlertState, State#state.active_alerts),
            Event = #{
                rule_id => RuleId,
                event => resolved,
                timestamp => erlang:system_time(second)
            },
            NewHistory = [Event | State#state.alert_history],
            NewState = State#state{
                active_alerts = NewActiveAlerts,
                alert_history = NewHistory
            },
            {reply, ok, NewState}
    end;

handle_call(get_alert_stats, _From, State) ->
    Stats = #{
        total_rules => length(router_alert_rules:get_all_rules()),
        active_alerts => count_active_alerts(State#state.active_alerts),
        total_events => length(State#state.alert_history),
        last_evaluation => State#state.last_evaluation
    },
    {reply, Stats, State};

handle_call(reset, _From, _State) ->
    ok = router_alert_rules:delete_dynamic_rules(),
    {reply, ok, #state{}};

handle_call({add_rule, Rule}, _From, State) ->
    %% Store rule in router_alert_rules module
    case router_alert_rules:add_rule(Rule) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({record_metric, Metric, Value}, _From, State) ->
    %% Store metric value (using router_metrics or ETS)
    router_metrics:ensure(),
    try
        ets:insert(router_metrics, {Metric, Value}),
        {reply, ok, State}
    catch
        _:_ ->
            {reply, {error, metrics_unavailable}, State}
    end;

handle_call(list_rules, _From, State) ->
    Rules = router_alert_rules:get_all_rules(),
    {reply, Rules, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(evaluate_periodic, State) ->
    Rules = router_alert_rules:get_all_rules(),
    Results = lists:map(fun(Rule) ->
        evaluate_rule_internal(Rule, State)
    end, Rules),
    NewState = update_alert_state(Results, State#state{last_evaluation = erlang:system_time(second)}),
    schedule_evaluation(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

evaluate_rule_internal(Rule, _) ->
    RuleId = maps:get(id, Rule),
    Enabled = maps:get(enabled, Rule, true),
    
    case Enabled of
        false ->
            #{
                rule_id => RuleId,
                status => disabled,
                firing => false
            };
        true ->
            Condition = maps:get(condition, Rule),
            Metric = maps:get(metric, Rule),
            Labels = maps:get(labels, Rule, #{}),
            
            Firing = evaluate_condition(Condition, Metric, Labels),
            
            #{
                rule_id => RuleId,
                status => enabled,
                firing => Firing,
                condition => Condition,
                metric => Metric
            }
    end.

evaluate_condition(Condition, Metric, Labels) ->
    ConditionType = maps:get(type, Condition),
    
    case ConditionType of
        threshold ->
            evaluate_threshold_condition(Condition, Metric, Labels);
        rate ->
            evaluate_rate_condition(Condition, Metric, Labels);
        rate_of_change ->
            evaluate_rate_of_change_condition(Condition, Metric, Labels);
        state_duration ->
            evaluate_state_duration_condition(Condition, Metric, Labels);
        count ->
            evaluate_count_condition(Condition, Metric, Labels);
        percentile ->
            evaluate_percentile_condition(Condition, Metric, Labels);
        _ ->
            false
    end.

evaluate_threshold_condition(Condition, Metric, Labels) ->
    Threshold = maps:get(threshold, Condition),
    Operator = maps:get(operator, Condition, greater_than),
    
    CurrentValue = get_metric_value(Metric, Labels),
    
    case Operator of
        greater_than ->
            CurrentValue > Threshold;
        less_than ->
            CurrentValue < Threshold;
        equal ->
            CurrentValue =:= Threshold;
        _ ->
            false
    end.

evaluate_rate_condition(Condition, Metric, Labels) ->
    Threshold = maps:get(threshold, Condition),
    DurationSeconds = maps:get(duration_seconds, Condition, 60),
    
    %% Calculate rate over duration
    Rate = calculate_metric_rate(Metric, Labels, DurationSeconds),
    Rate > Threshold.

evaluate_rate_of_change_condition(Condition, Metric, Labels) ->
    Threshold = maps:get(threshold, Condition),
    DurationSeconds = maps:get(duration_seconds, Condition, 60),
    Operator = maps:get(operator, Condition, greater_than),
    
    RateOfChange = calculate_rate_of_change(Metric, Labels, DurationSeconds),
    
    case Operator of
        greater_than ->
            RateOfChange > Threshold;
        less_than ->
            RateOfChange < Threshold;
        _ ->
            false
    end.

evaluate_state_duration_condition(Condition, Metric, Labels) ->
    RequiredState = maps:get(state, Condition),
    DurationSeconds = maps:get(duration_seconds, Condition, 300),
    
    CurrentState = get_metric_value(Metric, Labels),
    StateDuration = get_state_duration(Metric, Labels, RequiredState),
    
    (CurrentState =:= RequiredState) andalso (StateDuration >= DurationSeconds).

evaluate_count_condition(Condition, Metric, Labels) ->
    Threshold = maps:get(threshold, Condition),
    Operator = maps:get(operator, Condition, greater_than),
    
    Count = count_metric_instances(Metric, Labels),
    
    case Operator of
        greater_than ->
            Count > Threshold;
        less_than ->
            Count < Threshold;
        _ ->
            false
    end.

evaluate_percentile_condition(Condition, Metric, Labels) ->
    Percentile = maps:get(percentile, Condition),
    Threshold = maps:get(threshold, Condition),
    Operator = maps:get(operator, Condition, greater_than),
    
    PercentileValue = get_metric_percentile(Metric, Labels, Percentile),
    
    case Operator of
        greater_than ->
            PercentileValue > Threshold;
        less_than ->
            PercentileValue < Threshold;
        _ ->
            false
    end.

get_metric_value(Metric, Labels) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            0;
        _ ->
            Key = {Metric, router_metrics:normalize_labels(Labels)},
            case ets:lookup(router_metrics, Key) of
                [{Key, Value}] when is_number(Value) ->
                    Value;
                [] ->
                    %% Try without labels
                    case ets:lookup(router_metrics, Metric) of
                        [{Metric, Value}] when is_number(Value) ->
                            Value;
                        [] ->
                            0
                    end
            end
    end.

calculate_metric_rate(_Metric, _Labels, _DurationSeconds) ->
    %% Placeholder - would calculate rate over time window
    0.0.

calculate_rate_of_change(_Metric, _Labels, _DurationSeconds) ->
    %% Placeholder - would calculate rate of change
    0.0.

get_state_duration(_Metric, _Labels, _RequiredState) ->
    %% Placeholder - would track state duration
    0.

count_metric_instances(Metric, Labels) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            0;
        _ ->
            ets:foldl(fun(Entry, Acc) ->
                case Entry of
                    {{M, _LabelsKey}, _} when M =:= Metric ->
                        case matches_labels(Entry, Labels) of
                            true -> Acc + 1;
                            false -> Acc
                        end;
                    {M, _Value} when M =:= Metric ->
                        Acc + 1;
                    _ ->
                        Acc
                end
            end, 0, router_metrics)
    end.

get_metric_percentile(_Metric, _Labels, _Percentile) ->
    %% Placeholder - would calculate percentile from histogram
    0.0.

matches_labels({{_Metric, LabelsKey}, _Value}, RequiredLabels) ->
    LabelsMap = lists:foldl(fun({K, V}, Acc) ->
        maps:put(K, V, Acc)
    end, #{}, LabelsKey),
    maps:fold(fun(K, V, Acc) ->
        Acc andalso (maps:get(K, LabelsMap, undefined) =:= V)
    end, true, RequiredLabels);
matches_labels(_, _) ->
    false.

update_alert_state(Results, State) ->
    lists:foldl(fun(Result, AccState) ->
        RuleId = maps:get(rule_id, Result),
        Firing = maps:get(firing, Result, false),
        
        CurrentAlert = maps:get(RuleId, AccState#state.active_alerts, undefined),
        NewAlert = case {Firing, CurrentAlert} of
            {true, undefined} ->
                %% New alert firing
                RuleResult = router_alert_rules:get_rule(RuleId),
                Rule = case RuleResult of
                    {ok, R} -> R;
                    {error, _} -> #{}
                end,
                #alert_state{
                    rule_id = RuleId,
                    rule = Rule,
                    status = firing,
                    first_fired = erlang:system_time(second),
                    last_fired = erlang:system_time(second),
                    fire_count = 1
                };
            {true, Alert} when Alert#alert_state.status =:= firing ->
                %% Alert still firing
                Alert#alert_state{
                    last_fired = erlang:system_time(second),
                    fire_count = Alert#alert_state.fire_count + 1
                };
            {true, Alert} when Alert#alert_state.status =:= resolved ->
                %% Alert re-fired
                Alert#alert_state{
                    status = firing,
                    last_fired = erlang:system_time(second),
                    fire_count = Alert#alert_state.fire_count + 1
                };
            {false, undefined} ->
                %% No alert
                undefined;
            {false, Alert} when Alert#alert_state.status =:= firing ->
                %% Alert resolved
                Alert#alert_state{
                    status = resolved,
                    last_resolved = erlang:system_time(second)
                };
            {false, Alert} ->
                %% Alert already resolved
                Alert
        end,
        
        case NewAlert of
            undefined ->
                AccState#state{
                    active_alerts = maps:remove(RuleId, AccState#state.active_alerts)
                };
            _ ->
                FiringEvent = case Firing andalso (CurrentAlert =:= undefined orelse CurrentAlert#alert_state.status =/= firing) of
                    true ->
                        #{
                            rule_id => RuleId,
                            event => firing,
                            timestamp => erlang:system_time(second)
                        };
                    false ->
                        undefined
                end,
                UpdatedHistory = case FiringEvent of
                    undefined -> AccState#state.alert_history;
                    _ -> [FiringEvent | AccState#state.alert_history]
                end,
                AccState#state{
                    active_alerts = maps:put(RuleId, NewAlert, AccState#state.active_alerts),
                    alert_history = UpdatedHistory
                }
        end
    end, State, Results).

count_active_alerts(ActiveAlerts) ->
    maps:fold(fun(_RuleId, AlertState, Acc) ->
        case AlertState#alert_state.status of
            firing -> Acc + 1;
            resolved -> Acc
        end
    end, 0, ActiveAlerts).

alert_state_to_map(AlertState) ->
    Rule = AlertState#alert_state.rule,
    #{
        id => AlertState#alert_state.rule_id,
        rule_id => AlertState#alert_state.rule_id,
        status => AlertState#alert_state.status,
        severity => maps:get(severity, Rule, warning),
        first_fired => AlertState#alert_state.first_fired,
        last_fired => AlertState#alert_state.last_fired,
        last_resolved => AlertState#alert_state.last_resolved,
        fire_count => AlertState#alert_state.fire_count
    }.

schedule_evaluation() ->
    IntervalSeconds = application:get_env(beamline_router, alert_evaluation_interval, 60),
    erlang:send_after(IntervalSeconds * 1000, self(), evaluate_periodic),
    ok.

