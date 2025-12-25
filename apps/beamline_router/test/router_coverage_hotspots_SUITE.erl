%% @doc Coverage tests that target hot-spot helpers and data modules
-module(router_coverage_hotspots_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,

         test_dashboard_config_panels/1,
         test_alert_rules_lookup_and_validation/1,
         test_extension_error_mapper_messages/1,
         test_policy_static_defaults/1,
         test_license_compliance_checks/1,
         test_state_env_flags/1,
         test_config_validator_value_checks/1,
         test_r10_metrics_trigger_reason_and_deltas/1,
         test_dashboard_data_aggregations/1,
         test_payload_tracker_alert_detection/1,
         test_metrics_validator_helpers/1,
         test_cpu_profiler_usage/1,
         test_observability_attribute_helpers/1,
         test_network_tracker_round_trip_flow/1,
         test_telemetry_helper_toggles/1]).

-compile({nowarn_unused_function, [clean_shared_tables/0, safe_delete_table/1,
                                  with_app_env/3, with_app_envs/2]}).

%% Common Test callbacks
all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, coverage_hotspots}];
groups_for_level(full) ->
    [{group, coverage_hotspots}];
groups_for_level(_) ->
    [{group, coverage_hotspots}].
groups() ->
    [{coverage_hotspots, [sequential], [
        test_dashboard_config_panels,
        test_alert_rules_lookup_and_validation,
        test_extension_error_mapper_messages,
        test_policy_static_defaults,
        test_license_compliance_checks,
        test_state_env_flags,
        test_config_validator_value_checks,
        test_r10_metrics_trigger_reason_and_deltas,
        test_dashboard_data_aggregations,
        test_payload_tracker_alert_detection,
        test_metrics_validator_helpers,
        test_cpu_profiler_usage,
        test_observability_attribute_helpers,
        test_network_tracker_round_trip_flow,
        test_telemetry_helper_toggles
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
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
    clean_shared_tables(),
    router_metrics:clear_all(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ==========================================================================
%% Tests targeting modules selected for targeted coverage growth
%% ==========================================================================

test_dashboard_config_panels(_Config) ->
    Panels = router_dashboard_config:get_dashboard_panels(r10),
    ?assert(is_list(Panels)),
    ?assertNotEqual([], Panels),

    TriggerPanels = router_dashboard_config:get_dashboard_panels(trigger_reason),
    ?assert(is_list(TriggerPanels)),
    ?assertNotEqual([], TriggerPanels),

    Variables = router_dashboard_config:get_dashboard_variables(trigger_reason),
    ?assert(is_list(Variables)),
    ?assertEqual([], router_dashboard_config:get_dashboard_panels(unknown_dashboard)),

    Config = router_dashboard_config:get_r10_dashboard_config(),
    ?assertEqual(<<"30s">>, maps:get(refresh_interval, Config)),
    ok.


test_alert_rules_lookup_and_validation(_Config) ->
    RuleId = <<"r13_high_fault_rate">>,
    AllRules = router_alert_rules:get_all_rules(),
    ?assert(lists:any(fun(R) -> maps:get(id, R) =:= RuleId end, AllRules)),

    MissingIdRule = #{name => <<"missing">>, description => <<"desc">>,
                      severity => <<"warning">>, metric => router_fault_rate,
                      condition => #{type => threshold}, enabled => true},
    {error, {missing_fields, Missing}} = router_alert_rules:validate_rule(MissingIdRule),
    ?assert(lists:member(id, Missing)),

    InvalidSeverity = maps:put(id, RuleId, MissingIdRule),
    InvalidSeverity2 = maps:put(severity, <<"panic">>, InvalidSeverity),
    {error, {invalid_severity, <<"panic">>}} =
        router_alert_rules:validate_rule(InvalidSeverity2),
    ok.


test_extension_error_mapper_messages(_Config) ->
    Metadata = #{extension_id => <<"ext">>, retries => 2, reason => <<"timeout">>},
    {Code, Message, Details} =
        router_extension_error_mapper:map_extension_error({error, {extension_timeout, Metadata}}),
    ?assertEqual(<<"extension_timeout">>, Code),
    ?assertEqual(<<"Extension timeout after 2 retries: ext">>, Message),
    ?assertEqual(<<"extension_timeout">>, maps:get(<<"error_type">>, Details)),
    ?assertEqual(<<"extension_error">>, router_extension_error_mapper:map_extension_error_to_code(unknown_reason)),
    ok.


test_policy_static_defaults(_Config) ->
    Policy = router_policy_static:get_default_policy(<<"tenant">>),
    ?assertEqual(<<"default">>, Policy#policy.policy_id),
    ?assertEqual(<<"openai">>, maps:get(<<"provider">>, Policy#policy.defaults)),
    ?assertEqual(true, maps:get(<<"enabled">>, Policy#policy.sticky)),
    ok.


test_license_compliance_checks(_Config) ->
    ?assert(router_license_compliance:is_license_compliant(<<"Apache-2.0">>)),
    ?assertNot(router_license_compliance:is_license_compliant(<<"GPL-3.0">>)),
    Report = element(2, router_license_compliance:verify_dependencies()),
    ?assert(maps:get(compliant, Report)),
    Requirements = router_license_compliance:get_license_requirements(),
    ?assert(maps:is_key(allowed_licenses, Requirements)),
    ok.


test_state_env_flags(_Config) ->
    Fun = fun() ->
        ?assertEqual(<<"CP2">>, router_state:get_current_cp()),
        ?assert(router_state:is_cp2_plus_allowed())
    end,
    with_app_envs([{current_cp, <<"CP2">>}, {cp2_plus_allowed, true}], Fun),
    ok.


test_config_validator_value_checks(_Config) ->
    Envs = [
        {grpc_enabled, true},
        {nats_mode, mock},
        {admin_grpc_enabled, true},
        {metrics_export_enabled, false},
        {telemetry_enabled, true},
        {idempotency_enabled, true},
        {ack_enabled, false},
        {cp2_plus_allowed, true}
    ],
    Fun = fun() ->
        Template = router_config_validator:get_config_template(),
        ?assertEqual(9000, maps:get(grpc_port, Template)),
        Required = router_config_validator:check_required_config(),
        ?assert(maps:get(passed, Required)),
        Compatibility = router_config_validator:check_config_compatibility(#{
            grpc_enabled => true,
            admin_grpc_enabled => true,
            metrics_export_enabled => false,
            telemetry_enabled => true,
            idempotency_enabled => true,
            ack_enabled => false,
            cp2_plus_allowed => true
        }),
        ?assert(maps:get(passed, Compatibility)),
        {error, must_be_port_number} = router_config_validator:validate_config_value(grpc_port, 70000)
    end,
    with_app_envs(Envs, Fun),
    ok.


test_r10_metrics_trigger_reason_and_deltas(_Config) ->
    router_metrics:clear_all(),
    Tenant = <<"t1">>, Provider = <<"p1">>,
    Reason = router_r10_metrics:trigger_reason_failure_threshold(),
    router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{value => 1}, #{
        tenant_id => Tenant,
        provider_id => Provider,
        reason => Reason
    }),
    ?assertEqual({ok, Reason}, router_r10_metrics:get_latest_trigger_reason(Tenant, Provider)),

    {Before, After, Delta} =
        router_r10_metrics:get_publish_attempts_delta(fun() ->
            router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 2}, #{})
        end),
    ?assert(Before =< After),
    ?assertEqual(Delta, After - Before),
    ?assertEqual(2, Delta),

    ?assert(router_r10_metrics:metrics_table_exists()),
    router_r10_metrics:clear_metrics(),
    ?assertEqual(0, router_r10_metrics:get_metric_value(router_nats_publish_attempts_total, #{})),
    ok.


test_dashboard_data_aggregations(_Config) ->
    router_metrics:clear_all(),
    Tenant = <<"t1">>, Provider = <<"p1">>,
    Reason = router_r10_metrics:trigger_reason_error_rate(),
    router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{value => 1}, #{
        tenant_id => Tenant,
        provider_id => Provider,
        reason => Reason
    }),
    router_metrics:emit_metric(router_circuit_breaker_state, #{value => 1}, #{
        tenant_id => Tenant,
        provider_id => Provider,
        state => <<"open">>
    }),

    Aggregated = router_dashboard_data:aggregate_trigger_reasons(#{}),
    ?assertEqual(1, maps:get(Reason, Aggregated, 0)),

    R10Data = router_dashboard_data:get_r10_dashboard_data(#{}),
    ?assert(is_map(R10Data)),
    ?assert(maps:is_key(circuit_states, R10Data)),

    TriggerDashboard = router_dashboard_data:get_trigger_reason_dashboard_data(#{}),
    ?assertEqual(Aggregated, maps:get(distribution, TriggerDashboard)),
    ok.


test_payload_tracker_alert_detection(_Config) ->
    router_payload_tracker:init(),
    Tenant = <<"tenant">>, Endpoint = track_payload_endpoint,
    lists:foreach(fun(_) ->
        router_payload_tracker:track_payload(Tenant, Endpoint, 600000)
    end, lists:seq(1, 10)),

    {abuse, heavy_payload, Details} = router_payload_tracker:check_abuse_pattern(Tenant, Endpoint),
    ?assertEqual(10, maps:get(<<"total_requests">>, Details)),

    ?assertEqual({ok, normal}, router_payload_tracker:check_abuse_pattern(<<"other">>, serve_endpoint)),
    {ok, Stats} = router_payload_tracker:get_stats(Tenant),
    ?assertNotEqual([], Stats),

    {ok, CleanupRemoved} = router_payload_tracker:cleanup_old_entries(),
    ?assert(CleanupRemoved >= 0),
    ok.


test_metrics_validator_helpers(_Config) ->
    router_metrics:clear_all(),
    router_metrics:emit_metric(router_circuit_breaker_state, #{value => 1}, #{tenant_id => <<"t1">>}),
    router_metrics:emit_metric(router_network_round_trip_time_microseconds, #{value => 100}, #{operation_type => <<"net">>}),

    ?assertEqual({ok, router_circuit_breaker_state}, router_metrics_validator:validate_metric_name(router_circuit_breaker_state)),
    {error, _} = router_metrics_validator:validate_metric_name('1invalid'),

    {ok, Labels} = router_metrics_validator:validate_metric_labels(router_circuit_breaker_state, #{tenant_id => <<"t1">>, provider_id => <<"p1">>}),
    ?assertEqual(#{tenant_id => <<"t1">>, provider_id => <<"p1">>}, Labels),
    {error, Reserved} = router_metrics_validator:validate_metric_labels(router_circuit_breaker_state, #{<<"le">> => <<"1">>}),
    ?assert(lists:member(<<"le">>, Reserved)),

    AllMetrics = router_metrics_validator:get_all_metrics(),
    ?assert(lists:member(router_circuit_breaker_state, AllMetrics)),
    Doc = router_metrics_validator:get_metric_documentation(router_circuit_breaker_state),
    ?assertEqual(router_circuit_breaker_state, maps:get(metric_name, Doc)),

    {ok, Actual} = router_metrics_validator:find_metric_mismatches(AllMetrics),
    {error, Missing, Unexpected} = router_metrics_validator:find_metric_mismatches([]),
    ?assertEqual([], Missing),
    ?assertEqual(Actual, Unexpected),
    ok.


test_cpu_profiler_usage(_Config) ->
    router_cpu_profiler:track_hot_path(router_cpu_profiler, test_hot_path, 150),
    Paths = router_cpu_profiler:get_hot_paths(),
    ?assert(lists:any(fun({router_cpu_profiler, test_hot_path, _, Total, _}) -> Total >= 150; (_) -> false end, Paths)),

    {ok, ProcessUsage} = router_cpu_profiler:get_process_cpu_usage(self()),
    ?assert(is_float(ProcessUsage)),

    {ok, SchedulerUsage} = router_cpu_profiler:get_scheduler_utilization(),
    ?assert(is_float(SchedulerUsage)),
    ok.


test_observability_attribute_helpers(_Config) ->
    Context = #{<<"tenant_id">> => <<"t1">>, <<"trace_id">> => <<"trace">>, <<"run_id">> => <<"run">>},
    Attrs = router_observability:extract_cp1_attributes(Context),
    ?assertEqual(<<"t1">>, maps:get(tenant_id, Attrs)),

    ?assertEqual(Attrs, router_observability:get_span_attributes(Context)),
    ok.


test_network_tracker_round_trip_flow(_Config) ->
    safe_delete_table(router_network_round_trips),
    {ok, RoundTripId} =
        router_network_tracker:track_round_trip(<<"op-1">>, nats_publish, erlang:monotonic_time(microsecond)),
    EndTime = erlang:monotonic_time(microsecond) + 1000,
    {ok, Duration} = router_network_tracker:complete_round_trip(RoundTripId, EndTime),
    {ok, Recorded} = router_network_tracker:get_round_trip_time(<<"op-1">>),
    ?assertEqual(Duration, Recorded),

    Stats = router_network_tracker:get_network_stats(),
    ?assert(maps:is_key(count, Stats)),
    ?assert(maps:is_key(total_time_microseconds, Stats)),

    {ok, BatchResult} = router_network_tracker:batch_operations([1, 2], fun(X) -> X end),
    ?assertEqual([1, 2], BatchResult),

    {ok, Coalesced} = router_network_tracker:coalesce_requests([1, 1, 2], fun(Requests) -> lists:usort(Requests) end),
    ?assertEqual([1, 2], Coalesced),
    ok.


test_telemetry_helper_toggles(_Config) ->
    HandlerId = make_ref(),
    Event = [test, telemetry_helper],
    telemetry:attach(HandlerId, Event, fun(_, _, _, _) -> self() ! telemetry_helper_fired end, []),

    FunEnabled = fun() ->
        router_telemetry_helper:execute(Event, #{value => 1}, #{}),
        receive
            telemetry_helper_fired -> ok
        after 500 -> ct:fail("Telemetry event not fired")
        end
    end,
    with_app_env(telemetry_enabled, true, FunEnabled),

    FunDisabled = fun() ->
        router_telemetry_helper:execute(Event, #{value => 1}, #{}),
        receive
            telemetry_helper_fired -> ct:fail("Telemetry fired when disabled")
        after 100 -> ok
        end
    end,
    with_app_env(telemetry_enabled, false, FunDisabled),

    telemetry:detach(HandlerId),
    ok.

%% ==========================================================================
%% Helpers
%% ==========================================================================

clean_shared_tables() ->
    safe_delete_table(router_metrics),
    safe_delete_table(router_cpu_hot_paths),
    safe_delete_table(router_cpu_profiling_state),
    safe_delete_table(router_network_round_trips),
    safe_delete_table(router_payload_size_tracking),
    ok.

safe_delete_table(Table) ->
    case catch ets:info(Table) of
        undefined -> ok;
        _ ->
            catch ets:delete_all_objects(Table),
            ok
    end.

with_app_envs([], Fun) -> Fun();
with_app_envs([{Key, Value} | Rest], Fun) ->
    with_app_env(Key, Value, fun() -> with_app_envs(Rest, Fun) end).

with_app_env(Key, Value, Fun) ->
    OldValue = application:get_env(beamline_router, Key),
    ok = application:set_env(beamline_router, Key, Value),
    try
        Fun()
    after
        case OldValue of
            {ok, Prev} -> application:set_env(beamline_router, Key, Prev);
            undefined -> application:unset_env(beamline_router, Key)
        end
    end.
