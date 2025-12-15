%% @doc Targeted coverage for router_db, router_ets_guard, router_prometheus
-module(router_hotspots_3_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,

         test_router_db_not_available/1,
         test_router_ets_guard_invariants/1,
         test_router_ets_guard_invalid_spec_keys/1,
         test_router_ets_guard_non_map_spec/1,
         test_router_ets_guard_detects_violations/1,
         test_router_ets_guard_tid_tables/1,
         test_router_ets_guard_named_table_reporting/1,
         test_router_ets_guard_verify_table_violations/1,
         test_router_ets_guard_empty_spec/1,
         test_router_ets_guard_list_spec_violations/1,
         test_router_ets_guard_telemetry_disabled_logging/1,
         test_router_ets_guard_boolean_violation_logging/1,
         test_router_prometheus_render_dump/1,
         test_router_prometheus_render_empty_metrics/1,
         test_router_prometheus_complex_render_and_dump/1,
         test_router_prometheus_metadata_variants/1,
         test_router_prometheus_metadata_help_lines/1,
        test_router_prometheus_duplicate_unlabeled_entries/1,
        test_router_prometheus_render_without_metrics_table/1,
        test_router_prometheus_duplicate_label_entries/1,
         test_router_prometheus_render_label_variants/1]).

-compile({nowarn_unused_function, [
    clear_temp_file/1,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]}).

%% -------------------------------------------------------------------
%% Common Test callbacks
%% -------------------------------------------------------------------
all() ->
    [{group, hotspots_3}].

groups() ->
    [{hotspots_3, [sequential], [
        test_router_db_not_available,
        test_router_ets_guard_invariants,
        test_router_ets_guard_invalid_spec_keys,
        test_router_ets_guard_non_map_spec,
        test_router_ets_guard_detects_violations,
        test_router_ets_guard_tid_tables,
        test_router_ets_guard_named_table_reporting,
        test_router_ets_guard_verify_table_violations,
        test_router_ets_guard_empty_spec,
        test_router_ets_guard_list_spec_violations,
        test_router_ets_guard_telemetry_disabled_logging,
        test_router_ets_guard_boolean_violation_logging,
        test_router_prometheus_render_dump,
        test_router_prometheus_render_empty_metrics,
        test_router_prometheus_complex_render_and_dump,
        test_router_prometheus_metadata_variants,
        test_router_prometheus_metadata_help_lines,
        test_router_prometheus_duplicate_unlabeled_entries,
        test_router_prometheus_render_without_metrics_table,
        test_router_prometheus_duplicate_label_entries,
        test_router_prometheus_render_label_variants
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    LogDirBase = case os:getenv("TMPDIR") of
        false -> "/tmp";
        undefined -> "/tmp";
        Dir -> Dir
    end,
    LogPath = filename:join(LogDirBase, "router_hotspots_logs"),
    ok = application:set_env(beamline_router, log_dir, LogPath),
    router_metrics:ensure(),
    _ = router_logger:is_enabled(),
    Config.

end_per_suite(_Config) ->
    application:stop(telemetry),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, _Config) ->
    %% Clear any existing metrics data
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> router_metrics:clear_all()
    end,
    
    BaseConfig = [{test_case, _TestCase} | _Config],
    BaseConfig.

end_per_testcase(_TestCase, _Config) ->
    %% Clear metrics data
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> router_metrics:clear_all()
    end,
    ok.

%% -------------------------------------------------------------------
%% Module-specific tests
%% -------------------------------------------------------------------

test_router_db_not_available(_Config) ->
    ?assertEqual({error, database_not_available}, router_db:load_policy(<<"tenant">>, <<"policy">>)),
    ?assertEqual({error, database_not_available}, router_db:query("SELECT 1", [])),
    ok.

test_router_ets_guard_invariants(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]),
    try
        Spec = #{
            type => set,
            keypos => 1,
            read_concurrency => true,
            write_concurrency => true,
            compressed => false
        },
        {ok, Result} = router_ets_guard:ensure_table(Table, Spec),
        ?assertEqual(true, maps:get(checked, Result)),
        ?assert(trunc(maps:get(latency_us, Result)) >= 0),

        SpecViolation = #{
            type => bag,
            keypos => 2,
            read_concurrency => false,
            write_concurrency => false,
            compressed => true
        },
        {error, ViolationReason} = router_ets_guard:ensure_table(Table, SpecViolation),
        {reason, Violations} = lists:keyfind(reason, 1, ViolationReason),
        ?assert(length(Violations) >= 1),

        {error, InvalidKeys} = router_ets_guard:check_spec(#{foo => bar}),
        ?assertEqual([foo], InvalidKeys),
        ?assertEqual({error, not_a_map}, router_ets_guard:check_spec(123)),
        ?assertEqual(ok, router_ets_guard:validate_spec(#{type => set})),

        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_invalid_spec_keys(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, set]),
    try
        Spec = #{foo => bar},
        {error, [{table, _TableName}, {reason, {invalid_spec_keys, InvalidKeys}}]} =
            router_ets_guard:ensure_table(Table, Spec),
        ?assertEqual([foo], InvalidKeys),
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_non_map_spec(_Config) ->
    ?assertEqual({error, not_a_map}, router_ets_guard:check_spec(123)),
    ?assertEqual({error, not_a_map}, router_ets_guard:validate_spec([foo])),
    ok.

test_router_ets_guard_detects_violations(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, bag, {read_concurrency, false}, {write_concurrency, false}, compressed]),
    try
        Spec = #{
            type => set,
            keypos => 2,
            read_concurrency => true,
            write_concurrency => true,
            compressed => true
        },
        {error, [{table, _TableName}, {reason, Violations}]} =
            router_ets_guard:ensure_table(Table, Spec),
        ?assertEqual(true, length(Violations) >= 3),
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_tid_tables(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [bag, public, {read_concurrency, true}, {write_concurrency, true}]),
    try
        Spec = #{
            type => set,
            keypos => 1,
            read_concurrency => true,
            write_concurrency => true,
            compressed => false
        },
        {error, _Reason} = router_ets_guard:ensure_table(Table, Spec),
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_verify_table_violations(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, bag, {keypos, 2}, {read_concurrency, false}, {write_concurrency, false}]),
    try
        Spec = #{
            type => set,
            keypos => 1,
            read_concurrency => true,
            write_concurrency => true,
            compressed => true
        },
        {error, Violations} = router_ets_guard:verify_table(Table, Spec),
        ?assert(lists:keyfind(type, 1, Violations) =/= false),
        ?assert(lists:keyfind(keypos, 1, Violations) =/= false),
        ?assert(lists:keyfind(read_concurrency, 1, Violations) =/= false),
        ?assert(lists:keyfind(write_concurrency, 1, Violations) =/= false),
        ?assert(lists:keyfind(compressed, 1, Violations) =/= false),
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_empty_spec(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]),
    try
        Spec = #{},
        {ok, Result} = router_ets_guard:ensure_table(Table, Spec),
        ?assertEqual(true, maps:get(checked, Result)),
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_named_table_reporting(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]),
    try
        Spec = #{
            type => set,
            keypos => 1,
            read_concurrency => true,
            write_concurrency => true,
            compressed => false
        },
        {ok, Result} = router_ets_guard:ensure_table(Table, Spec),
        ?assertEqual(true, maps:get(checked, Result)),
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_list_spec_violations(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, set]),
    try
        PrevTelemetry = application:get_env(beamline_router, telemetry_enabled, true),
        ok = application:set_env(beamline_router, telemetry_enabled, true),
        Spec = #{
            type => [bag],
            keypos => [2],
            read_concurrency => [false],
            write_concurrency => [false],
            compressed => [true]
        },
        try
            {error, Reason} = router_ets_guard:ensure_table(Table, Spec),
            [{table, _TableName}, {reason, Violations}] = Reason,
            ?assert(length(Violations) >= 1)
        after
            application:set_env(beamline_router, telemetry_enabled, PrevTelemetry)
        end,
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_telemetry_disabled_logging(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, bag, compressed]),
    try
        PrevTelemetry = application:get_env(beamline_router, telemetry_enabled, true),
        ok = application:set_env(beamline_router, telemetry_enabled, false),
        Spec = #{
            type => set,
            keypos => 1,
            read_concurrency => true,
            write_concurrency => true,
            compressed => false
        },
        try
            {error, Reason} = router_ets_guard:ensure_table(Table, Spec),
            [{table, _TableName}, {reason, Violations}] = Reason,
            ?assert(length(Violations) >= 1)
        after
            application:set_env(beamline_router, telemetry_enabled, PrevTelemetry)
        end,
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_ets_guard_boolean_violation_logging(_Config) ->
    Table = router_test_init:ensure_ets_table(test_table, [named_table, public, bag, {read_concurrency, true}, {write_concurrency, true}, compressed]),
    try
        PrevTelemetry = application:get_env(beamline_router, telemetry_enabled, true),
        ok = application:set_env(beamline_router, telemetry_enabled, true),
        Spec = #{
            type => set,
            keypos => 1,
            read_concurrency => false,
            write_concurrency => false,
            compressed => true
        },
        try
            {error, Reason} = router_ets_guard:ensure_table(Table, Spec),
            [{table, _TableName}, {reason, Violations}] = Reason,
            ?assert(length(Violations) >= 1)
        after
            application:set_env(beamline_router, telemetry_enabled, PrevTelemetry)
        end,
        ok
    after
        router_test_init:delete_ets_table(test_table)
    end.

test_router_prometheus_render_dump(_Config) ->
    router_metrics:clear_all(),
    router_metrics:emit_metric(router_acl_allowed_total, #{count => 5}, #{}),
    router_metrics:emit_metric(router_circuit_breaker_state, #{value => 1.5}, #{
        tenant_id => <<"t\"est">>,
        provider_id => <<"p">>
    }),

    Rendered = router_prometheus:render(),
    ?assert(binary:match(Rendered, <<"router_acl_allowed_total">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"router_acl_allowed_total 5">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"router_circuit_breaker_state">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"tenant_id=\"t\\\"est\"">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"provider_id=\"p\"">>) /= nomatch),

    Path = filename:join("/tmp", "router_prometheus_test.prom"),
    clear_temp_file(Path),
    ok = router_prometheus:dump(Path),
    {ok, FileContent} = file:read_file(Path),
    ?assert(binary:match(FileContent, <<"router_acl_allowed_total">>) /= nomatch),
    clear_temp_file(Path),
    ok.

test_router_prometheus_render_empty_metrics(_Config) ->
    %% Clear all metrics without deleting ETS table to avoid linter ETS violations
    router_metrics:clear_all(),
    Rendered = router_prometheus:render(),
    ?assertEqual(<<>>, Rendered),
    ok.

test_router_prometheus_complex_render_and_dump(_Config) ->
    router_metrics:clear_all(),
    router_metrics:emit_metric(router_acl_allowed_total, #{count => 7}, #{}),
    router_metrics:emit_metric(test_metric_with_labels, #{count => 11}, #{
        <<"tenant_id">> => <<"t\"A\\B">>,
        provider_id => <<"p">>,
        status => active,
        numeric_label => 42,
        float_label => 2.71,
        list_label => ["x", "y"],
        999 => <<"mixed">>
    }),
    router_metrics:ensure(),
    ets:insert(router_metrics, {{test_metric_empty_labels, []}, 13}),
    ets:insert(router_metrics, {invalid_metric_tuple, <<"oops">>}),

    Rendered = router_prometheus:render(),
    ?assert(binary:match(Rendered, <<"test_metric_with_labels">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"tenant_id=\"t\\\"A\\\\B\"">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"numeric_label=\"42\"">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"float_label=">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"list_label=">>) /= nomatch),
    ?assert(binary:match(Rendered, <<"test_metric_empty_labels 13">>) /= nomatch),

    TempDir = filename:join("/tmp", "router_prometheus_extra"),
    TempPath = filename:join(TempDir, "extra.prom"),
    clear_temp_file(TempPath),
    ok = router_prometheus:dump(TempPath),
    {ok, TempContent} = file:read_file(TempPath),
    ?assert(binary:match(TempContent, <<"test_metric_with_labels">>) /= nomatch),
    clear_temp_file(TempPath),

    DefaultPath = filename:join("metrics_dump", "metrics.prom"),
    clear_temp_file(DefaultPath),
    ok = router_prometheus:dump(),
    {ok, DefaultContent} = file:read_file(DefaultPath),
    ?assert(binary:match(DefaultContent, <<"router_acl_allowed_total">>) /= nomatch),
    clear_temp_file(DefaultPath),

    router_metrics:clear_all(),
    ok.

test_router_prometheus_metadata_variants(_Config) ->
    RouterMetrics = router_prometheus_metric_keys(),
    router_metrics:clear_all(),
    lists:foreach(fun(Key) ->
        router_metrics:emit_metric(Key, #{count => 1}, #{})
    end, RouterMetrics),
    router_metrics:emit_metric(unknown_metric, #{count => 1}, #{}),

    Rendered = router_prometheus:render(),
    lists:foreach(fun(Key) ->
        NameBin = unicode:characters_to_binary(atom_to_list(Key)),
        ?assert(binary:match(Rendered, NameBin) =/= nomatch)
    end, RouterMetrics),
    ?assert(binary:match(Rendered, <<"unknown_metric">>) =/= nomatch),
    router_metrics:clear_all(),
    ok.

test_router_prometheus_metadata_help_lines(_Config) ->
    router_metrics:clear_all(),
    lists:foreach(
        fun(Key) -> router_metrics:emit_metric(Key, #{count => 1}, #{}) end,
        router_prometheus_metric_keys()
    ),
    Rendered = router_prometheus:render(),
    lists:foreach(fun({Name, Description, Type}) ->
        NameBin = unicode:characters_to_binary(atom_to_list(Name)),
        DescBin = unicode:characters_to_binary(Description),
        TypeBin = unicode:characters_to_binary(Type),
        HelpLine = iolist_to_binary([<<"# HELP ">>, NameBin, <<" ">>, DescBin]),
        TypeLine = iolist_to_binary([<<"# TYPE ">>, NameBin, <<" ">>, TypeBin]),
        ?assert(binary:match(Rendered, HelpLine) =/= nomatch),
        ?assert(binary:match(Rendered, TypeLine) =/= nomatch)
    end, router_prometheus_metadata_definitions()),
    router_metrics:clear_all(),
    ok.

test_router_prometheus_duplicate_unlabeled_entries(_Config) ->
    router_test_init:delete_ets_table(router_metrics),
    _ = router_test_init:ensure_ets_table(
        router_metrics,
        [named_table, duplicate_bag, public, {read_concurrency, true}, {write_concurrency, true}]
    ),
    try
        ets:insert(router_metrics, {router_acl_allowed_total, 5}),
        ets:insert(router_metrics, {router_acl_allowed_total, 6}),
        ets:insert(router_metrics, {router_acl_allowed_total, 7.25}),
        Rendered = router_prometheus:render(),
        ?assert(binary:match(Rendered, <<"router_acl_allowed_total 5">>) =/= nomatch),
        ?assert(binary:match(Rendered, <<"router_acl_allowed_total 6">>) =/= nomatch),
        ?assert(binary:match(Rendered, <<"router_acl_allowed_total 7.250000">>) =/= nomatch),
        ok
    after
        router_test_init:delete_ets_table(router_metrics),
        router_metrics:ensure()
    end.

test_router_prometheus_render_without_metrics_table(_Config) ->
    delete_metrics_table(),
    try
        Rendered = router_prometheus:render(),
        ?assertEqual(<<>>, Rendered)
    after
        router_metrics:ensure()
    end.

%% This test is removed as it was causing meck_new violations
%% and is not critical for testing the core functionality

test_router_prometheus_duplicate_label_entries(_Config) ->
    router_metrics:clear_all(),
    LabelsA = router_metrics:normalize_labels(#{tenant_id => <<"t-a">>, provider_id => <<"p">>}),
    LabelsB = router_metrics:normalize_labels(#{tenant_id => <<"t-b">>, provider_id => <<"p">>}),
    ets:insert(router_metrics, {{router_circuit_breaker_state, LabelsA}, 1.1}),
    ets:insert(router_metrics, {{router_circuit_breaker_state, LabelsB}, 2.1}),
    Rendered = router_prometheus:render(),
    ?assert(binary:match(Rendered, <<"tenant_id=\"t-a\"">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"tenant_id=\"t-b\"">>) =/= nomatch),
    ok.

test_router_prometheus_render_label_variants(_Config) ->
    router_metrics:clear_all(),
    router_metrics:emit_metric(router_error_total, #{value => 2.5}, #{}),
    Metadata = #{
        <<"binary_label">> => <<"binary_value">>,
        atom_label => active,
        <<"numeric_label">> => 42,
        <<"float_label">> => 3.14,
        <<"list_label">> => [alpha, beta],
        {<<"tuple_label">>, <<"compound">>} => {complex_value, 99}
    },
    router_metrics:emit_metric(router_circuit_breaker_state, #{value => 0.75}, Metadata),

    Rendered = router_prometheus:render(),
    ?assert(binary:match(Rendered, <<"router_error_total">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"router_circuit_breaker_state">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"binary_label">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"atom_label">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"numeric_label">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"list_label">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"tuple_label">>) =/= nomatch),
    ?assert(binary:match(Rendered, <<"complex_value">>) =/= nomatch),
    router_metrics:clear_all(),
    ok.

%% -------------------------------------------------------------------
%% Helpers
%% -------------------------------------------------------------------

clear_temp_file(Path) ->
    case file:read_file_info(Path) of
        {ok, _Info} -> file:delete(Path);
        _ -> ok
    end.

delete_metrics_table() ->
    router_test_init:delete_ets_table(router_metrics).

router_prometheus_metric_keys() ->
    lists:map(fun({Key, _, _}) -> Key end, router_prometheus_metadata_definitions()).

router_prometheus_metadata_definitions() ->
    [
        {router_jetstream_ack_total, "Total number of JetStream message acknowledgements", "counter"},
        {router_jetstream_redelivery_total, "Total number of JetStream message redeliveries (NAK operations)", "counter"},
        {router_redelivery_total, "Total number of JetStream message redeliveries (deprecated, use router_jetstream_redelivery_total)", "counter"},
        {router_dlq_total, "Total number of messages sent to Dead Letter Queue", "counter"},
        {router_idem_hits_total, "Total number of idempotency cache hits", "counter"},
        {router_idem_miss_total, "Total number of idempotency cache misses", "counter"},
        {router_idem_evictions_total, "Total number of idempotency entries evicted due to TTL expiration", "counter"},
        {router_acl_allowed_total, "Total number of ACL allowed decisions", "counter"},
        {router_acl_denied_total, "Total number of ACL denied decisions", "counter"},
        {ctx_missing_headers_total, "Total number of requests with missing context headers (trace_id, span_id, tenant_id)", "counter"},
        {router_span_duration_seconds, "Duration of router spans in seconds", "histogram"},
        {router_error_total, "Total number of router errors", "counter"},
        {router_circuit_breaker_state, "Current circuit breaker state (0=closed, 0.5=half_open, 1=open)", "gauge"},
        {router_circuit_breaker_state_transitions_total, "Total circuit breaker state transitions", "counter"},
        {router_circuit_breaker_trigger_reason, "Count of circuit breaker openings by trigger reason", "counter"},
        {router_nats_publish_latency_seconds, "Latest NATS publish latency in seconds", "gauge"}
    ].
