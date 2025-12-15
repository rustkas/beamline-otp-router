%% @doc Metrics Dump: Contract and Edge Case Tests
%%
%% Contract and exporter tests:
%% - Base metrics contract
%% - JetStream metrics
%% - Idempotency metrics
%% - ACL metrics
%% - Circuit breaker metrics
%% - Exporter edge cases
%%
%% @test_category metrics, contract, fast
-module(router_metrics_contract_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_base_metrics_contract/1,
    test_jetstream_metrics/1,
    test_idem_metrics/1,
    test_acl_metrics/1,
    test_circuit_breaker_metrics/1,
    test_exporter_parses_all_types/1,
    test_exporter_edge_cases/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    router_ct_groups:all_selection(?MODULE, [{group, contract_tests}]).

groups() ->
    router_ct_groups:groups_definitions(?MODULE, base_groups()).

base_groups() ->
    [{contract_tests, [sequence], [
        test_base_metrics_contract,
        test_jetstream_metrics,
        test_idem_metrics,
        test_acl_metrics,
        test_circuit_breaker_metrics,
        test_exporter_parses_all_types,
        test_exporter_edge_cases
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    Config.

end_per_suite(Config) -> Config.

init_per_testcase(_TC, Config) ->
    ok = router_metrics:ensure(),
    ok = router_metrics:clear_all(),
    Config.

end_per_testcase(_TC, Config) -> Config.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_base_metrics_contract(_Config) ->
    BaseMetrics = [
        {router_jetstream_ack_total, "JetStream acks"},
        {router_dlq_total, "DLQ"},
        {router_idem_hits_total, "Idem hits"},
        {router_idem_miss_total, "Idem misses"},
        {router_acl_allowed_total, "ACL allowed"},
        {router_acl_denied_total, "ACL denied"}
    ],
    
    lists:foreach(fun({Metric, _}) ->
        ok = router_metrics:inc(Metric)
    end, BaseMetrics),
    
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => <<"test">>, request_id => <<"test">>, reason => <<"backoff">>, source => <<"test">>
    }),
    
    ok = router_prometheus:dump("metrics_dump/contract_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/contract_test.prom"),
    Str = binary_to_list(Bin),
    
    lists:foreach(fun({Metric, _}) ->
        MetricName = atom_to_list(Metric),
        ?assert(string:find(Str, io_lib:format("# HELP ~s", [MetricName])) =/= nomatch),
        ?assert(string:find(Str, io_lib:format("# TYPE ~s counter", [MetricName])) =/= nomatch)
    end, BaseMetrics),
    ok.

test_jetstream_metrics(_Config) ->
    ok = router_metrics:inc(router_jetstream_ack_total),
    ok = router_metrics:inc(router_dlq_total),
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => <<"test">>, request_id => <<"test">>, reason => <<"backoff">>, source => <<"test">>
    }),
    
    ok = router_prometheus:dump("metrics_dump/jetstream_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/jetstream_test.prom"),
    Str = binary_to_list(Bin),
    
    lists:foreach(fun(M) ->
        ?assert(string:find(Str, atom_to_list(M)) =/= nomatch)
    end, [router_jetstream_ack_total, router_dlq_total, router_jetstream_redelivery_total]),
    ok.

test_idem_metrics(_Config) ->
    ok = router_metrics:inc(router_idem_hits_total),
    ok = router_metrics:inc(router_idem_miss_total),
    
    ok = router_prometheus:dump("metrics_dump/idem_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/idem_test.prom"),
    Str = binary_to_list(Bin),
    
    ?assert(string:find(Str, "router_idem_hits_total") =/= nomatch),
    ?assert(string:find(Str, "router_idem_miss_total") =/= nomatch),
    ok.

test_acl_metrics(_Config) ->
    ok = router_metrics:inc(router_acl_allowed_total),
    ok = router_metrics:inc(router_acl_denied_total),
    
    ok = router_prometheus:dump("metrics_dump/acl_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/acl_test.prom"),
    Str = binary_to_list(Bin),
    
    ?assert(string:find(Str, "router_acl_allowed_total") =/= nomatch),
    ?assert(string:find(Str, "router_acl_denied_total") =/= nomatch),
    ok.

test_circuit_breaker_metrics(_Config) ->
    ok = router_metrics:emit_metric(router_circuit_breaker_state, #{value => 1.0}, #{
        tenant_id => <<"test-tenant">>, provider_id => <<"openai">>, state => <<"open">>
    }),
    ok = router_metrics:emit_metric(router_circuit_breaker_state_transitions_total, #{count => 1}, #{
        tenant_id => <<"test-tenant">>, provider_id => <<"openai">>, from => <<"closed">>, to => <<"open">>
    }),
    ok = router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{count => 1}, #{
        tenant_id => <<"test-tenant">>, provider_id => <<"openai">>, reason => <<"latency_threshold_exceeded">>
    }),
    
    ok = router_prometheus:dump("metrics_dump/circuit_breaker_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/circuit_breaker_test.prom"),
    Str = binary_to_list(Bin),
    
    ?assert(string:find(Str, "# HELP router_circuit_breaker_state") =/= nomatch),
    ?assert(string:find(Str, "# TYPE router_circuit_breaker_state gauge") =/= nomatch),
    ?assert(string:find(Str, "router_circuit_breaker_state{") =/= nomatch),
    ?assert(string:find(Str, "tenant_id=\"test-tenant\"") =/= nomatch),
    ?assert(string:find(Str, "reason=\"latency_threshold_exceeded\"") =/= nomatch),
    ok.

test_exporter_parses_all_types(_Config) ->
    ok = router_metrics:inc(router_jetstream_ack_total),
    ok = router_metrics:inc(router_dlq_total),
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => <<"test">>, request_id => <<"req">>, reason => <<"backoff">>, source => <<"test">>
    }),
    ok = router_metrics:emit_metric(router_circuit_breaker_state, #{value => 0.5}, #{
        tenant_id => <<"t1">>, provider_id => <<"p1">>, state => <<"half_open">>
    }),
    
    RenderResult = router_prometheus:render(),
    
    ?assert(is_binary(RenderResult)),
    ?assert(byte_size(RenderResult) > 0),
    
    Str = binary_to_list(RenderResult),
    ?assert(string:find(Str, "# HELP router_jetstream_ack_total") =/= nomatch),
    ?assert(string:find(Str, "# TYPE router_circuit_breaker_state gauge") =/= nomatch),
    ok.

test_exporter_edge_cases(_Config) ->
    ets:insert(router_metrics, {router_test_large_int, 999999999999}),
    ets:insert(router_metrics, {router_test_small_float, 0.000001}),
    ets:insert(router_metrics, {router_test_zero, 0}),
    ok = router_metrics:emit_metric(router_test_binary_labels, #{count => 1}, #{
        tenant_id => <<"binary-tenant-123">>, provider_id => <<"binary-provider-456">>
    }),
    
    RenderResult = router_prometheus:render(),
    
    ?assert(is_binary(RenderResult)),
    Str = binary_to_list(RenderResult),
    ?assertEqual(nomatch, string:find(Str, "\n\n\n")),
    ok.
