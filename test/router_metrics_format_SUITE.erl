%% @doc Metrics Dump: Core Format Tests
%%
%% Core Prometheus format validation:
%% - Basic dump read
%% - Prometheus format
%% - Metric names
%% - Metric labels
%% - Metric invariants
%%
%% @test_category metrics, fast
-module(router_metrics_format_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_dump_read/1,
    test_prometheus_format/1,
    test_metric_names/1,
    test_metric_labels/1,
    test_metric_invariants/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, format_tests}];
        "full" -> [{group, format_tests}];
        _ -> [{group, format_tests}]
    end.

groups() ->
    [{format_tests, [sequence], [
        test_dump_read,
        test_prometheus_format,
        test_metric_names,
        test_metric_labels,
        test_metric_invariants
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

test_dump_read(_Config) ->
    ok = router_metrics:inc(router_jetstream_ack_total),
    ok = router_prometheus:dump("metrics_dump/test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/test.prom"),
    Str = binary_to_list(Bin),
    ?assert(string:find(Str, "router_jetstream_ack_total") =/= nomatch),
    ok.

test_prometheus_format(_Config) ->
    ok = router_metrics:inc(router_jetstream_ack_total),
    ok = router_metrics:inc(router_idem_hits_total),
    ok = router_metrics:inc(router_acl_allowed_total),
    
    ok = router_prometheus:dump("metrics_dump/format_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/format_test.prom"),
    Str = binary_to_list(Bin),
    
    ?assert(string:find(Str, "# HELP") =/= nomatch),
    ?assert(string:find(Str, "# TYPE") =/= nomatch),
    ?assert(string:find(Str, "# HELP router_jetstream_ack_total") =/= nomatch),
    ?assert(string:find(Str, "# TYPE router_jetstream_ack_total counter") =/= nomatch),
    ok.

test_metric_names(_Config) ->
    ok = router_metrics:inc(router_jetstream_ack_total),
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => <<"test">>, request_id => <<"test">>, reason => <<"backoff">>, source => <<"test">>
    }),
    ok = router_metrics:inc(router_dlq_total),
    ok = router_metrics:inc(router_idem_hits_total),
    
    ok = router_prometheus:dump("metrics_dump/names_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/names_test.prom"),
    Str = binary_to_list(Bin),
    
    RequiredMetrics = ["router_jetstream_ack_total", "router_jetstream_redelivery_total",
                       "router_dlq_total", "router_idem_hits_total"],
    lists:foreach(fun(M) -> ?assert(string:find(Str, M) =/= nomatch) end, RequiredMetrics),
    ok.

test_metric_labels(_Config) ->
    ok = router_metrics:inc(router_jetstream_ack_total),
    ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
        assignment_id => <<"test-assignment">>, request_id => <<"test-request">>,
        reason => <<"backoff">>, source => <<"backoff">>
    }),
    
    ok = router_prometheus:dump("metrics_dump/labels_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/labels_test.prom"),
    Str = binary_to_list(Bin),
    
    Lines = string:tokens(Str, "\n"),
    MetricLines = [L || L <- Lines, string:find(L, "router_") =/= nomatch, string:find(L, "#") =:= nomatch],
    
    UnlabeledMetric = lists:filter(fun(L) ->
        string:find(L, "router_jetstream_ack_total") =/= nomatch andalso string:find(L, "{") =:= nomatch
    end, MetricLines),
    ?assert(length(UnlabeledMetric) > 0),
    
    LabeledMetric = lists:filter(fun(L) ->
        string:find(L, "router_jetstream_redelivery_total{") =/= nomatch
    end, MetricLines),
    ?assert(length(LabeledMetric) > 0),
    ?assert(string:find(hd(LabeledMetric), "assignment_id=\"test-assignment\"") =/= nomatch),
    ok.

test_metric_invariants(_Config) ->
    lists:foreach(fun(_) ->
        ok = router_metrics:inc(router_jetstream_ack_total),
        ok = router_metrics:inc(router_idem_hits_total)
    end, lists:seq(1, 10)),
    
    ok = router_prometheus:dump("metrics_dump/invariants_test.prom"),
    {ok, Bin} = file:read_file("metrics_dump/invariants_test.prom"),
    Str = binary_to_list(Bin),
    
    ?assert(string:find(Str, "router_jetstream_ack_total 10") =/= nomatch),
    ?assert(string:find(Str, "router_idem_hits_total 10") =/= nomatch),
    
    Lines = string:tokens(Str, "\n"),
    MetricLines = [L || L <- Lines, string:find(L, "router_") =/= nomatch, string:find(L, "#") =:= nomatch],
    lists:foreach(fun(Line) ->
        case re:run(Line, "\\s+(\\d+)$", [{capture, all_but_first, list}]) of
            {match, [ValueStr]} -> ?assert(list_to_integer(ValueStr) >= 0);
            _ -> ok
        end
    end, MetricLines),
    ok.
