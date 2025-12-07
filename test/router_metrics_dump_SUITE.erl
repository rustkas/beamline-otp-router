-module(router_metrics_dump_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
%% -include("beamline_router.hrl"). %% Not needed - no records used

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2]}).


-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    test_dump_read/1,
    test_prometheus_format/1,
    test_metric_names/1,
    test_metric_labels/1,
    test_metric_invariants/1,
    test_base_metrics_contract/1,
    test_jetstream_metrics/1,
    test_idem_metrics/1,
    test_acl_metrics/1
]).

all() ->
  [
    {group, prometheus_tests}
  ].

groups() ->
  [
    {prometheus_tests, [sequence], [
      test_dump_read,
      test_prometheus_format,
      test_metric_names,
      test_metric_labels,
      test_metric_invariants,
      test_base_metrics_contract,
      test_jetstream_metrics,
      test_idem_metrics,
      test_acl_metrics
    ]}
  ].

init_per_suite(Config) ->
  _ = application:load(beamline_router),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_TestCase, Config) ->
  %% Reset metrics (using API instead of direct ETS access)
  ok = router_metrics:ensure(),
  ok = router_metrics:clear_all(),
  Config.

end_per_testcase(_TestCase, Config) ->
  Config.

%% Test: Basic dump read
test_dump_read(_Config) ->
  ok = router_metrics:ensure(),
  ok = router_metrics:inc(router_jetstream_ack_total),
  ok = router_prometheus:dump("metrics_dump/test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/test.prom"),
  Str = binary_to_list(Bin),
  ?assert(string:find(Str, "router_jetstream_ack_total") =/= nomatch),
  ok.

%% Test: Prometheus format validation
test_prometheus_format(_Config) ->
  ok = router_metrics:ensure(),
  ok = router_metrics:inc(router_jetstream_ack_total),
  ok = router_metrics:inc(router_idem_hits_total),
  ok = router_metrics:inc(router_acl_allowed_total),
  
  ok = router_prometheus:dump("metrics_dump/format_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/format_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Check for Prometheus format elements
  ?assert(string:find(Str, "# HELP") =/= nomatch),
  ?assert(string:find(Str, "# TYPE") =/= nomatch),
  
  %% Check HELP lines
  ?assert(string:find(Str, "# HELP router_jetstream_ack_total") =/= nomatch),
  ?assert(string:find(Str, "# HELP router_idem_hits_total") =/= nomatch),
  ?assert(string:find(Str, "# HELP router_acl_allowed_total") =/= nomatch),
  
  %% Check TYPE lines
  ?assert(string:find(Str, "# TYPE router_jetstream_ack_total counter") =/= nomatch),
  ?assert(string:find(Str, "# TYPE router_idem_hits_total counter") =/= nomatch),
  ?assert(string:find(Str, "# TYPE router_acl_allowed_total counter") =/= nomatch),
  
  %% Check metric lines
  ?assert(string:find(Str, "router_jetstream_ack_total 1") =/= nomatch),
  ?assert(string:find(Str, "router_idem_hits_total 1") =/= nomatch),
  ?assert(string:find(Str, "router_acl_allowed_total 1") =/= nomatch),
  
  ok.

%% Test: Metric names validation
test_metric_names(_Config) ->
  ok = router_metrics:ensure(),
  
  %% Increment all base metrics
  ok = router_metrics:inc(router_jetstream_ack_total),
  %% Use emit_metric for router_jetstream_redelivery_total (has labels)
  ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
    assignment_id => <<"test-assignment">>,
    request_id => <<"test-request">>,
    reason => <<"backoff">>,
    source => <<"backoff">>
  }),
  ok = router_metrics:inc(router_dlq_total),
  ok = router_metrics:inc(router_idem_hits_total),
  ok = router_metrics:inc(router_idem_miss_total),
  ok = router_metrics:inc(router_acl_allowed_total),
  ok = router_metrics:inc(router_acl_denied_total),
  
  ok = router_prometheus:dump("metrics_dump/names_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/names_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Verify all metric names are present
  RequiredMetrics = [
    "router_jetstream_ack_total",
    "router_jetstream_redelivery_total",
    "router_dlq_total",
    "router_idem_hits_total",
    "router_idem_miss_total",
    "router_acl_allowed_total",
    "router_acl_denied_total"
  ],
  
  lists:foreach(fun(MetricName) ->
    ?assert(string:find(Str, MetricName) =/= nomatch),
    ct:pal("Metric ~s found in dump", [MetricName])
  end, RequiredMetrics),
  
  ok.

%% Test: Metric labels validation
test_metric_labels(_Config) ->
  ok = router_metrics:ensure(),
  
  %% Test metric without labels (backward compatible)
  ok = router_metrics:inc(router_jetstream_ack_total),
  
  %% Test metric with labels
  ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
    assignment_id => <<"test-assignment">>,
    request_id => <<"test-request">>,
    reason => <<"backoff">>,
    source => <<"backoff">>
  }),
  
  ok = router_prometheus:dump("metrics_dump/labels_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/labels_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Check that metric lines are valid Prometheus format
  Lines = string:tokens(Str, "\n"),
  MetricLines = [L || L <- Lines, string:find(L, "router_") =/= nomatch, string:find(L, "#") =:= nomatch],
  
  %% Verify unlabeled metric (backward compatible)
  UnlabeledMetric = lists:filter(fun(Line) ->
    string:find(Line, "router_jetstream_ack_total") =/= nomatch andalso
    string:find(Line, "{") =:= nomatch
  end, MetricLines),
  ?assert(length(UnlabeledMetric) > 0),
  ct:pal("Unlabeled metric found: ~s", [hd(UnlabeledMetric)]),
  
  %% Verify labeled metric
  LabeledMetric = lists:filter(fun(Line) ->
    string:find(Line, "router_jetstream_redelivery_total{") =/= nomatch
  end, MetricLines),
  ?assert(length(LabeledMetric) > 0),
  LabeledLine = hd(LabeledMetric),
  ct:pal("Labeled metric found: ~s", [LabeledLine]),
  
  %% Verify all required labels are present
  ?assert(string:find(LabeledLine, "assignment_id=\"test-assignment\"") =/= nomatch),
  ?assert(string:find(LabeledLine, "request_id=\"test-request\"") =/= nomatch),
  ?assert(string:find(LabeledLine, "reason=\"backoff\"") =/= nomatch),
  ?assert(string:find(LabeledLine, "source=\"backoff\"") =/= nomatch),
  
  %% Verify metric value
  ?assert(string:find(LabeledLine, "} 1") =/= nomatch orelse string:find(LabeledLine, "}1") =/= nomatch),
  
  ct:pal("All label validations passed"),
  
  ok.

%% Test: Metric invariants
test_metric_invariants(_Config) ->
  ok = router_metrics:ensure(),
  
  %% Increment metrics multiple times
  lists:foreach(fun(_) ->
    ok = router_metrics:inc(router_jetstream_ack_total),
    ok = router_metrics:inc(router_idem_hits_total)
  end, lists:seq(1, 10)),
  
  ok = router_prometheus:dump("metrics_dump/invariants_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/invariants_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Verify metric values are correct (should be 10)
  ?assert(string:find(Str, "router_jetstream_ack_total 10") =/= nomatch),
  ?assert(string:find(Str, "router_idem_hits_total 10") =/= nomatch),
  
  %% Verify counters only increase (no negative values)
  Lines = string:tokens(Str, "\n"),
  MetricLines = [L || L <- Lines, string:find(L, "router_") =/= nomatch, string:find(L, "#") =:= nomatch],
  
  lists:foreach(fun(Line) ->
    %% Extract value (last number in line)
    case re:run(Line, "\\s+(\\d+)$", [{capture, all_but_first, list}]) of
      {match, [ValueStr]} ->
        Value = list_to_integer(ValueStr),
        ?assert(Value >= 0),  %% Counters must be non-negative
        ct:pal("Metric value ~b is valid", [Value]);
      _ ->
        ct:pal("Could not parse value from line: ~s", [Line])
    end
  end, MetricLines),
  
  ok.

%% Test: Base metrics contract (JetStream, Idem, ACL)
test_base_metrics_contract(_Config) ->
  ok = router_metrics:ensure(),
  
  %% Base metrics contract: These metrics must always be present in dump
  BaseMetrics = [
    {router_jetstream_ack_total, "JetStream acknowledgements"},
    {router_jetstream_redelivery_total, "JetStream redeliveries"},
    {router_dlq_total, "Dead Letter Queue messages"},
    {router_idem_hits_total, "Idempotency cache hits"},
    {router_idem_miss_total, "Idempotency cache misses"},
    {router_acl_allowed_total, "ACL allowed decisions"},
    {router_acl_denied_total, "ACL denied decisions"}
  ],
  
  %% Increment all base metrics
  lists:foreach(fun({Metric, _Desc}) ->
    case Metric of
      router_jetstream_redelivery_total ->
        %% Use emit_metric for labeled metric
        ok = router_metrics:emit_metric(Metric, #{count => 1}, #{
          assignment_id => <<"test">>,
          request_id => <<"test">>,
          reason => <<"backoff">>,
          source => <<"backoff">>
        });
      _ ->
        ok = router_metrics:inc(Metric)
    end
  end, BaseMetrics),
  
  ok = router_prometheus:dump("metrics_dump/contract_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/contract_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Verify all base metrics are present with correct format
  lists:foreach(fun({Metric, Desc}) ->
    MetricName = atom_to_list(Metric),
    
    %% Check HELP line
    HelpPattern = io_lib:format("# HELP ~s", [MetricName]),
    ?assert(string:find(Str, HelpPattern) =/= nomatch),
    
    %% Check TYPE line
    TypePattern = io_lib:format("# TYPE ~s counter", [MetricName]),
    ?assert(string:find(Str, TypePattern) =/= nomatch),
    
    %% Check metric line
    MetricPattern = io_lib:format("~s 1", [MetricName]),
    ?assert(string:find(Str, MetricPattern) =/= nomatch),
    
    ct:pal("Base metric contract validated: ~s (~s)", [MetricName, Desc])
  end, BaseMetrics),
  
  ok.

%% Test: JetStream metrics specifically
test_jetstream_metrics(_Config) ->
  ok = router_metrics:ensure(),
  
  JetStreamMetrics = [
    router_jetstream_ack_total,
    router_jetstream_redelivery_total,
    router_dlq_total
  ],
  
  %% Increment JetStream metrics
  lists:foreach(fun(Metric) ->
    case Metric of
      router_jetstream_redelivery_total ->
        %% Use emit_metric for labeled metric
        ok = router_metrics:emit_metric(Metric, #{count => 1}, #{
          assignment_id => <<"test">>,
          request_id => <<"test">>,
          reason => <<"backoff">>,
          source => <<"backoff">>
        });
      _ ->
        ok = router_metrics:inc(Metric)
    end
  end, JetStreamMetrics),
  
  ok = router_prometheus:dump("metrics_dump/jetstream_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/jetstream_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Verify JetStream metrics
  lists:foreach(fun(Metric) ->
    MetricName = atom_to_list(Metric),
    ?assert(string:find(Str, MetricName) =/= nomatch),
    ct:pal("JetStream metric ~s validated", [MetricName])
  end, JetStreamMetrics),
  
  ok.

%% Test: Idempotency metrics specifically
test_idem_metrics(_Config) ->
  ok = router_metrics:ensure(),
  
  IdemMetrics = [
    router_idem_hits_total,
    router_idem_miss_total
  ],
  
  %% Increment idempotency metrics
  lists:foreach(fun(Metric) ->
    ok = router_metrics:inc(Metric)
  end, IdemMetrics),
  
  ok = router_prometheus:dump("metrics_dump/idem_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/idem_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Verify idempotency metrics
  lists:foreach(fun(Metric) ->
    MetricName = atom_to_list(Metric),
    ?assert(string:find(Str, MetricName) =/= nomatch),
    ct:pal("Idempotency metric ~s validated", [MetricName])
  end, IdemMetrics),
  
  ok.

%% Test: ACL metrics specifically
test_acl_metrics(_Config) ->
  ok = router_metrics:ensure(),
  
  AclMetrics = [
    router_acl_allowed_total,
    router_acl_denied_total
  ],
  
  %% Increment ACL metrics
  lists:foreach(fun(Metric) ->
    ok = router_metrics:inc(Metric)
  end, AclMetrics),
  
  ok = router_prometheus:dump("metrics_dump/acl_test.prom"),
  {ok, Bin} = file:read_file("metrics_dump/acl_test.prom"),
  Str = binary_to_list(Bin),
  
  %% Verify ACL metrics
  lists:foreach(fun(Metric) ->
    MetricName = atom_to_list(Metric),
    ?assert(string:find(Str, MetricName) =/= nomatch),
    ct:pal("ACL metric ~s validated", [MetricName])
  end, AclMetrics),
  
  ok.

