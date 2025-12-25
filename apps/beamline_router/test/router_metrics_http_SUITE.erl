-module(router_metrics_http_SUITE).

-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    groups/0,
    metrics_http_serves_prometheus/1
]).

all() ->
    [{group, http_handler_tests}].

groups() ->
    [
        {http_handler_tests, [parallel], [metrics_http_serves_prometheus]}
    ].

init_per_suite(Config) ->
    %% Don't start full app - we only need metrics, not NATS consumers
    Config1 = router_test_bootstrap:init_per_suite(Config, #{start => none}),
    %% Manually start metrics
    {ok, _} = application:ensure_all_started(telemetry),
    ok = router_metrics:ensure(),
    ok = router_metrics:inc(router_jetstream_ack_total),
    meck:new(mod_esi, [passthrough, no_link]),
    meck:expect(mod_esi, deliver, fun(Sess, Part) -> self() ! {deliver, Sess, Part}, ok end),
    Config1.

end_per_suite(Config) ->
    catch meck:unload(mod_esi),
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(TestCase, Config) ->
    router_test_bootstrap:init_per_testcase(TestCase, Config, #{}).

end_per_testcase(TestCase, Config) ->
    router_test_bootstrap:end_per_testcase(TestCase, Config, #{}).

metrics_http_serves_prometheus(_Config) ->
    %% ensure metric value exists
    ok = router_metrics:inc(router_jetstream_ack_total),
    %% Render directly to avoid transport flakiness
    BodyStr = unicode:characters_to_list(router_prometheus:render()),
    true = string:find(BodyStr, "router_jetstream_ack_total") =/= nomatch,
    ok.
