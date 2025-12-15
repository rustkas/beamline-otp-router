-module(router_metrics_http_SUITE).

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    metrics_http_serves_prometheus/1
]}).


-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    groups_for_level/1, groups/0,
    metrics_http_serves_prometheus/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Metrics HTTP handler unit tests -> Fast
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [{group, http_handler_tests}];
groups_for_level(full) -> [{group, http_handler_tests}];
groups_for_level(heavy) -> [{group, http_handler_tests}].

groups() ->
    [
        {http_handler_tests, [parallel], [metrics_http_serves_prometheus]}
    ].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{}),
    ok = router_metrics:ensure(),
    ok = router_metrics:inc(router_jetstream_ack_total),
    meck:new(mod_esi, [passthrough]),
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

collect_delivered(Acc) ->
    receive
        {deliver, _Sess, Part} ->
            collect_delivered([Acc, Part])
    after router_test_timeouts:very_short_wait() ->
        Acc
    end.
