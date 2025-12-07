-module(router_metrics_http_SUITE).

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1,
    metrics_http_serves_prometheus/1
]}).


all() -> [metrics_http_serves_prometheus].

init_per_suite(Config) ->
    ok = router_metrics:ensure(),
    ok = router_metrics:inc(router_jetstream_ack_total),
    meck:new(mod_esi, [passthrough]),
    meck:expect(mod_esi, deliver, fun(Sess, Part) -> self() ! {deliver, Sess, Part}, ok end),
    Config.

end_per_suite(_Config) ->
    meck:unload(mod_esi),
    ok.

metrics_http_serves_prometheus(_Config) ->
    _ = router_metrics_http:metrics(make_ref(), [], <<>>),
    timer:sleep(50),
    Delivered = collect_delivered([]),
    BodyStr = unicode:characters_to_list(iolist_to_binary(Delivered)),
    true = string:find(BodyStr, "router_jetstream_ack_total") =/= nomatch,
    ok.

collect_delivered(Acc) ->
    receive
        {deliver, _Sess, Part} ->
            collect_delivered([Acc, Part])
    after 10 ->
        Acc
    end.
