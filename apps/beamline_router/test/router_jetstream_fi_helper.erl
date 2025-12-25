%% @doc Shared helper for JetStream fault injection test suites
%% Contains telemetry handlers, metric utilities, and common setup
%% @test_category test_helper
-module(router_jetstream_fi_helper).

-export([
    setup_telemetry_handler/0,
    cleanup_telemetry_handler/1,
    wait_for_metric/4,
    assert_metric_labels/3,
    assert_metric_labels/4,
    collect_metrics/1,
    get_metrics_table/0,
    init_common_suite/1,
    end_common_suite/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% @doc Setup telemetry handler to collect metric events
-spec setup_telemetry_handler() -> reference().
setup_telemetry_handler() ->
    HandlerId = make_ref(),
    Handler = fun(EventName, Measurements, Metadata, _Config) ->
        Table = get_metrics_table(),
        ets:insert(Table, {HandlerId, EventName, Measurements, Metadata, erlang:monotonic_time()})
    end,
    telemetry:attach({HandlerId, maxdeliver_exhausted}, 
                     [router_decide_consumer, router_jetstream_maxdeliver_exhausted_total], Handler, #{}),
    telemetry:attach({HandlerId, redelivery}, 
                     [router, jetstream, nak], Handler, #{}),
    HandlerId.

%% @doc Cleanup telemetry handler
-spec cleanup_telemetry_handler(reference()) -> ok.
cleanup_telemetry_handler(HandlerId) ->
    telemetry:detach({HandlerId, maxdeliver_exhausted}),
    telemetry:detach({HandlerId, redelivery}),
    Table = get_metrics_table(),
    ets:match_delete(Table, {HandlerId, '_', '_', '_', '_'}),
    ok.

%% @doc Get or create metrics ETS table
-spec get_metrics_table() -> ets:tid().
get_metrics_table() ->
    TableName = router_jetstream_fi_metrics,
    case ets:whereis(TableName) of
        undefined ->
            ets:new(TableName, [bag, named_table, public, {write_concurrency, true}]);
        Table ->
            Table
    end.

%% @doc Wait for metric event with timeout
-spec wait_for_metric(reference(), atom(), non_neg_integer(), fun((term()) -> boolean())) -> ok | timeout.
wait_for_metric(HandlerId, MetricName, TimeoutMs, MatchFun) ->
    Table = get_metrics_table(),
    wait_for_metric_loop(Table, HandlerId, MetricName, TimeoutMs, MatchFun, erlang:monotonic_time(millisecond)).

wait_for_metric_loop(Table, HandlerId, MetricName, TimeoutMs, MatchFun, StartTime) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    case Elapsed >= TimeoutMs of
        true -> timeout;
        false ->
            Metrics = ets:match_object(Table, {HandlerId, '_', '_', '_', '_'}),
            case lists:any(fun({_, EventName, Measurements, Metadata, _}) ->
                %% Check if MetricName is the last element or is in the event name
                MetricMatch = case is_list(EventName) of
                    true -> lists:member(MetricName, EventName);
                    false -> EventName =:= MetricName
                end,
                case MetricMatch of
                    true -> MatchFun({Measurements, Metadata});
                    false -> false
                end
            end, Metrics) of
                true -> ok;
                false ->
                    timer:sleep(50),
                    wait_for_metric_loop(Table, HandlerId, MetricName, TimeoutMs, MatchFun, StartTime)
            end
    end.

%% @doc Assert metric has specific labels
-spec assert_metric_labels(reference(), atom(), map()) -> ok.
assert_metric_labels(HandlerId, MetricName, ExpectedLabels) ->
    assert_metric_labels(HandlerId, MetricName, ExpectedLabels, 2000).

-spec assert_metric_labels(reference(), atom(), map(), non_neg_integer()) -> ok.
assert_metric_labels(HandlerId, MetricName, ExpectedLabels, TimeoutMs) ->
    MatchFun = fun({_Measurements, Metadata}) ->
        maps:fold(fun(Key, Value, Acc) ->
            Acc andalso (maps:get(Key, Metadata, undefined) =:= Value)
        end, true, ExpectedLabels)
    end,
    case wait_for_metric(HandlerId, MetricName, TimeoutMs, MatchFun) of
        ok -> ok;
        timeout -> ct:fail("Timeout waiting for metric ~p with labels ~p", [MetricName, ExpectedLabels])
    end.

%% @doc Collect all metrics for a handler
-spec collect_metrics(reference()) -> list().
collect_metrics(HandlerId) ->
    Table = get_metrics_table(),
    ets:match_object(Table, {HandlerId, '_', '_', '_', '_'}).

%% @doc Common suite initialization
-spec init_common_suite(term()) -> term().
init_common_suite(Config) ->
    %% Ensure telemetry is started for attach/detach
    _ = application:ensure_all_started(telemetry),
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    Config.

%% @doc Common suite teardown
-spec end_common_suite(term()) -> term().
end_common_suite(Config) ->
    application:stop(beamline_router),
    catch meck:unload(router_rate_limiter),
    Config.
