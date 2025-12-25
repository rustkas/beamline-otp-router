%% @doc Helper for extensions chaos tests
%%
%% Shared functions:
%% - Test execution
%% - Results analysis
%% - Statistics calculation
%% - Policy/request creation
%%
%% @test_category helper
-module(router_extensions_chaos_helper).
-include_lib("common_test/include/ct.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([
    init_suite/1,
    end_suite/1,
    init_testcase/2,
    end_testcase/2,
    execute_chaos_test/2,
    analyze_results/1,
    calculate_statistics/1,
    collect_metrics/1,
    create_policy_with_extensions/1,
    create_route_request/2
]).

-record(chaos_stats, {
    p50_latency_ms,
    p95_latency_ms,
    p99_latency_ms,
    max_latency_ms
}).

init_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, extension_registry, [{source, fixtures}]),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, circuit_breaker_enabled, true),
    ok = application:set_env(beamline_router, circuit_breaker_failure_threshold, 5),
    ok = application:set_env(beamline_router, circuit_breaker_timeout_seconds, 10),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_extension_registry, 1000),
            Config;
        Error ->
            ct:fail("Failed to start: ~p", [Error])
    end.

end_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_testcase(_TC, Config) ->
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        {error, connection_closed}
    end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload) ->
        {error, connection_closed}
    end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subj, _Stream, _Ack, _Dur, _Mode) ->
        {error, connection_closed}
    end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:new(router_extension_invoker, [passthrough]),
    
    setup_extension_mocks(),
    
    ChaosMetrics = ets:new(chaos_metrics, [set, private]),
    CBEvents = ets:new(cb_events, [set, private]),
    
    [{chaos_metrics, ChaosMetrics}, {cb_events, CBEvents} | Config].

end_testcase(_TC, Config) ->
    case proplists:get_value(chaos_metrics, Config) of
        T when T =/= undefined -> ets:delete(T);
        _ -> ok
    end,
    case proplists:get_value(cb_events, Config) of
        T2 when T2 =/= undefined -> ets:delete(T2);
        _ -> ok
    end,
    meck:unload([router_nats, router_extension_registry, router_extension_invoker]),
    Config.

setup_extension_mocks() ->
    meck:expect(router_extension_registry, lookup, fun(ExtId) ->
        case ExtId of
            <<"normalize_text">> ->
                {ok, #extension{
                    id = <<"normalize_text">>,
                    type = <<"pre">>,
                    subject = <<"beamline.ext.pre.normalize_text.v1">>,
                    timeout_ms = 1000,
                    retry = 0,
                    enabled = true
                }};
            <<"pii_guard">> ->
                {ok, #extension{
                    id = <<"pii_guard">>,
                    type = <<"validator">>,
                    subject = <<"beamline.ext.validate.pii_guard.v1">>,
                    timeout_ms = 1000,
                    retry = 0,
                    enabled = true
                }};
            _ -> {error, not_found}
        end
    end).

execute_chaos_test(NumRequests, Policy) ->
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    Results = lists:map(fun(_N) ->
        StartTime = erlang:monotonic_time(microsecond),
        Request = create_route_request(<<"Test message">>, Policy),
        Result = router_decider:decide(Request, Policy, Context),
        EndTime = erlang:monotonic_time(microsecond),
        Latency = EndTime - StartTime,
        timer:sleep(5),
        {Result, Latency}
    end, lists:seq(1, NumRequests)),
    {Results, [Latency || {_, Latency} <- Results]}.

analyze_results(Results) ->
    {SuccessResults, ErrorResults} = lists:partition(fun({Result, _}) ->
        case Result of
            {ok, _} -> true;
            _ -> false
        end
    end, Results),
    SuccessCount = length(SuccessResults),
    ErrorCount = length(ErrorResults),
    TimeoutCount = length([R || {R, _} <- ErrorResults, 
                                case R of
                                    {error, {timeout, _}} -> true;
                                    {error, {extension_timeout, _}} -> true;
                                    _ -> false
                                end]),
    ClientErrors = [R || {R, _} <- ErrorResults],
    {SuccessCount, ErrorCount, TimeoutCount, ClientErrors}.

calculate_statistics(Latencies) when length(Latencies) > 0 ->
    Sorted = lists:sort(Latencies),
    Length = length(Sorted),
    P50 = lists:nth(max(1, trunc(Length * 0.5)), Sorted) / 1000,
    P95 = lists:nth(max(1, trunc(Length * 0.95)), Sorted) / 1000,
    P99 = lists:nth(max(1, trunc(Length * 0.99)), Sorted) / 1000,
    Max = lists:max(Sorted) / 1000,
    #chaos_stats{p50_latency_ms = P50, p95_latency_ms = P95, p99_latency_ms = P99, max_latency_ms = Max};
calculate_statistics([]) ->
    #chaos_stats{p50_latency_ms = 0, p95_latency_ms = 0, p99_latency_ms = 0, max_latency_ms = 0}.

collect_metrics(Table) when Table =/= undefined ->
    ets:tab2list(Table);
collect_metrics(_) -> [].

create_policy_with_extensions(Extensions) ->
    Pre = proplists:get_value(pre, Extensions, []),
    PreItems = lists:map(fun(Item) ->
        case Item of
            {id, Id, mode, Mode} -> #{id => Id, mode => Mode, config => #{}};
            _ when is_map(Item) -> Item;
            _ -> #{}
        end
    end, Pre),
    %% Add a default validator to introduce realistic failures under chaos
    Validators = [#{id => <<"pii_guard">>, on_fail => <<"block">>, config => #{}}],
    #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = PreItems,
        validators = Validators,
        post = [],
        weights = #{<<"openai:gpt-4">> => 1.0},
        metadata = {}
    }.

create_route_request(Payload, Policy) ->
    #route_request{
        message = #{
            <<"message_id">> => <<"msg-001">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => Payload,
            <<"metadata">> => #{}
        },
        policy_id = Policy#policy.policy_id,
        context = #{}
    }.
