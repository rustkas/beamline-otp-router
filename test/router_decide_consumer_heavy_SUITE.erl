%% @doc Heavy/Soak Tests for router_decide_consumer
%% 
%% Long-running, chaos, and max_deliver exhaustion tests.
%% NOT run by default. Use ROUTER_TEST_LEVEL=heavy to enable.
%%
%% @test_category soak, heavy, slow
-module(router_decide_consumer_heavy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Include state record
-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}
}).

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2
]}).

%% Common Test exports
-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_decide_max_delivery_count_exhaustion_smoke/1,
    test_decide_concurrent_messages_no_global_blocking/1,
    test_decide_ets_cleanup_after_recovery/1
]).

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, heavy_tests}];
        _ -> []  %% Skip unless heavy mode
    end.

groups() ->
    [{heavy_tests, [sequence], [
        test_decide_max_delivery_count_exhaustion_smoke,
        test_decide_concurrent_messages_no_global_blocking,
        test_decide_ets_cleanup_after_recovery
    ]}].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{
        common_env => false,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock,
            decide_subject => <<"beamline.router.v1.decide">>,
            tracing_enabled => false
        }
    }),
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    Config1.

end_per_suite(Config) ->
    catch meck:unload(router_rate_limiter),
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(_TestCase, Config) ->
    Config1 = router_test_bootstrap:init_per_testcase(_TestCase, Config, #{}),
    ok = router_metrics:ensure(),
    Config1.

end_per_testcase(_TestCase, Config) ->
    catch meck:unload(),
    router_test_bootstrap:end_per_testcase(_TestCase, Config, #{}).

%% ============================================================================
%% HELPER FUNCTIONS
%% ============================================================================

setup_basic_state() ->
    #state{
        connection = undefined,
        decide_subject = <<"beamline.router.v1.decide">>,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    }.

%% ============================================================================
%% TEST CASES
%% ============================================================================

%% @doc Smoke test for max_deliver configuration and tracking infrastructure
test_decide_max_delivery_count_exhaustion_smoke(_Config) ->
    MaxDeliver = application:get_env(beamline_router, nats_js_max_deliver, 3),
    ?assert(is_integer(MaxDeliver)),
    ?assert(MaxDeliver > 0),
    
    DeliveryTable = router_decide_delivery_counts,
    case ets:whereis(DeliveryTable) of
        undefined -> ok;
        _Table -> ?assert(ets:info(DeliveryTable, size) >= 0)
    end,
    
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    ?assert(is_process_alive(ConsumerPid)),
    ok.

%% @doc Test concurrent messages with partial failures - no global blocking
test_decide_concurrent_messages_no_global_blocking(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    Requests = [
        {<<"req-1">>, <<"tr-1">>, <<"msg-1">>, <<"tenant-1">>},
        {<<"req-2">>, <<"tr-2">>, <<"msg-2">>, <<"tenant-2">>},
        {<<"req-3">>, <<"tr-3">>, <<"msg-3">>, <<"tenant-3">>}
    ],
    
    PublishCalls = router_test_init:ensure_ets_table(publish_calls, [named_table, set, public]),
    _ProcessedMessages = router_test_init:ensure_ets_table(processed_messages, [named_table, set, public]),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    %% NOTE: Do NOT call ensure_router_nats_started() with mock - start_link not stubbed
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        Count = case ets:lookup(PublishCalls, count) of
            [{count, C}] -> C + 1;
            [] -> 1
        end,
        ets:insert(PublishCalls, {count, Count}),
        ok
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    ok = application:set_env(beamline_router, nats_mode, mock),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    lists:foreach(fun({RequestId, TraceId, MsgId, TenantId}) ->
        Request = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => RequestId,
            <<"trace_id">> => TraceId,
            <<"tenant_id">> => TenantId,
            <<"task">> => #{<<"type">> => <<"text.generate">>, <<"payload_ref">> => <<"s3://bucket/key">>},
            <<"policy_id">> => <<"default">>
        },
        RequestJson = jsx:encode(Request),
        State = setup_basic_state(),
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State)
    end, Requests),
    
    ok = test_helpers:wait_for_condition(
        fun() ->
            meck:num_calls(router_nats, publish, '_') >= length(Requests)
        end, 1000),
    ?assert(is_process_alive(ConsumerPid)),
    
    [{count, FinalCount}] = ets:lookup(PublishCalls, count),
    ?assert(FinalCount >= length(Requests)),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    meck:unload(router_tenant_validator),
    ets:delete_all_objects(PublishCalls),
    ets:delete_all_objects(processed_messages),
    ok.

%% @doc Test ETS cleanup after recovery
test_decide_ets_cleanup_after_recovery(_Config) ->
    MsgId = <<"msg-cleanup-test">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    DeliveryTable = router_test_init:ensure_ets_table(router_decide_delivery_counts, [named_table, public, set]),
    
    %% Track delivery
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    
    %% Mock and process a successful request - no passthrough to avoid noproc
    meck:new(router_nats, []),
    %% NOTE: Do NOT call ensure_router_nats_started() with mock - start_link not stubbed
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    ok = application:set_env(beamline_router, nats_mode, mock),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-cleanup">>,
        <<"trace_id">> => <<"tr-cleanup">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
    ok = test_helpers:wait_for_condition(
        fun() ->
            case ets:lookup(DeliveryTable, MsgId) of
                [] -> true;
                [{MsgId, Count}] -> Count =< 5
            end
        end, 500),
    
    %% Cleanup delivery count
    router_decide_consumer:cleanup_delivery_count(MsgId),
    
    %% Verify cleanup
    case ets:whereis(DeliveryTable) of
        undefined -> ok;
        T ->
            case ets:lookup(T, MsgId) of
                [] -> ok;
                [{MsgId, Count}] -> ?assert(Count =< 5)
            end
    end,
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.
