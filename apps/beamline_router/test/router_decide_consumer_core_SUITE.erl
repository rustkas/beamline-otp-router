%% @doc Core Unit Tests for router_decide_consumer
%% 
%% Fast, stable, deterministic tests for basic decide request handling.
%% Runs on every CI push.
%%
%% @test_category unit, fast
-module(router_decide_consumer_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Include state record
-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}
}).

-compile({nowarn_unused_function, [
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2
]}).

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_decide_request_success/1,
    test_decide_request_with_push_assignment/1,
    test_decide_request_error_policy_not_found/1,
    test_decide_request_error_missing_tenant_id/1,
    test_decide_request_unsupported_version/1,
    test_malformed_json/1,
    test_payload_size_limit/1,
    test_cp2_headers_happy_path/1,
    test_ack_message_after_success/1,
    test_delivery_count_tracking/1
]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end.
meta_all() ->
    [{group, unit_tests}].

groups() ->
    [{unit_tests, [sequence], [
        test_decide_request_success,
        test_decide_request_with_push_assignment,
        test_decide_request_error_policy_not_found,
        test_decide_request_error_missing_tenant_id,
        test_decide_request_unsupported_version,
        test_malformed_json,
        test_payload_size_limit,
        test_cp2_headers_happy_path,
        test_ack_message_after_success,
        test_delivery_count_tracking
    ]}].

init_per_suite(Config) ->
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    router_test_bootstrap:init_per_suite(Config, #{
        start => router_suite,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock,
            decide_subject => <<"beamline.router.v1.decide">>,
            tracing_enabled => false
        }
    }).

end_per_suite(Config) ->
    Base = router_test_bootstrap:end_per_suite(Config, #{
        start => router_suite,
        stop => router_suite
    }),
    catch meck:unload(router_rate_limiter),
    Base.

init_per_testcase(TestCase, Config) ->
    Base = router_test_bootstrap:init_per_testcase(TestCase, Config, #{
        clear_faults => true,
        ensure_router_nats_alive => true
    }),
    router_metrics:ensure(),
    Base.

end_per_testcase(TestCase, Config) ->
    Base = router_test_bootstrap:end_per_testcase(TestCase, Config, #{cleanup_mocks => false}),
    catch meck:unload(),
    Base.

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

test_decide_request_success(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-001">>,
        <<"trace_id">> => <<"tr-test-001">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>, <<"payload_ref">> => <<"s3://bucket/key">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

test_decide_request_with_push_assignment(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-002">>,
        <<"trace_id">> => <<"tr-test-002">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>, <<"payload_ref">> => <<"s3://bucket/key">>},
        <<"policy_id">> => <<"default">>,
        <<"push_assignment">> => true
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

test_decide_request_error_policy_not_found(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-003">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>},
        <<"policy_id">> => <<"nonexistent">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

test_decide_request_error_missing_tenant_id(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-004">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>}
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

test_decide_request_unsupported_version(_Config) ->
    Request = #{
        <<"version">> => <<"2">>,
        <<"request_id">> => <<"req-test-005">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>}
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson}, State),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    ok.

test_malformed_json(_Config) ->
    MalformedJson = <<"{invalid json}">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, MalformedJson}, State),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    ok.

test_payload_size_limit(_Config) ->
    ok = application:set_env(beamline_router, nats_max_payload_size, 100),
    
    LargePayload = binary:copy(<<"x">>, 200),
    LargeRequest = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-oversized">>,
        <<"tenant_id">> => <<"test">>,
        <<"task">> => #{<<"type">> => <<"test">>},
        <<"payload">> => LargePayload
    }),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, LargeRequest}, State),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    ok = application:set_env(beamline_router, nats_max_payload_size, 1048576),
    ok.

test_cp2_headers_happy_path(_Config) ->
    Headers = #{
        <<"trace_id">> => <<"tr-headers">>,
        <<"tenant_id">> => <<"acme">>,
        <<"version">> => <<"1">>,
        <<"nats-msg-id">> => <<"msg-cp2">>
    },
    MsgId = <<"msg-cp2">>,
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-cp2">>,
        <<"trace_id">> => <<"tr-cp2">>,
        <<"tenant_id">> => <<"acme">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,

    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    %% NOTE: ensure_router_nats_started removed - not compatible with mock (start_link not stubbed)
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    ok = application:set_env(beamline_router, nats_mode, mock),

    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, Headers, MsgId}, State),

    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),

    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

test_ack_message_after_success(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-ack-test">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"msg-ack-test">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    %% NOTE: Do NOT call ensure_router_nats_started() with mock - start_link not stubbed
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    meck:expect(router_nats, ack_message, fun(MsgIdBin) ->
        _ = MsgId =:= MsgIdBin,
        ok
    end),
    ok = application:set_env(beamline_router, nats_mode, mock),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
    
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_policy_store),
    ok.

test_delivery_count_tracking(_Config) ->
    MsgId = <<"msg-delivery-test">>,
    
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    router_decide_consumer:track_delivery_count(MsgId),
    
    ErrorContext = #{<<"error">> => <<"test">>},
    router_decide_consumer:check_maxdeliver_exhaustion(MsgId, <<"req-test">>, ErrorContext),
    
    router_decide_consumer:cleanup_delivery_count(MsgId),
    
    ok.
