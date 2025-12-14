%% @doc Smoke tests for Extension Invoker telemetry and logging
%% Verifies that telemetry events and structured logs are emitted correctly
-module(router_extension_invoker_telemetry_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    end_per_suite/1,
    end_per_testcase/2,
    init_per_suite/1,
    init_per_testcase/2,
    suite/0,
    test_telemetry_success_event/1,
    test_telemetry_error_event/1,
    test_telemetry_timeout_event/1,
    test_telemetry_with_retries/1,
    test_telemetry_unified_fields/1,
    test_logging_success/1,
    test_logging_error/1,
    test_logging_with_correlation_fields/1,
    test_telemetry_handler/4,
    get_telemetry_events/0,
    clear_telemetry_events/0
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_logging_error/1,
    test_logging_success/1,
    test_logging_with_correlation_fields/1,
    test_telemetry_error_event/1,
    test_telemetry_success_event/1,
    test_telemetry_timeout_event/1,
    test_telemetry_unified_fields/1,
    test_telemetry_with_retries/1,
    test_telemetry_handler/4
]).


-export([groups_for_level/1]).

%% Test suite configuration
suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end.

meta_all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> groups_for_level(heavy);
        _ -> []
    end.

%% @doc Telemetry smoke tests use mocks and are fast
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [{group, telemetry_tests}];
groups_for_level(full) -> [{group, telemetry_tests}];
groups_for_level(heavy) -> [{group, telemetry_tests}].

groups() ->
    [
        %% MUST be sequence: tests use shared mocks (router_nats, router_extension_registry)
        %% parallel + shared mocks = already_started errors on meck:new
        {telemetry_tests, [sequence], [
            test_telemetry_success_event,
            test_telemetry_error_event,
            test_telemetry_timeout_event,
            test_telemetry_with_retries,
            test_telemetry_unified_fields,
            test_logging_success,
            test_logging_error,
            test_logging_with_correlation_fields
        ]}
    ].

init_per_suite(Config) ->
    %% Start router application
    application:ensure_all_started(beamline_router),
    Config.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Attach telemetry handler for THIS test case process
    HandlerId = list_to_binary("test-handler-" ++ pid_to_list(self())),
    telemetry:attach_many(
        HandlerId,
        [
            [router_extension_invoker, invocation_total]
        ],
        fun ?MODULE:test_telemetry_handler/4,
        self()
    ),
    
    %% Clear telemetry events
    clear_telemetry_events(),
    
    [{telemetry_handler_id, HandlerId} | Config].

end_per_testcase(_TestCase, Config) ->
    HandlerId = ?config(telemetry_handler_id, Config),
    telemetry:detach(HandlerId),
    clear_telemetry_events(),
    catch meck:unload([router_nats, router_extension_registry]),
    ok.

%% Test: Telemetry event emitted on successful invocation
test_telemetry_success_event(_Config) ->
    %% Mock extension registry lookup
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.test.v1">>,
        timeout_ms = 1000,
        retry = 0,
        enabled = true
    },
    
    %% Mock NATS request (success) - no passthrough to avoid noproc
    catch meck:unload(router_nats),
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {ok, jsx:encode(#{<<"result">> => <<"ok">>})}
    end),
    
    catch meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    %% Invoke extension
    Request = #{<<"payload">> => #{}},
    Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"policy_id">> => <<"policy_456">>
    },
    
    Result = router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    case Result of
        {ok, _} -> ok;
        Error -> ct:pal("INVOKE FAILED: ~p", [Error])
    end,
    ?assertMatch({ok, _}, Result),
    
    %% Wait for telemetry event
    timer:sleep(100),
    
    %% Verify telemetry event was emitted
    Events = get_telemetry_events(),
    ?assert(length(Events) > 0, "Telemetry event should be emitted"),
    
    %% Verify event structure (find the unified event with tenant_id)
    [Event | _] = filter_events_with_tenant(Events),
    ?assertMatch([router_extension_invoker, invocation_total], maps:get(event, Event)),
    
    Metadata = maps:get(metadata, Event),
    ?assertEqual(<<"test_extension">>, maps:get(extension_id, Metadata)),
    ?assertEqual(<<"pre">>, maps:get(type, Metadata)),
    ?assertEqual(<<"beamline.ext.pre.test.v1">>, maps:get(subject, Metadata)),
    ?assertEqual(success, maps:get(status, Metadata)),
    ?assert(is_number(maps:get(latency_ms, Metadata))),
    ?assertEqual(0, maps:get(retries_used, Metadata)),
    ?assertEqual(<<"tenant_123">>, maps:get(tenant_id, Metadata)),
    ?assertEqual(<<"policy_456">>, maps:get(policy_id, Metadata)),
    
    ok.

%% Helper: Filter events to find those with tenant_id (unified events)
filter_events_with_tenant(Events) ->
    lists:filter(fun(Event) ->
        Metadata = maps:get(metadata, Event),
        maps:is_key(tenant_id, Metadata)
    end, Events).

%% Test: Telemetry event emitted on error
test_telemetry_error_event(_Config) ->
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"validator">>,
        subject = <<"beamline.ext.validate.test.v1">>,
        timeout_ms = 1000,
        retry = 0,
        enabled = true
    },
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {error, connection_failed}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    Request = #{<<"payload">> => #{}},
    Context = #{},
    
    Result = router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    ?assertMatch({error, _}, Result),
    
    timer:sleep(100),
    
    Events = get_telemetry_events(),
    ?assert(length(Events) > 0, "Telemetry event should be emitted"),
    
    [Event | _] = Events,
    Metadata = maps:get(metadata, Event),
    ?assertEqual(error, maps:get(status, Metadata)),
    
    meck:unload([router_nats, router_extension_registry]),
    ok.

%% Test: Telemetry event emitted on timeout
test_telemetry_timeout_event(_Config) ->
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"post">>,
        subject = <<"beamline.ext.post.test.v1">>,
        timeout_ms = 100,
        retry = 0,
        enabled = true
    },
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {error, timeout}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    Request = #{<<"payload">> => #{}},
    Context = #{},
    
    Result = router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    ?assertMatch({error, _}, Result),
    
    timer:sleep(200),
    
    Events = get_telemetry_events(),
    ?assert(length(Events) > 0, "Telemetry event should be emitted"),
    
    [Event | _] = Events,
    Metadata = maps:get(metadata, Event),
    ?assertEqual(timeout, maps:get(status, Metadata)),
    
    meck:unload([router_nats, router_extension_registry]),
    ok.

%% Test: Telemetry event includes retries_used
test_telemetry_with_retries(_Config) ->
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.test.v1">>,
        timeout_ms = 100,
        retry = 2,
        enabled = true
    },
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {error, timeout}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    Request = #{<<"payload">> => #{}},
    Context = #{},
    
    Result = router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    ?assertMatch({error, _}, Result),
    
    timer:sleep(500),
    
    Events = get_telemetry_events(),
    ?assert(length(Events) > 0, "Telemetry events should be emitted"),
    
    %% Check that retries_used is present in events
    lists:foreach(fun(Event) ->
        Metadata = maps:get(metadata, Event),
        ?assert(is_integer(maps:get(retries_used, Metadata))),
        ?assert(maps:get(retries_used, Metadata) >= 0)
    end, Events),
    
    meck:unload([router_nats, router_extension_registry]),
    ok.

%% Test: Unified telemetry fields are present
test_telemetry_unified_fields(_Config) ->
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"validator">>,
        subject = <<"beamline.ext.validate.test.v1">>,
        timeout_ms = 1000,
        retry = 0,
        enabled = true
    },
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {ok, jsx:encode(#{<<"result">> => <<"ok">>})}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    Request = #{<<"payload">> => #{}},
    Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"policy_id">> => <<"policy_456">>
    },
    
    router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    timer:sleep(100),
    
    Events = get_telemetry_events(),
    ?assert(length(Events) > 0, "Telemetry event should be emitted"),
    
    [Event | _] = filter_events_with_tenant(Events),
    Metadata = maps:get(metadata, Event),
    
    %% Verify all required fields are present
    RequiredFields = [extension_id, type, subject, status, latency_ms, retries_used],
    lists:foreach(fun(Field) ->
        ?assert(maps:is_key(Field, Metadata), io_lib:format("Field ~p should be present", [Field]))
    end, RequiredFields),
    
    %% Verify optional correlation fields
    ?assert(maps:is_key(tenant_id, Metadata), "tenant_id should be present"),
    ?assert(maps:is_key(policy_id, Metadata), "policy_id should be present"),
    
    meck:unload([router_nats, router_extension_registry]),
    ok.

%% Test: Structured log emitted on success
test_logging_success(_Config) ->
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"pre">>,
        subject = <<"beamline.ext.pre.test.v1">>,
        timeout_ms = 1000,
        retry = 0,
        enabled = true
    },
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {ok, jsx:encode(#{<<"result">> => <<"ok">>})}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    %% Capture logs (simplified - in real test would use log capture mechanism)
    Request = #{<<"payload">> => #{}},
    Context = #{},
    
    Result = router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    ?assertMatch({ok, _}, Result),
    
    %% Log should be emitted (verification would require log capture)
    %% For smoke test, we just verify no crash
    meck:unload([router_nats, router_extension_registry]),
    ok.

%% Test: Structured log emitted on error
test_logging_error(_Config) ->
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"validator">>,
        subject = <<"beamline.ext.validate.test.v1">>,
        timeout_ms = 1000,
        retry = 0,
        enabled = true
    },
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {error, connection_failed}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    Request = #{<<"payload">> => #{}},
    Context = #{},
    
    Result = router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    ?assertMatch({error, _}, Result),
    
    %% Log should be emitted (verification would require log capture)
    meck:unload([router_nats, router_extension_registry]),
    ok.

%% Test: Logging includes correlation fields
test_logging_with_correlation_fields(_Config) ->
    Extension = #extension{
        id = <<"test_extension">>,
        type = <<"post">>,
        subject = <<"beamline.ext.post.test.v1">>,
        timeout_ms = 1000,
        retry = 0,
        enabled = true
    },
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {ok, jsx:encode(#{<<"result">> => <<"ok">>})}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
    meck:expect(router_extension_registry, lookup, fun(_) ->
        {ok, Extension}
    end),
    
    Request = #{
        <<"payload">> => #{},
        <<"tenant_id">> => <<"tenant_123">>,
        <<"policy_id">> => <<"policy_456">>
    },
    Context = #{
        <<"tenant_id">> => <<"tenant_123">>,
        <<"policy_id">> => <<"policy_456">>
    },
    
    Result = router_extension_invoker:invoke(<<"test_extension">>, Request, Context),
    
    ?assertMatch({ok, _}, Result),
    
    %% Log should include tenant_id and policy_id (verification would require log capture)
    meck:unload([router_nats, router_extension_registry]),
    ok.

%% Internal: Test telemetry handler
test_telemetry_handler(Event, Measurements, Metadata, Pid) ->
    Pid ! {telemetry_event, #{
        event => Event,
        measurements => Measurements,
        metadata => Metadata
    }},
    ok.

%% Internal: Get telemetry events
get_telemetry_events() ->
    receive
        {telemetry_event, Event} ->
            [Event | get_telemetry_events()]
    after
        0 ->
            []
    end.

%% Internal: Clear telemetry events
clear_telemetry_events() ->
    receive
        {telemetry_event, _} ->
            clear_telemetry_events()
    after
        0 ->
            ok
    end.
