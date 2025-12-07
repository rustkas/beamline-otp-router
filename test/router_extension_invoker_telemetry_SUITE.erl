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

%% Test suite configuration
suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [
        test_telemetry_success_event,
        test_telemetry_error_event,
        test_telemetry_timeout_event,
        test_telemetry_with_retries,
        test_telemetry_unified_fields,
        test_logging_success,
        test_logging_error,
        test_logging_with_correlation_fields
    ].

init_per_suite(Config) ->
    %% Start router application
    application:ensure_all_started(beamline_router),
    
    %% Attach test telemetry handler
    telemetry:attach_many(
        <<"test-handler">>,
        [
            [router_extension_invoker, invocation_total]
        ],
        fun test_telemetry_handler/4,
        self()
    ),
    
    Config.

end_per_suite(_Config) ->
    %% Detach test handler
    telemetry:detach(<<"test-handler">>),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear telemetry events
    clear_telemetry_events(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    clear_telemetry_events(),
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
    
    %% Mock NATS request (success)
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, request, fun(_, _, _) ->
        {ok, jsx:encode(#{<<"result">> => <<"ok">>})}
    end),
    
    meck:new(router_extension_registry, [passthrough]),
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
    
    ?assertMatch({ok, _}, Result),
    
    %% Wait for telemetry event
    timer:sleep(100),
    
    %% Verify telemetry event was emitted
    Events = get_telemetry_events(),
    ?assert(length(Events) > 0, "Telemetry event should be emitted"),
    
    %% Verify event structure
    [Event | _] = Events,
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
    
    meck:unload([router_nats, router_extension_registry]),
    ok.

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
    
    meck:new(router_nats, [passthrough]),
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
    
    meck:new(router_nats, [passthrough]),
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
    
    meck:new(router_nats, [passthrough]),
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
    
    meck:new(router_nats, [passthrough]),
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
    
    [Event | _] = Events,
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
    
    meck:new(router_nats, [passthrough]),
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
    
    meck:new(router_nats, [passthrough]),
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
    
    meck:new(router_nats, [passthrough]),
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

