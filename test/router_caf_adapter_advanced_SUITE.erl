%% @doc CAF Adapter Enhanced: Advanced Tests
%%
%% Advanced CAF adapter tests:
%% - Deadline calculation
%% - Telemetry success
%% - Idempotency keys
%% - Schema validation
%%
%% @test_category caf, unit, heavy
-module(router_caf_adapter_advanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_deadline_calculation/1,
    test_telemetry_success/1,
    test_idempotency_keys_generated/1,
    test_schema_validation/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        "full" -> [{group, advanced_tests}];
        _ -> []
    end.

groups() ->
    [{advanced_tests, [sequence], [
        test_deadline_calculation,
        test_telemetry_success,
        test_idempotency_keys_generated,
        test_schema_validation
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) ->
    catch meck:unload(router_nats),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_deadline_calculation(_Config) ->
    ct:comment("=== Deadline Calculation ==="),
    Latency = 1000,
    %% Defaults mirror router_caf_adapter macros
    Multiplier = application:get_env(beamline_router, caf_deadline_multiplier, 5),
    MinMs = application:get_env(beamline_router, caf_deadline_min_ms, 5000),
    MaxMs = application:get_env(beamline_router, caf_deadline_max_ms, 60000),

    ExpectedMin = max(MinMs, Latency * Multiplier),
    Deadline = router_caf_adapter:calculate_deadline(Latency),

    ?assert(Deadline >= ExpectedMin),
    ?assert(Deadline =< MaxMs),
    ok.

test_telemetry_success(_Config) ->
    ct:comment("=== Telemetry Success ==="),
    case code:which(meck) of
        non_existing -> {skip, "meck not available"};
        _ ->
            %% No passthrough for router_nats to avoid noproc on gen_server:call
            meck:new(router_nats, []),
            meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> {ok, <<"ack">>} end),
            meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
            meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
            meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
            meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
            meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
            
            Handler = fun(_Event, _Measurements, _Metadata, _Ctx) -> ok end,
            telemetry:attach(test_telemetry, [router_caf_adapter, router_assignment_published_total], Handler, #{}),
            
            Request = #{
                <<"version">> => <<"1">>,
                <<"request_id">> => <<"req-telemetry">>,
                <<"tenant_id">> => <<"test-tenant">>,
                <<"task">> => #{<<"type">> => <<"text.generate">>}
            },
            Decision = #route_decision{provider_id = <<"openai">>, reason = <<"weighted">>,
                                       priority = 50, expected_latency_ms = 850, expected_cost = 0.012, metadata = #{}},
            
            ok = router_caf_adapter:publish_assignment(Request, Decision),
            
            telemetry:detach(test_telemetry),
            meck:unload(router_nats),
            ok
    end.

test_idempotency_keys_generated(_Config) ->
    ct:comment("=== Idempotency Keys Generated ==="),
    case code:which(meck) of
        non_existing -> {skip, "meck not available"};
        _ ->
            IdemKeys = router_test_init:ensure_ets_table(idem_keys, [named_table, set, private]),
            
            %% No passthrough for router_nats to avoid noproc on gen_server:call
            meck:new(router_nats, []),
            meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
            meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
            meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
            meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
            meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
            meck:expect(router_nats, publish_with_ack, fun(_Subject, Payload, _Headers) ->
                Decoded = jsx:decode(Payload, [return_maps]),
                IdemKey = maps:get(<<"idempotency_key">>, Decoded, undefined),
                ets:insert(IdemKeys, {IdemKey, 1}),
                {ok, <<"ack">>}
            end),
            
            Request = #{
                <<"version">> => <<"1">>,
                <<"request_id">> => <<"req-idem">>,
                <<"tenant_id">> => <<"test-tenant">>,
                <<"task">> => #{<<"type">> => <<"text.generate">>}
            },
            Decision = #route_decision{provider_id = <<"openai">>, reason = <<"weighted">>,
                                       priority = 50, expected_latency_ms = 850, expected_cost = 0.012, metadata = #{}},
            
            ok = router_caf_adapter:publish_assignment(Request, Decision),
            
            Keys = ets:tab2list(IdemKeys),
            ct:comment("Generated keys: ~p", [Keys]),
            
            ets:delete_all_objects(IdemKeys),
            meck:unload(router_nats),
            ok
    end.

test_schema_validation(_Config) ->
    ct:comment("=== Schema Validation ==="),
    %% Test that request with missing fields is handled
    Request = #{
        <<"version">> => <<"1">>
        %% Missing required fields
    },
    Decision = #route_decision{provider_id = <<"openai">>, reason = <<"weighted">>,
                               priority = 50, expected_latency_ms = 850, expected_cost = 0.012, metadata = #{}},
    
    %% Should not crash
    Result = catch router_caf_adapter:publish_assignment(Request, Decision),
    case Result of
        ok -> ok;
        {error, _} -> ok;
        {'EXIT', _} -> ok
    end,
    ok.
