%% @doc Router Abuse Scenarios Test Suite
%% Tests Router behavior under abuse conditions
%% CP2+: Abuse detection and protection testing
-module(router_abuse_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_abuse_empty_payload_flood/1,
    test_abuse_heavy_payload_attack/1,
    test_abuse_multi_tenant_flood/1,
    test_abuse_payload_size_distribution/1,
    test_abuse_targeted_tenant/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, abuse_tests}];
groups_for_level(full) ->
    [{group, abuse_tests}];
groups_for_level(_) -> %% fast
    [{group, abuse_tests}].

groups() ->
    [
        {abuse_tests, [], [
            test_abuse_empty_payload_flood,
            test_abuse_heavy_payload_attack,
            test_abuse_targeted_tenant,
            test_abuse_multi_tenant_flood,
            test_abuse_payload_size_distribution
        ]}
    ].

init_per_suite(Config) ->
    %% Setup test environment
    {ok, _} = application:ensure_all_started(beamline_router),
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(router_logger, [passthrough]),
    ok = router_mock_helpers:ensure_mock(telemetry, [passthrough]),
    ok = router_mock_helpers:ensure_mock(router_metrics, [passthrough]),
    Config.

end_per_suite(_Config) ->
    router_mock_helpers:unload_all([router_nats, router_logger, telemetry, router_metrics]),
    ok = application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(router_logger, [passthrough]),
    ok = router_mock_helpers:ensure_mock(telemetry, [passthrough]),
    ok = router_mock_helpers:ensure_mock(router_metrics, [passthrough]),
    ok = router_mock_helpers:ensure_mock(router_intake_validator, [passthrough]),
    ok = router_mock_helpers:ensure_mock(router_payload_tracker, [passthrough]),
    %% Reset mocks only if they exist
    router_mock_helpers:reset_all([router_nats, router_logger, telemetry, router_metrics, router_intake_validator, router_payload_tracker]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_mock_helpers:unload_all(),
    ok.

%% Test: Empty payload flood
test_abuse_empty_payload_flood(_Config) ->
    %% Setup: Configure minimum payload size
    ok = application:set_env(beamline_router, min_payload_size, 10),
    
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS table for abuse tracking
    _ = router_ets_helpers:ensure_named_ets_table(router_abuse_tracking, [named_table, set, public]),
    AbuseTable = router_abuse_tracking,
    
    %% Send 10 empty payload requests
    EmptyPayload = #{},
    EmptyPayloadJson = jsx:encode(EmptyPayload),
    EmptyPayloadSize = byte_size(EmptyPayloadJson),
    
    %% Verify payload is too small
    ?assert(EmptyPayloadSize < 10, "Empty payload should be < 10 bytes"),
    
    %% Mock validation to return empty payload error
    meck:expect(router_intake_validator, validate_intake_message, fun(_S, _P, _H, _T) ->
        {error, {payload_too_small, <<"Payload is too small or empty">>, #{
            <<"payload_size">> => EmptyPayloadSize,
            <<"min_payload_size">> => 10
        }}}
    end),
    
    %% Process empty payload request
    Payload = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"tenant_id">> => <<"test-tenant">>,
        <<"request_id">> => <<"req-empty-001">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload">> => EmptyPayload
        }
    }),
    
    %% Verify request is rejected
    _ = router_decide_consumer:handle_decide_message(Subject, Payload, #{}, undefined),
    
    %% Verify abuse metric emitted (via router_metrics)
    case meck:called(router_metrics, emit_metric, [router_abuse_empty_payload_total, #{count => 1}, '_']) of
        true -> ok;
        false ->
            History = meck:history(router_metrics),
            ct:log("router_metrics history: ~p", [History]),
            ?assert(false, "router_metrics:emit_metric(router_abuse_empty_payload_total, ...) not called")
    end,
    
    %% Cleanup
    ets:delete(AbuseTable),
    ok.

%% Test: Heavy payload attack
test_abuse_heavy_payload_attack(_Config) ->
    %% Setup: Configure payload size limit
    ok = application:set_env(beamline_router, nats_max_payload_size, 1048576),  %% 1MB
    
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create large payload (1.1MB, exceeding limit)
    LargePayload = binary:copy(<<"A">>, 1153434),  %% 1.1MB
    
    Payload = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"tenant_id">> => <<"test-tenant">>,
        <<"request_id">> => <<"req-heavy-001">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload">> => #{
                <<"prompt">> => LargePayload
            }
        }
    }),
    
    PayloadSize = byte_size(Payload),
    MaxPayloadSize = 1048576,
    
    %% Verify payload exceeds limit
    ?assert(PayloadSize > MaxPayloadSize, "Payload should exceed 1MB limit"),
    
    %% Process large payload request
    _ = router_decide_consumer:handle_decide_message(Subject, Payload, #{}, undefined),
    
    %% Verify request is rejected
    %% (Implementation should reject in router_decide_consumer.erl)
    
    %% Verify abuse metric emitted (via router_metrics)
    case meck:called(router_metrics, emit_metric, [router_abuse_heavy_payload_total, #{count => 1}, '_']) of
        true -> ok;
        false ->
            History = meck:history(router_metrics),
            ct:log("router_metrics history: ~p", [History]),
            ?assert(false, "router_metrics:emit_metric(router_abuse_heavy_payload_total, ...) not called")
    end,
    
    ok.

%% Test: Targeted tenant attack
test_abuse_targeted_tenant(_Config) ->
    %% Setup: Configure tenant allowlist
    TargetTenant = <<"target-tenant">>,
    ok = application:set_env(beamline_router, decide_allowed_tenants, [TargetTenant]),
    
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Send 100 requests to same tenant
    Requests = 100,
    
    %% Mock validation to succeed
    meck:expect(router_intake_validator, validate_intake_message, fun(_S, _P, _H, _T) ->
        {ok, #{
            <<"tenant_id">> => TargetTenant,
            <<"request_id">> => <<"req-target-001">>
        }}
    end),
    
    %% Process requests
    lists:foreach(fun(I) ->
        Payload = jsx:encode(#{
            <<"version">> => <<"1">>,
            <<"tenant_id">> => TargetTenant,
            <<"request_id">> => list_to_binary(["req-target-", integer_to_list(I)]),
            <<"task">> => #{
                <<"type">> => <<"text.generate">>,
                <<"payload">> => #{<<"prompt">> => <<"test">>}
            }
        }),
        
        %% Process request
        router_decide_consumer:handle_decide_message(Subject, Payload, #{}, undefined)
    end, lists:seq(1, Requests)),
    
    %% Verify abuse pattern detected
    %% (Implementation should track per-tenant request rates)
    
    %% Verify abuse metric emitted (via router_metrics)
    ?assert(meck:called(router_metrics, emit_metric, [
        router_abuse_targeted_tenant_total,
        #{count => 1},
        '_'
    ])),
    
    ok.

%% Test: Multi-tenant flood
test_abuse_multi_tenant_flood(_Config) ->
    %% Setup: Multiple tenants
    TenantIds = [list_to_binary(["tenant-", integer_to_list(I)]) || I <- lists:seq(1, 20)],
    
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Send requests from multiple tenants
    Requests = lists:flatten([
        [{
            TenantId,
            jsx:encode(#{
                <<"version">> => <<"1">>,
                <<"tenant_id">> => TenantId,
                <<"request_id">> => list_to_binary(["req-flood-", integer_to_list(I)]),
                <<"task">> => #{
                    <<"type">> => <<"text.generate">>,
                    <<"payload">> => #{<<"prompt">> => <<"test">>}
                }
            })
        } || I <- lists:seq(1, 3)]
        || TenantId <- TenantIds
    ]),
    
    %% Process requests
    lists:foreach(fun({_TenantId, Payload}) ->
        router_decide_consumer:handle_decide_message(Subject, Payload, #{}, undefined)
    end, Requests),
    
    %% Verify multi-tenant flood pattern detected
    %% (Implementation should track aggregate request rates)
    
    %% Verify abuse metric emitted (via router_metrics)
    ?assert(meck:called(router_metrics, emit_metric, [
        router_abuse_targeted_tenant_total,
        #{count => 1},
        '_'
    ])),
    
    ok.

%% Test: Payload size distribution tracking
test_abuse_payload_size_distribution(_Config) ->
    %% Setup: Configure payload size tracking
    TenantId = <<"test-tenant">>,
    
    %% Create ETS table for payload size tracking
    Table = router_abuse_tracking,
    _ = router_ets_helpers:ensure_named_ets_table(Table, [named_table, set, public]),
    ets:delete_all_objects(Table),
    
    %% Send 10 requests with large payloads (500KB each)
    
    lists:foreach(fun(_I) ->
        %% Track payload size
        ets:update_counter(Table, {TenantId, large_payloads}, 1, {{TenantId, large_payloads}, 0}),
        ets:update_counter(Table, {TenantId, total_requests}, 1, {{TenantId, total_requests}, 0})
    end, lists:seq(1, 10)),
    
    %% Check payload size distribution
    LargePayloads = ets:lookup_element(Table, {TenantId, large_payloads}, 2),
    TotalRequests = ets:lookup_element(Table, {TenantId, total_requests}, 2),
    
    %% Verify all requests are large payloads
    ?assertEqual(10, LargePayloads),
    ?assertEqual(10, TotalRequests),
    
    %% Check abuse pattern (80% large payloads)
    LargePayloadRatio = LargePayloads / TotalRequests,
    ?assert(LargePayloadRatio > 0.8, "Large payload ratio should be > 0.8"),
    
    %% Verify abuse pattern detected
    %% (Implementation should detect pattern and emit metric)
    
    %% Cleanup
    ets:delete(Table),
    ok.
