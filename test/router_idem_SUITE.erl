-module(router_idem_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2,
    %% Test functions called via groups
    test_basic_dup_check/1,
    test_remember_and_evict/1,
    test_ttl_expiration/1,
    test_ttl_edge_case_zero/1,
    test_ttl_edge_case_negative/1,
    test_ttl_edge_case_very_large/1,
    test_evict_expired_mass_cleanup/1,
    test_evict_all_emergency/1,
    test_cleanup_protects_ets_growth/1,
    test_concurrent_operations/1,
    test_metrics_emission/1,
    test_telemetry_events/1,
    test_check_and_mark_atomic/1,
    test_zero_double_execution/1,
    test_p95_duplicate_handling_latency/1,
    test_keys_propagation/1,
    test_survives_restart/1
]}).


all() ->
  [
    {group, unit_tests}
  ].

groups() ->
  [
        {unit_tests, [sequence], [
      test_basic_dup_check,
      test_remember_and_evict,
      test_ttl_expiration,
      test_ttl_edge_case_zero,
      test_ttl_edge_case_negative,
      test_ttl_edge_case_very_large,
      test_evict_expired_mass_cleanup,
      test_evict_all_emergency,
      test_cleanup_protects_ets_growth,
      test_concurrent_operations,
      test_metrics_emission,
      test_telemetry_events,
      test_check_and_mark_atomic,
      test_zero_double_execution,
      test_p95_duplicate_handling_latency,
      test_keys_propagation,
  test_survives_restart
    ]}
  ].

init_per_suite(Config) ->
  _ = application:load(beamline_router),
  ok = application:set_env(beamline_router, idempotency_ttl_seconds, 3600),
  ok = application:set_env(beamline_router, idempotency_max_size, 1000000),
  Config.

end_per_suite(Config) ->
  %% Clean up ETS table if it exists (using API where possible)
  case ets:info(router_idem) of
    undefined -> ok;
    _ -> 
      %% Use evict_all to clear table, then delete table
      catch router_idem:evict_all(),
      ets:delete(router_idem)
  end,
  Config.

init_per_testcase(_TestCase, Config) ->
  %% Reset ETS table (using API instead of direct ETS access)
  %% Pattern: Use reset/0 for consistency with reset/lifecycle pattern
  case ets:info(router_idem) of
    undefined -> 
      router_idem:init([]);
    _ -> 
      %% Use reset API (pattern: reset/lifecycle) instead of direct ets:delete_all_objects
      router_idem:reset()
  end,
  %% Mock router_metrics and telemetry
  meck:new(router_metrics, [passthrough]),
  meck:new(telemetry, [passthrough]),
  meck:new(router_logger, [passthrough]),
  Config.

end_per_testcase(_TestCase, Config) ->
  meck:unload(router_metrics),
  meck:unload(telemetry),
  meck:unload(router_logger),
  Config.

%% Test: Basic duplicate check
test_basic_dup_check(_Config) ->
  ok = router_idem:init([]),
  Key = <<"test-key-1">>,
  
  %% Key doesn't exist - not a duplicate
  ?assertNot(router_idem:is_dup(Key)),
  
  %% Remember key
  ok = router_idem:remember(Key, 1000),
  
  %% Key exists - duplicate detected
  ?assert(router_idem:is_dup(Key)),
  
  %% Evict key
  ok = router_idem:evict(Key),
  
  %% Key doesn't exist anymore
  ?assertNot(router_idem:is_dup(Key)),
  ok.

%% Test: Remember and evict
test_remember_and_evict(_Config) ->
  ok = router_idem:init([]),
  Key1 = <<"key-1">>,
  Key2 = <<"key-2">>,
  
  ok = router_idem:remember(Key1, 1000),
  ok = router_idem:remember(Key2, 2000),
  
  ?assert(router_idem:is_dup(Key1)),
  ?assert(router_idem:is_dup(Key2)),
  
  ok = router_idem:evict(Key1),
  ?assertNot(router_idem:is_dup(Key1)),
  ?assert(router_idem:is_dup(Key2)),
  ok.

%% Test: TTL expiration
test_ttl_expiration(_Config) ->
  ok = router_idem:init([]),
  Key = <<"ttl-key">>,
  
  %% Remember with short TTL (10ms)
  ok = router_idem:remember(Key, 10),
  ?assert(router_idem:is_dup(Key)),
  
  %% Wait for expiration
  timer:sleep(20),
  
  %% Key should be expired and treated as new
  ?assertNot(router_idem:is_dup(Key)),
  ok.

%% Test: TTL edge case - zero TTL
test_ttl_edge_case_zero(_Config) ->
  ok = router_idem:init([]),
  Key = <<"zero-ttl-key">>,
  
  %% Zero TTL should be rejected
  {error, invalid_ttl} = router_idem:remember(Key, 0),
  ?assertNot(router_idem:is_dup(Key)),
  ok.

%% Test: TTL edge case - negative TTL
test_ttl_edge_case_negative(_Config) ->
  ok = router_idem:init([]),
  Key = <<"negative-ttl-key">>,
  
  %% Negative TTL should be rejected
  {error, invalid_ttl} = router_idem:remember(Key, -100),
  ?assertNot(router_idem:is_dup(Key)),
  ok.

%% Test: TTL edge case - very large TTL
test_ttl_edge_case_very_large(_Config) ->
  ok = router_idem:init([]),
  Key = <<"large-ttl-key">>,
  
  %% Very large TTL (1 year in milliseconds)
  LargeTTL = 365 * 24 * 60 * 60 * 1000,
  ok = router_idem:remember(Key, LargeTTL),
  ?assert(router_idem:is_dup(Key)),
  ok.

%% Test: Mass cleanup of expired entries
test_evict_expired_mass_cleanup(_Config) ->
  ok = router_idem:init([]),
  
  %% Create mix of expired and valid keys
  Now = erlang:system_time(millisecond),
  ExpiredKeys = [<<"expired-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 10)],
  ValidKeys = [<<"valid-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 5)],
  
  %% Insert expired keys (expired 100ms ago)
  lists:foreach(fun(Key) ->
    ets:insert(router_idem, {Key, Now - 100})
  end, ExpiredKeys),
  
  %% Insert valid keys (expire in 1000ms)
  lists:foreach(fun(Key) ->
    ets:insert(router_idem, {Key, Now + 1000})
  end, ValidKeys),
  
  %% Verify all keys exist
  15 = ets:info(router_idem, size),
  
  %% Evict expired entries
  EvictedCount = router_idem:evict_expired(),
  10 = EvictedCount,
  
  %% Verify only valid keys remain
  5 = ets:info(router_idem, size),
  
  %% Verify expired keys are gone
  lists:foreach(fun(Key) ->
    ?assertNot(router_idem:is_dup(Key))
  end, ExpiredKeys),
  
  %% Verify valid keys still exist
  lists:foreach(fun(Key) ->
    ?assert(router_idem:is_dup(Key))
  end, ValidKeys),
  ok.

%% Test: Emergency cleanup (evict all)
test_evict_all_emergency(_Config) ->
  ok = router_idem:init([]),
  
  %% Insert multiple keys
  Keys = [<<"key-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 20)],
  lists:foreach(fun(Key) ->
    ok = router_idem:remember(Key, 1000)
  end, Keys),
  
  %% Verify keys exist
  20 = ets:info(router_idem, size),
  
  %% Evict all
  EvictedCount = router_idem:evict_all(),
  20 = EvictedCount,
  
  %% Verify table is empty
  0 = ets:info(router_idem, size),
  
  %% Verify all keys are gone
  lists:foreach(fun(Key) ->
    ?assertNot(router_idem:is_dup(Key))
  end, Keys),
  ok.

%% Test: Cleanup protects ETS from unbounded growth
test_cleanup_protects_ets_growth(_Config) ->
  ok = router_idem:init([]),
  
  %% Create many expired entries
  Now = erlang:system_time(millisecond),
  ExpiredKeys = [<<"expired-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 100)],
  lists:foreach(fun(Key) ->
    ets:insert(router_idem, {Key, Now - 1000})
  end, ExpiredKeys),
  
  %% Verify table size
  100 = ets:info(router_idem, size),
  
  %% Run cleanup
  EvictedCount = router_idem:cleanup(),
  100 = EvictedCount,
  
  %% Verify table is cleaned
  0 = ets:info(router_idem, size),
  ok.

%% Test: Concurrent operations
test_concurrent_operations(_Config) ->
  ok = router_idem:init([]),
  
  %% Spawn multiple processes doing concurrent operations
  Keys = [<<"concurrent-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 50)],
  
  Pids = lists:map(fun(Key) ->
    spawn(fun() ->
      ok = router_idem:remember(Key, 1000),
      timer:sleep(10),
      _ = router_idem:is_dup(Key)
    end)
  end, Keys),
  
  %% Wait for all processes
  lists:foreach(fun(Pid) ->
    receive
      {'EXIT', Pid, _Reason} -> ok
    after 1000 -> ok
    end
  end, Pids),
  
  %% Verify all keys are remembered
  50 = ets:info(router_idem, size),
  ok.

%% Test: Metrics emission
test_metrics_emission(_Config) ->
  Metrics = router_idem:metrics(),
  ExpectedMetrics = [router_idem_hits_total, router_idem_miss_total],
  ExpectedMetrics = Metrics,
  
  ok = router_idem:init([]),
  Key = <<"metrics-key">>,
  
  meck:expect(router_metrics, inc, fun(Metric) ->
    case Metric of
      router_idem_hits_total -> ok;
      router_idem_miss_total -> ok;
      _ -> ok
    end
  end),
  
  ?assertNot(router_idem:is_dup(Key)),
  ok = router_idem:remember(Key, 1000),
  ?assert(router_idem:is_dup(Key)),
  
  %% Verify metrics were called
  test_helpers:wait_for_meck_call(router_metrics, inc, '_', 1000),
  ok.

%% Test: Telemetry events
test_telemetry_events(_Config) ->
  ok = router_idem:init([]),
  Key = <<"telemetry-key">>,
  
  TelemetryEvents = ets:new(telemetry_events, [set, private]),
  
  meck:expect(telemetry, execute, fun(Event, Measurements, Metadata) ->
    ets:insert(TelemetryEvents, {Event, Measurements, Metadata}),
    ok
  end),
  
  ?assertNot(router_idem:is_dup(Key)),
  ok = router_idem:remember(Key, 1000),
  ?assert(router_idem:is_dup(Key)),
  
  %% Verify telemetry events were emitted
  test_helpers:wait_for_condition(fun() -> 
    ets:match_object(TelemetryEvents, {[router, idempotency, miss], '$1', '$2'}) =/= []
  end, 1000),
  
  test_helpers:wait_for_condition(fun() -> 
    ets:match_object(TelemetryEvents, {[router, idempotency, remember], '$1', '$2'}) =/= []
  end, 1000),
  
  test_helpers:wait_for_condition(fun() -> 
    ets:match_object(TelemetryEvents, {[router, idempotency, hit], '$1', '$2'}) =/= []
  end, 1000),
  
  ets:delete(TelemetryEvents),
  ok.

%% Test: Atomic check_and_mark operation (CP2 requirement)
%% Verifies that check_and_mark atomically checks and marks key
test_check_and_mark_atomic(_Config) ->
  ok = router_idem:init([]),
  Key = <<"atomic-key-1">>,
  TTL = 1000,
  
  %% First call: not seen
  {ok, not_seen} = router_idem:check_and_mark(Key, TTL),
  
  %% Second call: already seen
  {ok, seen} = router_idem:check_and_mark(Key, TTL),
  
  %% Third call: still seen
  {ok, seen} = router_idem:check_and_mark(Key, TTL),
  
  %% Verify with is_dup
  ?assert(router_idem:is_dup(Key)),
  ok.

%% Test: Zero double-execution (CP2 requirement)
%% Verifies that concurrent check_and_mark calls prevent double-execution
test_zero_double_execution(_Config) ->
  ok = router_idem:init([]),
  Key = <<"double-exec-key">>,
  TTL = 5000,
  
  %% Spawn multiple processes trying to check_and_mark the same key concurrently
  NumProcesses = 100,
    _ = lists:map(fun(I) ->
    spawn(fun() ->
      Result = router_idem:check_and_mark(Key, TTL),
      %% Send result back to parent
      Parent = self(),
      Parent ! {result, I, Result}
    end)
  end, lists:seq(1, NumProcesses)),
  
  %% Collect results
  Results = lists:map(fun(_) ->
    receive
      {result, _I, Result} -> Result
    after 2000 ->
      {error, timeout}
    end
  end, lists:seq(1, NumProcesses)),
  
  %% Verify: exactly one process got {ok, not_seen}, all others got {ok, seen}
  NotSeenCount = length([R || R <- Results, R =:= {ok, not_seen}]),
  SeenCount = length([R || R <- Results, R =:= {ok, seen}]),
  
  %% Exactly one should be not_seen
  1 = NotSeenCount,
  %% All others should be seen
  ExpectedSeenCount = NumProcesses - 1,
  ExpectedSeenCount = SeenCount,
  
  %% Verify key is still in table
  ?assert(router_idem:is_dup(Key)),
  
  %% Verify no double-execution: key should exist exactly once
  1 = ets:info(router_idem, size),
  ok.

%% Test: p95 duplicate handling latency (CP2 requirement)
%% GIVEN: normal load (N messages)
%% WHEN: all processed with idempotency checks
%% THEN: p95 duplicate handling latency (by metric or test measurement) <= target from CP2 profile
test_p95_duplicate_handling_latency(_Config) ->
  ok = router_idem:init([]),
  TTL = 1000,
  
  %% Create unique keys for first pass
  NumMessages = 1000,
  Keys = [<<"latency-key-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, NumMessages)],
  
  %% First pass: all should be not_seen (measure latency)
    _ = lists:map(fun(Key) ->
    StartTime = erlang:system_time(microsecond),
    {ok, not_seen} = router_idem:check_and_mark(Key, TTL),
    EndTime = erlang:system_time(microsecond),
    EndTime - StartTime
  end, Keys),
  
  %% Second pass: all should be seen (measure latency for duplicates)
  DuplicateLatencies = lists:map(fun(Key) ->
    StartTime = erlang:system_time(microsecond),
    {ok, seen} = router_idem:check_and_mark(Key, TTL),
    EndTime = erlang:system_time(microsecond),
    EndTime - StartTime
  end, Keys),
  
  %% Calculate p95 latency for duplicates (this is what we care about)
  SortedDuplicateLatencies = lists:sort(DuplicateLatencies),
  P95Index = trunc(length(SortedDuplicateLatencies) * 0.95),
  P95LatencyUs = lists:nth(min(P95Index + 1, length(SortedDuplicateLatencies)), SortedDuplicateLatencies),
  P95LatencyMs = P95LatencyUs / 1000,
  
  %% CP2 target: p95 duplicate handling latency <= 1ms (adjustable)
  TargetP95Ms = 1.0,
  ?assert(P95LatencyMs =< TargetP95Ms),
  
  ok.

%% Test: Keys propagation (CP2 requirement)
%% Verifies that keys can be propagated across Router → Gateway
%% This test simulates key propagation by storing and retrieving keys
test_keys_propagation(_Config) ->
  ok = router_idem:init([]),
  
  %% Simulate Router → Gateway key propagation
  RouterKey = <<"router-key-123">>,
  GatewayKey = <<"gateway-key-456">>,
  TTL = 5000,
  
  %% Router marks key as processed
  {ok, not_seen} = router_idem:check_and_mark(RouterKey, TTL),
  
  %% Gateway tries to process same key (should be seen)
  %% In real scenario, Gateway would use the same key or a derived key
  {ok, seen} = router_idem:check_and_mark(RouterKey, TTL),
  
  %% Gateway processes different key (should be not_seen)
  {ok, not_seen} = router_idem:check_and_mark(GatewayKey, TTL),
  
  %% Verify both keys exist
  ?assert(router_idem:is_dup(RouterKey)),
  ?assert(router_idem:is_dup(GatewayKey)),
  
  %% Verify eviction policy: keys expire after TTL
  timer:sleep(6000),
  ?assertNot(router_idem:is_dup(RouterKey)),
  ?assertNot(router_idem:is_dup(GatewayKey)),
  
  ok.

%% Test: Survives restart (CP2 requirement)
%% GIVEN: keys stored before Router restart
%% WHEN: Router restarts, ETS table recreated
%% THEN: table is recreated and ready for new keys (old keys lost, which is expected for ETS)
test_survives_restart(_Config) ->
  ok = router_idem:init([]),
  
  %% Store some keys before restart
  Keys = [<<"restart-key-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 10)],
  TTL = 5000,
  
  lists:foreach(fun(Key) ->
    {ok, not_seen} = router_idem:check_and_mark(Key, TTL)
  end, Keys),
  
  %% Verify keys exist
  10 = ets:info(router_idem, size),
  lists:foreach(fun(Key) ->
    ?assert(router_idem:is_dup(Key))
  end, Keys),
  
  %% Simulate restart: delete ETS table
  ets:delete(router_idem),
  
  %% Reinitialize after restart
  ok = router_idem:init([]),
  
  %% Verify table is recreated (empty)
  0 = ets:info(router_idem, size),
  
  %% Old keys should not exist (ETS is not persistent)
  lists:foreach(fun(Key) ->
    ?assertNot(router_idem:is_dup(Key))
  end, Keys),
  
  %% Verify new keys can be stored after restart
  NewKey = <<"new-key-after-restart">>,
  {ok, not_seen} = router_idem:check_and_mark(NewKey, TTL),
  ?assert(router_idem:is_dup(NewKey)),
  
  %% Verify table is working
  1 = ets:info(router_idem, size),
  
  ok.
