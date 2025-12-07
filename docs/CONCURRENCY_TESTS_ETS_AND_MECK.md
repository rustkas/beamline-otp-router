# Concurrent Tests with ETS and Meck: Patterns and Best Practices

## Purpose

This document describes the established patterns for writing concurrent tests that use ETS (Erlang Term Storage) and meck (mocking library) in the `beamline_router` project.

**Key principle**: Test suites (`*_SUITE.erl`) must **never** access ETS directly. All ETS operations must go through dedicated helper modules (`*_store.erl`).

---

## Why Helper Modules for ETS?

### Problems with Direct ETS Access in Tests

1. **Race conditions**: Multiple test processes creating the same named table simultaneously can cause `badarg` errors or `case_clause` exceptions.

2. **Hard to debug**: When ETS operations fail, it's unclear whether the issue is in test logic or ETS lifecycle management.

3. **Code duplication**: Every SUITE that needs ETS reimplements the same creation/cleanup logic.

4. **Maintenance burden**: Changes to ETS table structure require updates across multiple test files.

### Benefits of Helper Modules

- **Encapsulation**: All ETS complexity is hidden in one place.
- **Race tolerance**: Helper modules handle concurrent table creation gracefully.
- **Consistency**: All tests use the same, proven pattern.
- **Testability**: Helper modules can be tested independently.

---

## Pattern: ETS Helper Module (`*_store.erl`)

### Structure

Every ETS helper module should follow this structure:

```erlang
-module(router_*_store).

-export([
    ensure/0,
    reset/0,
    put/2,      % or put/3, etc., depending on needs
    get/1,
    list/1,     % if needed
    delete/1    % if needed
]).

-define(TABLE, table_name).

%% @doc Ensure ETS table exists.
%% Returns:
%%   ok              - table exists or was created
%%   {error, Reason} - creation failed
ensure() ->
    case catch ets:whereis(?TABLE) of
        undefined ->
            case catch ets:new(?TABLE, [named_table, public, set]) of
                {'EXIT', {badarg, _}} ->
                    %% Table already created in parallel.
                    ok;
                {'EXIT', Reason} ->
                    {error, Reason};
                _Tid ->
                    %% Any valid table identifier.
                    ok
            end;
        {'EXIT', Reason} ->
            {error, Reason};
        _Tid ->
            %% Table already exists.
            ok
    end.

%% @doc Clear all data but keep table.
reset() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?TABLE),
            ok;
        Error ->
            Error
    end.

%% ... other functions (put, get, list, delete) ...
```

### Key Points for Helper Modules

1. **`ensure/0` is race-tolerant**:
   - Uses `ets:whereis/1` to check existence.
   - Always attempts `ets:new/2` if table doesn't exist.
   - Treats `{'EXIT', {badarg, _}}` as "table already created" → returns `ok`.
   - **Never** pattern-matches on table name atoms (avoids `case_clause` errors).

2. **`reset/0` clears data but keeps table**:
   - Calls `ensure/0` first to guarantee table exists.
   - Uses `ets:delete_all_objects/1` to clear all entries.
   - Does not delete the table itself.

3. **API functions** (`put/2`, `get/1`, etc.):
   - Always call `ensure/0` first.
   - Return consistent error tuples: `{error, Reason}` or `not_found`.
   - Never throw exceptions.

---

## Pattern: Concurrent Test Suite with ETS and Meck

### Suite Structure

```erlang
-module(router_*_concurrency_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([test_*/1]).

all() ->
    [
        test_sanity,
        {group, concurrency_tests}
    ].

groups() ->
    [
        {concurrency_tests, [], [
            test_concurrent_operation_1,
            test_concurrent_operation_2
        ]}
    ].

%% @doc Setup: ensure store and setup meck mocks
init_per_suite(Config) ->
    ct:pal("### init_per_suite: ensure store and setup meck mocks", []),

    case router_*_store:ensure() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_per_suite: failed to ensure store: ~p", [Reason])
    end,

    ok = setup_mocks(),

    Config.

%% @doc Cleanup: unload meck and reset store
end_per_suite(_Config) ->
    ct:pal("### end_per_suite: cleaning up meck and store", []),

    catch meck:unload(module_to_mock),

    _ = router_*_store:reset(),

    ok.

%% @doc Before each test: reset store and reinstall mocks
init_per_testcase(_Case, Config) ->
    case router_*_store:reset() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_per_testcase: failed to reset store: ~p", [Reason])
    end,

    ok = setup_mocks(),

    TestUserId = <<"test-user">>,
    [{test_user_id, TestUserId} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

%% @doc Setup meck mocks (idempotent)
setup_mocks() ->
    ct:pal("### setup_mocks/0: starting", []),

    case erlang:whereis(module_to_mock_meck) of
        undefined ->
            case meck:new(module_to_mock, [unstick]) of
                ok ->
                    ct:pal("### setup_mocks/0: meck:new(...) = ok", []);
                {error, Reason} ->
                    ct:fail("setup_mocks/0: meck:new/2 failed: ~p", [Reason])
            end;
        _Pid ->
            ok = meck:reset(module_to_mock),
            ct:pal("### setup_mocks/0: meck:reset(...) = ok (reusing existing mock)", [])
    end,

    ok = meck:expect(module_to_mock, function1, fun mock_function1/2),
    ok = meck:expect(module_to_mock, function2, fun mock_function2/2),

    ct:pal("### setup_mocks/0: all expectations installed", []),

    ok.

%% Mock functions use router_*_store, not direct ETS
mock_function1(_Ctx, Request) ->
    %% ... decode request ...
    Key = {TenantId, PolicyId},
    case router_*_store:put(Key, Data) of
        ok ->
            {ok, Response, #{}};
        {error, _Reason} ->
            {ok, ErrorResponse, #{}}
    end.

%% Concurrent test example
test_concurrent_operation(Config) ->
    NumConcurrent = 10,
    
    Monitors = lists:map(
        fun(_) ->
            erlang:spawn_monitor(fun() ->
                %% Perform operation
                case operation() of
                    {ok, _, _} -> ok;
                    Error -> exit({operation_failed, Error})
                end
            end)
        end,
        lists:seq(1, NumConcurrent)
    ),
    
    %% Wait for all processes
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, Reason} ->
                    ct:fail("Process ~p exited with reason: ~p", [Pid, Reason])
            after
                10000 ->
                    ct:fail("Timeout waiting for process ~p to complete", [Pid])
            end
        end,
        Monitors
    ),
    
    %% Verify final state through helper
    Key = {TenantId, PolicyId},
    case router_*_store:get(Key) of
        {ok, _Policy} ->
            ok;
        not_found ->
            ct:fail("Expected policy not found after concurrent operations");
        {error, Reason} ->
            ct:fail("Store error: ~p", [Reason])
    end.
```

### Key Points for Test Suites

1. **`init_per_suite/1`**:
   - Calls `*_store:ensure/0` to create ETS table.
   - Calls `setup_mocks/0` to install meck mocks.
   - **Never** touches ETS directly.

2. **`init_per_testcase/2`**:
   - Calls `*_store:reset/0` to clear data.
   - Calls `setup_mocks/0` to reinstall mocks (idempotent via `meck:reset/1`).
   - **Never** touches ETS directly.

3. **`end_per_suite/1`**:
   - Unloads meck mocks.
   - Calls `*_store:reset/0` to clean up.
   - **Never** calls `ets:delete/1` directly.

4. **Concurrent tests**:
   - Use `spawn_monitor/1` (not bare `spawn/1`).
   - Always wait for `{'DOWN', Ref, process, Pid, Reason}` with timeout.
   - Verify final state through helper API, not direct ETS.

5. **Mock functions**:
   - Use helper API (`*_store:put/2`, `*_store:get/1`, etc.).
   - **Never** call `ets:*` directly.
   - **Never** call real `throw_internal_error/1` or similar error-throwing functions.

---

## Example: `router_admin_grpc_concurrency_SUITE` + `router_admin_policy_store`

### Helper Module: `test/router_admin_policy_store.erl`

See `test/router_admin_policy_store.erl` for the complete implementation.

Key features:

- `ensure/0`: Race-tolerant table creation.
- `reset/0`: Clears all policies.
- `put/2`, `get/1`, `list/1`, `delete/1`: Policy operations.

### Test Suite: `test/router_admin_grpc_concurrency_SUITE.erl`

See `test/router_admin_grpc_concurrency_SUITE.erl` for the complete implementation.

Key features:

- `init_per_suite/1`: Ensures policy store and sets up meck mocks.
- `init_per_testcase/2`: Resets store and reinstalls mocks.
- Mock functions (`mock_upsert_policy/2`, etc.) use `router_admin_policy_store:*`.
- Concurrent tests verify state through `router_admin_policy_store:get/1`.

---

## Rules

### ❌ Forbidden in Test Suites

- **Direct ETS calls**:
  - `ets:new/2`
  - `ets:insert/2`
  - `ets:lookup/2`
  - `ets:match/2`
  - `ets:match_object/2`
  - `ets:delete/2`
  - `ets:delete_all_objects/1`
  - `ets:info/...`
  - `ets:whereis/1`

- **Bare spawn**:
  - `spawn/1` (use `spawn_monitor/1` instead)

- **Unhandled timeouts**:
  - `receive ... after Timeout -> ok end` (must call `ct:fail/2`)

### ✅ Required in Test Suites

- **Helper modules**:

  - All ETS operations through `*_store:*` functions.

- **Process monitoring**:
  - Use `spawn_monitor/1` for concurrent operations.
  - Always handle `{'DOWN', Ref, process, Pid, Reason}` with explicit timeout and failure.

- **State verification**:
  - After concurrent operations, verify final state through helper API.
  - Never assume "no errors" means "correct state".

- **Mock safety**:
  - Mock functions must never call real error-throwing functions.
  - Mock functions must use helper API, not direct ETS.

---

## Migration Checklist

When refactoring an existing SUITE to use helper modules:

1. [ ] Identify all ETS tables used in the SUITE.
2. [ ] Create a helper module (`router_*_store.erl`) with `ensure/0`, `reset/0`, and API functions.
3. [ ] Replace all `ets:*` calls in SUITE with helper API calls.
4. [ ] Update `init_per_suite/1` to call `*_store:ensure/0`.
5. [ ] Update `init_per_testcase/2` to call `*_store:reset/0`.
6. [ ] Update `end_per_suite/1` to call `*_store:reset/0` (or remove direct `ets:delete/1`).
7. [ ] Replace `spawn/1` with `spawn_monitor/1` in concurrent tests.
8. [ ] Add explicit `{'DOWN', ...}` handling with timeouts.
9. [ ] Verify final state through helper API in all concurrent tests.
10. [ ] Run `rebar3 ct --dir test --suite <SUITE>` and verify all tests pass.

---

## Current Status

### ✅ Completed

- `router_admin_grpc_concurrency_SUITE` + `router_admin_policy_store`

### 🔄 Needs Refactoring

Based on grep analysis, the following SUITE files use ETS directly and should be refactored:

1. **High priority** (heavy ETS usage):

   - `router_jetstream_extended_recovery_SUITE.erl` - Multiple private ETS tables for state tracking
   - `router_network_partition_SUITE.erl` - Leader state, pending tables
   - `router_extensions_pipeline_load_SUITE.erl` - Extension metrics, counters
   - `router_caf_adapter_load_thresholds_SUITE.erl` - Retry metrics, failure metrics
   - `router_advanced_concurrent_faults_SUITE.erl` - Metrics tracking

2. **Medium priority** (moderate ETS usage):
   - `router_admin_grpc_integration_SUITE.erl` - RBAC roles, permission cache
   - Various performance/load SUITE files

3. **Low priority** (minimal ETS usage):
   - SUITE files with single ETS table or simple counters

## References

- `test/router_admin_policy_store.erl` - Reference implementation
- `test/router_admin_grpc_concurrency_SUITE.erl` - Reference test suite
- Erlang/OTP documentation: [ETS](https://www.erlang.org/doc/man/ets.html)
- Meck documentation: [meck](https://github.com/eproxus/meck)
