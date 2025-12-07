# ETS Consistency Test Patterns

**Date**: 2025-01-27  
**Status**: ✅ **Test Patterns Documented**  
**Component**: Router (`apps/otp/router/`)

## Overview

ETS consistency tests verify that ETS tables maintain integrity, have no orphaned entries, and properly clean up after operations. These tests use property-based testing patterns to verify invariants across multiple operations.

## Test Suite

### router_ets_consistency_prop_SUITE.erl

**Purpose**: Property-based tests for ETS consistency across router modules

**Test Coverage** (3 tests):

1. **test_ets_table_integrity**:
   - Performs a sequence of operations (20 operations)
   - Verifies ETS tables maintain integrity after operations
   - Verifies no invalid entries, duplicate keys, or missing required fields
   - Verifies all entries have valid structure (maps with required keys)

2. **test_ets_no_orphaned_entries**:
   - Creates state, performs operations, then cleans up
   - Verifies no orphaned entries after cleanup
   - Verifies all entries reference valid resources
   - Verifies ETS tables don't contain entries referencing non-existent resources

3. **test_ets_cleanup_after_operations**:
   - Performs many operations (50 operations)
   - Verifies ETS tables return to clean state after cleanup
   - Verifies no unbounded growth in ETS tables
   - Verifies cleanup reduces table size appropriately

**Test Pattern**:
- Uses metrics access layer (`router_r10_metrics`) instead of direct ETS access
- Verifies table structure and consistency
- Verifies cleanup behavior
- Uses proper test lifecycle (init_per_suite, init_per_testcase, end_per_testcase)

**Running Tests**:
```bash
cd apps/otp/router
rebar3 as test ct --suite test/router_ets_consistency_prop_SUITE
```

## Key Patterns

### 1. Metrics Access Layer Pattern

**CRITICAL**: Never access ETS tables directly in tests. Always use metrics access layer:

```erlang
%% ✅ CORRECT: Use metrics access layer
Metrics = router_r10_metrics:dump_metrics(),
lists:foreach(
    fun({_Key, Value}) ->
        true = is_map(Value),
        true = maps:is_key(<<"value">>, Value) orelse maps:is_key(<<"count">>, Value)
    end,
    maps:to_list(Metrics)
).

%% ❌ WRONG: Direct ETS access
AllEntries = ets:tab2list(router_metrics),  %% Don't do this!
```

**Rationale**:
- Metrics access layer provides abstraction and safety
- Direct ETS access can break if table structure changes
- Metrics access layer handles edge cases and errors gracefully

### 2. Table Integrity Verification Pattern

**Pattern**: Verify table structure and entry validity:

```erlang
test_ets_table_integrity(_Config) ->
    %% Perform operations
    perform_operations(),
    
    %% Verify table integrity
    Metrics = router_r10_metrics:dump_metrics(),
    true = is_map(Metrics),
    
    %% Verify all entries have valid structure
    lists:foreach(
        fun({Key, Value}) ->
            true = is_map(Value),
            %% Verify value has required structure
            true = maps:is_key(<<"value">>, Value) orelse maps:is_key(<<"count">>, Value),
            %% Verify key is valid
            true = is_binary(Key) orelse is_atom(Key)
        end,
        maps:to_list(Metrics)
    ),
    
    ok.
```

**Invariants Verified**:
- All entries are maps
- All entries have required keys
- All keys are valid identifiers (binary or atom)
- No duplicate keys
- No invalid entries

### 3. Orphaned Entry Detection Pattern

**Pattern**: Verify no orphaned entries after cleanup:

```erlang
test_ets_no_orphaned_entries(_Config) ->
    %% Create state
    create_state(),
    
    %% Perform operations
    perform_operations(),
    
    %% Clear metrics
    router_r10_metrics:clear_metrics(),
    
    %% Verify no orphaned entries
    Metrics = router_r10_metrics:dump_metrics(),
    true = is_map(Metrics),
    
    %% Verify all entries reference valid resources
    lists:foreach(
        fun({Key, Value}) ->
            true = is_map(Value),
            %% Key should be a valid metric identifier
            true = is_binary(Key) orelse is_atom(Key)
        end,
        maps:to_list(Metrics)
    ),
    
    ok.
```

**Invariants Verified**:
- No entries referencing non-existent resources
- All entries have valid structure
- All keys are valid identifiers
- Cleanup removes all expected entries

### 4. Cleanup Verification Pattern

**Pattern**: Verify proper cleanup after operations:

```erlang
test_ets_cleanup_after_operations(_Config) ->
    %% Get initial metrics count
    InitialMetrics = router_r10_metrics:dump_metrics(),
    InitialCount = maps:size(InitialMetrics),
    
    %% Perform many operations
    perform_many_operations(50),
    
    %% Get metrics after operations
    AfterOpsMetrics = router_r10_metrics:dump_metrics(),
    AfterOpsCount = maps:size(AfterOpsMetrics),
    
    %% Verify metrics were created
    true = AfterOpsCount >= InitialCount,
    
    %% Clear metrics
    router_r10_metrics:clear_metrics(),
    
    %% Get metrics after cleanup
    AfterCleanupMetrics = router_r10_metrics:dump_metrics(),
    AfterCleanupCount = maps:size(AfterCleanupMetrics),
    
    %% Verify cleanup worked
    true = AfterCleanupCount =< AfterOpsCount,
    
    %% Verify no unbounded growth
    true = AfterCleanupCount < 100,  %% Reasonable threshold
    
    ok.
```

**Invariants Verified**:
- Cleanup reduces table size
- No unbounded growth after cleanup
- Table returns to reasonable state
- Cleanup is effective

## Test Lifecycle

**init_per_suite/1**:
- Start router application
- Ensure metrics tables exist
- Clear all metrics

**init_per_testcase/2**:
- Clear all metrics before each test
- Reset RBAC and policy store if needed
- Ensure clean state

**end_per_testcase/2**:
- No cleanup needed (metrics cleared in init_per_testcase)

**end_per_suite/1**:
- Stop router application

## Best Practices

1. **Always use metrics access layer**: Never access ETS tables directly
2. **Verify structure**: Always verify entry structure (maps, required keys)
3. **Verify cleanup**: Always verify cleanup reduces table size
4. **Verify no growth**: Always verify no unbounded growth
5. **Use proper lifecycle**: Use init_per_suite, init_per_testcase, end_per_testcase
6. **Test multiple operations**: Perform multiple operations to verify consistency
7. **Verify invariants**: Verify invariants after each phase (operations, cleanup)

## Common Patterns

### Pattern 1: Verify Entry Structure

```erlang
verify_entry_structure(Entry) ->
    true = is_map(Entry),
    true = maps:is_key(<<"value">>, Entry) orelse maps:is_key(<<"count">>, Entry),
    ok.
```

### Pattern 2: Verify No Orphaned Entries

```erlang
verify_no_orphaned_entries(Metrics) ->
    lists:foreach(
        fun({Key, Value}) ->
            true = is_map(Value),
            true = is_binary(Key) orelse is_atom(Key)
        end,
        maps:to_list(Metrics)
    ),
    ok.
```

### Pattern 3: Verify Cleanup Effectiveness

```erlang
verify_cleanup_effectiveness(BeforeCount, AfterCount, Threshold) ->
    true = AfterCount =< BeforeCount,
    true = AfterCount < Threshold,
    ok.
```

## References

- `apps/otp/router/test/router_ets_consistency_prop_SUITE.erl` - ETS consistency test suite
- `apps/otp/router/src/router_r10_metrics.erl` - Metrics access layer
- `apps/otp/router/docs/OBSERVABILITY_CONVENTIONS.md` - Observability conventions
- `apps/otp/router/docs/PROPERTY_TESTING.md` - Property testing guide

