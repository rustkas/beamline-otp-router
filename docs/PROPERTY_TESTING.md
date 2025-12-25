# Property-Based Testing with PropEr

## Overview

This project uses PropEr for property-based testing. All property-based tests follow a unified style and include `proper.hrl` in the test profile.

## Test Profile Configuration

PropEr is available in the test profile (`rebar3 as test`). The dependency is configured in `rebar.config`:

```erlang
{test, [
    {deps, [
        {meck, "~> 0.9"},
        {proper, "~> 1.4"}  %% Property-based testing: proper.hrl always included in test profile
    ]}
]}
```

## Unified Style for Property Tests

All property-based tests follow this unified style:

### 1. Include proper.hrl

**Always include `proper.hrl` at the top of the file** (in test profile):

```erlang
%% Always include proper.hrl in test profile (PropEr is available in test profile)
%% Runtime check for PropEr availability is done in prop_* functions
-include_lib("proper/include/proper.hrl").
```

**Rationale**: PropEr is available in test profile, so we always include the header. This allows generators to be defined at module level and used in property tests.

### 2. Runtime Check in all/0

Check PropEr availability in `all/0` function:

```erlang
all() ->
    case code:which(proper) of
        non_existing -> [
            prop_test1_skip,
            prop_test2_skip
        ];
        _ -> [
            prop_test1,
            prop_test2
        ]
    end.
```

### 3. Skip Fallback Functions

Provide skip fallback functions for when PropEr is not available:

```erlang
%% Skip tests if PropEr not available
prop_test1_skip(_Config) ->
    {skip, "PropEr not available"}.
prop_test2_skip(_Config) ->
    {skip, "PropEr not available"}.
```

### 4. Runtime Check in Property Functions

Each property function checks PropEr availability at runtime:

```erlang
%% Property: Test description
prop_test1(_Config) ->
    case code:which(proper) of
        non_existing ->
            {skip, "PropEr not available"};
        _ ->
            Prop = ?FORALL(
                Input,
                generator(),
                begin
                    %% Test logic
                    ExpectedResult
                end
            ),
            Result = proper:quickcheck(Prop, [{numtests, 100}]),
            case Result of
                true -> 
                    ok;
                false ->
                    ct:fail("Property failed");
                {false, CounterExample} ->
                    ct:fail("Property failed with counterexample: ~p", [CounterExample]);
                Other ->
                    ct:fail("Unexpected result from proper:quickcheck: ~p", [Other])
            end
    end.
```

### 5. Generators at Module Level

Generators are defined at module level (after property functions):

```erlang
%% Generators

input_generator() ->
    ?LET(
        {Value1, Value2},
        {generator1(), generator2()},
        #{~"key1" => Value1, ~"key2" => Value2}
    ).

generator1() ->
    oneof([value1, value2, value3]).

generator2() ->
    integer(1, 100).
```

## Property Test Suites

The following property test suites are available:

1. **router_policy_store_prop_SUITE**: Tests for policy store operations
   - `prop_concurrent_operations`: Concurrent operations maintain consistency
   - `prop_upsert_delete_consistency`: Upsert and delete maintain consistency
   - `prop_list_policies_ordering`: list_policies always returns sorted results
   - `prop_index_consistency`: Secondary index maintains consistency with main table

2. **router_options_merge_prop_SUITE**: Tests for options merge in ExecAssignment
   - `prop_options_merge_defaults`: Options merge with defaults correctly
   - `prop_options_merge_override`: Options merge with overrides correctly
   - `prop_options_merge_partial`: Options merge with partial values correctly

3. **router_normalize_boolean_prop_SUITE**: Tests for boolean normalization
   - `prop_normalize_boolean_boolean`: Boolean values normalize correctly
   - `prop_normalize_boolean_binary`: Binary "true"/"false" normalize correctly
   - `prop_normalize_boolean_integer`: Integer 0/1 normalize correctly
   - `prop_normalize_boolean_unknown`: Unknown values default to false

4. **router_decider_prop_SUITE**: Tests for router decider
   - `prop_weighted_distribution`: Weighted distribution respects weights
   - `prop_fallback_behavior`: Fallback is used when weights fail
   - `prop_weighted_statistical_distribution`: Weighted distribution follows statistical expectations

5. **router_ets_consistency_prop_SUITE**: Tests for ETS consistency
   - `test_ets_table_integrity`: ETS table integrity after operations (verifies no invalid entries, duplicate keys, or missing required fields)
   - `test_ets_no_orphaned_entries`: No orphaned entries after cleanup (verifies ETS tables don't contain orphaned entries referencing non-existent resources)
   - `test_ets_cleanup_after_operations`: Proper cleanup after operations (verifies ETS tables return to clean state without unbounded growth)
   
   **Pattern**: Uses metrics access layer (`router_r10_metrics`) instead of direct ETS access. Verifies table structure, consistency, and cleanup behavior.

## Running Property Tests

Run all property tests:

```bash
rebar3 as test ct --suite test/router_policy_store_prop_SUITE \
                  --suite test/router_options_merge_prop_SUITE \
                  --suite test/router_normalize_boolean_prop_SUITE \
                  --suite test/router_decider_prop_SUITE \
                  --suite test/router_ets_consistency_prop_SUITE
```

Run individual test suite:

```bash
rebar3 as test ct --suite test/router_policy_store_prop_SUITE
```

## Best Practices

1. **Always include proper.hrl**: Include `proper.hrl` at the top of the file, not conditionally
2. **Runtime checks**: Check PropEr availability at runtime in `all/0` and property functions
3. **Skip fallbacks**: Provide skip fallback functions for graceful degradation
4. **Generators at module level**: Define generators at module level for reuse
5. **Consistent error handling**: Use consistent error handling pattern for `proper:quickcheck` results
6. **Documentation**: Document what each property test verifies

## Migration Notes

If migrating existing property tests to this unified style:

1. Remove conditional includes: `-ifdef(HAVE_PROPER)` → always include
2. Update `all/0`: Use `code:which(proper)` instead of `?HAVE_PROPER`
3. Add skip functions: Create `prop_*_skip/1` functions
4. Update property functions: Add runtime check with `case code:which(proper)`
5. Remove `-endif` blocks: No longer needed

## References

- [PropEr Documentation](https://proper-testing.github.io/)
- [Property-Based Testing Guide](https://propertesting.com/)

