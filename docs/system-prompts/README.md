# System Prompts for ETS Refactoring

This directory contains ready-to-use SYSTEM PROMPT's for refactoring test suites to eliminate direct ETS usage.

## Available Prompts

### 1. `refactor-jetstream-extended-recovery-ets.md`

**Target**: `router_jetstream_extended_recovery_SUITE.erl`

**Status**: Ready to use

**Context**: This suite already has `router_jetstream_recovery_store.erl` and `router_jetstream_ct_helpers.erl`. The prompt focuses on ensuring zero direct ETS calls remain.

**Usage**:
1. Open Cursor/Claude
2. Copy the entire content of `refactor-jetstream-extended-recovery-ets.md`
3. Paste as SYSTEM PROMPT
4. Execute the refactoring

---

### 2. `refactor-network-partition-ets.md`

**Target**: `router_network_partition_SUITE.erl`

**Status**: Ready to use

**Context**: This suite has direct ETS calls for leader state, pending routes, and partition metrics. The prompt guides creation of `router_network_partition_store.erl` and full refactoring.

**Usage**:
1. Open Cursor/Claude
2. Copy the entire content of `refactor-network-partition-ets.md`
3. Paste as SYSTEM PROMPT
4. Execute the refactoring

---

## Execution Order

Recommended sequence:

1. **First**: Run `refactor-jetstream-extended-recovery-ets.md`
   - Easier (store already exists)
   - Validates the pattern
   - Builds confidence

2. **Second**: Run `refactor-network-partition-ets.md`
   - More complex (needs new store)
   - Applies learned patterns
   - Completes the refactoring

---

## Validation

After each refactoring:

```bash
cd ~/aigroup/apps/otp/router

# Compile
rebar3 compile

# Run specific suite
rebar3 ct --dir test --suite router_jetstream_extended_recovery_SUITE
# or
rebar3 ct --dir test --suite router_network_partition_SUITE
```

**Success criteria**:
- ✅ Zero direct `ets:*` calls in SUITE
- ✅ All tests pass
- ✅ No new compiler warnings
- ✅ Test semantics preserved

---

## Pattern Reference

All prompts follow the pattern documented in:
- `docs/CONCURRENCY_TESTS_ETS_AND_MECK.md`

Example implementations:
- `test/router_jetstream_recovery_store.erl`
- `test/router_admin_policy_store.erl` (if exists)

---

## Notes

- These prompts are designed to be used **one at a time**
- Each prompt is **self-contained** and includes all necessary context
- Prompts follow **strict constraints** to avoid breaking changes
- All changes are **localized** to test code only

