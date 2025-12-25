# Unify CT suites test selection by ROUTER_TEST_LEVEL

## Problem
Common Test suites in `apps/otp/router/test` use inconsistent patterns
for selecting test cases based on execution level (fast / full / heavy).

This leads to:
- accidental execution of heavy tests in fast/full runs,
- unclear coverage guarantees,
- difficulty enforcing quality gates.

## Goal
Standardize all `*_SUITE.erl` files to use a single, explicit pattern
for selecting test groups based on `ROUTER_TEST_LEVEL`, identical in
structure to `router_abuse_SUITE.erl`.

## Expected Outcome
Every test suite deterministically selects test groups via:
`all() -> Level -> groups_for_level(Level)`,
with all test cases defined only in `groups()`.
