# Task: Refactor router_jetstream_e2e_cp2_SUITE Mock Usage

## Objective
Refactor `router_jetstream_e2e_cp2_SUITE` to use `router_mock_helpers` instead of raw `meck` calls, aligning with project mock discipline (`.ai/decisions.md`).

## Context
The suite currently uses direct `meck:expect` calls, which violates the project's mock discipline policy. All NATS/router_nats interactions must go through approved helpers.

## Constraints
1. **Scope**: Only `test/router_jetstream_e2e_cp2_SUITE.erl`
2. **No semantic changes**: Test logic must remain identical
3. **Mock discipline**: Use `router_mock_helpers` exclusively
4. **Compilation**: Must compile clean (no warnings)
