# Plan

## Phase 1: Baseline
1. Compile current state
2. Run suite to establish baseline
3. Document current mock usage patterns

## Phase 2: Refactor
1. Replace `meck:expect` with `router_mock_helpers` calls
2. Verify each change compiles
3. Run suite after each logical change

## Phase 3: Verification
1. Run 5x stability loop
2. Verify no warnings
3. Log final state to `progress.md`
