# Common Test Quarantine Group Implementation

## Task Overview
This task implements a native Common Test group/tag approach for test quarantine functionality, building upon the existing runner-level quarantine implementation.

## Context
- **Iteration 1 (Complete)**: Implemented runner-level quarantine via metadata lists and script logic
- **Iteration 2 (This Task)**: Introduce native Common Test group/tag approach while maintaining backward compatibility

## Goals
1. Implement `{quarantine, [...]}` group in Common Test suites
2. Ensure tests with quarantine group are excluded from PR/CI runs
3. Include quarantined tests in nightly/heavy test runs
4. Maintain backward compatibility with existing quarantine mechanism

## Non-Goals
- Modifying existing runner-level quarantine scripts
- Moving existing quarantine metadata
- Changing how non-quarantine tests are executed

## Related Tasks
- `task_quarantine_flaky_tests` - Original quarantine implementation
- `task_flaky_probe` - Flakiness detection
- `task_enforce_mock_discipline` - Test reliability improvements
