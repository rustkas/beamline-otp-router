# Task: Hard Proof of Quarantine Group Mechanism

## Goal
Prove that the quarantine group is correctly excluded from full test runs and included in heavy test runs without modifying `ct-full.sh`.

## Current State (Iteration 2)
- Previous task (`task_ct_quarantine_group`) attempted to implement quarantine group handling
- Issues identified:
  - `--exclude-group` flag might be unsupported in the current Erlang/OTP version
  - `ct-heavy.sh` uses `-groups` which might not be fully supported
  - Suite gating might show "All 0 tests passed" incorrectly

## Why This Task is Needed
- Need to provide concrete proof of quarantine group behavior
- Previous tool-based approach was not executable by Windsurf
- Requires direct shell command evidence of test execution behavior
