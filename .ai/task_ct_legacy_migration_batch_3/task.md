# Task: Legacy CT Migration Batch 3

## Background
The migration of Common Test (CT) suites to a canonical format (static `all/0`, explicit `groups/0`) is ongoing. Previous batches have successfully reduced the usage of legacy patterns like `groups_for_level` and environment-dependent `all/0` functions. This batch focuses on continuing this effort for the remaining suites, specifically targeting those where the logic is identical across tiers, thus simplifying the test structure and improving determinism.

## Objective
Continue canonical Common Test migration by eliminating legacy patterns in remaining suites. This involves converting dynamic, environment-based test selection into static, declarative groups.

Key objectives:
1.  Migrate Batch 3 of legacy CT suites (target ≥3 suites).
2.  Reduce legacy pattern count by ≥50 from the current baseline.
3.  Isolate known technical debt (flaky tests) using explicit skips instead of quarantine abuse.

## Context
- **Repo:** `apps/otp/router`
- **Current Pattern Count:** ~301
- **Strict Quarantine:** New quarantine entries are OUT OF SCOPE. Flaky tests found during migration must be handled via `ct:comment` or explicit `skip` with justification.
