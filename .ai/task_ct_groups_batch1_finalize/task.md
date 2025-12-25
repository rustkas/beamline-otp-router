# Finalize CT Batch #1 with a clean reproducible run

## Problem
Batch #1 (soak / chaos heavy-only suites) was refactored successfully,
but verification required split runs, resume scripts, and manual intervention
due to timeouts and ETS-related failures.

As a result, Batch #1 does not yet have a single clean, reproducible
end-to-end execution record.

## Goal
Achieve a fully reproducible, clean execution of Batch #1 suites across
fast / full / heavy levels, without manual resume steps.

## Expected Outcome
Batch #1 can be executed from scratch using a documented script or command,
and all suites complete deterministically with results recorded in progress.md.
