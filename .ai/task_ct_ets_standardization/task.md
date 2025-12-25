# Standardize ETS usage in CT suites

## Problem
Multiple CT suites encountered ets:badarg errors due to inconsistent
ETS table creation and lookup patterns.

Fixes were applied ad-hoc, creating duplication and future risk.

## Goal
Introduce a single, documented ETS helper and migrate suites
to use it consistently.

## Expected Outcome
All relevant CT suites use a common ETS helper, eliminating badarg failures.
