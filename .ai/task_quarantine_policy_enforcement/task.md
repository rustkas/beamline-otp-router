# Task: Quarantine Policy Enforcement

## Problem
The CT-native `quarantine` group has been introduced to isolate flaky tests.
Without explicit governance, quarantine risks becoming a permanent dumping
ground for unstable tests, degrading test signal quality over time.

## Goal
Introduce enforceable policies that make quarantine:
- explicit (owner + reason),
- temporary (time-bounded),
- observable (CI-visible).

## Context
The project already uses CT-native groups and excludes `quarantine`
from full/PR runs while including it in heavy/nightly runs.
This task does NOT change execution semantics — only policy and enforcement.

## Expected Outcome
- Every quarantined suite/test is attributable and justified.
- Old or abandoned quarantines are visible and actionable.
- CI provides signals when quarantine policy is violated.
