# Resolve and document empty CT suites

## Problem
Several CT suites currently execute with zero test cases.
While technically valid, undocumented empty suites introduce noise
and make test coverage unclear.

## Goal
Ensure that every empty CT suite is either:
- intentionally empty and documented,
- removed from the repository, or
- refactored into a non-suite helper/module.

## Expected Outcome
No accidental or undocumented 0-test CT suites remain.
