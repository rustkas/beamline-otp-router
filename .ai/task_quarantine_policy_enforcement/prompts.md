# Task-Specific Operating Instructions

## Mandatory Rules
- Do NOT modify ct-full.sh or ct-heavy.sh.
- Do NOT change CT group semantics.
- Do NOT fix flaky tests as part of this task.

## Required Behavior
- Every completed step MUST be recorded in progress.md immediately.
- All checks must be automatable and non-interactive.
- Prefer simple shell or Python scripts over Erlang.

## Recommended Approach
- Central quarantine registry file (single source of truth).
- Policy checker script runnable locally and in CI.
- CI mode configurable via environment variable.

## Forbidden
- Silent quarantine.
- Implicit ownership.
- Indefinite quarantine without visibility.
