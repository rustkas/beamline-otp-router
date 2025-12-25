# Scope — T-CP2-CONTRACTS-01

## In-scope
1) **Subjects taxonomy**
   - Canonical subjects for CAF results, Router usage, backpressure, extensions
   - Versioning: keep v1 intact, introduce v2 as additive
   - Naming rules: prefix domain + version suffix

2) **Headers contract**
   - Required/optional per subject class
   - Type, encoding, max lengths
   - Backward compatibility (CP1 allows missing headers)

3) **Payload schema + versioning**
   - Format (proto/json) + schema id/version
   - Compatibility rules per MAJOR/MINOR/PATCH

4) **Correlation and idempotency**
   - assignment_id vs request_id precedence
   - msg_id semantics for JetStream ack/nak

5) **DLQ contract**
   - Subject naming + envelope fields
   - Replay-ready format

6) **CI enforcement**
   - Contract validation script
   - Fails on violations

## Out-of-scope
- DLQ stream production setup
- Protocol payload refactoring
- Full integration testing (separate task)
