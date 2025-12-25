# T-CGW-ROUTER-01 — C-Gateway ↔ Router Integration Validation (CP1)

## Goal

Validate that **c-gateway correctly, stably, and predictably interacts with CP1-frozen Router**, without hidden contract mismatches, degradations, or unaccounted failure modes.

This is **validation/audit**, not refactoring.

## Context (Frozen)

- **Router**: CP1 FROZEN ❄️
  - subjects v1
  - request-reply protocol
  - backpressure contract fixed
- **NATS**: Single-node, JetStream enabled
- **TLS**: Optional (CP1 baseline)
- **CP2**: Not in scope

## Expected Outcome

**Deliverables**:
1. Factual HTTP↔NATS contract documentation
2. Backpressure propagation validation
3. Observability end-to-end confirmation
4. Failure mode catalog
5. Risk assessment (if any)

**Decision**: Ready/Not-Ready for CP1 production via c-gateway

## Principle

> ❗ This is an **audit of reality**, not "let's improve while we look"

CP1 is frozen — we verify, not change.
