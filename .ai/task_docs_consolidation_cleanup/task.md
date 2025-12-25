# Documentation Consolidation & Cleanup

## Problem

The documentation tree under `docs/` has grown organically and now contains:
- duplicated documents (same topic, different names),
- outdated status reports and execution logs,
- overlapping CP1 / CP2 / task-level reports,
- obsolete MVP assumptions mixed with current architecture,
- excessive verbosity that obscures canonical contracts.

This creates confusion, increases maintenance cost, and weakens docs as a source of truth.

## Goal

Transform `docs/` into a **lean, authoritative, and navigable documentation set** where:
- each topic has a single canonical document,
- obsolete and execution-only artifacts are removed or archived,
- current architecture, contracts, and operational guides are clearly separated,
- documentation reflects the actual current system state.

## Expected Outcome

- Reduced document count (significant compression)
- Clear ownership of each doc category (architecture, contracts, ops, testing)
- Explicit separation between:
  - canonical specs
  - historical execution reports
- Updated, concise, and consistent documentation
