## In Scope

- Full audit of `docs/` and `docs/dev/`
- Classification of documents into:
  - canonical / keep
  - merge & compress
  - archive
  - delete
- Removal of obsolete status reports, TODO execution logs, and duplicate summaries
- Consolidation of overlapping CP1 / CP2 / fault-injection / observability docs
- Updating retained documents to reflect current architecture and decisions

## Out of Scope

- Changing code behavior
- Updating `.ai/` global architecture/decisions files (unless explicitly required later)
- Rewriting ADR history (only reference consistency)
- Generating new features documentation

## Constraints

- No loss of *canonical* information
- Historical artifacts must be either archived or explicitly marked as non-authoritative
- Result must be reviewable via git diff (no destructive hidden changes)
