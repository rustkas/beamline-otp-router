## Execution Plan

1. Inventory all documents with metadata:
   - path
   - topic
   - status (canonical / report / obsolete)
   - overlaps

2. Define canonical document set per domain:
   - Architecture
   - API & Contracts
   - NATS / Messaging
   - Observability
   - Security
   - Testing
   - Operations

3. Identify:
   - duplicates
   - outdated assumptions
   - execution-only artifacts

4. Propose actions per document:
   - keep as-is
   - merge into X
   - archive
   - delete

5. Apply changes incrementally with mandatory `progress.md` updates

6. Final verification pass + structure summary
