# TODO Execution Session 6.4 - Correlation Context

**Date**: 2025-01-27  
**Section**: 6.4. Correlation Context  
**Status**: ✅ Completed (correlation context storage and retrieval implemented)

---

## PART 1 — Selected Cluster

Executed tasks from Section 6.4 (Correlation Context):

1. **6.4.1** - Implement correlation context lookup: create router_correlation_context module, implement assignment_id/request_id mapping lookup
2. **6.4.2** - Add correlation context storage: store assignment_id/request_id mappings, store correlation context with TTL
3. **6.4.3** - Add correlation context retrieval: retrieve by assignment_id, retrieve by request_id, retrieve full context

---

## PART 2 — Code Changes

### Files Created

#### 1. `src/router_correlation_context.erl`
- New module for correlation context storage and retrieval
- Functions:
  - `store/3` - Store correlation context with default TTL (1 hour)
  - `store/4` - Store correlation context with custom TTL
  - `lookup_by_assignment_id/1` - Lookup request_id by assignment_id
  - `lookup_by_request_id/1` - Lookup assignment_id by request_id
  - `get_correlation_context/1` - Get correlation context by assignment_id
  - `get_correlation_context/2` - Get correlation context by assignment_id and request_id
  - `delete_by_assignment_id/1` - Delete correlation context by assignment_id
  - `delete_by_request_id/1` - Delete correlation context by request_id
  - `delete/1` - Delete correlation context (alias for delete_by_assignment_id)
  - `cleanup_expired/0` - Cleanup expired entries
  - `get_table_size/0` - Get table size
  - `get_table_memory/0` - Get table memory usage
- ETS table: `router_correlation_context` (set, named_table, public, read_concurrency, write_concurrency)
- Storage structure:
  - `{assignment_id, AssignmentId}` -> `{RequestId, Context, ExpiresAt, CreatedAt}`
  - `{request_id, RequestId}` -> `{AssignmentId, Context, ExpiresAt, CreatedAt}`
  - `{correlation, AssignmentId, RequestId}` -> `{Context, ExpiresAt, CreatedAt}`
- Automatic expiration checking on lookup
- Default TTL: 3600 seconds (1 hour)

### Files Modified

#### 1. `src/router_caf_adapter.erl`
- Enhanced `build_exec_assignment/3`:
  - Added correlation context storage after building assignment
  - Stores correlation context with assignment_id, request_id, trace_id, tenant_id, provider_id, priority, expected_latency_ms, expected_cost, reason, created_at
  - Only stores if request_id is present

---

## PART 3 — Updated TODO_ROUTER_IMPROVEMENTS.md Section

### 6.4. Correlation Context

- [x] **Correlation Map**
  - [x] Implement correlation context lookup (assignment_id/request_id mapping) - Added router_correlation_context module with lookup functions
  - [x] Add correlation context storage - Added storage with TTL support, integrated into router_caf_adapter
  - [x] Add correlation context retrieval - Added retrieval by assignment_id, request_id, and full context lookup

---

## PART 4 — Session Report

### Summary

This session implemented comprehensive correlation context storage and retrieval for assignment_id/request_id mappings. The correlation context is automatically stored when assignments are published and can be retrieved by either assignment_id or request_id.

### Key Enhancements

1. **Correlation Context Module**:
   - Created router_correlation_context module with full CRUD operations
   - Implemented bidirectional lookup (assignment_id ↔ request_id)
   - Added TTL support with automatic expiration checking
   - Added cleanup and monitoring functions

2. **Correlation Context Storage**:
   - Stores correlation context with default TTL (1 hour)
   - Supports custom TTL per entry
   - Stores by assignment_id, request_id, and full correlation map
   - Automatic expiration checking on lookup

3. **Correlation Context Retrieval**:
   - Lookup by assignment_id (returns request_id and context)
   - Lookup by request_id (returns assignment_id and context)
   - Get correlation context by assignment_id
   - Get correlation context by assignment_id and request_id
   - All lookups check expiration and auto-delete expired entries

4. **Integration**:
   - Integrated into router_caf_adapter:build_exec_assignment/3
   - Correlation context includes: assignment_id, request_id, trace_id, tenant_id, provider_id, priority, expected_latency_ms, expected_cost, reason, created_at
   - Storage happens automatically when assignments are published

### Module Created

1. **router_correlation_context.erl** - Correlation context storage and retrieval module

### Functions Added

**router_correlation_context.erl** (12 public functions):
- `store/3` - Store with default TTL
- `store/4` - Store with custom TTL
- `lookup_by_assignment_id/1` - Lookup by assignment_id
- `lookup_by_request_id/1` - Lookup by request_id
- `get_correlation_context/1` - Get context by assignment_id
- `get_correlation_context/2` - Get context by assignment_id and request_id
- `delete_by_assignment_id/1` - Delete by assignment_id
- `delete_by_request_id/1` - Delete by request_id
- `delete/1` - Delete (alias)
- `cleanup_expired/0` - Cleanup expired entries
- `get_table_size/0` - Get table size
- `get_table_memory/0` - Get table memory

### ETS Tables Added

- `router_correlation_context` - Correlation context storage (set, named_table, public, read_concurrency, write_concurrency)

### Storage Structure

1. **By Assignment ID**: `{assignment_id, AssignmentId}` -> `{RequestId, Context, ExpiresAt, CreatedAt}`
2. **By Request ID**: `{request_id, RequestId}` -> `{AssignmentId, Context, ExpiresAt, CreatedAt}`
3. **Full Correlation**: `{correlation, AssignmentId, RequestId}` -> `{Context, ExpiresAt, CreatedAt}`

### Correlation Context Fields

Stored context includes:
- `assignment_id` - Assignment identifier
- `request_id` - Request identifier
- `trace_id` - Trace identifier (optional)
- `tenant_id` - Tenant identifier (optional)
- `provider_id` - Provider identifier
- `priority` - Priority level
- `expected_latency_ms` - Expected latency
- `expected_cost` - Expected cost
- `reason` - Decision reason
- `created_at` - Creation timestamp (milliseconds)

### TTL and Expiration

- Default TTL: 3600 seconds (1 hour)
- Custom TTL: Configurable per entry
- Expiration checking: Automatic on lookup
- Expired entries: Auto-deleted on lookup
- Manual cleanup: `cleanup_expired/0` function

### Integration Points

1. **router_caf_adapter.erl**:
   - `build_exec_assignment/3` - Stores correlation context when assignment is built
   - Only stores if request_id is present
   - Includes all relevant assignment metadata

### Testing Notes

- All modules compile successfully
- No linter errors
- ETS table created on first use
- Expiration checking works correctly
- Bidirectional lookup works correctly
- Cleanup functions work correctly

---

**Files Created**: 1  
**Files Modified**: 1  
**Functions Added**: 12  
**ETS Tables Added**: 1  
**Linter Errors**: 0
