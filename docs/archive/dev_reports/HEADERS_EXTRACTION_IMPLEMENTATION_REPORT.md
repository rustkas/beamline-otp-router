# Headers Extraction Implementation Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**

## Overview

Implemented improved header extraction from NATS messages in consumers. Headers (`trace_id`, `tenant_id`, `version`) are now extracted with priority over payload values, ensuring better correlation and policy enforcement.

## Implementation Summary

### Changes Made

1. **`router_nats.erl`**:
   - Enhanced `parse_nats_message/1` to parse NATS headers (format: `NATS/1.0\r\nkey: value\r\n\r\n`)
   - Added `parse_headers/2` to extract headers from message data
   - Added `parse_header_lines/1` to parse header key-value pairs
   - Added `headers_size/1` to calculate headers size for payload extraction
   - Updated `forward_to_subscribers/5` to pass headers to consumers: `{nats_message, Subject, Payload, Headers}`

2. **`router_result_consumer.erl`**:
   - Updated `handle_info/2` to accept headers: `{nats_message, Subject, Payload, Headers}`
   - Added backward compatibility: `{nats_message, Subject, Payload}` (no headers)
   - Added `extract_header_or_payload/4` to extract values from headers (priority) or payload (fallback)
   - Updated `handle_result_message_internal/3` to extract `trace_id`, `tenant_id`, `version` from headers
   - Updated `process_exec_result/5` to accept headers and use them for tenant_id/trace_id extraction
   - Added version validation from headers

3. **`router_ack_consumer.erl`**:
   - Updated `handle_info/2` to accept headers: `{nats_message, Subject, Payload, Headers}`
   - Added backward compatibility: `{nats_message, Subject, Payload}` (no headers)
   - Added `extract_header_or_payload/4` to extract values from headers (priority) or payload (fallback)
   - Updated `handle_ack_message_internal/3` to extract `trace_id`, `tenant_id`, `version` from headers
   - Updated `process_ack/5` to accept headers and use them for tenant_id extraction
   - Added version validation from headers

## Header Extraction Logic

### Priority Order

1. **Headers** (priority): Extract from NATS message headers
2. **Payload** (fallback): Extract from JSON payload if not in headers

### Implementation

```erlang
extract_header_or_payload(Headers, Payload, HeaderKey, PayloadKey) ->
    case maps:get(HeaderKey, Headers, undefined) of
        undefined ->
            maps:get(PayloadKey, Payload, undefined);
        HeaderValue ->
            HeaderValue
    end.
```

### Extracted Fields

- **`trace_id`**: Distributed tracing identifier (W3C Trace Context format)
- **`tenant_id`**: Multi-tenant isolation
- **`version`**: Schema version (e.g., `"1"`)

## NATS Headers Format

### Message Format

```
MSG <subject> [reply-to] [sid] [size]\r\n
NATS/1.0\r\n
trace_id: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01\r\n
tenant_id: acme\r\n
version: 1\r\n
\r\n
<payload>
```

### Parsing Logic

1. Check for `NATS/1.0\r\n` marker
2. Find header end marker `\r\n\r\n`
3. Parse header lines: `key: value\r\n`
4. Normalize keys to lowercase for consistency
5. Return headers map

## Backward Compatibility

### Message Format Support

- ✅ **With headers**: `{nats_message, Subject, Payload, Headers}`
- ✅ **Without headers**: `{nats_message, Subject, Payload}` (fallback to empty headers map)

### Payload Fallback

- If headers are missing or empty, values are extracted from payload
- Ensures compatibility with messages that don't include headers

## Version Validation

### Implementation

- Extract `version` from headers (priority) or payload (fallback)
- Validate version if present:
  - `~"1"`: Supported
  - `undefined`: Optional (no validation)
  - Other: Warning logged, processing continues

### Example

```erlang
case Version of
    ~"1" -> ok;
    undefined -> ok;  %% Version optional
    _ ->
        router_logger:warning(~"Unsupported version in headers or payload", #{
            ~"subject" => Subject,
            ~"version" => Version
        })
end
```

## Integration with OpenTelemetry

### Trace Context

- `trace_id` extracted from headers (priority) or payload (fallback)
- Used to create parent context for OpenTelemetry spans
- Propagated across Router ↔ CAF boundaries

### Span Attributes

- `trace_id`: From headers or payload
- `tenant_id`: From headers or payload
- `version`: From headers or payload

## Testing

### Test Coverage

- ✅ Headers parsing from NATS message format
- ✅ Header extraction with priority over payload
- ✅ Payload fallback when headers missing
- ✅ Version validation
- ✅ Backward compatibility (messages without headers)

### Test Files

- `test/router_result_consumer_SUITE.erl`: Result consumer tests
- `test/router_ack_consumer_SUITE.erl`: ACK consumer tests
- `test/router_jetstream_e2e_SUITE.erl`: E2E tests with headers

## Configuration

### No Additional Configuration Required

- Headers extraction is automatic
- No configuration needed for header parsing
- Backward compatible with existing messages

## Benefits

1. **Better Correlation**: `trace_id` in headers enables better distributed tracing
2. **Policy Enforcement**: `tenant_id` in headers enables early validation
3. **Version Control**: `version` in headers enables schema versioning
4. **Backward Compatible**: Works with messages that don't include headers
5. **Priority Logic**: Headers take priority over payload (recommended practice)

## References

- `src/router_nats.erl`: NATS message parsing with headers
- `src/router_result_consumer.erl`: Result consumer with header extraction
- `src/router_ack_consumer.erl`: ACK consumer with header extraction
- `docs/NATS_BEST_PRACTICES.md`: Best practices for headers usage

