# TODO Execution Session: Core Modules Error Handling Improvements

**Date**: 2025-01-27  
**Sections**: 3.4. Code Organization  
**Status**: ✅ **COMPLETED**

## Summary

Enhanced error handling in 3 core modules (`router_tracing.erl`, `router_metrics.erl`, `router_logger.erl`) with explicit error patterns, error sanitization, and structured logging with event fields. Fixed all catch-all patterns in critical error handlers.

## Selected Cluster

**10 TODO items** from section 3.4 "Code Organization":
1. Fix `router_tracing.erl` catch blocks with `_:_` patterns in metrics emission
2. Fix `router_tracing.erl` catch blocks with `_:_` patterns in span operations
3. Fix `router_metrics.erl` catch block with `_:_` pattern
4. Fix `router_logger.erl` catch block with `_:_` pattern
5. Add error sanitization where needed
6. Normalize error logging in `router_tracing.erl`
7. Normalize error logging in `router_metrics.erl`
8. Normalize error logging in `router_logger.erl`
9. Add structured logging with event fields in catch blocks
10. Improve error context in all error handlers

## Code Changes

### 1. `src/router_tracing.erl`

**Changes**:
- Enhanced 8 error handlers with explicit error patterns:
  - `end_span/1`: Changed `catch _:_ -> ok` to explicit `Error:Reason` with debug logging
  - `set_otel_span_attribute/4`: Changed `catch _:_ -> ok` to explicit `Error:Reason` with debug logging
  - `set_otel_span_status/3`: Changed `catch _:_ -> ok` to explicit `Error:Reason` with debug logging
  - `extract_parent_span_context/1`: Changed `catch _:_ -> undefined` to explicit `Error:Reason` with debug logging
  - `extract_trace_id_from_span_ctx/1`: Changed `catch _:_ -> generate_trace_id()` to explicit `Error:Reason` with debug logging
  - `extract_span_id_from_span_ctx/1`: Changed `catch _:_ -> generate_span_id()` to explicit `Error:Reason` with debug logging
  - `safe_otel_set_status/3`: Changed `catch _:_ -> ok` to explicit `Error:Reason` with debug logging
  - Metrics emission in `end_span/1`: Changed `catch _:_ -> ok` to explicit `Error:Reason` with debug logging
- All error handlers now use:
  - Explicit `Error:Reason` patterns instead of catch-all
  - Error sanitization using `sanitize_error_for_logging/1`
  - Structured logging with `router_logger:debug` and event fields
  - Better error context with separated error class and reason
- **Lines Modified**: ~50 lines

**Before** (end_otel_span error handling):
```erlang
        _ -> try otel_span:end_span(SpanCtx) catch _:_ -> ok end
```

**After**:
```erlang
        _ -> try otel_span:end_span(SpanCtx) catch
            Error:Reason ->
                router_logger:debug(<<"Failed to end OpenTelemetry span">>, #{
                    <<"error">> => Error,
                    <<"reason">> => sanitize_error_for_logging(Reason),
                    <<"event">> => <<"otel_span_end_failed">>
                }),
                ok
        end
```

### 2. `src/router_metrics.erl`

**Changes**:
- Enhanced `emit_metric/3` error handling:
  - Changed catch-all `catch _:_ -> Metadata` to explicit `Error:Reason` pattern
  - Added error logging with `router_logger:debug` (non-critical errors)
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with `<<"event">> => <<"trace_id_get_failed">>` field
- **Lines Modified**: ~10 lines

**Before** (emit_metric error handling):
```erlang
                    catch _:_ -> Metadata end;
```

**After**:
```erlang
                    catch
                        Error:Reason ->
                            router_logger:debug(<<"Failed to get trace ID for metrics">>, #{
                                <<"error">> => Error,
                                <<"reason">> => sanitize_error_for_logging(Reason),
                                <<"event">> => <<"trace_id_get_failed">>
                            }),
                            Metadata
                    end;
```

### 3. `src/router_logger.erl`

**Changes**:
- Enhanced `build_log_context/2` error handling:
  - Changed catch-all `catch _:_ -> undefined` to explicit `Error:Reason` pattern
  - Added error logging with `router_logger:debug` (non-critical errors)
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with `<<"event">> => <<"trace_id_get_failed">>` field
- **Lines Modified**: ~10 lines

**Before** (build_log_context error handling):
```erlang
                    try router_tracing:get_trace_id() catch _:_ -> undefined end;
```

**After**:
```erlang
                    try router_tracing:get_trace_id() catch
                        Error:Reason ->
                            router_logger:debug(<<"Failed to get trace ID from router_tracing">>, #{
                                <<"error">> => Error,
                                <<"reason">> => sanitize_error_for_logging(Reason),
                                <<"event">> => <<"trace_id_get_failed">>
                            }),
                            undefined
                    end;
```

## Total Impact

- **Files Modified**: 3 source files
- **Error Handling Improvements**: 10 error handlers enhanced
- **Total Lines Modified**: ~70 lines
- **Pattern Applied**: 
  - Error logging: `_:_` → `Error:Reason` with explicit logging
  - Error sanitization: masks secrets in all error messages
  - Structured logging: added event fields for better observability
  - Error context: separated error class and reason for better debugging
  - Logging level: used `router_logger:debug` for non-critical errors (metrics, trace ID retrieval)

## Verification Status

All implemented changes compile successfully with no linter errors. The improvements ensure:
- Consistent error handling patterns across core modules (tracing, metrics, logger)
- Proper error logging and sanitization in all error handlers
- Security-conscious error logging (secrets are masked)
- Structured logging with event fields for better observability
- Better error context with separated error class and reason
- Appropriate logging levels (debug for non-critical failures)

