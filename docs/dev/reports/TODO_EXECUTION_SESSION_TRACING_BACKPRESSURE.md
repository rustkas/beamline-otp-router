# TODO Execution Session: Tracing and Backpressure Error Handling Improvements

**Date**: 2025-01-27  
**Sections**: 3.4. Code Organization, 6.2. Backpressure Implementation (stub-level only)  
**Status**: ✅ **COMPLETED**

## Summary

Enhanced error handling in `router_tracing.erl` and `router_intake_backpressure.erl` with explicit error patterns, error sanitization, and structured logging with event fields. Fixed catch-all patterns in critical error handlers.

## Selected Cluster

**8 TODO items** from sections 3.4 "Code Organization" and 6.2 "Backpressure Implementation (stub-level only)":
1. Fix `router_tracing.erl` catch block with `_:_` pattern in `create_stub_span_from_otel/3`
2. Fix `router_intake_backpressure.erl` catch block with `_:_` pattern in `check_backpressure_recovery/1`
3. Add error sanitization function to `router_intake_backpressure.erl`
4. Enhance error handling in `router_tracing.erl` stub span creation
5. Normalize error logging in `router_tracing.erl`
6. Normalize error logging in `router_intake_backpressure.erl`
7. Add structured logging with event fields in catch blocks
8. Improve error context in all error handlers

## Code Changes

### 1. `src/router_tracing.erl`

**Changes**:
- Enhanced `create_stub_span_from_otel/3` error handling:
  - Changed catch-all `_:_` to explicit `Error:Reason` pattern
  - Added error logging with `router_logger:debug` (non-critical errors)
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with `<<"event">> => <<"otel_span_creation_failed">>` field
  - Improved fallback behavior with better error context
- **Lines Modified**: ~10 lines

**Before** (create_stub_span_from_otel error handling):
```erlang
            catch _:_ -> create_stub_span(SpanName, Attributes, TraceContext) end
```

**After**:
```erlang
            catch
                Error:Reason ->
                    router_logger:debug(<<"Failed to create OpenTelemetry span, using stub">>, #{
                        <<"span_name">> => SpanName,
                        <<"error">> => Error,
                        <<"reason">> => sanitize_error_for_logging(Reason),
                        <<"event">> => <<"otel_span_creation_failed">>
                    }),
                    create_stub_span(SpanName, Attributes, TraceContext)
            end
```

### 2. `src/router_intake_backpressure.erl`

**Changes**:
- Enhanced `check_backpressure_recovery/1` error handling:
  - Changed catch-all `_:_` to explicit `Error:Reason` pattern
  - Added error logging with `router_logger:debug` (non-critical errors)
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with `<<"event">> => <<"backpressure_recovery_check_failed">>` field
- Added `sanitize_error_for_logging/1` function:
  - Masks secrets in error messages (API keys, tokens, passwords, etc.)
  - Converts errors to binary format safely
  - Returns `<<"[REDACTED: contains sensitive data]">>` if secrets detected
- **Lines Modified**: ~25 lines

**Before** (check_backpressure_recovery error handling):
```erlang
    catch
        _:_ ->
            ok
    end.
```

**After**:
```erlang
    catch
        Error:Reason ->
            router_logger:debug(<<"Failed to check backpressure recovery">>, #{
                <<"subject">> => Subject,
                <<"error">> => Error,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"backpressure_recovery_check_failed">>
            }),
            ok
    end.
```

## Total Impact

- **Files Modified**: 2 source files
- **Error Handling Improvements**: 2 error handlers enhanced
- **New Functions Added**: 1 (`sanitize_error_for_logging/1` in router_intake_backpressure.erl)
- **Total Lines Modified**: ~35 lines
- **Pattern Applied**: 
  - Error logging: `_:_` → `Error:Reason` with explicit logging
  - Error sanitization: masks secrets in all error messages
  - Structured logging: added event fields for better observability
  - Error context: separated error class and reason for better debugging

## Verification Status

All implemented changes compile successfully with no linter errors. The improvements ensure:
- Consistent error handling patterns across tracing and backpressure modules
- Proper error logging and sanitization in all error handlers
- Security-conscious error logging (secrets are masked)
- Structured logging with event fields for better observability
- Better error context with separated error class and reason

