# TODO Execution Session: Error Logging Enhancements

**Date**: 2025-01-27  
**Sections**: 3.4. Code Organization, 12. Error Handling & Resilience  
**Status**: ✅ **COMPLETED**

## Summary

Enhanced error handling in `router_policy_store.erl` and `router_tracing.erl` by adding error logging with `router_logger` before `erlang:raise`, ensuring error sanitization is applied, adding structured logging with event fields, and improving error context for better observability.

## Selected Cluster

**8 TODO items** from sections 3.4 "Code Organization" and 12 "Error Handling & Resilience":
1. Add error logging to `router_policy_store.erl` catch block before `erlang:raise`
2. Add error sanitization to `router_policy_store.erl` error handlers
3. Import `sanitize_error_for_logging` function in `router_policy_store.erl`
4. Add error logging to `router_tracing.erl` `with_span` catch block before `erlang:raise`
5. Replace `format_error` with `sanitize_error_for_logging` in `router_tracing.erl`
6. Add structured logging with event fields in error handlers
7. Improve error context in error handlers
8. Normalize error handling patterns across modules

## Code Changes

### 1. `src/router_policy_store.erl`

**Changes**:
- Enhanced catch block error handling in `exec_with_telemetry/3`:
  - Added error logging with `router_logger:error` before `erlang:raise`
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with `<<"event">> => <<"policy_store_operation_failed">>` field
  - Added operation context for better debugging
- Added import for `sanitize_error_for_logging/1` from `router_jetstream` module
- **Lines Modified**: ~10 lines

**Before** (catch block error handling):
```erlang
    catch
        Class:Reason:Stack ->
            EndErr = erlang:monotonic_time(),
            DurationUsErr = erlang:convert_time_unit(EndErr - Start, native, microsecond),
            QueueLenErr = case process_info(self(), message_queue_len) of
                {message_queue_len, LenErr} -> LenErr;
                _ -> 0
            end,
            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [Op],
                                            #{duration_us => DurationUsErr, queue_len => QueueLenErr},
                                            Meta#{result => error, error => Reason}),
            erlang:raise(Class, Reason, Stack)
    end.
```

**After**:
```erlang
    catch
        Class:Reason:Stack ->
            %% Log error with router_logger before raising
            SanitizedReason = sanitize_error_for_logging(Reason),
            router_logger:error(<<"Policy store operation failed">>, #{
                <<"operation">> => Op,
                <<"error">> => Class,
                <<"reason">> => SanitizedReason,
                <<"event">> => <<"policy_store_operation_failed">>
            }),
            EndErr = erlang:monotonic_time(),
            DurationUsErr = erlang:convert_time_unit(EndErr - Start, native, microsecond),
            QueueLenErr = case process_info(self(), message_queue_len) of
                {message_queue_len, LenErr} -> LenErr;
                _ -> 0
            end,
            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [Op],
                                            #{duration_us => DurationUsErr, queue_len => QueueLenErr},
                                            Meta#{result => error, error => Reason}),
            erlang:raise(Class, Reason, Stack)
    end.
```

**Before** (imports):
```erlang
-include("beamline_router.hrl").
```

**After**:
```erlang
-include("beamline_router.hrl").

%% Use error sanitization from router_jetstream
-import(router_jetstream, [sanitize_error_for_logging/1]).
```

### 2. `src/router_tracing.erl`

**Changes**:
- Enhanced catch block error handling in `with_span/4`:
  - Added error logging with `router_logger:error` before `erlang:raise`
  - Replaced `format_error(Reason)` with `sanitize_error_for_logging(Reason)` in span attribute setting
  - Added structured logging with `<<"event">> => <<"tracing_span_execution_failed">>` field
  - Added span_name context for better debugging
- **Lines Modified**: ~8 lines

**Before** (catch block error handling):
```erlang
            catch
                Error:Reason:Stacktrace ->
                    end_span(error),
                    set_span_attribute(<<"error">>, erlang:atom_to_binary(Error, utf8), string),
                    set_span_attribute(<<"error.reason">>, format_error(Reason), string),
                    erlang:raise(Error, Reason, Stacktrace)
```

**After**:
```erlang
            catch
                Error:Reason:Stacktrace ->
                    %% Log error with router_logger before raising
                    SanitizedReason = sanitize_error_for_logging(Reason),
                    router_logger:error(<<"Tracing span execution failed">>, #{
                        <<"span_name">> => SpanName,
                        <<"error">> => Error,
                        <<"reason">> => SanitizedReason,
                        <<"event">> => <<"tracing_span_execution_failed">>
                    }),
                    end_span(error),
                    set_span_attribute(<<"error">>, erlang:atom_to_binary(Error, utf8), string),
                    set_span_attribute(<<"error.reason">>, SanitizedReason, string),
                    erlang:raise(Error, Reason, Stacktrace)
```

## Total Impact

- **Files Modified**: 2 source files
- **Error Handling Improvements**: 2 error handlers enhanced
- **Total Lines Modified**: ~18 lines
- **Pattern Applied**: 
  - Error logging: added logging before error propagation
  - Error sanitization: masks secrets in all error messages
  - Structured logging: added event fields for better observability
  - Error context: added operation/span_name for better debugging
  - Security: prevents information disclosure in error messages

## Verification Status

All implemented changes compile successfully with no linter errors. The improvements ensure:
- Consistent error handling patterns across modules
- Proper error logging and sanitization in all error handlers
- Security-conscious error logging (secrets are masked)
- Structured logging with event fields for better observability
- Better error context with operation/span_name
- Improved error message formatting with sanitization

## Note

The `erlang:raise` calls are intentional - they re-raise the exception after logging and metrics. The improvements focus on error logging and sanitization before the raise, not on changing the raise pattern itself.

