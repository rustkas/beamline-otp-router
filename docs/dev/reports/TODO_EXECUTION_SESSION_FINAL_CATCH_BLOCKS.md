# TODO Execution Session: Final Catch Block Error Handling Improvements

**Date**: 2025-01-27  
**Sections**: 3.4. Code Organization  
**Status**: ✅ **COMPLETED**

## Summary

Enhanced error handling in `router_tracing.erl` and `router_stress_monitor.erl` with explicit error patterns, error sanitization, and structured logging with event fields. Fixed all remaining catch-all patterns in error handlers.

## Selected Cluster

**8 TODO items** from section 3.4 "Code Organization":
1. Fix `router_tracing.erl` catch block with `_:_` pattern in `get_tracer/0`
2. Fix `router_stress_monitor.erl` catch blocks with `_:_` patterns in `collect_ets_metrics/0`
3. Fix `router_stress_monitor.erl` catch blocks with `_:_` patterns in `collect_performance_metrics/0`
4. Add error sanitization where needed
5. Normalize error logging in `router_tracing.erl`
6. Normalize error logging in `router_stress_monitor.erl`
7. Add structured logging with event fields in catch blocks
8. Improve error context in all error handlers

## Code Changes

### 1. `src/router_tracing.erl`

**Changes**:
- Enhanced `get_tracer/0` error handling:
  - Changed catch-all `catch _:_ -> undefined` to explicit `Error:Reason` pattern
  - Added error logging with `router_logger:debug` (non-critical errors)
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with `<<"event">> => <<"tracer_get_failed">>` field
  - Added tracer name context for better debugging
- **Lines Modified**: ~10 lines

**Before** (get_tracer error handling):
```erlang
            try opentelemetry:get_tracer(?TRACER_NAME) catch _:_ -> undefined end
```

**After**:
```erlang
            try opentelemetry:get_tracer(?TRACER_NAME) catch
                Error:Reason ->
                    router_logger:debug(<<"Failed to get OpenTelemetry tracer">>, #{
                        <<"tracer_name">> => ?TRACER_NAME,
                        <<"error">> => Error,
                        <<"reason">> => sanitize_error_for_logging(Reason),
                        <<"event">> => <<"tracer_get_failed">>
                    }),
                    undefined
            end
```

### 2. `src/router_stress_monitor.erl`

**Changes**:
- Enhanced `collect_ets_metrics/0` error handling:
  - Changed catch-all `catch _:_ -> Acc` to explicit `Error:Reason` pattern in table info collection
  - Changed catch-all `catch _:_ -> #{...}` to explicit `Error:Reason` pattern in outer catch
  - Added error logging with `router_logger:debug` for table info failures (non-critical)
  - Added error logging with `router_logger:error` for overall collection failures
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with event fields
- Enhanced `collect_performance_metrics/0` error handling:
  - Changed catch-all `catch _:_ -> #{...}` to explicit `Error:Reason` pattern
  - Added error logging with `router_logger:error`
  - Added error sanitization using `sanitize_error_for_logging/1`
  - Added structured logging with `<<"event">> => <<"performance_metrics_collection_failed">>` field
- **Lines Modified**: ~25 lines

**Before** (collect_ets_metrics error handling):
```erlang
                catch
                    _:_ ->
                        Acc
                end
        end, #{}, AllTables),
        
        #{
            total_tables => length(AllTables),
            tables => TableInfo
        }
    catch
        _:_ ->
            #{
                total_tables => undefined,
                tables => #{}
            }
    end.
```

**After**:
```erlang
                catch
                    Error:Reason ->
                        router_logger:debug(<<"Failed to collect ETS table info">>, #{
                            <<"table">> => Table,
                            <<"error">> => Error,
                            <<"reason">> => sanitize_error_for_logging(Reason),
                            <<"event">> => <<"ets_table_info_collection_failed">>
                        }),
                        Acc
                end
        end, #{}, AllTables),
        
        #{
            total_tables => length(AllTables),
            tables => TableInfo
        }
    catch
        Error:Reason ->
            router_logger:error(<<"Failed to collect ETS metrics">>, #{
                <<"error">> => Error,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"ets_metrics_collection_failed">>
            }),
            #{
                total_tables => undefined,
                tables => #{}
            }
    end.
```

## Total Impact

- **Files Modified**: 2 source files
- **Error Handling Improvements**: 4 error handlers enhanced
- **Total Lines Modified**: ~35 lines
- **Pattern Applied**: 
  - Error logging: `_:_` → `Error:Reason` with explicit logging
  - Error sanitization: masks secrets in all error messages
  - Structured logging: added event fields for better observability
  - Error context: separated error class and reason for better debugging
  - Logging level: used `router_logger:debug` for non-critical errors, `router_logger:error` for critical failures

## Verification Status

All implemented changes compile successfully with no linter errors. The improvements ensure:
- Consistent error handling patterns across tracing and stress monitor modules
- Proper error logging and sanitization in all error handlers
- Security-conscious error logging (secrets are masked)
- Structured logging with event fields for better observability
- Better error context with separated error class and reason
- Appropriate logging levels (debug for non-critical, error for critical)

