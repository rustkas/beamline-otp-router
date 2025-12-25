# Gen Server Reset/Lifecycle Pattern

**Purpose**: Template for implementing safe reset and lifecycle management in gen_servers that use ETS tables and participate in tests.

**Pattern**: `init/1` → `do_init/1` + `handle_call(reset_all, ...)` + lifecycle helpers in `*_test_utils`.

## Problem Statement

When gen_servers with ETS tables participate in Common Test suites, we face several challenges:

1. **ETS Table Lifecycle**: ETS tables can persist across test runs, causing test pollution
2. **Process Crashes**: Direct ETS manipulation in `init/1` can crash the gen_server
3. **Test Isolation**: Tests need to reset state without restarting the entire process
4. **Supervisor Conflicts**: CT and supervisor can conflict over ETS table ownership

## Solution Pattern

### 1. Safe Initialization: `init/1` → `do_init/1`

**Pattern**: Wrap initialization logic in `do_init/1` to catch errors safely.

```erlang
-doc "Initialize gen_server (safe wrapper)".
init(Args) ->
    process_flag(trap_exit, true),
    try
        do_init(Args)
    catch
        Class:Reason:Stack ->
            error_logger:error_msg(
                "~p init failed: ~p:~p~nStack: ~p~n",
                [?MODULE, Class, Reason, Stack]
            ),
            {stop, {init_failed, Class, Reason}}
    end.

-doc "Internal initialization logic (safe, with error handling)".
%% All initialization logic is here to avoid crashes in init/1
-spec do_init(list()) -> {ok, #state{}} | {stop, term()}.
do_init(_Args) ->
    %% Ensure ETS table exists (safe creation/cleanup)
    Table = ensure_ets_table(?TABLE),
    
    State = #state{
        table = Table
    },
    
    {ok, State}.
```

**Benefits**:
- Errors in `do_init/1` don't crash the gen_server
- Stack traces are logged for debugging
- Process can stop gracefully on initialization failure

### 2. Safe ETS Table Creation

**Pattern**: Check for existing table, delete if exists, then create fresh.

```erlang
-doc "Safely ensure ETS table exists".
%% Handles existing table cleanup and creation
-spec ensure_ets_table(atom()) -> atom().
ensure_ets_table(Name) when is_atom(Name) ->
    case ets:info(Name) of
        undefined ->
            %% Table doesn't exist - create it
            ets:new(Name, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true},
                {keypos, 2}  % Use the 'key' field (2nd element) as ETS key
            ]);
        _Info ->
            %% Table already exists - delete and recreate (cleanup from previous run/test)
            ets:delete(Name),
            ets:new(Name, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true},
                {keypos, 2}
            ])
    end.
```

**Benefits**:
- Handles leftover tables from previous test runs
- Ensures clean state for each initialization
- Prevents "table already exists" errors

### 3. Safe Reset: `handle_call(reset_all, ...)`

**Pattern**: Provide a reset mechanism that clears ETS state without killing the process.

```erlang
handle_call(reset_all, _From, State = #state{table = Table}) ->
    %% Safe reset: clear all states but keep process and ETS table alive
    %% This is called from test utilities, should not kill the process
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(~"~p reset_all: ETS table undefined", #{
                ~"module" => atom_to_binary(?MODULE, utf8),
                ~"event" => ~"reset_all"
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(~"~p reset_all: table cleared", #{
                ~"module" => atom_to_binary(?MODULE, utf8),
                ~"event" => ~"reset_all"
            }),
            {reply, ok, State}
    end;
```

**Benefits**:
- Tests can reset state without restarting the process
- Process remains alive for subsequent tests
- Safe handling of missing tables

### 4. Lifecycle Helpers in `*_test_utils`

**Pattern**: Provide standardized helpers for test lifecycle management.

```erlang
-doc "Ensure <module> process is alive".
%% Fails immediately if process is not running (no fallback start)
-spec ensure_<module>_alive() -> ok.
ensure_<module>_alive() ->
    ensure_<module>_alive(20).  % 20 retries × 100ms = 2 seconds

ensure_<module>_alive(0) ->
    %% Diagnostic: check supervisor and children
    _ = dump_supervisor_children(),
    SupPid = whereis(beamline_router_sup),
    SupInfo = case SupPid of
        undefined -> {supervisor_not_found};
        P when is_pid(P) ->
            case catch supervisor:which_children(beamline_router_sup) of
                Children when is_list(Children) ->
                    Child = lists:keyfind(<module>, 1, Children),
                    {supervisor_found, P, children_count, length(Children), child, Child};
                Error -> {supervisor_error, Error}
            end
    end,
    ct:fail({<module>_not_started, supervisor_info, SupInfo});

ensure_<module>_alive(Retries) ->
    case whereis(<module>) of
        undefined ->
            timer:sleep(100),
            ensure_<module>_alive(Retries - 1);
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ok;
                false ->
                    timer:sleep(100),
                    ensure_<module>_alive(Retries - 1)
            end
    end.

-doc "Reset <module> state (safe, via gen_server:call)".
-spec reset_<module>() -> ok.
reset_<module>() ->
    case whereis(<module>) of
        undefined ->
            %% In tests: not started yet, not an error
            ct:pal("reset_<module>: ~p not started", [?MODULE]),
            ok;
        _ ->
            case gen_server:call(<module>, reset_all, 5000) of
                ok ->
                    ok;
                Other ->
                    ct:fail({reset_<module>_failed, Other})
            end
    end.
```

**Benefits**:
- Standardized test setup/teardown
- Consistent error handling
- Easy to use across test suites

## Complete Example: router_circuit_breaker.erl

See `apps/otp/router/src/router_circuit_breaker.erl` for a complete implementation:

- `init/1` → `do_init/1` pattern (lines 167-191)
- `ensure_ets_table/1` function (lines 193-219)
- `handle_call(reset_all, ...)` (lines 221-237)

## Test Usage

```erlang
init_per_testcase(_TestCase, Config) ->
    %% Idempotent start (safe for --case execution)
    ok = start_router_app(),
    
    %% Verify process is alive (fail immediately if not)
    ok = ensure_<module>_alive(),
    
    %% Reset state before each test (safe, via gen_server:call)
    ok = reset_<module>(),
    
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.
```

## Migration Checklist

For existing gen_servers with ETS tables:

- [ ] Wrap `init/1` logic in `do_init/1` with try-catch
- [ ] Implement `ensure_ets_table/1` with cleanup logic
- [ ] Add `handle_call(reset_all, ...)` for safe reset
- [ ] Add lifecycle helpers to `router_test_utils.erl`
- [ ] Update test suites to use lifecycle helpers
- [ ] Remove direct ETS access from tests (use metrics modules)

## Benefits

1. **Test Isolation**: Each test starts with clean state
2. **Process Stability**: Errors don't crash the gen_server
3. **Debugging**: Better error messages and stack traces
4. **Maintainability**: Consistent pattern across modules
5. **Reliability**: Handles edge cases (missing tables, crashed processes)

## References

- `router_circuit_breaker.erl` - Reference implementation
- `router_test_utils.erl` - Lifecycle helpers
- `METRICS_MODULE_TEMPLATE.md` - Metrics access layer pattern

