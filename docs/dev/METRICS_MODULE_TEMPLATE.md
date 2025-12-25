# Metrics Module Template (X_rN_metrics Pattern)

**Purpose**: Template for creating metrics access layer modules following the `router_r10_metrics` pattern.

**Pattern**: `<module>_metrics.erl` - Centralized metric names, labels, and access functions for a specific risk theme or module.

## Template Structure

```erlang
-doc "<Module> Metrics Constants and Helpers".
%%
%% Provides centralized metric names and labels for <Module> observability.
%% Aligned with OBSERVABILITY_CONVENTIONS and <Module> specification.
%%
%% @test_category metrics, observability, <module_tag>
-module(<module>_metrics).

-export([
    %% Metric names
    metric_<metric1>_total/0,
    metric_<metric2>_seconds/0,
    metric_<metric3>_gauge/0,
    
    %% Label names
    label_<label1>/0,
    label_<label2>/0,
    
    %% Label values
    <label1>_<value1>/0,
    <label1>_<value2>/0,
    
    %% Metric reading functions
    get_metric_value/2,
    get_<specific>_metric/1,
    wait_for_<specific>_metric/3,
    
    %% Debugging helpers
    dump_metrics/0,
    
    %% Metrics table management
    clear_metrics/0,
    metrics_table_exists/0,
    prune_old_test_metrics/1
]).

%% ========================================================================
%% METRIC NAMES (aligned with OBSERVABILITY_CONVENTIONS)
%% ========================================================================

-doc "<Description>".
-spec metric_<metric1>_total() -> atom().
metric_<metric1>_total() ->
    <module>_<metric1>_total.

%% ========================================================================
%% LABEL NAMES
%% ========================================================================

-doc "<Description>".
-spec label_<label1>() -> atom().
label_<label1>() ->
    <label1>.

%% ========================================================================
%% LABEL VALUES
%% ========================================================================

-doc "<Description>".
-spec <label1>_<value1>() -> binary().
<label1>_<value1>() ->
    ~"<value1>".

%% ========================================================================
%% METRIC READING FUNCTIONS
%% ========================================================================

-doc "Get metric value from ETS (wrapper around router_metrics)".
%% This provides a single entry point for reading <module> metrics
%% @param MetricName Atom name of the metric
%% @param Labels Map of label key-value pairs (empty map for metrics without labels)
%% @returns Metric value (integer/float) or 0 if not found
-spec get_metric_value(atom(), map()) -> integer() | float().
get_metric_value(MetricName, Labels) when map_size(Labels) =:= 0 ->
    router_metrics:ensure(),
    case ets:lookup(router_metrics, MetricName) of
        [{MetricName, Value}] -> Value;
        [] -> 0
    end;
get_metric_value(MetricName, Labels) ->
    router_metrics:ensure(),
    %% Normalize labels to match ETS key format
    LabelsKey = router_metrics:normalize_labels(Labels),
    Key = {MetricName, LabelsKey},
    case ets:lookup(router_metrics, Key) of
        [{Key, Value}] -> Value;
        [] -> 0
    end.

%% ========================================================================
%% DEBUGGING HELPERS
%% ========================================================================

-doc "Dump all metrics for debugging".
%% Safe wrapper for dumping metrics without direct ETS access in tests
-spec dump_metrics() -> list().
dump_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            router_logger:debug("router_metrics ETS undefined", #{
                ~"module" => ~"<module>_metrics",
                ~"function" => ~"dump_metrics"
            }),
            [];
        _ ->
            Metrics = ets:tab2list(router_metrics),
            MetricCount = length(Metrics),
            router_logger:debug("router_metrics snapshot", #{
                ~"module" => ~"<module>_metrics",
                ~"function" => ~"dump_metrics",
                ~"entry_count" => MetricCount
            }),
            %% Log first 50 entries to avoid log spam
            case MetricCount > 50 of
                true ->
                    router_logger:debug("router_metrics entries (first 50)", #{
                        ~"module" => ~"<module>_metrics",
                        ~"function" => ~"dump_metrics",
                        ~"entries" => lists:sublist(Metrics, 50),
                        ~"remaining_count" => MetricCount - 50
                    });
                false ->
                    router_logger:debug("router_metrics entries (all)", #{
                        ~"module" => ~"<module>_metrics",
                        ~"function" => ~"dump_metrics",
                        ~"entries" => Metrics
                    })
            end,
            Metrics
    end.

%% ========================================================================
%% METRICS TABLE MANAGEMENT
%% ========================================================================

-doc "Clear all metrics from ETS table".
%% Safe wrapper for clearing metrics without direct ETS access in tests
-spec clear_metrics() -> ok.
clear_metrics() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(router_metrics)
    end,
    ok.

-doc "Check if metrics table exists".
%% Safe wrapper for checking table existence without direct ETS access in tests
-spec metrics_table_exists() -> boolean().
metrics_table_exists() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> false;
        _ -> true
    end.

-doc "Prune test metrics matching tenant_* or provider_* patterns".
%% Removes all metrics with test tenant/provider IDs (tenant_* / provider_*)
%% @param RetentionMinutes Ignored (kept for API compatibility, but not used)
-spec prune_old_test_metrics(non_neg_integer()) -> {ok, non_neg_integer()}.
prune_old_test_metrics(_RetentionMinutes) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            {ok, 0};
        _ ->
            AllMetrics = ets:tab2list(router_metrics),
            %% Filter metrics with test patterns (tenant_* or provider_*)
            TestMetrics = lists:filter(
                fun({Key, _Value}) ->
                    is_test_key(Key)
                end,
                AllMetrics
            ),
            
            %% Remove test metrics
            lists:foreach(
                fun({Key, _Value}) ->
                    ets:delete(router_metrics, Key)
                end,
                TestMetrics
            ),
            
            {ok, length(TestMetrics)}
    end.

-doc "Check if metric key matches test pattern (tenant_* or provider_*)".
%% @private
is_test_key(Key) when is_tuple(Key) ->
    %% Check if any element in tuple matches test pattern
    lists:any(fun is_test_pattern/1, tuple_to_list(Key));
is_test_key(Key) when is_binary(Key) ->
    is_test_pattern(Key);
is_test_key(Key) when is_atom(Key) ->
    KeyStr = atom_to_list(Key),
    is_test_pattern(list_to_binary(KeyStr));
is_test_key(_Key) ->
    false.

-doc "Check if value matches test pattern (tenant_* or provider_*)".
%% @private
is_test_pattern(Value) when is_binary(Value) ->
    ValueStr = binary_to_list(Value),
    lists:prefix("tenant_", ValueStr) orelse lists:prefix("provider_", ValueStr);
is_test_pattern(Value) when is_atom(Value) ->
    ValueStr = atom_to_list(Value),
    lists:prefix("tenant_", ValueStr) orelse lists:prefix("provider_", ValueStr);
is_test_pattern(_Value) ->
    false.
```

## Usage Guidelines

### 1. Module Naming

- Pattern: `<module>_metrics.erl`
- Examples:
  - `router_r10_metrics.erl` (R10 circuit breaker metrics)
  - `router_r11_metrics.erl` (R11 risk theme metrics)
  - `router_r12_metrics.erl` (R12 risk theme metrics)
  - `router_jetstream_metrics.erl` (JetStream-specific metrics)

### 2. Required Functions

**Metric Names**:
- Export functions that return atom metric names
- Aligned with `OBSERVABILITY_CONVENTIONS.md`

**Label Names**:
- Export functions that return atom label names
- Consistent with metric definitions

**Label Values**:
- Export functions that return binary label values
- Use constants for consistency

**Metric Reading**:
- `get_metric_value/2` - Generic metric reader
- Module-specific helpers (e.g., `get_publish_attempts_total/0`)

**Table Management**:
- `clear_metrics/0` - Clear all metrics
- `metrics_table_exists/0` - Check table existence
- `prune_old_test_metrics/1` - Clean up test metrics

### 3. Test Usage

**CRITICAL**: All tests must use the metrics module, never direct ETS access:

```erlang
%% ✅ CORRECT: Use metrics module
router_r10_metrics:get_metric_value(router_nats_publish_attempts_total, #{}),
router_r10_metrics:clear_metrics(),

%% ❌ WRONG: Direct ETS access
ets:lookup(router_metrics, router_nats_publish_attempts_total),
ets:delete_all_objects(router_metrics),
```

### 4. Integration with Tests

Add lifecycle helpers to `router_test_utils.erl`:

```erlang
-doc "Clear <module> metrics before test".
-spec clear_<module>_metrics() -> ok.
clear_<module>_metrics() ->
    <module>_metrics:clear_metrics(),
    ok.
```

## Example: router_r10_metrics.erl

See `apps/otp/router/src/router_r10_metrics.erl` for a complete implementation example.

## References

- `OBSERVABILITY_CONVENTIONS.md` - Metric naming conventions
- `router_r10_metrics.erl` - Reference implementation
- `router_test_utils.erl` - Test lifecycle helpers

