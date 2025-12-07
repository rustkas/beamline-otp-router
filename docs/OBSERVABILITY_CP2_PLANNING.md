# Router Observability CP2 Planning

**Date**: 2025-11-30  
**Component**: Router (`apps/otp/router/`)  
**Status**: 📋 **PLANNING DOCUMENT** (CP2)  
**Current Phase**: CP1 (Complete) → CP2 (Planned)

---

## Current CP1 Status

Router observability is **100% CP1 compliant** with the following features:

### ✅ CP1 Features (Completed)

- ✅ **Structured JSON logging** with ISO 8601 timestamps (microseconds)
- ✅ **CP1 correlation fields** at top level (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
- ✅ **PII/secret filtering** (recursive filtering)
- ✅ **All log levels** (ERROR, WARN, INFO, DEBUG)
- ✅ **gRPC health check** (documented and tested)
- ✅ **Comprehensive testing** (unit + integration + E2E)
- ✅ **Full documentation**

**Reference**: `apps/otp/router/docs/OBSERVABILITY.md`

---

## CP2 Planned Features

CP2 will extend Router observability with metrics and distributed tracing:

### 📋 CP2 Features (Planned)

- 📋 **Prometheus metrics** export via HTTP endpoint (`/metrics`)
- 📋 **OpenTelemetry tracing** integration for distributed tracing
- 📋 **Metrics dashboard** (Grafana) for visualization
- 📋 **Alerting rules** (Alertmanager) for production monitoring
- 📋 **Log aggregation** (Loki) for centralized log collection

---

## Observability Stub File

### Current Implementation

The stub file `src/router_observability_stub.erl` provides integration points for CP2 features:

```erlang
-module(router_observability_stub).
-export([
    init_prometheus/0,
    init_opentelemetry/0,
    export_metrics/0
]).
```

**Current State**:
- Stub functions with TODO comments
- No actual implementation (planned for CP2)
- Integration points defined for Prometheus and OpenTelemetry

**File Location**: `apps/otp/router/src/router_observability_stub.erl`

### Stub Functions

#### `init_prometheus/0`

**Purpose**: Initialize Prometheus metrics exporter.

**Current Implementation** (stub):
```erlang
init_prometheus() ->
    %% TODO: Initialize prometheus.erl
    %% prometheus_registry:new(default),
    %% prometheus_http:start(),
    ok.
```

**CP2 Implementation Plan**:
1. Add `prometheus.erl` dependency to `rebar.config`
2. Create Prometheus registry for Router metrics
3. Start HTTP server on `/metrics` endpoint (port 9090 or configurable)
4. Define metrics:
   - `router_requests_total` (counter)
   - `router_request_duration_seconds` (histogram)
   - `router_errors_total` (counter)
   - `router_active_connections` (gauge)
   - `router_nats_messages_total` (counter)

**Dependencies**:
- `prometheus.erl` - Prometheus client library for Erlang
- HTTP server (cowboy or similar) for `/metrics` endpoint

#### `init_opentelemetry/0`

**Purpose**: Initialize OpenTelemetry tracer for distributed tracing.

**Current Implementation** (stub):
```erlang
init_opentelemetry() ->
    %% TODO: Initialize opentelemetry_erlang
    %% opentelemetry:set_tracer_provider(...),
    ok.
```

**CP2 Implementation Plan**:
1. Add `opentelemetry_erlang` dependency to `rebar.config`
2. Configure OpenTelemetry tracer provider
3. Set up trace exporter (OTLP or Jaeger)
4. Instrument Router code with spans:
   - NATS message processing spans
   - Routing decision spans
   - Policy loading spans
   - Provider selection spans

**Dependencies**:
- `opentelemetry_erlang` - OpenTelemetry SDK for Erlang
- Trace exporter (OTLP, Jaeger, or Zipkin)

#### `export_metrics/0`

**Purpose**: Export metrics in Prometheus format.

**Current Implementation** (stub):
```erlang
export_metrics() ->
    %% TODO: Export metrics via HTTP endpoint /metrics
    %% This should be handled by prometheus_http
    ok.
```

**CP2 Implementation Plan**:
1. Metrics are automatically exported via HTTP endpoint `/metrics`
2. Prometheus scrapes metrics from Router
3. Metrics follow Prometheus naming conventions:
   - `router_requests_total{status="success|error",provider="openai|anthropic"}`
   - `router_request_duration_seconds_bucket{le="0.1|0.5|1.0|5.0"}`
   - `router_errors_total{error_type="nats|routing|policy"}`

---

## CP2 Integration Points

### Metrics Integration

**Location**: `src/router_observability_stub.erl`

**Integration Points**:
1. **Request Metrics**: Increment counters in `router_core.erl` when processing requests
2. **Error Metrics**: Increment error counters in error handlers
3. **Latency Metrics**: Record histogram values in request processing paths
4. **Connection Metrics**: Update gauge values for NATS connections

**Example Integration**:
```erlang
%% In router_core.erl
handle_request(Request) ->
    StartTime = erlang:monotonic_time(millisecond),
    try
        Result = process_request(Request),
        router_observability_stub:record_request_success(),
        router_observability_stub:record_latency(StartTime),
        Result
    catch
        Error:Reason ->
            router_observability_stub:record_request_error(Error),
            {error, Reason}
    end.
```

### Tracing Integration

**Location**: `src/router_observability_stub.erl`

**Integration Points**:
1. **NATS Message Spans**: Create spans for NATS message processing
2. **Routing Decision Spans**: Create spans for routing decisions
3. **Policy Loading Spans**: Create spans for policy loading operations
4. **Provider Selection Spans**: Create spans for provider selection logic

**Example Integration**:
```erlang
%% In router_core.erl
handle_nats_message(Message) ->
    Span = router_observability_stub:start_span("router.process_message"),
    try
        router_observability_stub:set_span_attribute(Span, "subject", Message#message.subject),
        Result = process_message(Message),
        router_observability_stub:end_span(Span, ok),
        Result
    catch
        Error:Reason ->
            router_observability_stub:set_span_error(Span, Error, Reason),
            router_observability_stub:end_span(Span, error),
            {error, Reason}
    end.
```

---

## Migration Guide (CP1 → CP2)

### Step 1: Add Dependencies

Update `rebar.config`:
```erlang
{deps, [
    {grpcbox, "~> 0.17.1"},
    {jsx, "~> 3.0"},
    {telemetry, "~> 1.0"},
    {opentelemetry_api, "~> 1.3"},
    %% CP2 additions:
    {prometheus, "~> 4.10"},  %% Prometheus client
    {opentelemetry_erlang, "~> 1.3"},  %% OpenTelemetry SDK
    {opentelemetry_exporter_otlp, "~> 1.3"}  %% OTLP exporter
]}.
```

### Step 2: Implement Stub Functions

Replace stub implementations in `router_observability_stub.erl` with actual implementations:

1. **Prometheus Initialization**:
   - Create Prometheus registry
   - Start HTTP server for `/metrics` endpoint
   - Define metrics (counters, histograms, gauges)

2. **OpenTelemetry Initialization**:
   - Configure tracer provider
   - Set up trace exporter
   - Configure sampling strategy

3. **Metrics Export**:
   - HTTP endpoint automatically exports metrics
   - Prometheus scrapes from `/metrics`

### Step 3: Instrument Router Code

Add metrics and tracing calls throughout Router codebase:

1. **Request Processing**: Record request metrics and create spans
2. **Error Handling**: Record error metrics and set span errors
3. **NATS Operations**: Create spans for NATS message processing
4. **Policy Operations**: Create spans for policy loading and evaluation

### Step 4: Configuration

Add CP2 observability configuration to `sys.config`:

```erlang
[
  {beamline_router, [
    %% CP1 configuration (existing)
    {telemetry_enabled, true},
    {log_dir, ".windsurf/reports"},
    
    %% CP2 configuration (new)
    {prometheus_enabled, true},
    {prometheus_port, 9090},
    {opentelemetry_enabled, true},
    {opentelemetry_endpoint, "http://localhost:4317"},
    {opentelemetry_service_name, "beamline-router"}
  ]}
].
```

### Step 5: Testing

1. **Metrics Testing**:
   - Verify `/metrics` endpoint returns Prometheus format
   - Check metrics are updated correctly
   - Validate metric names and labels

2. **Tracing Testing**:
   - Verify spans are created and exported
   - Check trace context propagation
   - Validate span attributes and events

---

## Metrics Dashboard Planning

### Planned Metrics

**Request Metrics**:
- `router_requests_total` - Total number of requests (counter)
- `router_request_duration_seconds` - Request latency (histogram)
- `router_requests_by_status` - Requests by status (counter with status label)
- `router_requests_by_provider` - Requests by provider (counter with provider label)

**Error Metrics**:
- `router_errors_total` - Total errors (counter)
- `router_errors_by_type` - Errors by type (counter with error_type label)
- `router_nats_errors_total` - NATS errors (counter)
- `router_routing_errors_total` - Routing errors (counter)

**Connection Metrics**:
- `router_active_connections` - Active NATS connections (gauge)
- `router_nats_messages_total` - Total NATS messages (counter)
- `router_nats_messages_by_subject` - Messages by subject (counter with subject label)

**Performance Metrics**:
- `router_policy_load_duration_seconds` - Policy load time (histogram)
- `router_provider_selection_duration_seconds` - Provider selection time (histogram)
- `router_cache_hits_total` - Cache hits (counter)
- `router_cache_misses_total` - Cache misses (counter)

### Grafana Dashboard Structure

**Panels**:
1. **Request Rate**: `rate(router_requests_total[5m])`
2. **Request Latency (p50, p95, p99)**: `histogram_quantile(0.95, router_request_duration_seconds_bucket)`
3. **Error Rate**: `rate(router_errors_total[5m])`
4. **Requests by Provider**: `sum by (provider) (router_requests_by_provider)`
5. **Active Connections**: `router_active_connections`
6. **NATS Messages**: `rate(router_nats_messages_total[5m])`

**Reference**: `apps/otp/router/docs/OBSERVABILITY_DASHBOARD.md` - Metrics dashboard planning and Grafana template

---

## Alerting Rules Planning

### Planned Alert Rules

**High Error Rate**:
```yaml
alert: RouterHighErrorRate
expr: rate(router_errors_total[5m]) > 0.1
for: 5m
labels:
  severity: warning
annotations:
  summary: "Router error rate is high"
  description: "Router error rate is {{ $value }} errors/second"
```

**High Latency**:
```yaml
alert: RouterHighLatency
expr: histogram_quantile(0.95, router_request_duration_seconds_bucket) > 1.0
for: 5m
labels:
  severity: warning
annotations:
  summary: "Router latency is high"
  description: "Router p95 latency is {{ $value }} seconds"
```

**NATS Connection Issues**:
```yaml
alert: RouterNATSConnectionIssues
expr: router_active_connections < 1
for: 1m
labels:
  severity: critical
annotations:
  summary: "Router NATS connection is down"
  description: "Router has no active NATS connections"
```

**Reference**: Alertmanager configuration (planned for CP2)

---

## Log Aggregation Planning

### Loki Integration

**Collection Method**: File-based collection using Promtail

**Configuration** (Promtail):
```yaml
scrape_configs:
  - job_name: router
    static_configs:
      - targets:
          - localhost
        labels:
          job: router
          component: router
          __path__: /var/log/router/router_*.jsonl
```

**Query Examples**:
- Error logs: `{component="router"} |= "ERROR"`
- High latency: `{component="router"} | json | latency_ms > 1000`
- Specific tenant: `{component="router"} | json | tenant_id="tenant_123"`

---

## Current Limitations

### CP1 Limitations

1. **No Metrics**: No Prometheus metrics export (planned for CP2)
2. **No Tracing**: No distributed tracing (planned for CP2)
3. **No Dashboards**: No Grafana dashboards (planned for CP2)
4. **No Alerting**: No Alertmanager integration (planned for CP2)
5. **No Log Aggregation**: Logs are file-based only (Loki planned for CP2)

### Workarounds

- **Metrics**: Use log analysis to extract metrics (not recommended for production)
- **Tracing**: Use `trace_id` in logs for correlation (manual tracing)
- **Dashboards**: Use log analysis tools (limited functionality)
- **Alerting**: Use log monitoring tools (limited functionality)
- **Log Aggregation**: Use file-based log collection (manual setup)

---

## References

- `apps/otp/router/src/router_observability_stub.erl` - Observability stub file
- `apps/otp/router/docs/OBSERVABILITY.md` - CP1 observability documentation
- `apps/otp/router/docs/OBSERVABILITY_DASHBOARD.md` - Metrics dashboard planning (planned)
- `docs/OBSERVABILITY.md` - Unified observability requirements
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants

---

## Next Steps

1. **CP2 Planning**: Complete CP2 observability planning document
2. **Dependencies**: Add Prometheus and OpenTelemetry dependencies
3. **Implementation**: Implement stub functions in `router_observability_stub.erl`
4. **Instrumentation**: Add metrics and tracing calls throughout Router code
5. **Testing**: Create tests for CP2 observability features
6. **Documentation**: Update documentation with CP2 features

**Status**: ✅ **CP1 Complete** → 📋 **CP2 Planned**

