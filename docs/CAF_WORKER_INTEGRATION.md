# CAF Worker Integration

Supplement to Integration Guide explaining CAF Worker integration patterns.

## Overview

**CAF Worker** (`~/aigroup/apps/caf/processor`) is the execution engine that processes tasks assigned by the Router.

### Architecture Flow

```
Router Decision → NATS Assignment → CAF Worker → Execution → NATS Result → Router
```

**Message Flow**:
1. Router publishes `ExecAssignment` to `caf.exec.assign.v1`
2. CAF Worker subscribes and receives assignment
3. Worker validates and publishes `ExecAssignmentAck` to `caf.exec.assign.v1.ack`
4. Worker executes block (HTTP, FS, SQL, etc.)
5. Worker publishes `ExecResult` to `caf.exec.result.v1`
6. Router processes result and continues flow

---

## NATS Subjects

### Input (Router → Worker)

**`caf.exec.assign.v1`**: Execution assignments from Router

**Message Format** (`ExecAssignment`):
```json
{
  "version": "1",
  "assignment_id": "assign-123",
  "request_id": "req-456",
  "tenant_id": "tenant-789",
  "trace_id": "trace-abc",
  "run_id": "run-def",
  "executor": {
    "provider_id": "openai",
    "endpoint": "https://api.openai.com/v1/chat/completions"
  },
  "job": {
    "type": "http.request",
    "method": "POST",
    "url": "https://api.example.com/process",
    "headers": {
      "Content-Type": "application/json"
    },
    "body": "{\"input\":\"test\"}",
    "timeout_ms": 5000
  }
}
```

### Output (Worker → Router)

**`caf.exec.assign.v1.ack`**: Assignment acknowledgment

**Message Format** (`ExecAssignmentAck`):
```json
{
  "assignment_id": "assign-123",
  "status": "accepted",
  "message": "Assignment accepted for execution",
  "request_id": "req-456",
  "trace_id": "trace-abc",
  "tenant_id": "tenant-789",
  "acknowledged_at": 1640995200000
}
```

**`caf.exec.result.v1`**: Execution results

**Message Format** (`ExecResult`):
```json
{
  "assignment_id": "assign-123",
  "status": "success",
  "result": {
    "status_code": 200,
    "body": "{\"output\":\"processed\"}",
    "headers": {
      "Content-Type": "application/json"
    }
  },
  "latency_ms": 150,
  "metadata": {
    "block_type": "http.request",
    "provider_id": "openai"
  },
  "trace_id": "trace-abc",
  "tenant_id": "tenant-789",
  "completed_at": 1640995200150
}
```

**Status Values**:
- `success`: Execution completed successfully
- `error`: Execution failed (retryable or non-retryable)
- `timeout`: Execution timed out
- `cancelled`: Execution was cancelled

**`caf.exec.dlq.v1`**: Dead Letter Queue for non-retriable failures

---

## Block Types

CAF Worker supports multiple block executors:

### 1. HTTP Block

**Type**: `http.request`

**Purpose**: Execute HTTP requests to external APIs

**Configuration**:
```json
{
  "type": "http.request",
  "method": "POST",
  "url": "https://api.example.com/endpoint",
  "headers": {
    "Authorization": "Bearer token",
    "Content-Type": "application/json"
  },
  "body": "{\"key\":\"value\"}",
  "timeout_ms": 5000
}
```

**Result**:
```json
{
  "status_code": 200,
  "body": "{\"result\":\"success\"}",
  "headers": {
    "Content-Type": "application/json"
  },
  "latency_ms": 123
}
```

### 2. FS Block

**Type**: `fs.put` / `fs.get`

**Purpose**: File system operations (blob storage)

**Configuration** (PUT):
```json
{
  "type": "fs.put",
  "path": "/data/output.json",
  "content": "{\"data\":\"value\"}",
  "content_type": "application/json"
}
```

**Configuration** (GET):
```json
{
  "type": "fs.get",
  "path": "/data/input.json"
}
```

**Result**:
```json
{
  "path": "/data/output.json",
  "size_bytes": 1024,
  "content_type": "application/json"
}
```

### 3. SQL Block

**Type**: `sql.query`

**Purpose**: Database queries (read-only for CP1)

**Configuration**:
```json
{
  "type": "sql.query",
  "connection_string": "sqlite://db.sqlite",
  "query": "SELECT * FROM users WHERE id = ?",
  "parameters": ["123"]
}
```

**Result**:
```json
{
  "rows": [
    {"id": "123", "name": "John", "email": "john@example.com"}
  ],
  "row_count": 1
}
```

### 4. Human Block

**Type**: `human.approval`

**Purpose**: Human-in-the-loop approvals with timeout

**Configuration**:
```json
{
  "type": "human.approval",
  "prompt": "Approve this request?",
  "timeout_ms": 60000,
  "approvers": ["user1@example.com", "user2@example.com"]
}
```

**Result**:
```json
{
  "approved": true,
  "approver": "user1@example.com",
  "comment": "Looks good",
  "approved_at": 1640995200000
}
```

---

## Error Handling

### Error Codes

**Validation Errors** (1xxx):
- `1001`: Invalid input
- `1002`: Missing required field
- `1003`: Invalid format

**Execution Errors** (2xxx):
- `2001`: Execution failed
- `2002`: Resource unavailable
- `2003`: Permission denied
- `2004`: Quota exceeded

**Network Errors** (3xxx):
- `3001`: Network error
- `3002`: Connection timeout
- `3003`: HTTP error (4xx/5xx)

**System Errors** (4xxx):
- `4001`: Internal error
- `4002`: System overload

**Cancellation** (5xxx):
- `5001`: Cancelled by user
- `5002`: Cancelled by timeout

### Retry Behavior

**Worker-Side Retry**:
- Automatic retry for transient errors
- Exponential backoff (100ms, 200ms, 400ms, ...)
- Max 3 attempts by default
- Configurable via `retry_config`

**Router-Side Behavior**:
- Receives `error` status from Worker
- Can retry with different provider
- Or fail workflow step

---

## Observability

### Metrics (Prometheus)

**Worker Metrics**:
```
# Task execution count
worker_tasks_total{type="http.request",status="success"} 1234

# Task latency
worker_task_latency_ms_bucket{type="http.request",le="100"} 800
worker_task_latency_ms_bucket{type="http.request",le="500"} 1200

# Resource usage
worker_resource_usage{type="cpu_time_ms"} 5000
worker_resource_usage{type="memory_mb"} 512

# Queue depth
worker_pool_queue_depth{pool="cpu"} 5
worker_pool_queue_depth{pool="io"} 12
```

### Tracing (OpenTelemetry)

**Spans**:
- `worker.assignment.receive`: Receiving assignment from NATS
- `worker.block.execute`: Executing block
- `worker.result.publish`: Publishing result to NATS

**Attributes**:
- `tenant_id`: Tenant identifier
- `trace_id`: Trace ID for correlation
- `run_id`: Run identifier (CP1 observability invariant)
- `block_type`: Type of block being executed
- `worker_id`: Worker instance identifier

### Logs (Structured JSON)

```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "INFO",
  "component": "worker",
  "message": "Step execution completed",
  "fields": {
    "assignment_id": "assign-123",
    "block_type": "http.request",
    "status": "success",
    "latency_ms": 150
  },
  "trace_id": "trace-abc",
  "tenant_id": "tenant-789"
}
```

---

## Configuration

### Command-Line Arguments

```bash
./beamline_worker \
  --cpu-pool-size=8 \
  --gpu-pool-size=2 \
  --io-pool-size=16 \
  --max-memory-mb=2048 \
  --max-cpu-time-ms=7200000 \
  --nats-url=nats://localhost:4222 \
  --prometheus-endpoint=0.0.0.0:9090
```

### Environment Variables

```bash
# NATS Configuration
NATS_URL=nats://localhost:4222

# Resource Limits
WORKER_CPU_POOL_SIZE=8
WORKER_IO_POOL_SIZE=16
WORKER_MAX_MEMORY_MB=2048

# Observability
PROMETHEUS_ENDPOINT=0.0.0.0:9090
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318

# Sandbox Mode (for testing)
WORKER_SANDBOX_MODE=false
```

---

## Health Check

**Endpoint**: `GET /_health`

**Response** (200 OK):
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z"
}
```

**Health Indicators**:
- NATS connection is active
- Resource pools are not overloaded
- No critical errors in recent window

---

## Integration Example: Router → Worker

### 1. Router Makes Decision

```erlang
% In router_decide_handler.erl
Decision = #{
    <<"decision_id">> => <<"dec-123">>,
    <<"provider_id">> => <<"openai">>,
    <<"priority">> => 1
}.
```

### 2. Router Publishes Assignment

```erlang
Assignment = #{
    <<"version">> => <<"1">>,
    <<"assignment_id">> => <<"assign-123">>,
    <<"executor">> => #{
        <<"provider_id">> => <<"openai">>,
        <<"endpoint">> => <<"https://api.openai.com/v1/chat/completions">>
    },
    <<"job">> => #{
        <<"type">> => <<"http.request">>,
        <<"method">> => <<"POST">>,
        <<"url">> => <<"https://api.openai.com/v1/chat/completions">>,
        <<"headers">> => #{<<"Authorization">> => <<"Bearer token">>},
        <<"body">> => <<"{\"prompt\":\"Hello\"}">>
    }
},
router_nats:publish(<<"caf.exec.assign.v1">>, jsx:encode(Assignment)).
```

### 3. Worker Receives and Executes

```cpp
// Worker subscribes to caf.exec.assign.v1
// Receives assignment, validates, executes HTTP block
// Publishes ACK and result
```

### 4. Router Receives Result

```erlang
% Router subscribes to caf.exec.result.v1
handle_result(Result) ->
    case maps:get(<<"status">>, Result) of
        <<"success">> -> 
            % Continue workflow
            ok;
        <<"error">> ->
            % Retry or fail
            error
    end.
```

---

## Testing CAF Worker

### Unit Tests

```bash
cd ~/aigroup/apps/caf/processor/build
make test
```

**Test Coverage**:
- Assignment validation
- Block execution (HTTP, FS, SQL)
- Error handling
- Status reporting
- NATS integration

### Integration Tests

**Prerequisites**:
- NATS running with JetStream
- Router running and listening

**Test Flow**:
1. Publish test assignment to `caf.exec.assign.v1`
2. Worker receives and processes
3. Verify ACK on `caf.exec.assign.v1.ack`
4. Verify result on `caf.exec.result.v1`

---

## Performance Targets

**CP3-LC Gate**:
- **Throughput**: ≥500 tasks/s
- **Latency**:
  - HTTP requests: ≤500ms (internal API calls)
  - File operations: ≤200ms (local FS)
  - SQL queries: ≤300ms (simple SELECT)

**Monitoring**:
```bash
# Check metrics
curl http://localhost:9090/metrics | grep worker
```

---

## Troubleshooting

### Worker Not Receiving Assignments

**Check**:
```bash
# Verify NATS connection
nats sub caf.exec.assign.v1

# Check worker logs
tail -f worker.log
```

**Solution**:
- Verify NATS URL is correct
- Check NATS JetStream is enabled
- Verify subject names match

### Executions Timing Out

**Check**:
- Worker logs for timeout errors
- Block execution latency metrics
- External API response times

**Solution**:
- Increase `timeout_ms` in job config
- Check network connectivity
- Scale worker instances

### High Error Rate

**Check**:
```bash
# Worker metrics
curl http://localhost:9090/metrics | grep worker_tasks_total

# Worker logs
grep "ERROR" worker.log
```

**Common Causes**:
- Invalid job configuration
- External API failures
- Resource exhaustion

---

## Next Steps

- **Integration Testing**: Test Router → Worker → Result flow
- **Performance Testing**: Measure throughput and latency
- **Monitoring**: Set up Prometheus + Grafana
- **Advanced Blocks**: Implement AI inference, media processing (CP3+)

## References

- CAF Worker README: `~/aigroup/apps/caf/processor/README.md`
- Architecture Role: `~/aigroup/apps/caf/processor/docs/ARCHITECTURE_ROLE.md`
- API Contracts: `~/aigroup/docs/API_CONTRACTS.md` (if exists)
- NATS Subjects: Defined in Router codebase
