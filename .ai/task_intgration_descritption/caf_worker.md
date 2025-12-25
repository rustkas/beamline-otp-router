# CAF Worker Integration Description

## Обзор

**Компонент**: CAF Worker (Processor)  
**Язык**: C++ (CAF - C++ Actor Framework)  
**Расположение**: `~/aigroup/apps/caf/processor`  
**Назначение**: Execution engine для Flow DSL blocks

## Архитектура

### Core Components

1. **Worker Actor** - Main coordinator
2. **Pool Actors** - CPU/GPU/IO resource pools
3. **Executor Actors** - Individual block execution
4. **Scheduler** - Resource-aware task assignment
5. **Sandbox** - Mock execution environment

## NATS Integration

### Input Subject

**`caf.exec.assign.v1`** - Assignments from Router

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

**Required Fields**:
- `version`: "1"
- `assignment_id`: Unique assignment identifier
- `request_id`: Request tracking ID
- `tenant_id`: Tenant identifier
- `executor.provider_id`: Provider identifier
- `job.type`: Block type (http.request, fs.put, etc.)

### Output Subjects

**`caf.exec.assign.v1.ack`** - Assignment acknowledgment

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

**Status Values**:
- `accepted`: Worker will process
- `rejected`: Worker cannot process (invalid config, quota exceeded)

**`caf.exec.result.v1`** - Execution results

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
- `error`: Execution failed (may be retryable)
- `timeout`: Execution timed out
- `cancelled`: Execution was cancelled

**`caf.exec.dlq.v1`** - Dead Letter Queue

**Purpose**: Non-retriable failures

## Block Executors (CP1)

### 1. HTTP Block

**Type**: `http.request`

**Job Configuration**:
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

**Features**:
- Streaming support
- Connection pooling
- Retry on network errors

### 2. FS Block

**Types**: `fs.put`, `fs.get`

**PUT Configuration**:
```json
{
  "type": "fs.put",
  "path": "/data/output.json",
  "content": "{\"data\":\"value\"}",
  "content_type": "application/json"
}
```

**GET Configuration**:
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
  "content_type": "application/json",
  "content": "{\"data\":\"value\"}"
}
```

**Security**:
- Path restrictions (safe directories only)
- No directory traversal

### 3. SQL Block

**Type**: `sql.query`

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

**Security**:
- Parameterized queries only (no SQL injection)
- Read-only queries (CP1)
- Connection pooling

### 4. Human Block

**Type**: `human.approval`

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

**Behavior**:
- Timeout after `timeout_ms`
- Notifies approvers
- Waits for response

## Error Handling

### Error Codes

**Validation Errors (1xxx)**:
- `1001`: Invalid input
- `1002`: Missing required field
- `1003`: Invalid format

**Execution Errors (2xxx)**:
- `2001`: Execution failed
- `2002`: Resource unavailable
- `2003`: Permission denied
- `2004`: Quota exceeded

**Network Errors (3xxx)**:
- `3001`: Network error
- `3002`: Connection timeout
- `3003`: HTTP error (4xx/5xx)

**System Errors (4xxx)**:
- `4001`: Internal error
- `4002`: System overload

**Cancellation (5xxx)**:
- `5001`: Cancelled by user
- `5002`: Cancelled by timeout

### Retry Behavior

**Worker-Side**:
- Automatic retry for transient errors
- Exponential backoff (100ms, 200ms, 400ms)
- Max 3 attempts default
- Configurable via `retry_config`

**Error Result Format**:
```json
{
  "assignment_id": "assign-123",
  "status": "error",
  "error": {
    "code": 3001,
    "message": "Network error",
    "retryable": true,
    "details": {
      "block_type": "http.request",
      "url": "https://api.example.com/process"
    }
  },
  "latency_ms": 5000
}
```

## Observability

### Metrics (Prometheus)

**Endpoint**: `http://localhost:9090/metrics`

**Key Metrics**:
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

**Configuration**:
- `OTEL_EXPORTER_OTLP_ENDPOINT` - OTLP endpoint

**Spans**:
- `worker.assignment.receive` - Receiving assignment
- `worker.block.execute` - Block execution
- `worker.result.publish` - Publishing result

**Attributes**:
- `tenant_id`: Tenant identifier
- `trace_id`: Trace ID
- `run_id`: Run identifier (CP1 invariant)
- `block_type`: Block type
- `worker_id`: Worker instance ID

### Logs (Structured JSON)

**Format**:
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
# NATS
NATS_URL=nats://localhost:4222

# Resource Limits
WORKER_CPU_POOL_SIZE=8
WORKER_IO_POOL_SIZE=16
WORKER_MAX_MEMORY_MB=2048

# Observability
PROMETHEUS_ENDPOINT=0.0.0.0:9090
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318

# Sandbox Mode (testing)
WORKER_SANDBOX_MODE=false
```

## Health Check

**Endpoint**: `GET /_health`

**Response (200 OK)**:
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z"
}
```

**Health Indicators**:
- NATS connection active
- Resource pools not overloaded
- No critical errors

## Multi-tenancy

### Resource Quotas

**Per-tenant limits**:
- CPU time
- Memory usage
- Execution count

**Enforcement**:
- Quota check before execution
- `2004` error if quota exceeded

### Isolation

**Execution contexts**:
- Separate per tenant
- No data sharing
- Independent resource pools

### Fair Scheduling

**Algorithms**:
- Round-robin
- Priority-based
- Quota-aware

## Performance Targets

**CP3-LC Gate**:
- **Throughput**: ≥500 tasks/s
- **Latency**:
  - HTTP requests: ≤500ms (internal APIs)
  - File operations: ≤200ms (local FS)
  - SQL queries: ≤300ms (simple SELECT)

## Testing

### Build and Test

```bash
cd ~/aigroup/apps/caf/processor
mkdir build && cd build
cmake ..
make -j$(nproc)
make test
```

### Test Coverage

**Suites**:
- Assignment validation
- Block execution (HTTP, FS, SQL)
- Error handling
- Status reporting
- NATS integration

**Files**:
- `tests/worker_assignment_SUITE.cpp`
- `tests/worker_blocks_SUITE.cpp`
- `tests/worker_status_SUITE.cpp`

## Integration with Router

### Message Flow

```
Router Decision
   ↓
Router publishes ExecAssignment to caf.exec.assign.v1
   ↓
Worker subscribes and receives
   ↓
Worker validates and publishes ACK to caf.exec.assign.v1.ack
   ↓
Worker executes block (HTTP/FS/SQL/Human)
   ↓
Worker publishes ExecResult to caf.exec.result.v1
   ↓
Router processes result and continues flow
```

### StepResult Contract (CP1 Invariant)

**Type**:
```cpp
struct StepResult {
    StepStatus status;        // ok | error | timeout | cancelled
    ErrorCode error_code;     // Machine-readable error code
    ResultMetadata metadata;  // Correlation metadata
    std::unordered_map<std::string, std::string> outputs;
    std::string error_message;
    int64_t latency_ms;
    int32_t retries_used;
};
```

**Required Fields**:
- `status`: Execution status
- `error_code`: ErrorCode enum
- `metadata`: trace_id, run_id, flow_id, step_id, tenant_id

**Conversion**: StepResult → ExecResult JSON

## Production Deployment

### Scaling

**Horizontal**:
- Run multiple worker instances
- NATS queue groups for load distribution
- Share resource pools via coordination

**Vertical**:
- CPU: 4-8 cores per instance
- Memory: 2-4GB per instance

### Monitoring

**Key Metrics**:
- Task execution rate
- Latency per block type
- Error rate
- Queue depth

**Alerts**:
- High error rate (> 5%)
- High latency (p95 > 1s)
- Queue backlog (> 100)
- NATS disconnected

## Documentation References

- **README**: `~/aigroup/apps/caf/processor/README.md`
- **Architecture Role**: `~/aigroup/apps/caf/processor/docs/ARCHITECTURE_ROLE.md`
- **CP1 Profile**: `~/aigroup/apps/caf/processor/docs/CP1_WORKER_CORE_PROFILE.md`

## Summary

CAF Worker - это **execution engine** для Flow DSL:
- ✅ Actor-based parallelism (CPU/GPU/IO pools)
- ✅ 4 block types (HTTP, FS, SQL, Human)
- ✅ NATS integration (assign → ACK → result)
- ✅ Multi-tenant isolation
- ✅ Error handling with retry
- ✅ Full observability (metrics, tracing, logs)

**Ключевой принцип**: Worker выполняет blocks от Router через NATS с четким контрактом StepResult.
