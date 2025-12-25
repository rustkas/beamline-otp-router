# Extensions Integration Description

## Обзор

**Механизм**: Extensions Pipeline  
**Документация**: `~/aigroup/docs/EXTENSIONS_API.md`  
**Назначение**: Configuration-driven extensibility without code changes

## Определение

**Extension** - это отдельный NATS сервис, который:
- Имеет свой NATS subject (например, `beamline.ext.pre.normalize_text.v1`)
- Получает и возвращает сообщения по фиксированному контракту
- Регистрируется в Extension Registry по logical `extension_id`
- Подключается к pipeline через Routing Policy без изменения кода

## Типы Extensions

### 1. Pre-processor

**Назначение**: Модификация/обогащение входящего сообщения **до** routing

**NATS Subject Pattern**: `beamline.ext.pre.{extension_id}.v{version}`

**Примеры**:
- Text normalization (lowercase, trim)
- Language detection
- Context enrichment
- Format conversion

**Request**:
```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Original text",
    "metadata": {}
  },
  "metadata": {
    "lang": "en"
  }
}
```

**Response**:
```json
{
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "normalized text",
    "metadata": {
      "normalized": "true"
    }
  },
  "metadata": {
    "lang": "en",
    "detected_lang": "en"
  }
}
```

**Router Behavior**:
- Substitutes updated `payload`
- Merges `metadata` into context
- Continues pipeline

### 2. Validator

**Назначение**: Решение accept/reject запроса

**NATS Subject Pattern**: `beamline.ext.validate.{extension_id}.v{version}`

**Примеры**:
- PII detection (credit cards, SSN)
- Content policy (profanity, violence)
- Rate limiting
- Authorization checks

**Request**: Same as pre-processor

**Success Response**:
```json
{
  "status": "ok"
}
```

**Reject Response**:
```json
{
  "status": "reject",
  "reason": "pii_detected",
  "details": {
    "field": "payload",
    "pattern": "credit_card"
  }
}
```

**Router Behavior**:
- `status: "ok"` (or missing) → Continue
- `status: "reject"` → Apply `on_fail`:
  - `block`: Stop, return error (fail-closed)
  - `warn`: Log, continue (fail-open with logging)
  - `ignore`: Continue silently (fail-open)

### 3. Post-processor

**Назначение**: Модификация provider response **после** execution

**NATS Subject Pattern**: `beamline.ext.post.{extension_id}.v{version}`

**Примеры**:
- PII masking
- Response translation
- Format transformation
- Response enrichment

**Request**: Same structure, but `payload` contains provider response

**Response**: Same as pre-processor

**Router Behavior**:
- Substitutes provider response with processed version
- Enriches metadata
- Returns to client

### 4. Custom Provider

**Назначение**: Acts as another provider (LLM, RAG, CRM, etc.)

**NATS Subject Pattern**: `beamline.provider.{provider_id}.v{version}`

**Примеры**:
- CRM summarizer
- RAG search
- Custom LLM
- Database query service

**Request** (CP2-style):
```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "provider_id": "crm_summarizer",
  "prompt": "User message + context",
  "parameters": {
    "max_tokens": 512
  },
  "context": {
    "conversation_id": "conv-1",
    "customer_id": "cust-42"
  }
}
```

**Response**:
```json
{
  "provider_id": "crm_summarizer",
  "output": "Summarized answer...",
  "usage": {
    "prompt_tokens": 120,
    "completion_tokens": 80
  },
  "metadata": {
    "source": "crm"
  }
}
```

**Router Behavior**:
- Treats custom provider same as OpenAI/Anthropic
- Selection via Routing Policy

## Extension Registry

**Purpose**: Source of truth о доступных extensions

**Storage**: PostgreSQL + cache in Mnesia/ETS

**Structure**:
```json
{
  "normalize_text": {
    "type": "pre",
    "subject": "beamline.ext.pre.normalize_text.v1",
    "timeout_ms": 80,
    "retry": 0
  },
  "pii_guard": {
    "type": "validator",
    "subject": "beamline.ext.validate.pii_guard.v1",
    "timeout_ms": 100,
    "retry": 0
  },
  "mask_pii": {
    "type": "post",
    "subject": "beamline.ext.post.mask_pii.v1",
    "timeout_ms": 100,
    "retry": 0
  },
  "crm_summarizer": {
    "type": "provider",
    "subject": "beamline.provider.crm_summarizer.v1",
    "timeout_ms": 5000,
    "retry": 1
  }
}
```

**Fields**:
- `type`: Extension type (pre / validator / post / provider)
- `subject`: NATS subject
- `timeout_ms`: Request timeout
- `retry`: Number of retries

**Router Usage**:
- Lookup extension by `id` from policy
- Find NATS `subject`, `timeout_ms`, `retry`
- Call extension via NATS
- Handle errors based on config

## Routing Policy

**Purpose**: Defines which extensions are used and in what order

**Example**:
```json
{
  "policy_id": "support_en",
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {
        "lowercase": true
      }
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    }
  ],
  "providers": [
    "openai:gpt-4.1-mini",
    "crm_summarizer"
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {
        "mask_email": true
      }
    }
  ]
}
```

**Fields**:

**`pre[]`** - Pre-processors:
- `id`: Logical extension ID (lookup in Registry)
- `mode`:
  - `required`: Failure stops pipeline
  - `optional`: Failure logged, continues
- `config`: Extension-specific config (per-policy)

**`validators[]`** - Validators:
- `id`: Logical extension ID
- `on_fail`:
  - `block`: Stop request, return error
  - `warn`: Log event, continue
  - `ignore`: Continue silently

**`providers[]`** - List of provider_ids (can include custom extensions)

**`post[]`** - Post-processors (same structure as `pre[]`)

## Pipeline Flow

```
Client Request
   ↓
c-gateway (HTTP → NATS)
   ↓
Router receives beamline.router.v1.decide
   ↓
Execute pre[] in sequence
   ├→ NATS beamline.ext.pre.normalize_text.v1
   ├→ NATS beamline.ext.pre.language_detect.v1
   └→ Context enriched
   ↓
Execute validators[] in sequence
   ├→ NATS beamline.ext.validate.pii_guard.v1
   ├→ NATS beamline.ext.validate.content_policy.v1
   └→ If any reject + on_fail=block → Stop
   ↓
Provider Selection (from policy providers[])
   ↓
Provider Execution
   ├→ OpenAI/Anthropic (standard)
   ├→ NATS beamline.provider.crm_summarizer.v1 (custom)
   └→ NATS caf.exec.assign.v1 (CAF Worker)
   ↓
Execute post[] in sequence
   ├→ NATS beamline.ext.post.mask_pii.v1
   ├→ NATS beamline.ext.post.translate.v1
   └→ Response modified
   ↓
Return to Client
```

## Extension Lifecycle

### 1. Create Extension Service

**Language**: Any (Node.js, Go, Rust, Python, C++, Erlang)

**Requirements**:
- Subscribe to NATS subject
- Implement CP-Ext contract (request/response)
- Handle timeouts gracefully

**Node.js Example**:
```javascript
const { connect, JSONCodec } = require('nats');

async function main() {
  const nc = await connect({ servers: 'nats://localhost:4222' });
  const jc = JSONCodec();
  
  const sub = nc.subscribe('beamline.ext.pre.normalize_text.v1');
  
  for await (const msg of sub) {
    const request = jc.decode(msg.data);
    
    // Process
    const normalized = request.payload.payload.toLowerCase().trim();
    
    const response = {
      payload: {
        ...request.payload,
        payload: normalized
      },
      metadata: request.metadata
    };
    
    msg.respond(jc.encode(response));
  }
}
```

### 2. Register in Extension Registry

**SQL**:
```sql
INSERT INTO extension_registry (extension_id, type, subject, timeout_ms, retry)
VALUES ('normalize_text', 'pre', 'beamline.ext.pre.normalize_text.v1', 80, 0);
```

### 3. Add to Routing Policy

**Policy Update**:
```json
{
  "policy_id": "my_policy",
  "pre": [
    {"id": "normalize_text", "mode": "required"}
  ]
}
```

### 4. Deploy and Test

```bash
# Start extension
NATS_URL=nats://localhost:4222 node normalize_text.js &

# Test directly
nats req "beamline.ext.pre.normalize_text.v1" \
  '{"payload":{"payload":"Hello World"}}'

# Test via Router
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -d '{"message":{"payload":"Hello World"},"policy_id":"my_policy"}'
```

## Extension Versioning

**Subject Versioning**:
- `beamline.ext.pre.normalize_text.v1`
- `beamline.ext.pre.normalize_text.v2`

**Version Routing**:
```json
{
  "normalize_text": {
    "type": "pre",
    "versions": [
      {
        "subject": "beamline.ext.pre.normalize_text.v1",
        "routing_rules": {"environment": "prod"}
      },
      {
        "subject": "beamline.ext.pre.normalize_text.v2",
        "routing_rules": {"environment": "staging"}
      }
    ]
  }
}
```

**Routing Rules**:
- Empty `{}`: Default/fallback
- List values: IN semantics (`tenant_id` in list)
- String values: Exact match (`environment` = "prod")
- Multiple conditions: AND logic

## Error Handling

### Extension Timeout

**Behavior (mode-dependent)**:
- `mode: "required"` → Pipeline fails
- `mode: "optional"` → Router skips, continues

**Metrics**: `router_extension_timeout_total{extension_id}`

### Extension Error Response

**Malformed JSON**:
- Router logs error
- Treats as timeout based on `mode`

**Network Failure**:
- NATS timeout
- Retry based on `retry` count
- After retries, treat as timeout

### Validator Rejection

**On `status: "reject"`**:
- `on_fail: "block"` → Return error to client
- `on_fail: "warn"` → Log warning, continue
- `on_fail: "ignore"` → Continue silently

**Error Response to Client** (on_fail=block):
```json
{
  "error": "validation_failed",
  "message": "Request rejected by validator",
  "details": {
    "validator": "pii_guard",
    "reason": "pii_detected",
    "pattern": "credit_card"
  }
}
```

## Observability

### Metrics (Prometheus)

```
# Extension calls
router_extension_calls_total{extension_id="normalize_text",status="success"} 1234

# Extension latency
router_extension_latency_ms{extension_id="normalize_text",le="50"} 800

# Extension errors
router_extension_errors_total{extension_id="normalize_text",error_type="timeout"} 5
```

### Tracing (OpenTelemetry)

**Spans**:
- `router.extension.call` - Extension invocation
  - Parent: `router.decide`
  - Attributes: `extension_id`, `extension_type`, `tenant_id`

**Propagation**:
- `trace_id` passed in extension request
- Full trace context in NATS headers

### Logs

```json
{
  "timestamp": "2025-12-21T17:10:00Z",
  "level": "INFO",
  "component": "router_extension_invoker",
  "message": "Extension call completed",
  "fields": {
    "extension_id": "normalize_text",
    "latency_ms": 45,
    "status": "success"
  },
  "trace_id": "trace-abc"
}
```

## C-Gateway and Extensions

**Important**: c-gateway НЕ знает об extensions!

**Separation**:
- **c-gateway**: HTTP ↔ NATS adapter (CP1/CP2 layer)
- **Router**: Extension orchestration

**Implications**:
- Adding/changing/removing extensions **never requires c-gateway changes**
- c-gateway only propagates `trace_id`, `tenant_id`
- All extension logic in Router + Registry + Policies

## Testing

### Reference Extensions

**Location**: `~/aigroup/tools/extensions/` (if exists)

**Examples**:
- `normalize_text.js` - Text normalization
- `pii_guard.js` - PII detection validator
- `mask_pii.js` - PII masking post-processor
- `test_provider.js` - Mock provider

### E2E Tests

**Test Suite**: `apps/otp/router/test/router_extensions_e2e_SUITE.erl`

**Tests**:
- Pre-processor execution
- Validator accept/reject
- Post-processor execution
- Custom provider execution
- Full pipeline (pre → validate → provider → post)
- Extension timeout handling
- Extension error handling

**Run**:
```bash
cd ~/aigroup/apps/otp/router
rebar3 ct --suite test/router_extensions_e2e_SUITE
```

## Performance Considerations

### Latency Budget

**Target Total Latency**: < 500ms (p95)

**Breakdown**:
- Pre-processors: 50-100ms total
- Validators: 50-100ms total
- Provider: 200-300ms
- Post-processors: 50-100ms total

**Optimization**:
- Keep extensions fast (< 50ms each)
- Use `mode: "optional"` for non-critical
- Cache results when possible
- Consider parallel execution (future)

### Throughput

**Target**: 500+ req/s with extensions

**Recommendations**:
- Connection pooling in extensions
- Circuit breakers
- Horizontal scaling

## Use Cases

**1. Text Normalization** (Pre):
- Lowercase, trim whitespace
- Remove special characters
- Standardize format

**2. PII Detection** (Validator):
- Credit card numbers
- SSN, phone numbers
- Email addresses

**3. PII Masking** (Post):
- Replace emails with [EMAIL]
- Mask credit cards
- Remove sensitive data

**4. CRM Integration** (Custom Provider):
- Fetch customer data
- Generate summary
- Enrich context

## Summary

**Extensions** - это механизм **configuration-driven extensibility**:
- ✅ No Router code changes
- ✅ Any language (Node.js, Go, Rust, Python)
- ✅ NATS-based communication
- ✅ Version routing per tenant/environment
- ✅ Full observability
- ✅ Easy testing (E2E suite)

**Ключевой принцип**: Вся extension логика living **outside** core
Router - добавление через Registry + Policy, без deploy Router.
