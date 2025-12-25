# Extensions Integration Guide

Supplement to Integration Guide explaining the Extensions Pipeline - a powerful mechanism for extending Router behavior without code changes.

## Overview

**Extensions** are separate NATS services that enable custom logic in the routing pipeline **without modifying core Router code**. All extension behavior is configuration-driven through Routing Policies and Extension Registry.

### Key Principle

> An extension is a separate service that communicates with Beamline **only through NATS using a fixed contract**, and enabling/disabling extensions is done **only through configuration/policy**, without changing Gateway, Router, or CAF-workers code.

---

## Architecture

### Extension Types

1. **Pre-processor** - Modifies/enriches incoming message *before* routing
2. **Validator** - Validates request and decides accept/reject
3. **Post-processor** - Modifies provider response *after* execution
4. **Custom Provider** - Acts as another provider (LLM, RAG, CRM, etc.)

### Pipeline Flow

```
Client Request
   ↓
c-gateway (HTTP → NATS)
   ↓
Router receives request
   ↓
Pre-processors (normalize, enrich, transform)
   ↓
Validators (PII guard, rate limit, policy check)
   ↓
Provider Selection (Router decides provider)
   ↓
Provider Execution (OpenAI, Anthropic, Custom Extension Provider)
   ↓
Post-processors (mask PII, transform response)
   ↓
Response to Client
```

---

## NATS Subjects

Each extension has its own NATS subject with versioning:

**Pre-processors**:
- `beamline.ext.pre.normalize_text.v1`
- `beamline.ext.pre.language_detect.v1`
- `beamline.ext.pre.context_enrichment.v1`

**Validators**:
- `beamline.ext.validate.pii_guard.v1`
- `beamline.ext.validate.content_policy.v1`
- `beamline.ext.validate.rate_limit.v1`

**Post-processors**:
- `beamline.ext.post.mask_pii.v1`
- `beamline.ext.post.translate.v1`
- `beamline.ext.post.format_response.v1`

**Custom Providers**:
- `beamline.provider.crm_summarizer.v1`
- `beamline.provider.rag_search.v1`
- `beamline.provider.custom_llm.v1`

---

## Message Contracts

### Pre-processor / Post-processor

**Request** (JSON over NATS):
```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Original text",
    "metadata": {
      "channel": "telegram"
    }
  },
  "metadata": {
    "lang": "en",
    "policy_id": "policy-456"
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
      "channel": "telegram",
      "normalized": "true"
    }
  },
  "metadata": {
    "lang": "en",
    "detected_lang": "en"
  }
}
```

**Behavior**:
- Extension modifies `payload` (message content)
- Extension can enrich `metadata` (context)
- Router merges results and continues pipeline

### Validator

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

**Behavior**:
- `status: "ok"` (or missing) → Continue pipeline
- `status: "reject"` → Apply `on_fail` behavior:
  - `block`: Stop request, return error to client
  - `warn`: Log event but continue
  - `ignore`: Continue silently

### Custom Provider

**Request** (CP2-style):
```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "provider_id": "my_crm_summarizer",
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
  "provider_id": "my_crm_summarizer",
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

**Behavior**:
- Router treats custom provider same as OpenAI/Anthropic
- Selection via Routing Policy `provider_id`

---

## Extension Registry

**Extension Registry** is the source of truth for Router about available extensions.

### Registry Structure

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
- `type`: Extension type (`pre` / `validator` / `post` / `provider`)
- `subject`: NATS subject to call
- `timeout_ms`: Request timeout
- `retry`: Number of retries on failure

**Storage**: PostgreSQL + cache in Mnesia/ETS

---

## Routing Policy

**Routing Policy** defines which extensions are used and in what order.

### Policy Example

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
- `id`: Logical ID (lookup in Registry)
- `mode`:
  - `required`: Failure stops pipeline
  - `optional`: Failure is logged, continues
- `config`: Extension-specific configuration

**`validators[]`** - Validators:
- `id`: Logical ID
- `on_fail`:
  - `block`: Stop request, return error (fail-closed)
  - `warn`: Log event, continue (fail-open with logging)
  - `ignore`: Continue silently (fail-open)

**`providers[]`** - List of provider_ids in priority order

**`post[]`** - Post-processors (same structure as `pre[]`)

---

## Creating an Extension

### 1. Implement Extension Service

**Any language** (Node.js, Go, Rust, Python, C++, Erlang):

```javascript
// Example: Node.js pre-processor
const { connect, JSONCodec } = require('nats');

async function main() {
  const nc = await connect({ servers: 'nats://localhost:4222' });
  const jc = JSONCodec();
  
  const sub = nc.subscribe('beamline.ext.pre.normalize_text.v1');
  
  for await (const msg of sub) {
    const request = jc.decode(msg.data);
    
    // Process message
    const normalized = request.payload.payload.toLowerCase().trim();
    
    const response = {
      payload: {
        ...request.payload,
        payload: normalized,
        metadata: {
          ...request.payload.metadata,
          normalized: 'true'
        }
      },
      metadata: {
        ...request.metadata
      }
    };
    
    msg.respond(jc.encode(response));
  }
}

main();
```

### 2. Register in Extension Registry

Add to database or configuration:

```sql
INSERT INTO extension_registry (extension_id, type, subject, timeout_ms, retry)
VALUES ('normalize_text', 'pre', 'beamline.ext.pre.normalize_text.v1', 80, 0);
```

### 3. Add to Routing Policy

```json
{
  "policy_id": "my_policy",
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required"
    }
  ]
}
```

### 4. Deploy and Test

```bash
# Start extension
NATS_URL=nats://localhost:4222 node normalize_text.js &

# Test directly via NATS
nats req "beamline.ext.pre.normalize_text.v1" \
  '{"payload":{"payload":"Hello World"}}'

# Test via Router
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "X-Tenant-ID: test" \
  -d '{"message":{"payload":"Hello World"},"policy_id":"my_policy"}'
```

---

## Extension Versioning

Extensions support versioning via NATS subjects:

**Version in Subject**:
- `beamline.ext.pre.normalize_text.v1`
- `beamline.ext.pre.normalize_text.v2`

**Version Routing**:
Different tenants/environments can use different versions:

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

**See**: `docs/ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md`

---

## Error Handling

### Extension Timeout

**Behavior**:
-  `mode: "required"` → Request fails
- `mode: "optional"` → Router skips extension, continues

**Metrics**: `router_extension_timeout_total{extension_id}`

### Extension Error Response

**Malformed response**:
- Router logs error
- Treats as timeout based on `mode`

**Network failure**:
- NATS timeout
- Retry based on `retry` count
- After retries, treat as timeout

### Validator Rejection

**On `status: "reject"`**:
- `on_fail: "block"` → Return error to client
- `on_fail: "warn"` → Log warning, continue
- `on_fail: "ignore"` → Continue silently

---

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
  - Attributes: `extension_id`, `extension_type`, `tenant_id`
  - Parent: `router.decide` span

**Propagation**:
- `trace_id` passed in extension request
- Span context propagated via NATS headers

### Logs (Structured JSON)

```json
{
  "timestamp": "2025-12-21T14:50:00Z",
  "level": "INFO",
  "component": "router_extension_invoker",
  "message": "Extension call completed",
  "fields": {
    "extension_id": "normalize_text",
    "latency_ms": 45,
    "status": "success"
  },
  "trace_id": "trace-abc",
  "tenant_id": "tenant-123"
}
```

---

## C-Gateway and Extensions

**Important**: C-Gateway does **NOT** know about extensions.

**Separation**:
- **C-Gateway**: HTTP ↔ NATS adapter (CP1/CP2 layer)
- **Router**: Extension orchestration and policy evaluation

**Implications**:
- Adding/changing/removing extensions **never requires C-Gateway changes**
- C-Gateway only needs to:
  - Convert HTTP to NATS messages
  - Propagate `trace_id` and `tenant_id`
- All extension logic is in Router + Extension Registry + Routing Policies

---

## Testing Extensions

### E2E Tests

**Test Suite**: `apps/otp/router/test/router_extensions_e2e_SUITE.erl`

**Start Reference Extensions**:
```bash
cd tools/extensions
npm install
./start_extensions.sh
```

**Run Tests**:
```bash
cd apps/otp/router
rebar3 ct --suite test/router_extensions_e2e_SUITE
```

### Manual Testing

**Test Extension Directly**:
```bash
nats req "beamline.ext.pre.normalize_text.v1" \
  '{"payload":{"payload":"Hello World"},"metadata":{}}'
```

**Test Full Pipeline**:
```bash
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "X-Tenant-ID: test" \
  -H "X-API-Key: test-key" \
  -d '{
    "message": {"payload": "Hello World"},
    "policy_id": "test_policy"
  }'
```

---

## Use Cases

### 1. Text Normalization (Pre-processor)

```javascript
// normalize_text.js
function normalize(text) {
  return text.toLowerCase().trim();
}
```

**Policy**:
```json
{"pre": [{"id": "normalize_text", "mode": "required"}]}
```

### 2. PII Detection (Validator)

```javascript
// pii_guard.js
function detectPII(text) {
  const creditCardRegex = /\b\d{4}[- ]?\d{4}[- ]?\d{4}[- ]?\d{4}\b/;
  if (creditCardRegex.test(text)) {
    return {status: "reject", reason: "credit_card_detected"};
  }
  return {status: "ok"};
}
```

**Policy**:
```json
{"validators": [{"id": "pii_guard", "on_fail": "block"}]}
```

### 3. PII Masking (Post-processor)

```javascript
// mask_pii.js
function maskEmail(text) {
  return text.replace(/[\w.]+@[\w.]+/g, '[EMAIL]');
}
```

**Policy**:
```json
{"post": [{"id": "mask_pii", "mode": "required"}]}
```

### 4. CRM Integration (Custom Provider)

```javascript
// crm_provider.js
async function summarize(prompt, context) {
  const crmData = await fetch(`https://crm.api/customer/${context.customer_id}`);
  const summary = await generateSummary(prompt, crmData);
  return {
    provider_id: "crm_summarizer",
    output: summary,
    usage: {prompt_tokens: 100, completion_tokens: 50}
  };
}
```

**Policy**:
```json
{"providers": ["crm_summarizer", "openai:gpt-4"]}
```

---

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
- Cache extension results when possible
- Run parallel where possible (future enhancement)

### Throughput

**Target**: 500+ req/s with extensions

**Recommendations**:
- Use connection pooling in extensions
- Implement circuit breakers
- Monitor queue depths
- Scale extensions horizontally

---

## Next Steps

- **Read Full API**: `docs/EXTENSIONS_API.md`
- **E2E Guide**: `docs/EXTENSIONS_E2E_GUIDE.md` (Router docs)
- **Reference Extensions**: `tools/extensions/` (if available)
- **Routing Strategy**: `docs/ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md`

---

## Summary

**Extensions** enable:
- ✅ Custom logic without Router code changes
- ✅ Configuration-driven behavior
- ✅ Multi-tenancy with version routing
- ✅ Language-agnostic implementation
- ✅ Easy deployment and testing

**Key Benefits**:
- Fast iteration (no core deploys)
- Clean separation of concerns
- Testable in isolation
- Observable and traceable
- Backward compatible
