# External Integration Readiness Assessment

**Generated**: 2025-12-20  
**Component**: Beamline Router (Erlang/OTP)

---

## Executive Summary

| Dimension | Status |
|-----------|--------|
| **Contract Stability** | ✅ Stable (v1) |
| **Integration Patterns** | ✅ Well-defined |
| **Documentation** | ✅ Comprehensive |
| **External Dependencies** | ⚠️ NATS required |

---

## 1. Integration Surfaces

The Router exposes the following integration surfaces:

| Surface | Protocol | Direction | Stability |
|---------|----------|-----------|-----------|
| **DecideRequest** | NATS/JSON | Inbound | ✅ Stable v1 |
| **DecideResponse** | NATS/JSON | Outbound | ✅ Stable v1 |
| **ExecAssignment** | NATS/JSON | Outbound | ✅ Stable v1 |
| **ExecAssignmentAck** | NATS/JSON | Inbound | ✅ Stable v1 |
| **ExecResult** | NATS/JSON | Inbound | ✅ Stable v1 |
| **Usage Event** | NATS/JSON | Outbound | ✅ Stable v1 |
| **Metrics** | Prometheus HTTP | Outbound | ✅ Stable |
| **Health Check** | HTTP | Outbound | ✅ Stable |

---

## 2. Integration Pattern: Gateway → Router

### Pattern
```
Gateway (HTTP/REST) → NATS → Router → NATS → Gateway
```

### Requirements for Gateway Integrator

| Requirement | Details |
|-------------|---------|
| **NATS Client** | Must support request-reply pattern |
| **Subject** | Publish to `beamline.router.v1.decide` |
| **Message Format** | JSON (see `DecideRequest` schema) |
| **Required Fields** | `version`, `request_id`, `tenant_id`, `task.type` |
| **Timeout** | 5s default (configurable) |

### Contract: DecideRequest

```json
{
  "version": "1",
  "request_id": "uuid-string",
  "tenant_id": "string",
  "task": {
    "type": "string",
    "payload_ref": "string" 
  }
}
```

### Contract: DecideResponse

```json
{
  "ok": true,
  "decision": {
    "provider_id": "string",
    "priority": 0-100,
    "expected_latency_ms": number,
    "expected_cost": number,
    "reason": "weighted|sticky|fallback|best_score"
  },
  "context": {
    "request_id": "string"
  }
}
```

### Error Codes

| Code | HTTP Equivalent | When |
|------|-----------------|------|
| `invalid_request` | 400 | Validation failed |
| `unauthorized` | 401 | Auth failed |
| `policy_not_found` | 404 | Policy missing |
| `decision_failed` | 500 | No provider |
| `internal` | 500 | System error |

---

## 3. Integration Pattern: Router → CAF Backend

### Pattern
```
Router → NATS → CAF Backend (receives assignment)
CAF Backend → NATS → Router (sends result/ack)
```

### Requirements for CAF Integrator

| Requirement | Details |
|-------------|---------|
| **Subscribe** | `caf.exec.assign.v1` (JetStream recommended) |
| **Publish** | `caf.exec.result.v1`, `caf.exec.assign.v1.ack` |
| **Headers** | Include `trace_id`, `tenant_id`, `version` |
| **Durability** | JetStream consumer with explicit ACK |

### Contract: ExecAssignment (Router → CAF)

```json
{
  "version": "1",
  "assignment_id": "uuid",
  "request_id": "uuid",
  "executor": {
    "provider_id": "string",
    "channel": "nats|grpc"
  },
  "job": {
    "type": "string",
    "payload_ref": "string"
  },
  "options": {
    "deadline_ms": number,
    "retry": { "max_attempts": 2 }
  }
}
```

### Contract: ExecResult (CAF → Router)

```json
{
  "assignment_id": "uuid",
  "request_id": "uuid",
  "status": "success|error|timeout|cancelled",
  "provider_id": "string",
  "latency_ms": number,
  "cost": number
}
```

---

## 4. Integration Pattern: Observability

### Metrics Endpoint

| Endpoint | Format | Data |
|----------|--------|------|
| `/metrics` | Prometheus | All router metrics |

### Key Metrics for Integrators

| Metric | Labels | Purpose |
|--------|--------|---------|
| `router_decide_total` | `tenant_id`, `reason` | Request count |
| `router_decide_latency_seconds` | `tenant_id` | Latency histogram |
| `router_nats_publish_total` | `subject` | NATS activity |
| `router_nats_connection_status` | - | Connection health |

### Tracing

| Header | Format | When |
|--------|--------|------|
| `trace_id` | String | All messages |
| `traceparent` | W3C Trace Context | OpenTelemetry |

---

## 5. Contract Stability Matrix

| Contract | Version | Stability | Breaking Changes |
|----------|---------|-----------|------------------|
| DecideRequest | v1 | ✅ Stable | None planned |
| DecideResponse | v1 | ✅ Stable | None planned |
| ExecAssignment | v1 | ✅ Stable | None planned |
| ExecAssignmentAck | v1 | ✅ Stable | None planned |
| ExecResult | v1 | ✅ Stable | None planned |
| Error Codes | v1 | ✅ Stable | None planned |
| NATS Subjects | v1 | ✅ Stable | None planned |

### Versioning Policy

- Version in `version` field (currently `"1"`)
- New versions will be additive (v2 alongside v1)
- Breaking changes = new subject version (`v2`)
- Deprecation: 6 months notice

---

## 6. Integration Assumptions

### What Router ASSUMES from Integrators

| Assumption | Validation | Consequence if Violated |
|------------|------------|------------------------|
| `tenant_id` is valid | Validated against ACL | `unauthorized` error |
| `version` is `"1"` | Checked first | `invalid_request` error |
| `request_id` is UUID | Format check | `invalid_request` error |
| NATS is available | Health check | Fail-open mode (configurable) |
| JSON is valid | Parse error | `invalid_request` error |

### What Integrators CAN ASSUME from Router

| Guarantee | Evidence |
|-----------|----------|
| Response always includes `ok` field | Contract test |
| `provider_id` always non-empty on success | Contract test |
| Error code is always one of 5 values | Contract test |
| Latency/cost always >= 0 | Contract test |
| Response within timeout | Config `router_request_timeout_ms` |

---

## 7. Integration Gaps

### For Gateway Integrators

| Gap | Impact | Workaround |
|-----|--------|------------|
| No HTTP interface | Must use NATS | Build HTTP-NATS bridge |
| No SDK | Manual integration | Use contract docs |
| No mock server | Testing harder | Use NATS test server |

### For CAF Integrators

| Gap | Impact | Workaround |
|-----|--------|------------|
| No client library | Manual integration | Use contract docs |
| Headers optional | Tracing may break | Always send headers |
| Result correlation | Must track assignment_id | Maintain local map |

### For Observability Integrators

| Gap | Impact | Workaround |
|-----|--------|------------|
| Tracing incomplete | Some spans missing | Check `trace_id` header |
| No dashboards exported | Build own | Use metrics docs |

---

## 8. What is NOT Supported

| Capability | Status | Notes |
|------------|--------|-------|
| HTTP/REST API | ❌ Not supported | Use NATS only |
| gRPC API | ⚠️ Partial | Admin only |
| WebSocket | ❌ Not supported | Use NATS |
| Sync request/response | ✅ Supported | NATS request-reply |
| Async fire-and-forget | ❌ Not recommended | Use push_assignment |
| Multi-region | ❌ Not implemented | Single cluster |
| Custom routing plugins | ❌ Not supported | Extensions limited |

---

## 9. Integration Checklist for New Systems

### Gateway Integration

- [ ] NATS client with request-reply
- [ ] Publish to `beamline.router.v1.decide`
- [ ] Handle `DecideResponse` and `ErrorResponse`
- [ ] Map error codes to HTTP status
- [ ] Add `trace_id` for observability
- [ ] Configure timeout (default 5s)

### CAF Backend Integration

- [ ] Subscribe to `caf.exec.assign.v1`
- [ ] Process `ExecAssignment` messages
- [ ] Publish `ExecResult` to `caf.exec.result.v1`
- [ ] Include headers (`trace_id`, `tenant_id`)
- [ ] Handle deadline from `options.deadline_ms`
- [ ] Implement retry logic if needed

### Monitoring Integration

- [ ] Scrape `/metrics` endpoint
- [ ] Set up alerts for key metrics
- [ ] Configure distributed tracing
- [ ] Build dashboards for visibility

---

## 10. Readiness Summary

### Gateway Integration

| Metric | Score | Notes |
|--------|-------|-------|
| Contract defined | ✅ 100% | API_CONTRACTS.md |
| Examples provided | ✅ 100% | Multiple examples |
| Error handling | ✅ 100% | All codes documented |
| Testing guidance | ⚠️ 70% | No mock server |
| **Overall** | **90%** | Ready for integration |

### CAF Integration

| Metric | Score | Notes |
|--------|-------|-------|
| Contract defined | ✅ 100% | API_CONTRACTS.md |
| Headers documented | ✅ 100% | NATS_SUBJECTS.md |
| Flow documented | ✅ 100% | Interaction flows |
| Correlation guidance | ✅ 100% | assignment_id |
| **Overall** | **95%** | Ready for integration |

### Observability Integration

| Metric | Score | Notes |
|--------|-------|-------|
| Metrics exposed | ✅ 100% | Prometheus format |
| Tracing supported | ⚠️ 80% | OTel partial |
| Logs structured | ✅ 100% | JSON format |
| **Overall** | **85%** | Ready with caveats |

---

## Conclusion

The Router is **ready for external integration** with:
- ✅ Stable v1 contracts
- ✅ Comprehensive documentation
- ✅ Well-defined NATS subjects
- ✅ Standard error handling

**Gaps to address**:
1. No HTTP interface (NATS required)
2. No client SDK/libraries
3. No mock server for testing
4. OTel tracing partially validated

**Recommendation**: Proceed with integration using documented contracts.
