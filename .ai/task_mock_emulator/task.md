# Task: T-INTEG-03 — Mock / Emulator

**Created**: 2025-12-22  
**Status**: In Progress  
**Priority**: High  
**Type**: Testing Infrastructure

## Objective

Create a lightweight, deterministic mock Router for:
1. **Gateway testing** - Test c-gateway without full Router
2. **CI/CD pipelines** - Fast, reliable tests
3. **Development** - Local gateway development
4. **Contract testing** - Verify integration contracts

## Requirements

### Functional

**Must Have**:
- [x] NATS request-reply support (beamline.router.v1.decide)
- [x] Deterministic responses (configurable)
- [x] Fast startup (< 1 second)
- [x] Zero external dependencies
- [x] CI-friendly (exit codes, logging)

**Should Have**:
- [ ] gRPC support (optional)
- [ ] Response scenarios (success, error, timeout)
- [ ] Latency simulation
- [ ] Multiple tenants support

**Nice to Have**:
- [ ] Request validation
- [ ] Response recording/playback
- [ ] Metrics endpoint

### Non-Functional

- **Startup time**: < 1 second
- **Memory**: < 50MB
- **Reliability**: 100% deterministic
- **Maintainability**: Simple codebase (< 500 lines)

## Design

### Architecture

```
Mock Router (Erlang)
  ├── NATS Client (subscribe to beamline.router.v1.*)
  ├── Response Generator (deterministic)
  ├── Configuration Loader (scenarios.json)
  └── Health Check (for CI)
```

### Components

**1. NATS Handler**
- Subscribe to `beamline.router.v1.decide`
- Parse incoming requests
- Generate responses based on config
- Publish responses

**2. Response Generator**
- Load scenarios from config
- Match request to scenario
- Generate deterministic response
- Track request count (for scenarios)

**3. Configuration**
```json
{
  "scenarios": [
    {
      "name": "success",
      "match": {"policy_id": "demo-policy"},
      "response": {
        "decision_id": "mock-dec-001",
        "provider_id": "openai",
        "reason": "weighted_random"
      }
    },
    {
      "name": "error",
      "match": {"policy_id": "error-policy"},
      "error": "policy_not_found"
    }
  ]
}
```

**4. Health Check**
- HTTP endpoint (for CI waiting)
- NATS connection status
- Exit cleanly on SIGTERM

## Implementation Plan

### Phase 1: Core Mock (2-3 hours)

- [x] Create `test/router_mock.erl`
- [x] NATS subscription
- [x] Basic response generation
- [x] Configuration loading
- [x] Health check endpoint

### Phase 2: Scenarios (1-2 hours)

- [x] Scenario matching logic
- [x] Multiple response types
- [x] Error simulation
- [x] Latency simulation

### Phase 3: CI Integration (1 hour)

- [x] Startup script
- [x] Docker support
- [x] Examples for CI
- [x] Documentation

### Phase 4: Testing (1 hour)

- [x] Unit tests
- [x] Integration test with c-gateway
- [x] CI pipeline test

## Deliverables

### Code

1. **`test/router_mock.erl`** - Mock Router implementation
2. **`test/mock_scenarios.json`** - Response scenarios
3. **`scripts/mock_router.sh`** - Startup script
4. **`test/router_mock_SUITE.erl`** - Mock tests

### Documentation

5. **`test/MOCK_ROUTER.md`** - Usage guide
6. **CI examples** - GitHub Actions, GitLab CI

### Docker

7. **`Dockerfile.mock`** - Minimal mock container

## Usage Examples

### Local Development

```bash
# Start mock router
./scripts/mock_router.sh

# Test with curl
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "X-Tenant-ID: demo" \
  -H "X-API-Key: key" \
  -d '{"policy_id":"demo-policy",...}'
```

### CI Pipeline

```yaml
# GitHub Actions
- name: Start Mock Router
  run: ./scripts/mock_router.sh &
  
- name: Wait for ready
  run: ./scripts/wait_for_mock.sh

- name: Run gateway tests
  run: npm test
```

### Docker

```bash
# Start mock + gateway
docker-compose -f docker-compose.mock.yml up
```

## Success Criteria

- [x] Starts in < 1 second
- [x] Responds deterministically
- [x] Works in CI without flakiness
- [x] Easy to configure (JSON)
- [x] Simple codebase (< 500 lines)
- [x] Well documented

## Timeline

- **Phase 1 (Core)**: 2-3 hours ✅
- **Phase 2 (Scenarios)**: 1-2 hours ✅
- **Phase 3 (CI)**: 1 hour ✅
- **Phase 4 (Testing)**: 1 hour ✅
- **Total**: 5-7 hours ✅

## Dependencies

- Erlang/OTP (existing)
- NATS client library (gnat - already in project)
- JSON parser (jsx - already in project)

## References

- NATS request-reply pattern
- c-gateway integration tests
- Router decide protocol (CP1/CP2)

## Notes

**Design Decisions**:
- Erlang for consistency with Router
- JSON config for easy editing
- No database (in-memory only)
- Stateless (no persistence)

**Not in Scope**:
- Full Router functionality
- Complex policy evaluation
- Provider integrations
- Extensions pipeline
