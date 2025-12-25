# How to Run Full Extensions E2E Locally

This guide explains how to run end-to-end tests for the Extensions Pipeline with real NATS services.

## Prerequisites

1. **NATS Server**: Running locally or via Docker
2. **Node.js**: Version 20+ (for reference extensions)
3. **Erlang/OTP**: For Router tests
4. **PostgreSQL**: For Extension Registry (optional, can use fixtures)

## Quick Start

### Option 1: Using Docker Compose (Recommended)

```bash
# Start NATS and extensions
docker-compose -f docker-compose.yml -f tools/extensions/docker-compose.extensions.yml up -d

# Run E2E tests
cd apps/otp/router
rebar3 ct --suite test/router_extensions_e2e_SUITE
```

### Option 2: Manual Setup

#### 1. Start NATS Server

```bash
# Using Docker
docker run -d --name nats -p 4222:4222 -p 8222:8222 nats:2.10-alpine -js -m 8222

# Or using local NATS installation
nats-server -js -m 8222
```

#### 2. Install and Start Extensions

```bash
cd tools/extensions
npm install
./start_extensions.sh
```

Or start individually:

```bash
NATS_URL=nats://localhost:4222 node src/normalize_text.js &
NATS_URL=nats://localhost:4222 node src/pii_guard.js &
NATS_URL=nats://localhost:4222 node src/mask_pii.js &
NATS_URL=nats://localhost:4222 node src/test_provider.js &
```

#### 3. Run E2E Tests

```bash
cd apps/otp/router

# Set NATS URL
export NATS_URL=nats://localhost:4222

# Run tests
rebar3 ct --suite test/router_extensions_e2e_SUITE
```

## Test Suite Overview

**File**: `apps/otp/router/test/router_extensions_e2e_SUITE.erl`

**Tests**:
- `test_e2e_pre_processor` - Pre-processor extension
- `test_e2e_validator` - Validator extension
- `test_e2e_post_processor` - Post-processor extension
- `test_e2e_custom_provider` - Custom provider extension
- `test_e2e_full_pipeline` - Full pipeline (pre → validator → provider → post)
- `test_e2e_multiple_extensions` - Multiple extensions in pipeline
- `test_e2e_extension_timeout` - Extension timeout handling
- `test_e2e_extension_error` - Extension error handling

## Extension Fixtures

Extensions are registered via fixtures in `apps/otp/router/priv/fixtures/extensions/`:

- `normalize_text.json` - Pre-processor
- `pii_guard.json` - Validator
- `mask_pii.json` - Post-processor
- `test_provider.json` - Custom provider

## Reference Extensions

Reference extensions are located in `tools/extensions/`:

- **normalize_text.js** - Text normalization (lowercase, trim)
- **pii_guard.js** - PII detection validator
- **mask_pii.js** - PII masking post-processor
- **test_provider.js** - Mock provider extension

### Extension Contracts

All extensions implement contracts from `docs/EXTENSIONS_API.md`:

**Pre/Post-processors**:
- Request: `{trace_id, tenant_id, payload, metadata}`
- Response: `{payload, metadata}`

**Validators**:
- Request: Same as pre-processor
- Response: `{status: "ok" | "reject", reason?, details?}`

**Custom Providers**:
- Request: CP2-style provider request
- Response: CP2-style provider response

## Troubleshooting

### NATS Connection Issues

```bash
# Check NATS is running
curl http://localhost:8222/healthz

# Check NATS subjects
nats stream ls
nats sub "beamline.ext.>"
```

### Extension Not Responding

```bash
# Check extension logs
tail -f /tmp/normalize_text.log
tail -f /tmp/pii_guard.log
tail -f /tmp/mask_pii.log
tail -f /tmp/test_provider.log

# Test extension directly
nats req "beamline.ext.pre.normalize_text.v1" '{"payload":{"payload":"Hello World"}}'
```

### Router Not Finding Extensions

```bash
# Check Extension Registry
# In Erlang shell:
router_extension_registry:lookup(~"normalize_text").

# Reload extensions
router_extension_registry:reload().
```

### Test Failures

1. **Extension not found**: Ensure extensions are started and NATS is running
2. **Timeout errors**: Check extension response time (should be < timeout_ms)
3. **Invalid response format**: Check extension response matches contract

## Development Workflow

### 1. Modify Extension

```bash
# Edit extension
vim tools/extensions/src/normalize_text.js

# Restart extension
pkill -f normalize_text.js
NATS_URL=nats://localhost:4222 node tools/extensions/src/normalize_text.js &
```

### 2. Update Extension Registry

```bash
# Update fixture
vim apps/otp/router/priv/fixtures/extensions/normalize_text.json

# Reload in Router
# In Erlang shell:
router_extension_registry:reload().
```

### 3. Run Tests

```bash
cd apps/otp/router
rebar3 ct --suite test/router_extensions_e2e_SUITE --case test_e2e_pre_processor
```

## Docker Compose Integration

Extensions can be started alongside other services:

```bash
# Start all services including extensions
docker-compose -f docker-compose.yml -f tools/extensions/docker-compose.extensions.yml up

# Check extension logs
docker logs extension-normalize-text
docker logs extension-pii-guard
docker logs extension-mask-pii
docker logs extension-test-provider
```

## CI/CD Integration

For CI/CD, use Docker Compose:

```yaml
# .github/workflows/extensions-e2e.yml
- name: Start services
  run: |
    docker-compose -f docker-compose.yml -f tools/extensions/docker-compose.extensions.yml up -d
    
- name: Run E2E tests
  run: |
    cd apps/otp/router
    rebar3 ct --suite test/router_extensions_e2e_SUITE
```

## Next Steps

- Add more extension types (custom validators, complex pre-processors)
- Implement extension health monitoring
- Add extension versioning tests
- Test load balancing between multiple instances

## References

- `docs/EXTENSIONS_API.md` - Extension contracts
- `tools/extensions/README.md` - Reference extensions documentation
- `apps/otp/router/test/router_extensions_e2e_SUITE.erl` - E2E test suite

