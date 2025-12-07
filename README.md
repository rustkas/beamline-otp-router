# Router Core (Erlang/OTP)  CP1

Minimal implementation of the routing core for CP1 with gRPC and ABI support.

## Description

Router Core processes message routing requests, applies routing policies (static/rule-set for CP1), and returns provider selection decisions in an ABIcompatible format.

## Architecture

### OTP Process Tree

```
beamline_router_sup (supervisor)
  router_policy_store (gen_server)  ETS cache of policies, loaded from fixtures
  router_grpc_sup (supervisor)  gRPC server
```

### Core Modules

- `router_core.erl`  decision interface, minimal algorithm stub
- `router_decider.erl`  minimal algorithm implementation (weights/sticky/fallback)
- `router_policy_store.erl`  in-memory ETS policy cache, loading from fixtures
- `router_grpc.erl`  gRPC server (grpcbox), implements Router.Decide
- `router_nats_adapter.erl`  NATS interface (mock/stub for CP1)

## Requirements

- Erlang/OTP 26+
- rebar3
- grpcbox (pulled automatically via rebar3)
- jsx (pulled automatically via rebar3)

## Project Structure

```
apps/otp/router/
  src/                          # Source code
    router_core.erl            # Decision interface
    router_decider.erl         # Minimal algorithm stub
    router_policy_store.erl    # ETS cache, load from fixtures
    router_grpc.erl            # gRPC Router.Decide service
    router_nats_adapter.erl    # NATS interface (mock)
    ...
  include/                      # Header files
    beamline_router.hrl
  priv/
    fixtures/
      policies/                # Policy fixtures
  test/                         # Tests
    router_core_SUITE.erl      # Common Test suite
    router_grpc_smoke.erl      # gRPC smoke test
  rebar.config                  # rebar3 configuration
  README.md                     # This file
```

## Operational Readiness (CP1)

### Status

- **Compilation**: ✅ Successful
- **Tests**: ✅ New integration tests added and compile
- **Dialyzer**: ✅ Warnings addressed (dependencies updated, targeted nowarn)
- **Configuration**: ✅ Finalized (NATS TLS, timeouts, reconnects)
- **Documentation**: ✅ Updated across `CONFIG.md`, `API_CONTRACTS.md`, `ROUTER_CAF_GATING_CHECK_REPORT.md`

### Key Artifacts

- **`OPERATIONAL_GUIDE.md`**: Pre-production checklist, configuration guidance, smoke tests, gradual rollout strategy, emergency procedures, monitoring recommendations
- **New Integration Tests**: 
  - `test_payload_size_limit/1`: Validates NATS payload size limit
  - `test_version_validation_missing/1`: Validates missing version rejection
  - `test_version_validation_unsupported/1`: Validates unsupported version rejection
- **Updated Documentation**:
  - `ROUTER_CAF_GATING_CHECK_REPORT.md`: Dialyzer warnings details and resolution
  - `CONFIG.md`: NATS configuration (TLS, limits, timeouts)
  - `API_CONTRACTS.md`: Schema version validation rules

### Staging Rollout Checklist

Before deploying to staging, ensure:

1. **Pre-Production Checklist**: Follow checklist in `OPERATIONAL_GUIDE.md`
2. **NATS Payload Size Limit**: Validate payload size limit before JSON parsing
3. **Schema Version Validation**: Confirm validation for missing/unsupported versions
4. **Retry Parameters**: Set `caf_max_retries` and `caf_retry_base_ms` per SLA/latency guidance
5. **Smoke Tests**: Run 5 smoke tests from `OPERATIONAL_GUIDE.md` and monitor telemetry counters/spans
6. **Kill-Switch and Allowlist**: Keep `caf_push_assignment_enabled` and `caf_push_assignment_allowed_tenants` aligned with access policies

See `docs/OPERATIONAL_GUIDE.md` for detailed staging rollout plan.

### Training Videos Quickstart

**Recommended Topics** (for future video series):

1. **Architecture Overview**: Router ↔ CAF modules, configuration, message flow
2. **Quickstart with NATS TLS**: Setup, connection, verification
3. **DecideRequest Validation**: Schema version rules and tests
4. **ExecAssignment Publishing**: `push_assignment` flag and subject configuration
5. **Deadlines**: `deadline_ms` calculation and overrides
6. **Retry Logic**: Exponential backoff with jitter and counters
7. **Publication Controls**: Kill-switch and tenant allowlist
8. **Observability**: Telemetry counters and spans
9. **Error Handling**: NATS limits, routing errors, schema versions
10. **Smoke Tests & Dashboard**: Staging validation and monitoring

## CP1 Limitations

- No telemetry: Prometheus/OTel excluded from dependencies
- Minimal algorithm: stub logic (weights/sticky/fallback)

## Capabilities

- Inmemory policy caching: ETS-backed cache for policies loaded from JSON fixtures (fast lookup of requests/responses).
- gRPC service: `Router.Decide` implemented in `router_grpc.erl` with full Protobuf serialization/deserialization.
- API documentation: see [docs/GRPC_API.md](docs/GRPC_API.md) for call examples, response schema, and error codes.
  - [Retryability and error handling](docs/GRPC_API.md#retryability-unavailable-vs-resource_exhausted)
  - [Canonical reasons](docs/GRPC_API.md#canonical-reasons-summary)
  - [Playbook: diagnostics and correlation](docs/GRPC_API.md#playbook-diagnostics-and-correlation)

## Libraries

- `grpcbox`  gRPC for Erlang: server/client, Protobuf handling, ABI at wire-protocol level.
- `jsx`  JSON parser for simple serialization/deserialization and helper tasks.

## Sample Policy (JSON)

```json
{
  "policy_id": "default",
  "providers": [
    {"id": "openai", "weight": 0.7},
    {"id": "anthropic", "weight": 0.3}
  ],
  "rules": [
    {"when": {"message_type": "chat"}, "prefer": ["openai"]},
    {"when": {"all_providers_failed": true}, "fallback": "anthropic"}
  ],
  "sticky": true
}
```

## Commands for Verification

```bash
# Generate Protobuf (after changing flow.proto)
# Important: proto files must be under apps/otp/router/proto/
rebar3 clean
rebar3 protobuf compile

# Compile
rebar3 compile

# Tests
rebar3 ct --suite test/router_core_SUITE
rebar3 ct --suite test/router_grpc_SUITE

# Concurrency test for RouterAdmin
rebar3 ct --suite test/router_admin_grpc_concurrency_SUITE

# Property-based tests (PropEr)
rebar3 as test ct --suite test/router_policy_store_prop_SUITE
rebar3 as test ct --suite test/router_options_merge_prop_SUITE
rebar3 as test ct --suite test/router_normalize_boolean_prop_SUITE
rebar3 as test ct --suite test/router_decider_prop_SUITE

# Parallel test execution (recommended for faster runs)
rebar3 ct -j 4

# Dialyzer
rebar3 dialyzer

# Full check
rebar3 check
```

Note: all tests use ephemeral ports (0 = OS assigns an available port) to isolate runs, allowing parallel execution without port conflicts. Use `rebar3 ct -j 4` to run tests in parallel with 4 workers for faster execution.

**Property-based tests**: All property-based tests use PropEr and include `proper.hrl` in test profile. PropEr is available in test profile (`rebar3 as test`). Runtime check for PropEr availability is done in `prop_*` functions, with skip fallbacks when PropEr is not available. See [docs/PROPERTY_TESTING.md](docs/PROPERTY_TESTING.md) for detailed documentation.

**Testing recommendations**: See [docs/TESTING_RECOMMENDATIONS.md](docs/TESTING_RECOMMENDATIONS.md) for best practices, performance optimization, and CI/CD integration guidelines.

## RouterAdmin API

RouterAdmin provides a gRPC API to manage routing policies without restarting the service.

### Setup

```erlang
application:set_env(beamline_router, admin_api_key, <<"your-secret-key">>).
```

Default: `<<"dev-admin-key">>` (development only).

### Usage

```erlang
%% Create an authorized context
Ctx = #{metadata => [{<<"x-api-key">>, <<"dev-admin-key">>}]}.

%% Create a policy
AdminPolicyPb = #'AdminPolicy'{
    policy_id = <<"my_policy">>,
    providers = [
        #'AdminProvider'{id = <<"openai">>, weight = 0.7},
        #'AdminProvider'{id = <<"anthropic">>, weight = 0.3}
    ],
    sticky = false,
    rules = []
},

RequestPb = #'UpsertPolicyRequest'{
    tenant_id = <<"my_tenant">>,
    policy = AdminPolicyPb
},

Request = flow_pb:encode_msg(RequestPb, 'UpsertPolicyRequest'),
{ok, Response, _} = router_admin_grpc:upsert_policy(Ctx, Request).
```

### grpcurl Examples

```bash
# UpsertPolicy
grpcurl -plaintext \
  -H "x-api-key: dev-admin-key" \
  -d '{
    "tenant_id": "my_tenant",
    "policy": {
      "policy_id": "my_policy",
      "providers": [
        {"id": "openai", "weight": 0.7},
        {"id": "anthropic", "weight": 0.3}
      ],
      "sticky": false
    }
  }' \
  localhost:9000 beamline.flow.v1.RouterAdmin/UpsertPolicy

# GetPolicy
grpcurl -plaintext \
  -H "x-api-key: dev-admin-key" \
  -d '{"tenant_id": "my_tenant", "policy_id": "my_policy"}' \
  localhost:9000 beamline.flow.v1.RouterAdmin/GetPolicy

# ListPolicies
grpcurl -plaintext \
  -H "x-api-key: dev-admin-key" \
  -d '{"tenant_id": "my_tenant"}' \
  localhost:9000 beamline.flow.v1.RouterAdmin/ListPolicies

# DeletePolicy
grpcurl -plaintext \
  -H "x-api-key: dev-admin-key" \
  -d '{"tenant_id": "my_tenant", "policy_id": "my_policy"}' \
  localhost:9000 beamline.flow.v1.RouterAdmin/DeletePolicy
```

Detailed API docs: [docs/GRPC_API.md](docs/GRPC_API.md)

Key sections:
- Retryability and error handling: [docs/GRPC_API.md#retryability-unavailable-vs-resource_exhausted](docs/GRPC_API.md#retryability-unavailable-vs-resource_exhausted)
- Canonical reasons: [docs/GRPC_API.md#canonical-reasons-summary](docs/GRPC_API.md#canonical-reasons-summary)
- Playbook: diagnostics and correlation: [docs/GRPC_API.md#playbook-diagnostics-and-correlation](docs/GRPC_API.md#playbook-diagnostics-and-correlation)

## Windows/WSL Notes

When working in WSL, avoid UNC paths (`\\wsl.localhost\...`) for `rebar3` commands. Use local paths inside WSL:

```bash
# Correct (inside WSL)
cd /home/rustkas/aigroup/apps/otp/router
rebar3 compile

# Incorrect (UNC path)
cd \\wsl.localhost\Ubuntu\home\rustkas\aigroup\apps\otp\router
rebar3 compile  # May not work
```

## Glossary

- CP1  first checkpoint of Router Core development (minimum: core, tests, gRPC stubs, basic ABI).
- gRPC  RPC over HTTP/2; baseline integration via `Router.Decide`.
- ABI  Application Binary Interface: formal interaction contracts (Protobuf messages).
- NATS  message broker (pub/sub, req/rep) for future integration.
- Smoke Test  simple liveness check (call `Router.Decide`).
