# Router Health Endpoint Verification

**Date**: 2025-11-30  
**Status**: Verification Complete

## Summary

Router uses **gRPC health check** (not HTTP) on port 9000, which is different from the HTTP `/_health` endpoint specified in `docs/OBSERVABILITY.md`.

## Current Implementation

### gRPC Health Service

Router implements gRPC health check via `grpcbox_health_service`:

- **Protocol**: gRPC (not HTTP)
- **Port**: 9000 (not 8080)
- **Service**: `grpc.health.v1.Health`
- **Method**: `Check`
- **Response Format**: Protobuf (not JSON)

### Response Format

gRPC health check returns protobuf message:

```erlang
{ok, #{status => 'SERVING'}, Ctx}  %% For empty service name
{ok, #{status => 'UNKNOWN'}, Ctx}   %% For specific service names
```

Status values:
- `SERVING`: Service is healthy
- `UNKNOWN`: Service status unknown (for specific service checks)
- `NOT_SERVING`: Service is not serving (not currently used)

### Verification

Health check can be verified using `grpc_health_probe`:

```bash
grpc_health_probe -addr=:9000
```

## Documentation Discrepancy

### Expected (from `docs/OBSERVABILITY.md`)

- **Protocol**: HTTP
- **Path**: `GET /_health`
- **Port**: 8080
- **Format**: JSON
- **Required fields**: `status`, `timestamp`
- **Optional fields**: `checks` (object)

### Actual

- **Protocol**: gRPC
- **Service**: `grpc.health.v1.Health/Check`
- **Port**: 9000
- **Format**: Protobuf
- **Fields**: `status` (enum: SERVING/UNKNOWN/NOT_SERVING)

## Recommendations

### Option 1: Update Documentation (Recommended)

Update `docs/OBSERVABILITY.md` to reflect that Router uses gRPC health check:

```markdown
### Components and Ports

- Router (Erlang): gRPC health check on port 9000 (use `grpc_health_probe -addr=:9000`)
- Gateway (NestJS): `http://localhost:3000/_health`
- Provider services: specify per provider
```

### Option 2: Add HTTP Health Endpoint

Add HTTP health endpoint to Router (requires HTTP server):

- **Path**: `GET /_health`
- **Port**: 8080 (or separate port)
- **Format**: JSON (as per `docs/OBSERVABILITY.md`)
- **Implementation**: Add HTTP server (e.g., `cowboy`, `elli`) or use existing HTTP infrastructure

## Security Verification

### Current gRPC Health Check

✅ **No secrets exposed**: gRPC health check only returns status enum (SERVING/UNKNOWN/NOT_SERVING)  
✅ **Stable schema**: gRPC health protocol is standardized (grpc.health.v1.Health)  
✅ **No sensitive data**: Response contains only service status

### If HTTP Health Endpoint Added

Must ensure:
- ✅ JSON format (not protobuf)
- ✅ No secrets in response
- ✅ Stable schema (fields: `status`, `timestamp`, optional `checks`)
- ✅ ISO 8601 timestamp format

## Compliance Status

| Requirement | Status | Notes |
|------------|--------|-------|
| Health endpoint exists | ✅ | gRPC health on port 9000 |
| JSON format | ❌ | Uses protobuf (gRPC standard) |
| No secrets | ✅ | Only status enum returned |
| Stable schema | ✅ | gRPC health protocol is standardized |
| HTTP endpoint | ❌ | Uses gRPC (not HTTP) |
| Port 8080 | ❌ | Uses port 9000 (gRPC) |

## Conclusion

Router health check **does not follow** `docs/OBSERVABILITY.md` specification for HTTP `/_health` endpoint. However, it **does comply** with security requirements (no secrets, stable schema) and uses standard gRPC health protocol.

**Recommendation**: Update `docs/OBSERVABILITY.md` to document gRPC health check for Router, or add HTTP health endpoint if HTTP access is required.

