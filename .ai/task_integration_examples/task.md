# Task: T-INTEG-02 — Gateway SDK & Integration Examples

**Status**: Planning  
**Priority**: Medium  
**Category**: Integration, Developer Experience, Documentation

## Problem Statement

External systems need to integrate with the Beamline Router, but currently there are:
- **No HTTP gateway** for clients that can't use gRPC or NATS
- **No gRPC client examples** in common languages (Python, Node.js, Go)
- **No code examples** for handling retry, backpressure, and circuit breakers
- **No adapter patterns** for integrating legacy HTTP services

This creates a barrier to adoption and increases integration time for new clients.

## Goal

Create integration examples and SDK components that enable external systems to interact with the Router:
1. **HTTP → NATS Adapter**: Lightweight gateway for HTTP clients
2. **gRPC Client Examples**: Reference implementations in Python, Node.js, Go
3. **Retry & Backpressure Patterns**: Code snippets for resilient integration
4. **Integration Guides**: Step-by-step tutorials with working examples

## Expected Outcomes

- ✅ HTTP-to-NATS adapter: `gateway/http_nats_adapter/`
- ✅ gRPC client examples: `examples/clients/`
- ✅ Retry/backpressure snippets: `examples/patterns/`
- ✅ Integration guide: `docs/INTEGRATION_GUIDE.md`
- ✅ Docker Compose setup for local testing: `examples/docker-compose.yml`

## Success Criteria

1. HTTP adapter handles 1000 req/s with < 10ms overhead
2. gRPC examples work in Python, Node.js, and Go
3. Retry patterns demonstrate exponential backoff and circuit breaking
4. Integration guide allows developers to integrate in < 1 hour
5. Docker Compose setup starts full environment in < 2 minutes

## Business Impact

- **Reduced Integration Time**: From days to hours for new clients
- **Broader Adoption**: Support for HTTP-only environments
- **Best Practices**: Reference implementations reduce production issues
