# Scope: T-SLO-01

## In Scope

### 1. Latency SLOs
- **p95 latency**: 95th percentile routing decision latency
- **p99 latency**: 99th percentile routing decision latency
- Target: p95 < 100ms, p99 < 200ms (initial targets, subject to validation)
- Measurement: `router_grpc_request_duration_seconds` histogram

### 2. Availability SLOs
- **Uptime target**: 99.9% (three nines) availability
- **Measurement window**: 30-day rolling window
- **Definition**: Percent of successful requests (HTTP 2xx, gRPC OK)
- Measurement: `router_grpc_requests_total` (success vs error)

### 3. Error Budget
- **Calculation**: (1 - Availability SLO) × Total requests
- **Burn rate**: How fast we consume error budget
- **Alerting**: Alert when burn rate exceeds safe threshold
- **Reporting**: Daily/weekly error budget remaining

### 4. Prometheus Alerting Rules
- **Latency violation**: p95 > 100ms for 5 minutes
- **Latency critical**: p99 > 200ms for 5 minutes
- **Availability violation**: Error rate > 0.1% (99.9% SLO)
- **Error budget exhaustion**: < 10% budget remaining
- **Fast burn**: Consuming 5% budget in 1 hour (6x normal rate)
- **Slow burn**: Consuming 10% budget in 6 hours (2x normal rate)

### 5. Recording Rules
- Pre-calculate SLI (Service Level Indicator) metrics:
  - `slo:router_latency_p95:5m` - 5-minute rolling p95
  - `slo:router_latency_p99:5m` - 5-minute rolling p99
  - `slo:router_error_rate:5m` - 5-minute error rate
  - `slo:router_availability:30d` - 30-day availability

### 6. Documentation
- SLO specification document
- Runbook for SLO incident response
- Error budget policy (what to do when budget exhausted)

## Out of Scope

- **Grafana dashboard creation** - defer to T-OBS-01 (Observability)
- **Multi-region SLOs** - single region only for now
- **Custom SLO per tenant** - global SLO only
- **Provider-specific SLOs** - Router-level only, not per provider
- **End-to-end SLOs** - Router-only, not including Gateway or CAF

## Dependencies

- Existing metrics: `router_grpc_request_duration_seconds`, `router_grpc_requests_total`
- Prometheus deployment (assumed available)
- Access to historical metrics for baseline

## References

- [Google SRE Book - SLOs](https://sre.google/sre-book/service-level-objectives/)
- [Prometheus Alerting Best Practices](https://prometheus.io/docs/practices/alerting/)
