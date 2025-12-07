# Prometheus Alerts for Router/CAF

This document provides alerting rules for critical and warning signals emitted by Router/CAF components. Adapt thresholds to your environment and SLOs.

## Metrics Overview

- **Critical**:
  - `router_assignment_publish_failures_total`: publication failures to broker
  - `router_payload_rejected_total`: payload exceeds broker or app limit
  - `router_retry_exhausted_total`: all retries exhausted without success

- **Warning**:
  - `router_assignment_retry_total`: retry frequency (load/health indicator)
  - `router_schema_version_mismatch_total`: missing/unsupported schema versions

- **Latency**:
  - `router_decide_latency_ms_bucket`: histogram for Decide latency
  - `router_result_latency_ms`: histogram for ExecResult latency

- **Connectivity**:
  - `nats_disconnects_total`: broker disconnect events (if available)

- **Controls**:
  - `caf_push_assignment_enabled`: gauge for kill-switch (0/1)

- **Assignment Metrics**:
  - `router_assignment_published_total{tenant,job_type}`: successful assignments
  - `router_assignment_skipped_total{reason}`: skipped assignments (kill-switch)
  - `router_assignment_blocked_total{tenant_id,reason}`: blocked assignments (tenant allowlist)

- **Result Metrics**:
  - `router_results_total{status,job_type,provider_id}`: received ExecResult messages
  - `router_results_parse_failed_total{subject,error}`: JSON parse failures
  - `router_results_validation_failed_total{assignment_id,request_id,reason}`: validation failures

- **Usage Metrics**:
  - `router_usage_emitted_total{status,provider_id}`: usage events published
  - `router_usage_emit_failed_total{subject,error}`: usage event publish failures

- **ACK Metrics** (optional):
  - `router_acks_total{status,assignment_id}`: received ACK messages
  - `router_assignments_rejected_total{assignment_id,reason}`: rejected assignments
  - `router_assignments_ack_error_total{assignment_id,error}`: ACK processing errors
  - `router_acks_tenant_rejected_total{assignment_id,reason}`: ACK tenant validation failures
  - `router_acks_duplicate_total{assignment_id}`: duplicate ACK messages (idempotency)

- **Tenant Validation Metrics**:
  - `router_results_tenant_rejected_total{assignment_id,request_id,reason}`: result tenant validation failures (emits NAK)
  - `router_acks_tenant_rejected_total{assignment_id,reason}`: ACK tenant validation failures (emits NAK)
  - `router_tenant_audit_total{tenant_id,reason,source}`: tenant validation audit events (all validation attempts)

- **Idempotency Metrics**:
  - `router_results_duplicate_total{assignment_id,request_id}`: duplicate result messages
  - `router_acks_duplicate_total{assignment_id}`: duplicate ACK messages
  - `router_idempotency_hit_total{key_type,message_id}`: idempotency cache hits (message already seen)
  - `router_idempotency_miss_total{key_type,message_id,reason}`: idempotency cache misses (message not seen or expired)

- **Extension Metrics** (CP2+):
  - `router_extension_invocation_total{extension_id,type,status,tenant_id}`: extension invocations (success/error/timeout)
  - `router_extension_invocation_latency_ms{extension_id,type}`: extension invocation latency histogram
  - `router_extension_retries_total{extension_id,retry_count}`: extension retry attempts
  - `router_extension_circuit_breaker_state{extension_id,state}`: circuit breaker state (closed/open/half_open)
  - `router_extension_circuit_breaker_opened_total{extension_id}`: circuit breaker opening events
  - `router_extension_health_success_rate{extension_id}`: extension success rate (0.0-1.0)
  - `router_extension_health_latency_p95_ms{extension_id}`: extension P95 latency
  - `router_extension_registry_extensions_total{source,type}`: total extensions in registry
  - `router_extension_registry_sync_total{status}`: registry sync operations (success/error)

**For detailed Extension alerts and runbook, see**: `apps/otp/router/docs/EXTENSIONS_RUNBOOK.md`

- **JetStream Metrics**:
  - `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}`: message redeliveries (NAK calls) - emitted when NAK is called
  - `router_jetstream_maxdeliver_exhausted_total{assignment_id,request_id,reason}`: messages that exceeded MaxDeliver (⚠️ **Note**: Requires JetStream server integration to track delivery count; currently not emitted, but alert configured)
  
  **Alerts**:
  - `RouterJetStreamRedeliveryHigh`: High JetStream redelivery rate (> 20/min for 10m, warning)
  - `RouterJetStreamMaxDeliverExhausted`: MaxDeliver exhausted (> 0/min for 5m, critical)
  
  **Test Coverage**: See `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` for concurrent fault scenarios and `test/FAULT_INJECTION_TEST_SCENARIOS.md` for general fault injection tests.
  
  **Reliability**: See `docs/RELIABILITY_FAULT_TOLERANCE.md` for reliability guarantees and fault tolerance mechanisms.

- **Circuit Breaker Metrics** (CP2):
  - `router_circuit_breaker_events_total{tenant_id,provider_id,event_type}`: circuit breaker events (success/failure) counter
  - `router_circuit_breaker_state_transitions_total{tenant_id,provider_id,state}`: circuit breaker state transitions counter
  
  **Alerts**:
  - `RouterCircuitBreakerOpened`: Circuit breaker opened for provider (> 0/min for 10m, warning)
  - `RouterCircuitBreakerHighFailureRate`: High failure rate for provider (> 50% for 5m, critical)
  - `RouterCircuitBreakerMultipleProvidersOpen`: Multiple providers have open circuit breakers (> 2 for 5m, critical)
  - `RouterCircuitBreakerHalfOpenProbesFailed`: Circuit breaker half-open probes failing (repeated opens after half-open, warning)
  
  **Reference**: `docs/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` for detailed alert definitions and runbook

## Example Alerting Rules (Prometheus)

```yaml
groups:
  - name: beamline-router-alerts
    rules:
      - alert: RouterPublishFailuresBurst
        expr: sum(rate(router_assignment_publish_failures_total[5m])) > 0
        for: 5m
        labels: { severity: critical, service: router }
        annotations:
          summary: "Router publish failures detected"
          description: "Publication to broker failing over the last 5m. Investigate broker health, auth, and subject routing."

      - alert: RouterPayloadRejectedSpike
        expr: sum(rate(router_payload_rejected_total[5m])) > 0
        for: 5m
        labels: { severity: critical, service: router }
        annotations:
          summary: "Payload rejected due to size limit"
          description: "Incoming payloads exceed configured limits. Validate environment configs and client behavior."

      - alert: RouterRetryExhausted
        expr: sum(rate(router_retry_exhausted_total[5m])) > 0
        for: 5m
        labels: { severity: critical, service: router }
        annotations:
          summary: "Retries exhausted without success"
          description: "Exponential backoff reached max attempts. Check downstream availability and retry parameters."

      - alert: RouterRetryAttemptHigh
        expr: sum(rate(router_assignment_retry_total[5m])) > 10
        for: 10m
        labels: { severity: warning, service: router }
        annotations:
          summary: "High retry attempt rate"
          description: "Increased retry activity may indicate transient load or downstream issues."

      - alert: RouterSchemaVersionMismatch
        expr: sum(rate(router_schema_version_mismatch_total[10m])) > 0
        for: 10m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Schema version mismatches detected"
          description: "Requests with missing or unsupported schema versions. Validate client versions and contracts."

      - alert: RouterDecideLatencyP95SLOBreach
        expr: |
          histogram_quantile(
            0.95,
            sum(rate(router_decide_latency_ms_bucket[5m])) by (le)
          ) > 250
        for: 15m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Decide latency p95 exceeds SLO"
          description: "p95 latency above 250ms over 15m window. Investigate load, retries, and downstream latency."

      - alert: NATSDisconnectsSpike
        expr: sum(rate(nats_disconnects_total[5m])) > 0
        for: 5m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Broker disconnects observed"
          description: "NATS disconnect events detected. Check network stability and broker health."

      - alert: RouterKillSwitchEngaged
        expr: caf_push_assignment_enabled == 0
        for: 1m
        labels: { severity: info, service: router }
        annotations:
          summary: "Kill-switch engaged"
          description: "Publication disabled via kill-switch. Confirm intended state before rollout."

      - alert: RouterTenantRejectedHigh
        expr: sum(rate(router_results_tenant_rejected_total[5m]) + rate(router_acks_tenant_rejected_total[5m])) > 5
        for: 10m
        labels: { severity: warning, service: router }
        annotations:
          summary: "High tenant rejection rate"
          description: "Many messages rejected due to tenant validation failures. Check tenant allowlists and policy registry."
          test_coverage: "Fault injection tests: test_ack_error_with_tenant_validation_fail_concurrent, test_tenant_isolation_during_concurrent_faults. See apps/otp/router/test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md#scenario-s1-ack-error--tenant-validation-fail-concurrent-messages"

      - alert: RouterIdempotencyDuplicatesHigh
        expr: sum(rate(router_results_duplicate_total[5m]) + rate(router_acks_duplicate_total[5m])) > 10
        for: 10m
        labels: { severity: warning, service: router }
        annotations:
          summary: "High duplicate message rate"
          description: "Many duplicate messages detected. Check JetStream redelivery configuration and message processing."

      - alert: RouterJetStreamMaxDeliverExhausted
        expr: sum(rate(router_jetstream_maxdeliver_exhausted_total[5m])) > 0
        for: 5m
        labels: { severity: critical, service: router }
        annotations:
          summary: "JetStream MaxDeliver exhausted"
          description: "Messages exceeded MaxDeliver attempts. Check message processing logic and downstream dependencies."
          test_coverage: "Fault injection tests: test_max_delivery_count_exhaustion (router_result_consumer_SUITE.erl - real scenario). See apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md#4-max-delivery-count-exhaustion"

      - alert: RouterJetStreamRedeliveryHigh
        expr: sum(rate(router_jetstream_redelivery_total[5m])) > 20
        for: 10m
        labels: { severity: warning, service: router }
        annotations:
          summary: "High JetStream redelivery rate"
          description: "Many messages being redelivered (NAK calls). Check message processing performance, tenant validation, and ACK handling."
          test_coverage: "Fault injection tests: test_nak_with_publish_failure_recovery (router_result_consumer_SUITE.erl - real scenario). See apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md#scenario-s3-nak--publish-failure-with-recovery"
      
      # Circuit Breaker Alerts (CP2)
      - alert: RouterCircuitBreakerOpened
        expr: sum(rate(router_circuit_breaker_state_transitions_total{state="open"}[5m])) by (tenant_id, provider_id) > 0
        for: 10m
        labels: { severity: warning, service: router, component: circuit_breaker }
        annotations:
          summary: "Circuit breaker opened for provider {{ $labels.provider_id }}"
          description: "Circuit breaker opened for provider {{ $labels.provider_id }} in tenant {{ $labels.tenant_id }}. Provider may be unhealthy. Check provider health and error rates."

      - alert: RouterCircuitBreakerHighFailureRate
        expr: |
          sum(rate(router_circuit_breaker_events_total{event_type="failure"}[5m])) by (tenant_id, provider_id) 
          / 
          sum(rate(router_circuit_breaker_events_total[5m])) by (tenant_id, provider_id) 
          > 0.5
        for: 5m
        labels: { severity: critical, service: router, component: circuit_breaker }
        annotations:
          summary: "High failure rate for provider {{ $labels.provider_id }}"
          description: "Failure rate > 50% for provider {{ $labels.provider_id }} in tenant {{ $labels.tenant_id }}. Circuit breaker may open soon. Investigate provider health."

      - alert: RouterCircuitBreakerMultipleProvidersOpen
        expr: count(sum(rate(router_circuit_breaker_state_transitions_total{state="open"}[5m])) by (tenant_id, provider_id) > 0) > 2
        for: 5m
        labels: { severity: critical, service: router, component: circuit_breaker }
        annotations:
          summary: "Multiple circuit breakers open"
          description: "{{ $value }} providers have open circuit breakers. System may be experiencing widespread provider issues. Check provider health and fallback policies."

      - alert: RouterCircuitBreakerHalfOpenProbesFailed
        expr: |
          sum(rate(router_circuit_breaker_state_transitions_total{state="open"}[5m])) by (tenant_id, provider_id) 
          > 
          sum(rate(router_circuit_breaker_state_transitions_total{state="half_open"}[5m])) by (tenant_id, provider_id)
        for: 10m
        labels: { severity: warning, service: router, component: circuit_breaker }
        annotations:
          summary: "Circuit breaker half-open probes failing for provider {{ $labels.provider_id }}"
          description: "Circuit breaker for provider {{ $labels.provider_id }} in tenant {{ $labels.tenant_id }} is repeatedly opening after half-open attempts. Provider may not have recovered. Check provider health."

      # Backpressure Alerts (CP2+)
      - alert: RouterIntakeBackpressureActive
        expr: router_intake_backpressure_active{subject="beamline.router.v1.decide"} == 1
        for: 1m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Router intake backpressure is active"
          description: "Router intake is overloaded. Check pending messages, processing latency, and in-flight message count."
      
      - alert: RouterIntakeQueueDepthHigh
        expr: router_jetstream_pending_messages{subject="beamline.router.v1.decide"} > 100
        for: 5m
        labels: { severity: critical, service: router }
        annotations:
          summary: "Router intake queue depth is high"
          description: "Pending messages: {{ $value }} (threshold: 100). Router may be overloaded."
      
      - alert: RouterIntakeLatencyHigh
        expr: router_intake_processing_latency_p95{subject="beamline.router.v1.decide"} > 2000
        for: 5m
        labels: { severity: critical, service: router }
        annotations:
          summary: "Router intake processing latency is high"
          description: "P95 latency: {{ $value }}ms (threshold: 2000ms). Router may be overloaded."
      
      - alert: RouterIntakeInflightHigh
        expr: router_intake_inflight_messages{subject="beamline.router.v1.decide"} > 200
        for: 5m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Router intake in-flight messages is high"
          description: "In-flight messages: {{ $value }} (threshold: 200). Router may be overloaded."
      
      # Abuse Detection Alerts (CP2+)
      - alert: RouterAbuseEmptyPayloadDetected
        expr: sum(rate(router_abuse_empty_payload_total[5m])) > 0
        for: 5m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Router abuse detected: empty payload pattern"
          description: "Empty payload abuse pattern detected: {{ $value }} events/min. Check logs for tenant {{ $labels.tenant_id }}."
      
      - alert: RouterAbuseEmptyPayloadHigh
        expr: sum(rate(router_abuse_empty_payload_total[5m])) > 20
        for: 10m
        labels: { severity: warning, service: router }
        annotations:
          summary: "High rate of empty payload abuse events"
          description: "Empty payload abuse events detected at high rate: {{ $value }} events/min. Possible flood attack."
      
      - alert: RouterAbuseHeavyPayloadDetected
        expr: sum(rate(router_abuse_heavy_payload_total[5m])) > 0
        for: 5m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Router abuse detected: heavy payload pattern"
          description: "Heavy payload abuse pattern detected for tenant {{ $labels.tenant_id }}. Large payload ratio: {{ $labels.large_payload_ratio }}% (threshold: 80%)."
      
      - alert: RouterAbuseHeavyPayloadHigh
        expr: sum(rate(router_abuse_heavy_payload_total[5m])) > 10
        for: 10m
        labels: { severity: critical, service: router }
        annotations:
          summary: "High rate of heavy payload abuse events"
          description: "Heavy payload abuse events detected at high rate: {{ $value }} events/min. Possible DDoS or resource exhaustion attack."
      
      - alert: RouterAbuseTargetedTenantDetected
        expr: sum(rate(router_abuse_targeted_tenant_total[5m])) > 0
        for: 5m
        labels: { severity: warning, service: router }
        annotations:
          summary: "Router abuse detected: targeted tenant pattern"
          description: "Targeted tenant abuse pattern detected: {{ $value }} events/min. Check logs for tenant {{ $labels.tenant_id }}."
      
      - alert: RouterAbuseTargetedTenantHigh
        expr: sum(rate(router_abuse_targeted_tenant_total[5m])) > 5
        for: 10m
        labels: { severity: critical, service: router }
        annotations:
          summary: "High rate of targeted tenant attacks"
          description: "Targeted tenant attack detected: {{ $value }} events/min for tenant {{ $labels.tenant_id }}. Possible targeted DDoS."
```

## Gateway Abuse Detection Alerts

```yaml
groups:
  - name: beamline-gateway-abuse-alerts
    interval: 30s
    rules:
      - alert: GatewayAbuseEventDetected
        expr: sum(rate(gateway_abuse_events_total[5m])) > 0
        for: 5m
        labels: { severity: warning, service: gateway }
        annotations:
          summary: "Gateway abuse event detected"
          description: "Abuse event detected. Check specific abuse metrics for details."
      
      - alert: GatewayAbuseEmptyPayloadHigh
        expr: sum(rate(gateway_abuse_empty_payload_total[5m])) > 20
        for: 10m
        labels: { severity: warning, service: gateway }
        annotations:
          summary: "High rate of empty payload abuse events"
          description: "Empty payload abuse events detected at high rate: {{ $value }} events/min. Possible flood attack."
      
      - alert: GatewayAbuseTargetedTenantHigh
        expr: sum(rate(gateway_abuse_targeted_tenant_total[5m])) > 5
        for: 10m
        labels: { severity: critical, service: gateway }
        annotations:
          summary: "High rate of targeted tenant attacks"
          description: "Targeted tenant attack detected: {{ $value }} events/min. Possible targeted DDoS."
      
      - alert: GatewayAbuseRateLimitEvasionHigh
        expr: sum(rate(gateway_abuse_rate_limit_evasion_total[5m])) > 3
        for: 10m
        labels: { severity: warning, service: gateway }
        annotations:
          summary: "High rate of rate limit evasion attempts"
          description: "Rate limit evasion attempts detected: {{ $value }} events/min. Multiple API keys or IPs detected."
      
      - alert: GatewayAbuseHeavyPayloadHigh
        expr: sum(rate(gateway_abuse_heavy_payload_total[5m])) > 10
        for: 10m
        labels: { severity: warning, service: gateway }
        annotations:
          summary: "High rate of heavy payload abuse events"
          description: "Heavy payload abuse events detected at high rate: {{ $value }} events/min. Possible resource exhaustion attack."
      
      - alert: GatewayAbuseMultiTenantFloodHigh
        expr: sum(rate(gateway_abuse_multi_tenant_flood_total[5m])) > 5
        for: 10m
        labels: { severity: critical, service: gateway }
        annotations:
          summary: "High rate of multi-tenant flood attacks"
          description: "Multi-tenant flood attack detected: {{ $value }} events/min across multiple tenants. Possible coordinated attack."
      
      - alert: GatewayAbuseBlockedTenantsHigh
        expr: gateway_abuse_blocked_tenants > 100
        for: 5m
        labels: { severity: warning, service: gateway }
        annotations:
          summary: "High number of blocked tenants"
          description: "Number of blocked tenants is high: {{ $value }}. Review abuse detection thresholds and blocked tenant list."
```

## Operational Notes

- **Tune thresholds per environment**:
  - `dev`: permissive to reduce noise
  - `staging`: near-prod values to simulate conditions
  - `prod`: strict thresholds aligned with SLOs

- **Pair alerts with dashboards** for retry counters, publish failure rate, payload rejection trends, and Decide latency p50/p95/p99.

- **Document emergency procedures**: enable kill-switch on publication failures; roll back retry parameters if queues saturate.

- **Keep labels consistent** (`service`, `tenant`, `subject`) for filtering.

## References

- `apps/otp/router/docs/OPERATIONAL_GUIDE.md`: Operational guide with monitoring recommendations
- `apps/otp/router/docs/CONFIG.md`: Configuration reference
- `apps/otp/router/docs/dev/ROUTER_CAF_GATING_CHECK_REPORT.md`: Gating check report with telemetry details
