# Metrics Documentation

**Date**: 2025-01-27  
**Status**: Auto-generated from router_metrics_validator

## Overview

This document lists all metrics emitted by the Router application, including their names, labels, types, and cardinality information.

## Metrics List

### Circuit Breaker Metrics (R10)

- `router_circuit_breaker_state_transitions_total`
  - Type: counter
  - Labels: `tenant_id`, `provider_id`, `from`, `to`
  - Description: Count of circuit breaker state transitions

### Extension Execution Metrics

- `router_extension_execution_total`
  - Type: counter
  - Labels: `extension_id`, `extension_type`, `status`, `tenant_id`, `policy_id`
  - Description: Count of extension executions

- `router_extension_execution_latency_ms`
  - Type: gauge
  - Labels: `extension_id`, `extension_type`, `tenant_id`, `policy_id`
  - Description: Extension execution latency in milliseconds

### Assignment Metrics

- `router_assignment_skipped_total`
  - Type: counter
  - Labels: `reason`
  - Description: Count of skipped assignments

- `router_assignment_blocked_total`
  - Type: counter
  - Labels: `tenant_id`, `reason`
  - Description: Count of blocked assignments

- `router_assignment_publish_failures_total`
  - Type: counter
  - Labels: `tenant_id`, `error_type`
  - Description: Count of assignment publish failures

- `router_assignment_retry_total`
  - Type: counter
  - Labels: `tenant_id`, `retry_count`
  - Description: Count of assignment retries

- `router_assignment_published_total`
  - Type: counter
  - Labels: `tenant_id`
  - Description: Count of published assignments

- `router_retry_exhausted_total`
  - Type: counter
  - Labels: `tenant_id`
  - Description: Count of exhausted retries

### Sticky Session Metrics

- `router_sticky_hits_total`
  - Type: counter
  - Labels: `tenant_id`
  - Description: Count of sticky session hits

- `router_sticky_miss_total`
  - Type: counter
  - Labels: `tenant_id`
  - Description: Count of sticky session misses

### Backpressure Metrics

- `router_backpressure_events_total`
  - Type: counter
  - Labels: `subject`, `event_type`
  - Description: Count of backpressure events

- `router_backpressure_recovery_total`
  - Type: counter
  - Labels: `subject`
  - Description: Count of backpressure recoveries

- `router_intake_jetstream_query_total`
  - Type: counter
  - Labels: `subject`, `status`
  - Description: Count of JetStream queries

- `router_intake_p95_calculation_total`
  - Type: counter
  - Labels: `subject`, `status`
  - Description: Count of P95 calculations

### Gateway Backpressure Metrics

- `router_gateway_backpressure_notification_total`
  - Type: counter
  - Labels: `gateway_endpoint`, `status`
  - Description: Count of Gateway backpressure notifications

### Connection Pool Metrics

- `router_connection_pool_created_total`
  - Type: counter
  - Labels: `pool_name`
  - Description: Count of created connection pools

- `router_connection_pool_usage_total`
  - Type: counter
  - Labels: `pool_name`, `status`
  - Description: Count of connection pool usage

### CPU Profiling Metrics

- `router_cpu_profiling_started_total`
  - Type: counter
  - Labels: `profile_id`
  - Description: Count of started CPU profiles

- `router_cpu_profiling_stopped_total`
  - Type: counter
  - Labels: `profile_id`
  - Description: Count of stopped CPU profiles

### Network Tracking Metrics

- `router_network_round_trip_time_microseconds`
  - Type: gauge
  - Labels: `operation`, `target`
  - Description: Network round trip time in microseconds

- `router_network_batch_operations_total`
  - Type: counter
  - Labels: `operation`, `status`
  - Description: Count of batch operations

- `router_network_batch_duration_microseconds`
  - Type: gauge
  - Labels: `operation`
  - Description: Batch operation duration in microseconds

- `router_network_requests_coalesced_total`
  - Type: counter
  - Labels: `operation`
  - Description: Count of coalesced requests

## Label Naming Conventions

All labels follow Prometheus naming conventions:
- Use lowercase letters, numbers, and underscores
- Start with a letter or underscore
- Avoid reserved label names: `le`, `quantile`, `__name__`

## Cardinality Management

Metrics with high cardinality labels should be monitored:
- Use `router_metrics:count_label_cardinality/1` to check cardinality
- Use `router_metrics:check_cardinality_limit/2` to enforce limits
- Use `router_metrics:monitor_cardinality/1` to monitor multiple metrics

## Validation

Use `router_metrics_validator` module to:
- Validate metric names: `validate_metric_name/1`
- Validate metric labels: `validate_metric_labels/2`
- Get metric documentation: `get_metric_documentation/1`
- Document all metrics: `document_all_metrics/0`
- Find metric mismatches: `find_metric_mismatches/1`

## Notes

- This documentation is auto-generated and may not include all metrics
- Use `router_metrics_validator:document_all_metrics/0` to get current list
- Metric names and labels may change - always validate before use
