# Network Partition Tests - Quick Start

## Overview

Network partition tests verify Router's resilience to network isolation scenarios, including single-instance, multi-instance (split-brain), service-broker partitions, and flapping network conditions.

## Quick Start

### Run All Network Partition Tests

```bash
cd apps/otp/router
rebar3 ct --suite router_network_partition_SUITE
```

### Run Specific Test Group

```bash
# Single-instance tests
rebar3 ct --suite router_network_partition_SUITE --group single_instance_tests

# Multi-instance tests
rebar3 ct --suite router_network_partition_SUITE --group multi_instance_tests

# Service-broker tests
rebar3 ct --suite router_network_partition_SUITE --group service_broker_tests

# Flapping network tests
rebar3 ct --suite router_network_partition_SUITE --group flapping_network_tests
```

### Run Single Test

```bash
rebar3 ct --suite router_network_partition_SUITE --case test_single_instance_partial_partition
```

## Test Modes

### Mock Mode (Default)

Tests run in mock mode by default, using `meck` to simulate network partitions. This works without root privileges and is suitable for CI/CD pipelines.

```bash
# Mock mode (default)
export NETWORK_PARTITION_REAL=false
rebar3 ct --suite router_network_partition_SUITE
```

### Real Network Tools Mode (Optional)

For real network manipulation using `tc` and `iptables`, enable real mode. This requires root privileges or container capabilities.

```bash
# Real network tools mode (requires root/privileges)
export NETWORK_PARTITION_REAL=true
sudo rebar3 ct --suite router_network_partition_SUITE
```

**Note**: Real mode is optional. Mock mode provides sufficient coverage for most scenarios.

## Test Scenarios

### Single-Instance Tests

- **Partial Partition**: One-way network isolation (router → NATS)
- **Full Partition**: Bidirectional network isolation
- **Partition Healing**: Recovery after partition resolution

### Multi-Instance Tests

- **Split-Brain**: Cluster divided into two groups
- **Partial Partition**: Some instances isolated
- **Leader Election**: Correct leader election after healing

### Service-Broker Tests

- **Service-Broker Partition**: All instances lose connection to JetStream
- **Retry Behavior**: Exponential backoff during partition
- **Recovery**: State consistency after partition resolution

### Flapping Network Tests

- **Stability**: Resilience to rapid connect/disconnect cycles
- **No Resource Leaks**: Process count and memory stability
- **Recovery**: Stable operation after flapping stops

## Expected Behavior

### During Partition

- Router process remains alive (fail-open strategy)
- Metrics reflect partition (connection failures)
- No crashes or data loss
- Retry/backoff behavior (for service-broker partitions)

### After Healing

- Router reconnects automatically
- Normal operation restored
- New messages processed successfully
- State consistency maintained

## Troubleshooting

### Tests Fail with "Process Not Found"

**Issue**: Tests fail because router processes are not started.

**Solution**: Ensure `beamline_router` application is started in `init_per_suite/1`.

### Mock Mode Not Working

**Issue**: Tests fail because mocks are not set up correctly.

**Solution**: Check that `nats_mode = mock` is set in test configuration.

### Real Mode Requires Privileges

**Issue**: Real network tools mode fails with permission errors.

**Solution**: Run with `sudo` or use mock mode (default).

## Documentation

- **Complete Guide**: `apps/otp/router/docs/dev/NETWORK_PARTITION_TESTING.md`
- **Fault Injection Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **Requirements**: `apps/otp/router/test/FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`

## Integration

Network partition tests are integrated into CI/CD pipelines:

- **Tags**: `@test_category network_partition, slow, integration, fault_injection`
- **Execution Time**: ~10-15 minutes (all tests)
- **Resource Requirements**: ~500 MB memory per test

## Status

✅ **Implemented** - All test groups implemented and passing

**Test Coverage**:
- ✅ Single-instance partitions (3 tests)
- ✅ Multi-instance/split-brain (3 tests)
- ✅ Service-broker partitions (3 tests)
- ✅ Flapping network (3 tests)

**Total**: 12 test cases covering all network partition scenarios.

