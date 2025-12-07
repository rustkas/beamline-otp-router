# Recovery Guide

This guide documents recovery procedures for circuit breakers and network partitions in the router system.

## Circuit Breaker Recovery

### Overview

Circuit breakers protect the system from cascading failures by temporarily blocking requests to unhealthy providers. Recovery procedures allow the system to automatically or manually recover from open circuit states.

### Circuit Breaker States

- **Closed**: Normal operation, requests flow through
- **Open**: Failing fast, requests fail immediately without calling provider
- **Half-open**: Testing recovery, limited requests allowed

### Automatic Recovery

#### Open → Half-open Transition

Circuit breakers automatically transition from `open` to `half_open` after the configured timeout:

```erlang
%% Configure timeout
Config = #{
    <<"timeout_ms">> => 60000  % 60 seconds
},
router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),

%% Circuit will automatically transition to half_open after timeout
%% Check state periodically or wait for timeout
timer:sleep(60000),
{ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
%% State should be half_open
```

#### Half-open → Closed Transition

Circuit breakers automatically transition from `half_open` to `closed` after meeting the success threshold:

```erlang
%% Configure success threshold
Config = #{
    <<"success_threshold">> => 2  % 2 consecutive successes
},
router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),

%% Record successes to meet threshold
router_circuit_breaker:record_success(TenantId, ProviderId),
router_circuit_breaker:record_success(TenantId, ProviderId),

%% Circuit will automatically transition to closed
{ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
%% State should be closed
```

### Manual Recovery

#### Force Recovery: Open → Closed

Force immediate recovery from `open` to `closed` (bypasses half-open):

```erlang
%% Force recovery
ok = router_circuit_breaker:force_recovery(TenantId, ProviderId),

%% Verify closed
{ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
?assertEqual(closed, State).
```

**Use Cases**:
- Manual intervention after fixing underlying issue
- Testing scenarios
- Emergency recovery

**Warning**: Use with caution - bypasses normal recovery flow.

#### Force Recovery: Open → Half-open

Force immediate transition from `open` to `half_open` (bypasses timeout):

```erlang
%% Force recovery to half-open
ok = router_circuit_breaker:force_recovery_to_half_open(TenantId, ProviderId),

%% Verify half-open
{ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
?assertEqual(half_open, State).
```

**Use Cases**:
- Testing recovery scenarios
- Manual intervention for controlled recovery
- Debugging recovery procedures

**Warning**: Use with caution - bypasses timeout mechanism.

#### Reset Recovery State

Reset recovery state: clear failure/success counts and reset to `closed`:

```erlang
%% Reset recovery state
ok = router_circuit_breaker:reset_recovery_state(TenantId, ProviderId),

%% Verify closed
{ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
?assertEqual(closed, State).
```

**Use Cases**:
- Complete state reset for testing
- Manual intervention after fixing underlying issue
- Recovery from corrupted state

**Warning**: Use with caution - clears all recovery progress.

### Recovery Status Monitoring

#### Get Recovery Status

Get detailed recovery status for monitoring and debugging:

```erlang
%% Get recovery status
{ok, Status} = router_circuit_breaker:get_recovery_status(TenantId, ProviderId),

%% Status for open state
#{
    state => open,
    recovery_type => <<"timeout_based">>,
    time_until_half_open_ms => 45000,
    timeout_ms => 60000,
    elapsed_ms => 15000,
    progress_percent => 25
} = Status.

%% Status for half-open state
#{
    state => half_open,
    recovery_type => <<"success_based">>,
    half_open_calls_count => 1,
    half_open_max_calls => 3,
    success_count => 1,
    success_threshold => 2,
    progress_percent => 50,
    calls_remaining => 2
} = Status.
```

**Status Fields**:
- `state`: Current circuit breaker state
- `recovery_type`: Type of recovery (`timeout_based` or `success_based`)
- For `open` state:
  - `time_until_half_open_ms`: Time remaining until half-open transition
  - `timeout_ms`: Configured timeout
  - `elapsed_ms`: Time elapsed since opening
  - `progress_percent`: Recovery progress (0-100)
- For `half_open` state:
  - `half_open_calls_count`: Current calls in half-open
  - `half_open_max_calls`: Maximum calls allowed
  - `success_count`: Current success count
  - `success_threshold`: Required success threshold
  - `progress_percent`: Recovery progress (0-100)
  - `calls_remaining`: Remaining calls before rate limit

### Recovery Best Practices

1. **Monitor Recovery Status**: Use `get_recovery_status/2` to monitor recovery progress
2. **Automatic Recovery Preferred**: Let automatic recovery handle normal scenarios
3. **Manual Recovery for Emergencies**: Use manual recovery only when necessary
4. **Verify Underlying Issue Fixed**: Ensure underlying issue is resolved before forcing recovery
5. **Test Recovery Procedures**: Test recovery procedures in staging before production

### Recovery Configuration

```erlang
Config = #{
    <<"failure_threshold">> => 5,           % Failures before opening
    <<"timeout_ms">> => 60000,              % Time before half-open (60s)
    <<"half_open_max_calls">> => 3,         % Max calls in half-open
    <<"success_threshold">> => 2,           % Successes before closing
    <<"error_rate_threshold">> => 0.5,      % Error rate threshold (50%)
    <<"error_rate_window_seconds">> => 60   % Error rate window (60s)
},
router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config).
```

## Network Partition Recovery

### Overview

Network partitions can isolate services from each other or from external dependencies. Recovery procedures allow the system to detect, monitor, and recover from network partitions.

### Partition Types

- **Single Instance**: One service instance isolated
- **Multi Instance**: Split-brain scenarios
- **Service-to-Broker**: Service isolated from NATS/JetStream
- **Flapping**: Unstable connectivity (periodic connect/disconnect)

### Automatic Recovery

#### Heal Partition

Automatically heal a partition by removing it:

```erlang
%% Heal partition
ok = router_network_partition:heal_partition(PartitionId),

%% Verify partition removed
Partitions = router_network_partition:list_partitions(),
%% PartitionId should not be in list
```

#### Auto-heal Partition

Attempt automatic recovery with enhanced logic:

```erlang
%% Auto-heal partition
ok = router_network_partition:auto_heal_partition(PartitionId),

%% Auto-heal stops flapping if applicable and removes partition
```

### Manual Recovery

#### Get Recovery Procedures

Get recovery procedures documentation:

```erlang
%% Get recovery procedures
{ok, Procedures} = router_network_partition:get_recovery_procedures(),

%% Procedures include:
%% - automatic_recovery: Steps for automatic recovery
%% - manual_recovery: Steps for manual recovery
%% - flapping_recovery: Steps for flapping network recovery
```

#### Get Partition Recovery Status

Get detailed recovery status for a specific partition:

```erlang
%% Get partition recovery status
{ok, Status} = router_network_partition:get_partition_recovery_status(PartitionId),

%% Status includes:
%% - partition_id: Partition identifier
%% - status: Current status (active, healed, etc.)
%% - duration_seconds: How long partition has been active
%% - type: Partition type
%% - recovery_available: Whether recovery is available
%% - recovery_procedure: Recommended recovery procedure
```

#### Get All Partitions Recovery Status

Get recovery status for all active partitions:

```erlang
%% Get all partitions recovery status
{ok, AllStatuses} = router_network_partition:get_all_partitions_recovery_status(),

%% Returns list of recovery statuses for all active partitions
```

### Recovery Procedures

#### Automatic Recovery Procedure

1. Identify partition using `list_partitions/0`
2. Check status using `get_partition_status/1`
3. Heal partition using `heal_partition/1`
4. Verify recovery using `get_partition_recovery_status/1`

#### Manual Recovery Procedure

1. Identify partition using `list_partitions/0`
2. Attempt auto-heal using `auto_heal_partition/1`
3. If auto-heal fails, use manual `heal_partition/1`
4. Monitor recovery status

#### Flapping Recovery Procedure

1. Stop flapping using `stop_flapping/1`
2. Heal partition using `heal_partition/1`
3. Verify network stability

### Recovery Best Practices

1. **Monitor Partitions**: Regularly check `list_partitions/0` for active partitions
2. **Auto-heal First**: Attempt `auto_heal_partition/1` before manual intervention
3. **Verify Network Stability**: Ensure network is stable before considering recovery complete
4. **Document Recovery Actions**: Log all recovery actions for audit trail
5. **Test Recovery Procedures**: Test recovery procedures in staging before production

### Recovery Monitoring

```erlang
%% Monitor all partitions
{ok, AllStatuses} = router_network_partition:get_all_partitions_recovery_status(),

%% Filter active partitions
ActivePartitions = [P || P <- AllStatuses, maps:get(recovery_available, P) =:= true],

%% Get recovery status for each
lists:foreach(fun(Partition) ->
    PartitionId = maps:get(partition_id, Partition),
    {ok, Status} = router_network_partition:get_partition_recovery_status(PartitionId),
    %% Process status...
end, ActivePartitions).
```

## Related Documentation

- `src/router_circuit_breaker.erl` - Circuit breaker implementation
- `src/router_network_partition.erl` - Network partition management
- `test/router_circuit_breaker_recovery_SUITE.erl` - Recovery tests
- `DESIGN_PATTERNS.md#error-handling` - Error handling patterns

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team

