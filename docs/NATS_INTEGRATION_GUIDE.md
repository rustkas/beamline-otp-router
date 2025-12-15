# NATS Integration Guide

## Overview

This guide describes the NATS integration in the Beamline Router project, including configuration, usage, and troubleshooting.

## Architecture

The NATS integration consists of several components:

- **router_nats.erl**: Core NATS client interface with connection management
- **router_jetstream.erl**: JetStream consumer management  
- **router_jetstream_sup.erl**: Supervisor for JetStream processes
- **router_jetstream_consumer_manager.erl**: Manages JetStream consumers

## Configuration

### Basic NATS Configuration

Add these settings to your `beamline_router.app.src` or runtime configuration:

```erlang
{env, [
    %% NATS server URL
    {nats_url, "nats://localhost:4222"},
    
    %% Authentication (optional)
    {nats_username, "user"},
    {nats_password, "pass"},
    
    %% TLS Configuration (optional)
    {nats_tls_enabled, false},
    {nats_tls_cert_file, "/path/to/cert.pem"},
    {nats_tls_key_file, "/path/to/key.pem"},
    {nats_tls_ca_cert_file, "/path/to/ca.pem"},
    
    %% Connection Settings
    {nats_reconnect_attempts, 10},
    {nats_reconnect_delay_ms, 1000},
    {nats_max_reconnect_delay_ms, 30000},
    {nats_fail_open_mode, true},
    
    %% JetStream Configuration
    {jetstream_enabled, true},
    {decide_subject, "beamline.router.v1.decide"},
    {results_subject, "beamline.router.v1.results"},
    {ack_subject, "beamline.router.v1.acks"}
]}
```

### Authentication Methods

The router supports multiple authentication methods:

1. **Username/Password**:
   ```erlang
   {nats_username, "user"},
   {nats_password, "password"}
   ```

2. **JWT Token**:
   ```erlang
   {nats_jwt, "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."}
   ```

3. **NKey**:
   ```erlang
   {nats_nkey, "UAKDR..."}
   ```

## Usage

### Starting the Router

```erlang
%% Start the router with NATS integration
{ok, _Pid} = beamline_router:start_link().
```

### Publishing Messages

```erlang
%% Simple publish (fire and forget)
ok = router_nats:publish(<<"subject">>, <<"payload">>).

%% Publish with acknowledgment
{ok, MsgId} = router_nats:publish_with_ack(
    <<"subject">>, 
    <<"payload">>, 
    #{<<"header">> => <<"value">>}
).
```

### JetStream Consumers

```erlang
%% Subscribe to decide subject
{ok, ConsumerId} = router_jetstream:subscribe_decide(#{
    subject => <<"beamline.router.v1.decide">>,
    durable_group => <<"router-decide-consumer">>,
    ack_policy => explicit,
    deliver_group => undefined,
    mode => pull_mode
}).
```

### Connection Status

```erlang
%% Get connection status
{ok, Status} = router_nats:get_connection_status().

%% Get connection health
Health = router_nats:get_connection_health().
```

## Modes of Operation

### Stub Mode (Default)

When no NATS URL is configured, the router operates in stub mode:
- All operations succeed immediately
- Message IDs are generated locally
- No actual network communication
- Safe for testing and development

### Real NATS Mode

When `nats_url` is configured:
- Real NATS connections are established
- Messages are published to NATS server
- JetStream consumers are created
- Network failures are handled with reconnection

### Fail-Open Mode

When `nats_fail_open_mode` is `true`:
- Router continues operating even if NATS is unavailable
- Publish operations return success but don't actually send
- Graceful degradation for production resilience

## Monitoring and Metrics

The router emits several metrics for monitoring:

- `router_nats_connection_status`: Connection status (0=disconnected, 1=connected)
- `router_nats_publish_total`: Total publish operations
- `router_nats_publish_failures_total`: Failed publish operations
- `router_nats_publish_latency_seconds`: Publish latency
- `router_nats_ack_total`: ACK operations
- `router_nats_nak_total`: NAK operations

## Troubleshooting

### Common Issues

1. **Connection Failures**:
   - Check NATS server is running
   - Verify URL format: `nats://host:port`
   - Check authentication credentials

2. **Stub Mode Active**:
   - Ensure `nats_url` is configured
   - Check logs for "stub mode" messages

3. **JetStream Errors**:
   - Verify JetStream is enabled on NATS server
   - Check stream and consumer configurations

### Debug Logging

Enable debug logging for troubleshooting:

```erlang
application:set_env(beamline_router, log_level, debug).
```

### Testing

Run the NATS integration tests:

```bash
rebar3 ct --suite=router_nats_real_connection_SUITE
```

## Production Deployment

### Security Considerations

1. **TLS**: Always enable TLS in production
2. **Authentication**: Use strong authentication methods
3. **Network**: Restrict NATS port access
4. **Credentials**: Store credentials securely

### Performance Tuning

1. **Connection Pooling**: Configure appropriate reconnection settings
2. **Batching**: Use publish_with_ack for critical messages
3. **Monitoring**: Set up alerts for connection failures
4. **Fail-Open**: Consider fail-open mode for high availability

### High Availability

1. **NATS Cluster**: Deploy NATS in cluster mode
2. **Multiple Routers**: Run multiple router instances
3. **Load Balancing**: Use queue groups for consumers
4. **Monitoring**: Monitor cluster health and connection status

## Migration from Stub Mode

To migrate from stub mode to real NATS:

1. Install and configure NATS server
2. Add `nats_url` to configuration
3. Test with `nats_fail_open_mode = true` first
4. Monitor connection status and metrics
5. Gradually disable fail-open mode once stable

## API Reference

### router_nats

- `start_link/0`: Start NATS client
- `stop/0`: Stop NATS client
- `publish/2`: Publish message
- `publish_with_ack/3`: Publish with acknowledgment
- `get_connection_status/0`: Get connection status
- `get_connection_health/0`: Get health status

### router_jetstream

- `subscribe_decide/1`: Subscribe to decide subject
- `subscribe_results/1`: Subscribe to results subject
- `subscribe_acks/1`: Subscribe to acks subject
- `ack/1`: Acknowledge message
- `nak/2`: Negative acknowledge message
