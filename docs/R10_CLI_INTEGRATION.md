# R10 CLI Integration Guide

This document describes how to integrate `router_ctl_r10.erl` into the router CLI system.

## Module: `router_ctl_r10.erl`

**Location**: `apps/otp/router/src/router_ctl_r10.erl`

**Exports**:
- `status/2` - Get R10 circuit breaker status for tenant/provider
- `help/0` - Show help message

## Integration Options

### Option 1: Standalone Escript

If you have a standalone `router_ctl` escript, add command routing:

```erlang
%% In router_ctl.erl main/1 function
main(["r10", "status", TenantStr, ProviderStr]) ->
    TenantId = list_to_binary(TenantStr),
    ProviderId = list_to_binary(ProviderStr),
    router_ctl_r10:status(TenantId, ProviderId);

main(["r10", "help"]) ->
    router_ctl_r10:help();

main([_ | _]) ->
    io:format("Unknown command. Use 'r10 help' for R10 commands.~n"),
    erlang:halt(1);
    
main([]) ->
    io:format("Usage: router_ctl r10 status <tenant> <provider>~n"),
    io:format("       router_ctl r10 help~n"),
    erlang:halt(1).
```

### Option 2: Rebar3 Shell

For development/testing, you can call directly from Erlang shell:

```erlang
%% Start router application
application:ensure_all_started(beamline_router).

%% Check circuit breaker status
router_ctl_r10:status(<<"tenant1">>, <<"provider1">>).

%% Show help
router_ctl_r10:help().
```

## Usage Examples

### Check Circuit Breaker Status

```bash
./bin/router_ctl r10 status tenant1 provider1
```

**Output**:
```
=== R10 Circuit Breaker Status ===
Tenant: tenant1
Provider: provider1

State: closed (normal operation)
Last trigger reason: none (circuit never opened)
Error rate: 0.00% (0.0000)
Should allow: yes (requests will be processed)

Runbook: https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md
```

### Help Command

```bash
./bin/router_ctl r10 help
```

**Output**:
```
R10 Circuit Breaker Commands:
  r10 status <tenant> <provider>  - Show circuit breaker status
  r10 help                        - Show this help message

Runbook: https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md
```

## Implementation Notes

- **Application Startup**: The CLI automatically ensures `beamline_router` is started
- **Error Handling**: If application fails to start, CLI exits with code 1
- **Metric Access**: All metrics accessed through `router_r10_metrics` API (no direct ETS)
- **Runbook Links**: All output includes runbook URL for reference

## Future Enhancements

Potential future CLI commands:
- `r10 reset <tenant> <provider>` - Reset circuit breaker state
- `r10 list` - List all circuit breakers with their states
- `r10 config <tenant> <provider>` - Show/update circuit breaker configuration

