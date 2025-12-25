# Deployment Guide

## Overview

This guide documents deployment automation, validation, rollback procedures, and configuration management for the Router application.

## Deployment Automation

### Deployment Script

The deployment script (`scripts/deploy.sh`) provides automated deployment with validation and rollback support.

#### Usage

```bash
# Deploy a new version
./scripts/deploy.sh v1.0.0

# Rollback to previous version
./scripts/deploy.sh v1.0.0 --rollback

# Get deployment status
./scripts/deploy.sh --status

# Validate deployment (without deploying)
./scripts/deploy.sh --validate
```

#### Features

- **Pre-deployment validation**: Compiles application, runs Dialyzer, runs smoke tests
- **Backup creation**: Creates backup before deployment
- **Post-deployment health checks**: Verifies application health after deployment
- **Rollback support**: Restores from backup if deployment fails

### Deployment Module

The `router_deployment` module provides programmatic deployment control.

#### Deploy New Version

```erlang
%% Deploy new version
case router_deployment:deploy(~"v1.0.0") of
    {ok, DeploymentResult} ->
        Version = maps:get(version, DeploymentResult),
        Status = maps:get(status, DeploymentResult),
        %% Process successful deployment
        process_deployment(DeploymentResult);
    {error, Reason} ->
        %% Handle deployment error
        handle_deployment_error(Reason)
end.
```

#### Validate Deployment

```erlang
%% Validate deployment before executing
case router_deployment:validate_deployment(~"v1.0.0") of
    {ok, ValidationReport} ->
        Passed = maps:get(passed, ValidationReport),
        Health = maps:get(health, ValidationReport),
        Configuration = maps:get(configuration, ValidationReport),
        %% Process validation results
        process_validation(ValidationReport);
    {error, Reason} ->
        %% Handle validation error
        handle_validation_error(Reason)
end.
```

#### Rollback Deployment

```erlang
%% Rollback to previous version
case router_deployment:rollback(undefined) of
    {ok, RollbackResult} ->
        Version = maps:get(version, RollbackResult),
        Status = maps:get(status, RollbackResult),
        %% Process successful rollback
        process_rollback(RollbackResult);
    {error, no_previous_version} ->
        %% No previous version available
        handle_no_rollback();
    {error, Reason} ->
        %% Handle rollback error
        handle_rollback_error(Reason)
end.
```

#### Get Deployment Status

```erlang
%% Get current deployment status
case router_deployment:get_deployment_status() of
    {ok, Status} ->
        Version = maps:get(version, Status),
        DeploymentStatus = maps:get(status, Status),
        Timestamp = maps:get(timestamp, Status),
        %% Process status
        process_status(Status);
    {error, Reason} ->
        handle_status_error(Reason)
end.
```

#### Get Deployment History

```erlang
%% Get deployment history
case router_deployment:get_deployment_history() of
    {ok, History} ->
        %% Process history
        lists:foreach(fun(Deployment) ->
            Version = maps:get(version, Deployment),
            Status = maps:get(status, Deployment),
            process_deployment_entry(Deployment)
        end, History);
    {error, Reason} ->
        handle_history_error(Reason)
end.
```

### Pre-Deployment Checks

The deployment process includes pre-deployment checks:

- **Application running**: Verifies application is currently running
- **Supervisor running**: Verifies supervisor is running
- **NATS connection**: Checks NATS connection (if enabled)
- **Disk space**: Verifies sufficient disk space is available

```erlang
%% Run pre-deployment checks
case router_deployment:check_pre_deployment() of
    {ok, Results} ->
        Passed = maps:get(passed, Results),
        ApplicationRunning = maps:get(application_running, Results),
        SupervisorRunning = maps:get(supervisor_running, Results),
        %% Process results
        process_pre_deployment_results(Results);
    {error, Reason} ->
        handle_pre_deployment_error(Reason)
end.
```

### Post-Deployment Checks

The deployment process includes post-deployment checks:

- **Application health**: Verifies application health
- **gRPC endpoint**: Checks gRPC endpoint (if enabled)
- **Metrics endpoint**: Checks metrics endpoint (if enabled)

```erlang
%% Run post-deployment checks
case router_deployment:check_post_deployment() of
    {ok, Results} ->
        Passed = maps:get(passed, Results),
        Health = maps:get(health, Results),
        GrpcEndpoint = maps:get(grpc_endpoint, Results),
        %% Process results
        process_post_deployment_results(Results);
    {error, Reason} ->
        handle_post_deployment_error(Reason)
end.
```

## Configuration Management

### Configuration Validation

The `router_config_validator` module provides configuration validation.

#### Validate All Configuration

```erlang
%% Validate all configuration
case router_config_validator:validate_config() of
    {ok, Report} ->
        AllValid = maps:get(all_valid, Report),
        RequiredConfig = maps:get(required_config, Report),
        ValidationResults = maps:get(validation_results, Report),
        Compatibility = maps:get(compatibility, Report),
        %% Process validation report
        process_validation_report(Report);
    {error, Reason} ->
        handle_validation_error(Reason)
end.
```

#### Validate Specific Configuration Value

```erlang
%% Validate specific configuration value
case router_config_validator:validate_config_value(grpc_port, 9000) of
    {ok, ValidValue} ->
        %% Value is valid
        use_config_value(ValidValue);
    {error, Reason} ->
        %% Value is invalid
        handle_invalid_config(Reason)
end.
```

### Configuration Template

Get configuration template for new deployments:

```erlang
%% Get configuration template
Template = router_config_validator:get_config_template(),

%% Template includes:
%% - grpc_enabled => false
%% - grpc_port => 9000
%% - admin_api_key => ~"CHANGE_ME"
%% - nats_mode => mock
%% - audit_retention_days => 90
%% - rbac_enabled => true
%% - tracing_enabled => true
%% - idempotency_enabled => true
%% - telemetry_enabled => true
%% - log_level => info
%% - metrics_export_enabled => false

%% Customize template
CustomTemplate = router_config_validator:get_config_template(#{
    grpc_enabled => true,
    grpc_port => 8080
}).
```

### Required Configuration

Check required configuration:

```erlang
%% Check required configuration
RequiredCheck = router_config_validator:check_required_config(),

Passed = maps:get(passed, RequiredCheck),
RequiredKeys = maps:get(required_keys, RequiredCheck),
Results = maps:get(results, RequiredCheck),

%% Required keys:
%% - grpc_enabled
%% - nats_mode
```

### Configuration Compatibility

Check configuration compatibility:

```erlang
%% Check configuration compatibility
Config = #{
    grpc_enabled => true,
    admin_grpc_enabled => true,
    telemetry_enabled => true,
    metrics_export_enabled => true
},

Compatibility = router_config_validator:check_config_compatibility(Config),

Passed = maps:get(passed, Compatibility),
AdminGrpc = maps:get(admin_grpc, Compatibility),
Metrics = maps:get(metrics, Compatibility),
Cp2Features = maps:get(cp2_features, Compatibility).

%% Compatibility checks:
%% - Admin gRPC requires gRPC to be enabled
%% - Metrics export requires telemetry to be enabled
%% - CP2+ features require cp2_plus_allowed to be true
```

## Configuration Templates

### Development Template

```erlang
#{
    grpc_enabled => false,
    grpc_port => 9000,
    admin_api_key => ~"dev_key",
    nats_mode => mock,
    audit_retention_days => 7,
    rbac_enabled => false,
    tracing_enabled => false,
    idempotency_enabled => false,
    telemetry_enabled => true,
    log_level => debug,
    metrics_export_enabled => false
}
```

### Production Template

```erlang
#{
    grpc_enabled => true,
    grpc_port => 9000,
    admin_api_key => ~"PRODUCTION_KEY_FROM_ENV",
    nats_mode => real,
    audit_retention_days => 90,
    rbac_enabled => true,
    tracing_enabled => true,
    idempotency_enabled => true,
    telemetry_enabled => true,
    log_level => info,
    metrics_export_enabled => true
}
```

## Deployment Procedures

### Standard Deployment

1. **Pre-deployment validation**
   ```bash
   ./scripts/deploy.sh --validate
   ```

2. **Deploy new version**
   ```bash
   ./scripts/deploy.sh v1.0.0
   ```

3. **Verify deployment**
   ```bash
   ./scripts/deploy.sh --status
   ```

### Rollback Procedure

1. **Check deployment status**
   ```bash
   ./scripts/deploy.sh --status
   ```

2. **Rollback to previous version**
   ```bash
   ./scripts/deploy.sh v1.0.0 --rollback
   ```

3. **Verify rollback**
   ```bash
   ./scripts/deploy.sh --status
   ```

## Testing

### Deployment Tests

```erlang
test_validate_deployment(_Config) ->
    Version = ~"v1.0.0",
    {ok, Report} = router_deployment:validate_deployment(Version),
    ?assertEqual(true, maps:get(passed, Report)),
    ok.
```

### Configuration Tests

```erlang
test_validate_config(_Config) ->
    {ok, Report} = router_config_validator:validate_config(),
    ?assertEqual(true, maps:get(all_valid, Report)),
    ok.
```

## Related Documentation

- `src/router_deployment.erl` - Deployment automation module
- `src/router_config_validator.erl` - Configuration validation module
- `scripts/deploy.sh` - Deployment script
- `CONFIGURATION_REFERENCE.md` - Configuration reference
- `OPERATIONAL_RUNBOOK.md` - Operational procedures

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team

