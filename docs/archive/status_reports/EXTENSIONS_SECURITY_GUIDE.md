# Extensions Security Guide

**Version**: CP2-LC  
**Date**: 2025-11-30  
**Status**: Production Ready  
**Target Audience**: Security teams, SRE/Ops, Extension developers

---

## Overview

This guide provides security best practices and hardening procedures for the Extensions ecosystem. It covers authorization, payload validation, abuse prevention, and production deployment checklists.

**Key Security Areas**:
- Authorization and RBAC for extension registry operations
- Payload validation and DoS protection
- Abuse prevention (retry limits, timeouts, pipeline depth)
- Production deployment security checklist

---

## Table of Contents

1. [Authorization](#authorization)
2. [Payload Validation](#payload-validation)
3. [Abuse Prevention](#abuse-prevention)
4. [Production Deployment Checklist](#production-deployment-checklist)
5. [Security Testing](#security-testing)
6. [References](#references)

---

## Authorization

### 1. RBAC for Extension Registry Operations

**Resource**: `extension_registry`

**Actions**:
- `read` - View extension registry entries
- `write` - Register/update extensions
- `delete` - Remove extensions
- `admin` - Administrative operations (enable/disable, version management)

**Default Roles**:

#### Admin Role
- Full access to extension registry
- Can register, update, delete extensions
- Can enable/disable extensions
- Can manage extension versions and routing rules

**Permissions**:
```erlang
[
    {~"read", ~"extension_registry"},
    {~"write", ~"extension_registry"},
    {~"delete", ~"extension_registry"},
    {~"admin", ~"extension_registry"}
]
```

#### Operator Role
- Can read and update extensions
- Cannot delete extensions
- Cannot manage versions/routing rules

**Permissions**:
```erlang
[
    {~"read", ~"extension_registry"},
    {~"write", ~"extension_registry"}
]
```

#### Viewer Role
- Read-only access to extension registry

**Permissions**:
```erlang
[
    {~"read", ~"extension_registry"}
]
```

### 2. Extension Registry Operations Authorization

**Register Extension**:
```erlang
%% Check authorization before registering
case router_rbac:can_access(UserId, TenantId, ~"write", ~"extension_registry") of
    true ->
        router_extension_registry_db:register_extension(Extension);
    false ->
        {error, unauthorized}
end.
```

**Update Extension**:
```erlang
%% Check authorization before updating
case router_rbac:can_access(UserId, TenantId, ~"write", ~"extension_registry") of
    true ->
        router_extension_registry_db:update_extension(ExtensionId, Updates);
    false ->
        {error, unauthorized}
end.
```

**Delete Extension**:
```erlang
%% Check authorization before deleting
case router_rbac:can_access(UserId, TenantId, ~"delete", ~"extension_registry") of
    true ->
        router_extension_registry_db:delete_extension(ExtensionId);
    false ->
        {error, unauthorized}
end.
```

**Enable/Disable Extension**:
```erlang
%% Check admin authorization
case router_rbac:can_access(UserId, TenantId, ~"admin", ~"extension_registry") of
    true ->
        router_extension_registry_db:update_extension(ExtensionId, [{enabled, Enabled}]);
    false ->
        {error, unauthorized}
end.
```

**Manage Extension Versions**:
```erlang
%% Check admin authorization for version management
case router_rbac:can_access(UserId, TenantId, ~"admin", ~"extension_registry") of
    true ->
        router_extension_registry_db:register_extension_version(ExtensionId, Version, Config);
    false ->
        {error, unauthorized}
end.
```

### 3. Policy-Level Extension Authorization

**Control Extension Usage in Policies**:
- Policies can restrict which extensions are available per tenant
- Extension registry can include `allowed_tenants` list
- Router validates extension access before invocation

**Implementation**:
```erlang
%% Check if extension is allowed for tenant
check_extension_access(ExtensionId, TenantId) ->
    case router_extension_registry:lookup(ExtensionId) of
        {ok, Extension} ->
            AllowedTenants = maps:get(allowed_tenants, Extension#extension.metadata, undefined),
            case AllowedTenants of
                undefined ->
                    true;  % No restrictions
                List when is_list(List) ->
                    lists:member(TenantId, List);
                _ ->
                    false
            end;
        {error, _} ->
            false
    end.
```

---

## Payload Validation

### 1. Payload Size Limits

**Configuration**: `nats_max_payload_size` (default: 1MB)

**Validation Points**:
1. **Before NATS Request**: Validate request payload size
2. **After NATS Response**: Validate response payload size
3. **Per-Extension Limits**: Configurable per extension

**Implementation**:
```erlang
%% Validate payload size before sending to extension
validate_payload_size(Payload, MaxSize) ->
    PayloadSize = byte_size(jsx:encode(Payload)),
    case PayloadSize > MaxSize of
        true ->
            {error, {payload_too_large, #{
                size => PayloadSize,
                max_size => MaxSize
            }}};
        false ->
            ok
    end.
```

**Per-Extension Limits**:
```json
{
  "id": "normalize_text",
  "type": "pre",
  "subject": "beamline.ext.pre.normalize_text.v1",
  "timeout_ms": 100,
  "retry": 0,
  "metadata": {
    "max_payload_size": 512000,  // 500KB per extension
    "max_response_size": 512000
  }
}
```

### 2. JSON Schema Validation

**Request Schema** (CP-Ext):
```json
{
  "type": "object",
  "required": ["payload"],
  "properties": {
    "trace_id": {
      "type": "string",
      "format": "uuid",
      "maxLength": 128
    },
    "tenant_id": {
      "type": "string",
      "maxLength": 255
    },
    "payload": {
      "type": "object",
      "required": ["message_id", "message_type", "payload"]
    },
    "metadata": {
      "type": "object",
      "maxProperties": 50,
      "additionalProperties": true
    }
  },
  "maxProperties": 10
}
```

**Response Schema** (Pre/Post):
```json
{
  "type": "object",
  "properties": {
    "payload": {
      "type": "object"
    },
    "metadata": {
      "type": "object",
      "maxProperties": 50
    }
  },
  "maxProperties": 10
}
```

**Response Schema** (Validator):
```json
{
  "type": "object",
  "required": ["status"],
  "properties": {
    "status": {
      "type": "string",
      "enum": ["ok", "reject"]
    },
    "reason": {
      "type": "string",
      "maxLength": 255
    },
    "details": {
      "type": "object",
      "maxProperties": 20
    }
  },
  "maxProperties": 10
}
```

**Validation Implementation**:
```erlang
%% Validate extension request payload
validate_extension_request(Payload) ->
    %% Check required fields
    case maps:is_key(~"payload", Payload) of
        false ->
            {error, {validation_failed, ~"Missing required field: payload"}};
        true ->
            %% Check payload size
            PayloadSize = byte_size(jsx:encode(Payload)),
            MaxSize = application:get_env(beamline_router, extension_max_payload_size, 1048576),
            case PayloadSize > MaxSize of
                true ->
                    {error, {payload_too_large, #{
                        size => PayloadSize,
                        max_size => MaxSize
                    }}};
                false ->
                    %% Check metadata size
                    Metadata = maps:get(~"metadata", Payload, #{}),
                    MetadataSize = byte_size(jsx:encode(Metadata)),
                    MaxMetadataSize = 65536,  % 64KB
                    case MetadataSize > MaxMetadataSize of
                        true ->
                            {error, {metadata_too_large, #{
                                size => MetadataSize,
                                max_size => MaxMetadataSize
                            }}};
                        false ->
                            ok
                    end
            end
    end.
```

### 3. DoS Protection

**Protection Mechanisms**:

1. **Payload Size Limits**:
   - Global limit: 1MB (configurable)
   - Per-extension limit: Configurable in metadata
   - Request and response validation

2. **Complexity Limits**:
   - Maximum nested depth: 10 levels
   - Maximum array size: 1000 elements
   - Maximum object properties: 50 per object

3. **Rate Limiting** (Future):
   - Per-extension rate limits
   - Per-tenant rate limits
   - Circuit breaker for high error rates

**Implementation**:
```erlang
%% Validate JSON complexity
validate_json_complexity(Json) ->
    Depth = calculate_json_depth(Json),
    case Depth > 10 of
        true ->
            {error, {json_too_deep, #{depth => Depth, max_depth => 10}}};
        false ->
            ArraySize = calculate_max_array_size(Json),
            case ArraySize > 1000 of
                true ->
                    {error, {array_too_large, #{size => ArraySize, max_size => 1000}}};
                false ->
                    ObjectProps = calculate_max_object_properties(Json),
                    case ObjectProps > 50 of
                        true ->
                            {error, {object_too_large, #{properties => ObjectProps, max_properties => 50}}};
                        false ->
                            ok
                    end
            end
    end.
```

---

## Abuse Prevention

### 1. Retry Limits

**Configuration**:
- `extension_max_retries` (default: 3)
- Per-extension retry limit in registry
- Global retry limit override

**Validation**:
```erlang
%% Validate retry count
validate_retry_count(RetryCount) ->
    MaxRetries = application:get_env(beamline_router, extension_max_retries, 3),
    case RetryCount > MaxRetries of
        true ->
            {error, {retry_limit_exceeded, #{
                retry_count => RetryCount,
                max_retries => MaxRetries
            }}};
        false ->
            ok
    end.
```

**Per-Extension Limits**:
```json
{
  "id": "normalize_text",
  "retry": 2,  // Max 2 retries for this extension
  "metadata": {
    "max_retries_override": 1  // Can override to lower value
  }
}
```

### 2. Timeout Limits

**Configuration**:
- `extension_max_timeout_ms` (default: 5000ms)
- `extension_min_timeout_ms` (default: 10ms)
- Per-extension timeout in registry

**Validation**:
```erlang
%% Validate timeout
validate_timeout(TimeoutMs) ->
    MinTimeout = application:get_env(beamline_router, extension_min_timeout_ms, 10),
    MaxTimeout = application:get_env(beamline_router, extension_max_timeout_ms, 5000),
    case TimeoutMs < MinTimeout orelse TimeoutMs > MaxTimeout of
        true ->
            {error, {timeout_out_of_range, #{
                timeout_ms => TimeoutMs,
                min_timeout_ms => MinTimeout,
                max_timeout_ms => MaxTimeout
            }}};
        false ->
            ok
    end.
```

### 3. Pipeline Depth Limits

**Configuration**:
- `extension_max_pipeline_depth` (default: 10)
- Limits total extensions in pipeline (pre + validators + post)

**Validation**:
```erlang
%% Validate pipeline depth
validate_pipeline_depth(Pre, Validators, Post) ->
    TotalDepth = length(Pre) + length(Validators) + length(Post),
    MaxDepth = application:get_env(beamline_router, extension_max_pipeline_depth, 10),
    case TotalDepth > MaxDepth of
        true ->
            {error, {pipeline_too_deep, #{
                depth => TotalDepth,
                max_depth => MaxDepth,
                pre_count => length(Pre),
                validators_count => length(Validators),
                post_count => length(Post)
            }}};
        false ->
            ok
    end.
```

**Per-Type Limits**:
```erlang
%% Validate per-type limits
validate_extension_counts(Pre, Validators, Post) ->
    MaxPre = application:get_env(beamline_router, extension_max_pre_count, 5),
    MaxValidators = application:get_env(beamline_router, extension_max_validators_count, 5),
    MaxPost = application:get_env(beamline_router, extension_max_post_count, 5),
    
    case length(Pre) > MaxPre of
        true ->
            {error, {too_many_pre_processors, #{
                count => length(Pre),
                max_count => MaxPre
            }}};
        false ->
            case length(Validators) > MaxValidators of
                true ->
                    {error, {too_many_validators, #{
                        count => length(Validators),
                        max_count => MaxValidators
                    }}};
                false ->
                    case length(Post) > MaxPost of
                        true ->
                            {error, {too_many_post_processors, #{
                                count => length(Post),
                                max_count => MaxPost
                            }}};
                        false ->
                            ok
                    end
            end
    end.
```

### 4. Per-Tenant/Policy Limits

**Configuration**:
- `extension_max_per_tenant` (default: 20)
- `extension_max_per_policy` (default: 10)

**Validation**:
```erlang
%% Validate tenant extension count
validate_tenant_extension_count(TenantId) ->
    Count = get_tenant_extension_count(TenantId),
    MaxCount = application:get_env(beamline_router, extension_max_per_tenant, 20),
    case Count > MaxCount of
        true ->
            {error, {tenant_extension_limit_exceeded, #{
                tenant_id => TenantId,
                count => Count,
                max_count => MaxCount
            }}};
        false ->
            ok
    end.

%% Validate policy extension count
validate_policy_extension_count(Policy) ->
    Pre = Policy#policy.pre,
    Validators = Policy#policy.validators,
    Post = Policy#policy.post,
    TotalCount = length(Pre) + length(Validators) + length(Post),
    MaxCount = application:get_env(beamline_router, extension_max_per_policy, 10),
    case TotalCount > MaxCount of
        true ->
            {error, {policy_extension_limit_exceeded, #{
                policy_id => Policy#policy.policy_id,
                count => TotalCount,
                max_count => MaxCount
            }}};
        false ->
            ok
    end.
```

### 5. Extension Subject Validation

**Validation Rules**:
- Subject must match pattern: `beamline.ext.{type}.{id}.{version}`
- Subject must be registered in Extension Registry
- Subject cannot be modified after registration (immutable)

**Implementation**:
```erlang
%% Validate extension subject
validate_extension_subject(Subject) ->
    Pattern = "^beamline\\.ext\\.(pre|validator|post|provider)\\.[a-z0-9_]+\\.[v][0-9]+$",
    case re:run(Subject, Pattern, [{capture, none}]) of
        match ->
            ok;
        nomatch ->
            {error, {invalid_subject_format, #{
                subject => Subject,
                expected_pattern => Pattern
            }}}
    end.
```

---

## Production Deployment Checklist

### Pre-Deployment Security Checks

#### 1. Authorization Configuration

- [ ] RBAC enabled for extension registry operations
- [ ] Admin role assigned only to authorized users
- [ ] Operator role assigned to extension maintainers
- [ ] Viewer role assigned to monitoring/audit users
- [ ] Extension registry operations require authentication
- [ ] API keys rotated and stored securely

**Commands**:
```erlang
%% Verify RBAC is enabled
application:get_env(beamline_router, rbac_enabled, false).

%% Verify admin role exists
router_rbac:get_user_roles(UserId, TenantId).
```

#### 2. Payload Validation

- [ ] Global payload size limit configured (default: 1MB)
- [ ] Per-extension payload limits configured (if needed)
- [ ] JSON schema validation enabled
- [ ] Complexity limits enforced (depth, array size, object properties)
- [ ] Request and response validation active

**Configuration**:
```erlang
{beamline_router, [
    {nats_max_payload_size, 1048576},  % 1MB
    {extension_max_payload_size, 1048576},  % 1MB
    {extension_max_metadata_size, 65536},  % 64KB
    {extension_json_max_depth, 10},
    {extension_json_max_array_size, 1000},
    {extension_json_max_object_properties, 50}
]}
```

#### 3. Abuse Prevention Limits

- [ ] Retry limits configured (default: 3)
- [ ] Timeout limits configured (min: 10ms, max: 5000ms)
- [ ] Pipeline depth limit configured (default: 10)
- [ ] Per-type extension limits configured (default: 5 each)
- [ ] Per-tenant extension limit configured (default: 20)
- [ ] Per-policy extension limit configured (default: 10)

**Configuration**:
```erlang
{beamline_router, [
    {extension_max_retries, 3},
    {extension_min_timeout_ms, 10},
    {extension_max_timeout_ms, 5000},
    {extension_max_pipeline_depth, 10},
    {extension_max_pre_count, 5},
    {extension_max_validators_count, 5},
    {extension_max_post_count, 5},
    {extension_max_per_tenant, 20},
    {extension_max_per_policy, 10}
]}
```

#### 4. Extension Registry Security

- [ ] Extension registry source mode configured (database recommended for production)
- [ ] Database credentials stored securely (environment variables, secrets manager)
- [ ] Extension registry sync interval configured (default: 60 seconds)
- [ ] Extension subject validation enabled
- [ ] Extension versioning enabled (if using multiple versions)
- [ ] Circuit breaker enabled (CP3)

**Configuration**:
```erlang
{beamline_router, [
    {extension_registry, [
        {source, database},  % Use database in production
        {db_enabled, true},
        {db_host, os:getenv("EXTENSION_REGISTRY_DB_HOST")},
        {db_port, list_to_integer(os:getenv("EXTENSION_REGISTRY_DB_PORT", "5432"))},
        {db_name, os:getenv("EXTENSION_REGISTRY_DB_NAME")},
        {db_user, os:getenv("EXTENSION_REGISTRY_DB_USER")},
        {db_password, os:getenv("EXTENSION_REGISTRY_DB_PASSWORD")},  % From secrets manager
        {sync_interval_seconds, 60},
        {circuit_breaker_enabled, true}  % CP3
    ]}
]}
```

#### 5. Network Security

- [ ] NATS TLS enabled for extension communication
- [ ] NATS certificates valid and not expired
- [ ] NATS subject namespace isolation (per-tenant subjects if needed)
- [ ] Extension services authenticate to NATS
- [ ] NATS connection limits configured

**Configuration**:
```erlang
{beamline_router, [
    {nats_tls_enabled, true},
    {nats_tls_cert_file, "/path/to/cert.pem"},
    {nats_tls_key_file, "/path/to/key.pem"},
    {nats_tls_ca_file, "/path/to/ca.pem"},
    {nats_connect_timeout_ms, 5000},
    {nats_reconnect_attempts, 10}
]}
```

#### 6. Logging and Monitoring

- [ ] Structured JSON logging enabled
- [ ] PII filtering enabled in logs
- [ ] Extension invocation metrics collected
- [ ] Circuit breaker metrics collected
- [ ] Security events logged (unauthorized access, validation failures)
- [ ] Alert rules configured for security events

**Configuration**:
```erlang
{beamline_router, [
    {telemetry_enabled, true},
    {log_dir, "/var/log/router"},
    {pii_filtering_enabled, true}
]}
```

#### 7. Extension Service Security

- [ ] Extension services run with least privilege
- [ ] Extension services authenticate to NATS
- [ ] Extension services validate incoming requests
- [ ] Extension services rate limit their own processing
- [ ] Extension services log security events
- [ ] Extension services use secure communication (TLS)

### Post-Deployment Security Monitoring

#### 1. Security Metrics

**Monitor**:
- Unauthorized extension registry access attempts
- Payload size violations
- Retry limit violations
- Pipeline depth violations
- Extension subject validation failures
- Circuit breaker activations

**Prometheus Queries**:
```promql
# Unauthorized access attempts
sum(rate(router_rbac_denied_total{resource="extension_registry"}[5m]))

# Payload size violations
sum(rate(router_extension_payload_too_large_total[5m]))

# Retry limit violations
sum(rate(router_extension_retry_limit_exceeded_total[5m]))

# Pipeline depth violations
sum(rate(router_extension_pipeline_too_deep_total[5m]))
```

#### 2. Security Alerts

**Alert Rules** (in `docs/observability/router-alert-rules.yaml`):
```yaml
- alert: ExtensionUnauthorizedAccess
  expr: sum(rate(router_rbac_denied_total{resource="extension_registry"}[5m])) > 0.1
  for: 5m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Unauthorized extension registry access attempts"
    description: "{{ $value }} unauthorized access attempts/sec to extension registry"

- alert: ExtensionPayloadSizeViolations
  expr: sum(rate(router_extension_payload_too_large_total[5m])) > 0.1
  for: 5m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension payload size violations"
    description: "{{ $value }} payload size violations/sec"

- alert: ExtensionRetryLimitViolations
  expr: sum(rate(router_extension_retry_limit_exceeded_total[5m])) > 0.1
  for: 5m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension retry limit violations"
    description: "{{ $value }} retry limit violations/sec"
```

#### 3. Security Audit

**Regular Audits**:
- Review extension registry access logs (weekly)
- Review extension invocation patterns (monthly)
- Review security metrics and alerts (daily)
- Review extension service security configurations (quarterly)
- Review RBAC role assignments (monthly)

**Audit Checklist**:
- [ ] All extension registry operations are authorized
- [ ] No unauthorized extensions registered
- [ ] Extension subjects follow naming conventions
- [ ] Payload sizes within limits
- [ ] Retry/timeout limits enforced
- [ ] Pipeline depths within limits
- [ ] Circuit breakers functioning correctly
- [ ] Security alerts configured and tested

---

## Security Testing

### 1. Negative Tests

**Test Cases**:
- Unauthorized extension registry access
- Payload size violations
- Retry limit violations
- Pipeline depth violations
- Invalid extension subjects
- Malformed extension requests/responses
- DoS attempts (large payloads, deep nesting)

**Test Suite**: `router_extensions_security_SUITE.erl`

**Example Tests**:
```erlang
%% Test: Unauthorized extension registry access
test_unauthorized_extension_registry_access(_Config) ->
    %% Attempt to register extension without authorization
    Result = router_extension_registry_db:register_extension(Extension, ~"unauthorized_user", ~"tenant"),
    ?assertMatch({error, unauthorized}, Result).

%% Test: Payload size violation
test_payload_size_violation(_Config) ->
    %% Create oversized payload
    LargePayload = binary:copy(~"x", 2000000),  % 2MB > 1MB limit
    Request = #{~"payload" => LargePayload},
    Result = router_extension_invoker:invoke(~"normalize_text", Request, #{}),
    ?assertMatch({error, {payload_too_large, _}}, Result).

%% Test: Pipeline depth violation
test_pipeline_depth_violation(_Config) ->
    %% Create policy with too many extensions
    Pre = [#{id => <<"pre_", (integer_to_binary(N))/binary>>} || N <- lists:seq(1, 11)],  % 11 > 10 limit
    Policy = #policy{pre = Pre, validators = [], post = []},
    Result = router_decider:decide(RouteRequest, Policy, Context),
    ?assertMatch({error, {pipeline_too_deep, _}}, Result).
```

### 2. Fuzz Testing

**Fuzz Test Cases**:
- Random payload sizes
- Random JSON structures
- Random extension IDs
- Random policy configurations
- Random retry counts
- Random timeout values

**Fuzz Test Suite**: `router_extensions_fuzz_SUITE.erl`

**Example Fuzz Test**:
```erlang
%% Fuzz test: Random payload sizes
fuzz_payload_sizes() ->
    lists:foreach(fun(_) ->
        PayloadSize = rand:uniform(5000000),  % 0-5MB
        Payload = binary:copy(~"x", PayloadSize),
        Request = #{~"payload" => Payload},
        Result = router_extension_invoker:invoke(~"normalize_text", Request, #{}),
        %% Should either succeed or fail with payload_too_large
        case Result of
            {ok, _} -> ok;
            {error, {payload_too_large, _}} -> ok;
            _ -> ct:fail("Unexpected result: ~p", [Result])
        end
    end, lists:seq(1, 1000)).
```

### 3. Penetration Testing

**Test Scenarios**:
1. **Unauthorized Access**:
   - Attempt to register extension without admin role
   - Attempt to delete extension without admin role
   - Attempt to enable/disable extension without admin role

2. **Payload Injection**:
   - Attempt to inject malicious JSON
   - Attempt to inject SQL (if extension uses database)
   - Attempt to inject code (if extension evaluates code)

3. **DoS Attacks**:
   - Send oversized payloads
   - Send deeply nested JSON
   - Send requests with maximum retries
   - Send requests with maximum pipeline depth

4. **Subject Hijacking**:
   - Attempt to register extension with existing subject
   - Attempt to modify extension subject after registration
   - Attempt to use extension subject without registration

---

## References

- `docs/SECURITY_GUIDE.md` - General security guide
- `docs/EXTENSIONS_API.md` - Extensions API specification
- `apps/otp/router/docs/EXTENSIONS_RUNBOOK.md` - Extensions operations runbook
- `apps/otp/router/src/router_rbac.erl` - RBAC implementation (includes extension_registry resource)
- `apps/otp/router/src/router_extension_invoker.erl` - Extension invoker (includes payload/timeout/retry validation)
- `apps/otp/router/src/router_decider.erl` - Pipeline execution (includes pipeline depth validation)
- `apps/otp/router/test/router_extensions_security_SUITE.erl` - Security test suite
- `docs/observability/router-alert-rules.yaml` - Alert rules

---

## Change History

**v1.0 (2025-11-30)**:
- Initial security guide creation
- Authorization and RBAC procedures
- Payload validation and DoS protection
- Abuse prevention limits
- Production deployment checklist
- Security testing guidelines
- RBAC resource `extension_registry` added to `router_rbac.erl`
- Validation functions added to `router_extension_invoker.erl` and `router_decider.erl`
- Security test suite created: `router_extensions_security_SUITE.erl`
- Configuration limits added to `beamline_router.app.src`

