# Security Guide

This guide provides comprehensive information on security best practices, audit procedures, and vulnerability reporting for the router project.

## Table of Contents

1. [Security Best Practices](#security-best-practices)
2. [Security Audit Procedures](#security-audit-procedures)
3. [Vulnerability Reporting](#vulnerability-reporting)
4. [Security Controls](#security-controls)
5. [Incident Response](#incident-response)

## Security Best Practices

### Input Validation

#### Request Validation

Always validate input data:

```erlang
%% In router_grpc.erl
validate_request(Request) ->
    case maps:get(~"tenant_id", Request, undefined) of
        undefined ->
            {error, missing_tenant_id};
        TenantId when is_binary(TenantId), byte_size(TenantId) > 0 ->
            validate_tenant_id(TenantId);
        _ ->
            {error, invalid_tenant_id}
    end.
```

#### Type Checking

Use Dialyzer for type checking:

```erlang
%% Use -spec for type checking
-spec validate_tenant_id(binary()) -> ok | {error, term()}.
validate_tenant_id(TenantId) ->
    case byte_size(TenantId) =< 255 of
        true -> ok;
        false -> {error, tenant_id_too_long}
    end.
```

#### Sanitization

Sanitize user input:

```erlang
%% Sanitize string input
sanitize_string(Input) ->
    %% Remove control characters
    lists:filter(fun(Char) ->
        Char >= 32 andalso Char =< 126
    end, Input).
```

### Secret Management

#### No Secrets in Code

Never hardcode secrets:

```erlang
%% Bad: Hardcoded secret
API_KEY = ~"secret_key_123".

%% Good: Load from environment
API_KEY = application:get_env(beamline_router, api_key, undefined).
```

#### No Secrets in Logs

Never log secrets:

```erlang
%% Bad: Logging secret
router_logger:info(~"API call", #{
    ~"api_key" => API_KEY  % Secret in log!
}).

%% Good: Redact secret
router_logger:info(~"API call", #{
    ~"api_key" => ~"***REDACTED***"
}).
```

#### Secret Rotation

Implement secret rotation:

```erlang
%% In router_secrets.erl
get_secret(Key) ->
    case ets:lookup(secrets_cache, Key) of
        [{Key, Secret, Expiry}] ->
            case erlang:system_time(second) < Expiry of
                true -> Secret;
                false -> refresh_secret(Key)
            end;
        [] ->
            load_secret(Key)
    end.
```

### Access Control

#### RBAC Implementation

Use RBAC for access control:

```erlang
%% In router_grpc.erl
check_permission(UserId, Action, Resource) ->
    case router_rbac:can_access(UserId, Action, Resource, undefined) of
        true -> ok;
        false -> {error, permission_denied}
    end.
```

#### Role-Based Checks

Check roles before operations:

```erlang
%% Check admin role
case router_rbac:is_admin(UserId, TenantId) of
    true -> perform_admin_operation();
    false -> {error, admin_required}
end.
```

### Error Handling

#### Error Message Sanitization

Don't leak sensitive information in errors:

```erlang
%% Bad: Leaking internal details
{error, {database_error, "Connection failed: user=admin, password=secret"}}.

%% Good: Generic error message
{error, database_connection_failed}.
```

#### Error Mapping

Use centralized error mapping:

```erlang
%% In router_error.erl
to_grpc(Error, Context) ->
    case Error of
        {error, permission_denied} ->
            {grpc_error, ?GRPC_STATUS_PERMISSION_DENIED, ~"Access denied"};
        {error, invalid_input} ->
            {grpc_error, ?GRPC_STATUS_INVALID_ARGUMENT, ~"Invalid input"};
        _ ->
            {grpc_error, ?GRPC_STATUS_INTERNAL, ~"Internal error"}
    end.
```

### Data Privacy

#### PII Filtering

Filter PII from logs:

```erlang
%% In router_logger.erl
filter_pii(LogMap) ->
    PIIKeys = [~"user_id", ~"email", ~"phone"],
    maps:fold(fun(Key, Value, Acc) ->
        case lists:member(Key, PIIKeys) of
            true -> maps:put(Key, ~"***REDACTED***", Acc);
            false -> maps:put(Key, Value, Acc)
        end
    end, #{}, LogMap).
```

#### Data Retention

Implement data retention policies:

```erlang
%% Clean up old data
cleanup_old_data(Table, RetentionDays) ->
    Cutoff = erlang:system_time(second) - (RetentionDays * 86400),
    MatchSpec = [{{'_', '$1'}, [{'<', '$1', Cutoff}], [true]}],
    ets:select_delete(Table, MatchSpec).
```

## Security Audit Procedures

### Code Review Checklist

#### Security Review Items

- [ ] Input validation is performed
- [ ] No secrets in code or logs
- [ ] RBAC checks are in place
- [ ] Error messages don't leak sensitive information
- [ ] PII is filtered from logs
- [ ] SQL injection prevention (if applicable)
- [ ] XSS prevention (if applicable)
- [ ] CSRF protection (if applicable)

#### Automated Security Checks

Run security tools:

```bash
# Dialyzer for type checking
rebar3 dialyzer

# Xref for cross-reference analysis
rebar3 xref

# Security linter (if available)
rebar3 lint
```

### Dependency Audit

#### Dependency Scanning

Scan dependencies for vulnerabilities:

```bash
# Check for known vulnerabilities
mix deps.audit  # For Elixir projects
# Or use: rebar3 audit (if available)
```

#### License Compliance

Verify license compliance:

```bash
# Check licenses
rebar3 licenses
```

### Configuration Audit

#### Security Configuration

Review security-related configuration:

```erlang
%% In application config
{beamline_router, [
    {rbac_enabled, true},
    {pii_filtering_enabled, true},
    {secret_rotation_enabled, true},
    {audit_logging_enabled, true}
]}.
```

#### Access Control Configuration

Review access control settings:

```erlang
%% RBAC configuration
{rbac, [
    {default_role, viewer},
    {admin_roles, [admin, super_admin]},
    {permission_cache_ttl, 3600}
]}.
```

### Penetration Testing

#### Test Scenarios

Test common attack vectors:

1. **Input Injection**: Malformed requests
2. **Privilege Escalation**: Unauthorized access attempts
3. **DoS Attacks**: Resource exhaustion
4. **Information Disclosure**: Error message analysis

#### Test Procedures

```erlang
%% In test/router_security_SUITE.erl
test_input_injection(_Config) ->
    %% Test SQL injection (if applicable)
    MaliciousInput = ~"'; DROP TABLE users; --",
    Result = router_grpc:decide(#{~"input" => MaliciousInput}, #{}),
    ?assertMatch({error, _}, Result).

test_privilege_escalation(_Config) ->
    %% Test unauthorized access
    UserId = ~"regular_user",
    Result = router_rbac:can_access(UserId, ~"admin", ~"policy", undefined),
    ?assertEqual(false, Result).
```

## Vulnerability Reporting

### Reporting Process

#### Report Format

Include in vulnerability report:

1. **Description**: What the vulnerability is
2. **Impact**: What can be exploited
3. **Steps to Reproduce**: How to trigger
4. **Proof of Concept**: Code or commands
5. **Suggested Fix**: How to address

#### Report Channels

- **Email**: security@example.com
- **Issue Tracker**: Create private security issue
- **PGP Key**: Use for sensitive reports

### Response Timeline

#### Acknowledgment

- **Within 24 hours**: Acknowledge receipt
- **Within 7 days**: Initial assessment
- **Within 30 days**: Fix or mitigation plan

#### Disclosure

- **Coordinated Disclosure**: After fix is available
- **CVE Assignment**: For significant vulnerabilities
- **Security Advisory**: Public notification

### Responsible Disclosure

#### Guidelines

- Don't publicly disclose before fix is available
- Allow reasonable time for fix (30-90 days)
- Work with maintainers on disclosure timeline
- Don't exploit vulnerabilities beyond proof of concept

## Security Controls

### Authentication

#### API Authentication

Implement API authentication:

```erlang
%% In router_grpc.erl
authenticate_request(Context) ->
    case extract_api_key(Context) of
        {ok, APIKey} ->
            validate_api_key(APIKey);
        {error, _} ->
            {error, authentication_required}
    end.
```

#### Token Validation

Validate tokens:

```erlang
%% Validate JWT token
validate_token(Token) ->
    case jwt:decode(Token, Secret) of
        {ok, Claims} ->
            validate_claims(Claims);
        {error, _} ->
            {error, invalid_token}
    end.
```

### Authorization

#### Permission Checks

Check permissions before operations:

```erlang
%% In router_admin_grpc.erl
upsert_policy(Req, Context) ->
    UserId = extract_user_id(Context),
    case router_rbac:can_access(UserId, ~"write", ~"policy", undefined) of
        true -> do_upsert_policy(Req);
        false -> {error, permission_denied}
    end.
```

#### Resource-Level Authorization

Implement resource-level checks:

```erlang
%% Check tenant access
check_tenant_access(UserId, TenantId) ->
    case router_rbac:can_access(UserId, ~"read", ~"tenant", TenantId) of
        true -> ok;
        false -> {error, tenant_access_denied}
    end.
```

### Encryption

#### Data in Transit

Use TLS for data in transit:

```erlang
%% gRPC TLS configuration
{grpc, [
    {tls, true},
    {tls_cert, "/path/to/cert.pem"},
    {tls_key, "/path/to/key.pem"}
]}.
```

#### Data at Rest

Encrypt sensitive data at rest:

```erlang
%% Encrypt sensitive data
encrypt_data(Data, Key) ->
    crypto:crypto_one_time(aes_256_gcm, Key, IV, Data, true).
```

### Audit Logging

#### Security Events

Log security events:

```erlang
%% Log authentication failure
log_security_event(EventType, Details) ->
    router_logger:security(~"Security event", #{
        ~"event_type" => EventType,
        ~"timestamp" => erlang:system_time(second),
        ~"details" => Details
    }).
```

#### Audit Trail

Maintain audit trail:

```erlang
%% Audit log entry
audit_log(UserId, Action, Resource, Result) ->
    Entry = #{
        user_id => UserId,
        action => Action,
        resource => Resource,
        result => Result,
        timestamp => erlang:system_time(second)
    },
    store_audit_entry(Entry).
```

## Incident Response

### Security Incident Process

#### Detection

- Monitor security logs
- Review error patterns
- Check for unusual activity

#### Response

1. **Contain**: Isolate affected systems
2. **Assess**: Determine scope and impact
3. **Remediate**: Fix vulnerability
4. **Recover**: Restore normal operations
5. **Review**: Post-incident analysis

#### Communication

- Notify security team immediately
- Document incident details
- Prepare public communication (if needed)

### Post-Incident

#### Lessons Learned

- Review incident timeline
- Identify root cause
- Document improvements
- Update procedures

#### Prevention

- Implement additional controls
- Update monitoring
- Enhance testing
- Improve documentation

## Related Documentation

- `DEVELOPER_GUIDE.md` - Development workflow
- `OPERATIONAL_RUNBOOK.md` - Operational procedures
- `INCIDENT_RESPONSE_PROCEDURES.md` - Incident response
- `API_DOCUMENTATION.md` - API security considerations

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team  
**Security Contact**: security@example.com

