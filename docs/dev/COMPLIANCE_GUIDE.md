# Compliance Guide

## Overview

This guide documents license compliance and data privacy (GDPR) compliance for the Router application.

## License Compliance

### Allowed Licenses

The Router uses dependencies with the following licenses (permissive and compatible):

- **Apache-2.0** - Apache License 2.0
- **MIT** - MIT License
- **BSD-3-Clause** - BSD 3-Clause License
- **BSD-2-Clause** - BSD 2-Clause License
- **ISC** - ISC License
- **MPL-2.0** - Mozilla Public License 2.0
- **LGPL-2.1** - GNU Lesser General Public License v2.1
- **LGPL-3.0** - GNU Lesser General Public License v3.0
- **Erlang Public License** - Erlang/OTP Public License

### Restricted Licenses

The following licenses require review and are not automatically allowed:

- **GPL-2.0** - GNU General Public License v2.0
- **GPL-3.0** - GNU General Public License v3.0
- **AGPL-3.0** - GNU Affero General Public License v3.0

### License Requirements

#### Apache-2.0
- Include license notice in distribution
- Include NOTICE file if applicable

#### MIT
- Include license notice in distribution

#### BSD-3-Clause / BSD-2-Clause
- Include license notice in distribution

#### ISC
- Include license notice in distribution

#### MPL-2.0
- Include license notice in distribution

#### LGPL-2.1 / LGPL-3.0
- Include license notice in distribution
- Link to LGPL license text

#### Erlang Public License
- Include license notice in distribution

### Verification

Verify license compliance:

```erlang
%% Verify all dependencies
case router_license_compliance:verify_dependencies() of
    {ok, Report} ->
        Compliant = maps:get(compliant, Report),
        Dependencies = maps:get(dependencies, Report),
        %% Process compliance report
        process_compliance_report(Report);
    {error, Reason} ->
        handle_compliance_error(Reason)
end.

%% Get dependency licenses
Dependencies = router_license_compliance:get_dependency_licenses(),

%% Check specific license
IsCompliant = router_license_compliance:is_license_compliant(~"Apache-2.0"),

%% Get license requirements
Requirements = router_license_compliance:get_license_requirements().
```

### Standard Dependencies

The Router uses the following standard Erlang/OTP dependencies:

- **stdlib** - Erlang Public License
- **kernel** - Erlang Public License
- **common_test** - Erlang Public License
- **eunit** - Erlang Public License

### Third-Party Dependencies

The Router uses the following third-party dependencies:

- **grpcbox** - Apache-2.0
- **jsx** - MIT
- **nats** - Apache-2.0
- **telemetry** - Apache-2.0

## Data Privacy (GDPR Compliance)

### PII Handling

The Router implements PII (Personally Identifiable Information) filtering to comply with GDPR and other privacy regulations.

#### PII Fields

The following fields are considered PII and are automatically filtered:

- `email` - Email addresses
- `phone` - Phone numbers
- `ssn` - Social Security Numbers
- `credit_card` - Credit card numbers
- `ip_address` - IP addresses
- `user_id` - User identifiers
- `tenant_id` - Tenant identifiers
- `session_id` - Session identifiers
- `correlation_id` - Correlation identifiers
- `trace_id` - Trace identifiers

#### PII Filtering

PII filtering is applied automatically in:

- **Logging** - `router_logger:filter_pii/1` filters PII from log context
- **Audit Logs** - PII is filtered before storing audit entries
- **Error Messages** - PII is filtered from error messages

Example:

```erlang
%% PII filtering in logs
Context = #{
    ~"email" => ~"user@example.com",
    ~"password" => ~"secret",
    ~"normal_field" => ~"value"
},
Filtered = router_logger:filter_pii(Context),
%% Result: email and password are replaced with "[REDACTED]"
```

### Data Retention Policies

The Router implements data retention policies for GDPR compliance:

#### Audit Logs
- **Retention**: 90 days
- **Policy**: Audit logs retained for compliance and security
- **Auto-delete**: Yes

#### Application Logs
- **Retention**: 30 days
- **Policy**: Application logs retained for debugging
- **Auto-delete**: Yes

#### Metrics
- **Retention**: 90 days
- **Policy**: Metrics retained for performance monitoring
- **Auto-delete**: Yes

#### Policies
- **Retention**: Unlimited
- **Policy**: Policies retained until explicitly deleted
- **Auto-delete**: No

#### RBAC Data
- **Retention**: Unlimited
- **Policy**: RBAC data retained until explicitly deleted
- **Auto-delete**: No

### Right to Be Forgotten (GDPR Article 17)

The Router supports the right to be forgotten:

```erlang
%% Delete user data
case router_data_privacy:get_right_to_be_forgotten(UserId) of
    {ok, DeletedCount} ->
        %% User data deleted
        process_deletion_result(DeletedCount);
    {error, Reason} ->
        handle_deletion_error(Reason)
end.
```

This function:
- Deletes user data from audit logs
- Deletes user data from RBAC
- Anonymizes user data in logs (if retention period not expired)

### Data Anonymization

The Router provides data anonymization functions:

```erlang
%% Anonymize PII in data
Data = #{
    ~"user_id" => ~"user123",
    ~"email" => ~"test@example.com",
    ~"normal_field" => ~"value"
},
Anonymized = router_data_privacy:anonymize_pii(Data),
%% Result: user_id and email are replaced with "[ANONYMIZED]"
```

### Verification

Verify GDPR compliance:

```erlang
%% Verify PII handling
case router_data_privacy:verify_pii_handling() of
    {ok, Report} ->
        Compliant = maps:get(compliant, Report),
        %% Process report
        process_pii_report(Report);
    {error, Reason} ->
        handle_verification_error(Reason)
end.

%% Verify GDPR compliance
case router_data_privacy:verify_gdpr_compliance() of
    {ok, Report} ->
        Compliant = maps:get(compliant, Report),
        %% Process report
        process_gdpr_report(Report);
    {error, Reason} ->
        handle_verification_error(Reason)
end.

%% Get data retention policies
Policies = router_data_privacy:get_data_retention_policies(),

%% Check data retention
{ok, ShouldRetain} = router_data_privacy:check_data_retention(
    ~"audit_logs",
    Timestamp
).
```

### Configuration

Configure data retention:

```erlang
%% Set audit retention days (default: 90)
application:set_env(beamline_router, audit_retention_days, 90),

%% Set log retention days (default: 30)
application:set_env(beamline_router, log_retention_days, 30),

%% Set metrics retention days (default: 90)
application:set_env(beamline_router, metrics_retention_days, 90).
```

## Testing

### License Compliance Tests

```erlang
test_license_compliance(_Config) ->
    {ok, Report} = router_license_compliance:verify_dependencies(),
    ?assertEqual(true, maps:get(compliant, Report)),
    ok.
```

### Data Privacy Tests

```erlang
test_pii_handling(_Config) ->
    {ok, Report} = router_data_privacy:verify_pii_handling(),
    ?assertEqual(true, maps:get(compliant, Report)),
    ok.

test_gdpr_compliance(_Config) ->
    {ok, Report} = router_data_privacy:verify_gdpr_compliance(),
    ?assertEqual(true, maps:get(compliant, Report)),
    ok.
```

## Related Documentation

- `src/router_license_compliance.erl` - License compliance verification
- `src/router_data_privacy.erl` - Data privacy (GDPR) compliance
- `src/router_logger.erl` - PII filtering in logging
- `src/router_audit.erl` - Audit logging with retention policies
- `SECURITY_GUIDE.md` - Security best practices

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team

