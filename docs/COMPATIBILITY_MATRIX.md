# Component Compatibility Matrix

**Version compatibility rules and upgrade paths for Beamline Router components**

---

## Overview

Beamline Router is a distributed system with multiple components that must work together. This document defines:
- **Component versions** and their compatibility
- **Versioning rules** (Semantic Versioning)
- **Safe upgrade paths**
- **Breaking changes log**

## Components

### Core Components

1. **Router** (Erlang/OTP)
   - Version: 1.0.0
   - Role: Core routing logic and policy evaluation
   - Protocol: CP1/CP2 (Control Protocol)

2. **c-gateway** (C++)
   - Version: 1.0.0
   - Role: HTTP-to-NATS gateway
   - Protocol: CP1 (NATS beamline.router.v1.*)

3. **CAF Worker** (C++/CAF)
   - Version: 1.0.0
   - Role: Block execution engine
   - Protocol: CAF Protocol (NATS caf.exec.*.v1)

### Supporting Components

4. **NATS Server**
   - Version: 2.10+
   - Role: Message bus
   - Compatibility: Independent

5. **Extensions** (Any language)
   - Version: Per-extension
   - Role: Custom logic
   - Protocol: Extension Protocol (NATS beamline.ext.*.v{N})

---

## Compatibility Matrix

### Router ↔ c-gateway

| Router Version | c-gateway Version | Status | Notes |
|----------------|-------------------|--------|-------|
| 1.0.x | 1.0.x | ✅ Fully Compatible | Recommended |
| 1.1.x | 1.0.x | ✅ Fully Compatible | Minor backward compatible |
| 1.0.x | 1.1.x | ⚠️ Compatible | Update Router recommended |
| 2.0.x | 1.x.x | ❌ Incompatible | Major version mismatch |
| 1.x.x | 2.0.x | ❌ Incompatible | Major version mismatch |

**Rules**:
- **MAJOR version must match**: Router 1.x.x ↔ c-gateway 1.x.x
- **MINOR version backward compatible**: Newer minor works with older
- **PATCH version always compatible**: 1.2.3 ↔ 1.2.0

### Router ↔ CAF Worker

| Router Version | CAF Worker Version | Protocol Version | Status | Notes |
|----------------|-------------------|------------------|--------|-------|
| 1.0.x | 1.0.x | v1 | ✅ Fully Compatible | Recommended |
| 1.1.x | 1.0.x | v1 | ✅ Fully Compatible | Minor backward compatible |
| 1.0.x | 1.1.x | v1 | ⚠️ Compatible | Update Router for new features |
| 1.x.x | 2.0.x | v1 vs v2 | ❌ Incompatible | Protocol version mismatch |
| 2.0.x | 1.x.x | v2 vs v1 | ❌ Incompatible | Protocol version mismatch |

**Rules**:
- **Protocol version must match**: Both use caf.exec.*.v1
- **MAJOR version coordinated**: Both on 1.x.x or 2.x.x
- **MINOR version backward compatible**: Router can use older CAF Worker

### c-gateway ↔ CAF Worker

**Relationship**: Indirect (through Router)

**Rule**: No direct compatibility requirement
- c-gateway → Router → CAF Worker
- Router mediates all communication

### NATS Server Compatibility

| NATS Version | Router | c-gateway | CAF Worker |
|--------------|--------|-----------|------------|
| 2.9.x | ✅ | ✅ | ✅ |
| 2.10.x | ✅ | ✅ | ✅ |
| 2.11.x | ✅ | ✅ | ✅ |

**Rules**:
- **NATS 2.9+** required (JetStream support)
- **2.10+** recommended (performance improvements)
- **Minor version updates** safe

---

## Versioning Rules

### Semantic Versioning (SemVer)

**Format**: `MAJOR.MINOR.PATCH`

**MAJOR version** (X.0.0):
- Breaking changes to APIs or protocols
- Incompatible with previous major version
- Requires coordinated upgrade

**Examples**:
- Protocol format change (CP1 → CP2)
- NATS subject change (v1 → v2)
- Removing deprecated features

**MINOR version** (1.X.0):
- New features (backward compatible)
- Compatible with same major version
- Adds functionality, doesn't break existing

**Examples**:
- New extension types
- Additional NATS subjects
- New configuration options

**PATCH version** (1.0.X):
- Bug fixes only
- Always backward compatible
- No new features

**Examples**:
- Bug fixes
- Performance improvements
- Documentation updates

### Protocol Versioning

**CP1 (Control Protocol v1)**:
```json
{
  "version": "1",
  "request_id": "...",
  "message": {...}
}
```

- Version in message header
- Router validates version
- Mismatch → error response

**CAF Protocol**:
```
NATS Subject: caf.exec.assign.v1
                              ^^
                         Version number
```

- Version in NATS subject
- Strict matching required
- No automatic fallback

**Extension Protocol**:
```
NATS Subject: beamline.ext.pre.normalize_text.v1
                                              ^^
                                         Version number
```

- Per-extension versioning
- Router routing rules specify version
- Multiple versions can coexist

### Version Checking

**At Router Startup**:
```erlang
% Router checks component versions
case router_compatibility:check_versions() of
    ok -> 
        logger:info("All component versions compatible");
    {warning, Warnings} ->
        logger:warning("Version compatibility warnings: ~p", [Warnings]);
    {error, Errors} ->
        error({incompatible_versions, Errors})
end.
```

**At Runtime** (c-gateway request):
```erlang
% Validate protocol version
case maps:get(<<"version">>, Request, undefined) of
    <<"1">> -> process_request_v1(Request);
    <<"2">> -> process_request_v2(Request);
    Other -> {error, {unsupported_version, Other}}
end.
```

**At NATS Subscribe** (CAF Worker):
```erlang
% Subscribe to versioned subject
Subject = <<"caf.exec.assign.v1">>,
gnat:sub(Conn, self(), Subject).

% Protocol version mismatch → no messages received
```

---

## Safe Upgrade Paths

### Scenario 1: Patch Release

**Example**: Router 1.2.0 → 1.2.1 (bug fix)

**Components**:
- Router: 1.2.0 → 1.2.1
- c-gateway: 1.2.0 (no change)
- CAF Worker: 1.2.0 (no change)

**Steps**:
1. Deploy new Router version
2. Verify health checks
3. Monitor for errors

**Risk**: ✅ Low (patch releases always safe)

**Rollback**: Redeploy 1.2.0

### Scenario 2: Minor Release (New Features)

**Example**: Router 1.2.0 → 1.3.0 (new extension type)

**Components**:
- Router: 1.2.0 → 1.3.0
- c-gateway: 1.2.0 (compatible, update optional)
- CAF Worker: 1.2.0 (compatible, update optional)

**Steps**:
1. **Verify compatibility matrix** (1.3.0 ↔ 1.2.0)
2. **Deploy Router 1.3.0** (backward compatible)
3. **Test existing functionality** (should work unchanged)
4. **Optionally upgrade c-gateway/CAF** for new features
5. **Monitor metrics and logs**

**Risk**: ⚠️ Medium (new code paths)

**Rollback**: Redeploy 1.2.0 (no data migration)

### Scenario 3: Major Release (Breaking Changes)

**Example**: Router 1.5.0 → 2.0.0 (protocol v2)

**Components**:
- Router: 1.5.0 → 2.0.0
- c-gateway: 1.5.0 → 2.0.0
- CAF Worker: 1.5.0 → 2.0.0

**Steps**:
1. **Review breaking changes** (CHANGELOG.md)
2. **Test in staging environment**
3. **Upgrade sequence**:
   - Deploy CAF Worker 2.0.0 (supports both v1 and v2 during transition)
   - Deploy Router 2.0.0 (switches to v2 protocol)
   - Deploy c-gateway 2.0.0 (sends v2 requests)
4. **Verify all integration points**
5. **Monitor closely for 24-48 hours**

**Risk**: 🔴 High (breaking changes)

**Rollback**: Complex, requires data migration planning

**Recommended**: Blue-green deployment or canary rollout

### Scenario 4: Extension Update

**Example**: PII Guard extension 1.0.0 → 1.1.0

**Components**:
- Extension: 1.0.0 → 1.1.0
- Router: No change
- c-gateway: No change

**Steps**:
1. **Deploy new extension version** (beamline.ext.validate.pii_guard.v1)
2. **Update Extension Registry** (point to new version)
3. **Update Routing Policies** (if needed, specify v1 vs v2)
4. **Test with canary tenant**
5. **Gradually roll out** to all tenants

**Risk**: ✅ Low (isolated to extension)

**Rollback**: Redeploy old extension, update registry

---

## Upgrade Matrix

### Safe Upgrade Combinations

| From Version | To Version | CAF Upgrade | c-gateway Upgrade | Risk |
|--------------|------------|-------------|-------------------|------|
| 1.0.0 → 1.0.1 | Patch | No | No | ✅ Low |
| 1.0.0 → 1.1.0 | Minor | Optional | Optional | ⚠️ Medium |
| 1.0.0 → 1.2.0 | Minor | Optional | Optional | ⚠️ Medium |
| 1.2.0 → 2.0.0 | Major | **Required** | **Required** | 🔴 High |

### Version Skew Policy

**Maximum version skew**: 1 minor version

**Supported**:
- Router 1.3.0 ↔ c-gateway 1.2.0 ✅
- Router 1.2.0 ↔ CAF 1.1.0 ✅

**Not Supported**:
- Router 1.5.0 ↔ c-gateway 1.2.0 ❌ (skew > 1)
- Router 1.3.0 ↔ CAF 1.0.0 ❌ (skew > 1)

**Recommendation**: Keep all components within 1 minor version

---

## Breaking Changes Log

### Version 2.0.0 (Planned)

**Router**:
- ❌ Protocol changed from CP1 to CP2
- ❌ NATS subjects renamed (beamline.router.v2.*)
- ✅ Migration guide provided

**c-gateway**:
- ❌ Requires Protocol v2
- ⚠️ Deprecated rate limit mode removed
- ✅ Backward compat mode for 1 minor version

**CAF Worker**:
- ❌ NATS subjects changed (caf.exec.*.v2)
- ❌ New block executor interface
- ✅ Migration guide provided

**Mitigation**:
- Blue-green deployment
- Phased rollout (CAF → Router → Gateway)
- 2-week rollback window

### Version 1.1.0 (Example Minor)

**Router**:
- ✅ New extension type: custom-provider
- ✅ Additional metrics
- ✅ Fully backward compatible

**c-gateway**:
- ✅ New health check endpoint
- ✅ Fully backward compatible

**CAF Worker**:
- ✅ New block executor: gpu.inference
- ✅ Fully backward compatible

---

## Version Enforcement

### Startup Checks

**Router startup validation**:
```erlang
-module(router_compatibility).

-export([check_versions/0]).

check_versions() ->
    % Get component versions
    RouterVsn = router_version:get(),
    GatewayVsn = get_gateway_version(),
    CAFVsn = get_caf_version(),
    
    % Validate compatibility
    Results = [
        check_gateway_compat(RouterVsn, GatewayVsn),
        check_caf_compat(RouterVsn, CAFVsn)
    ],
    
    % Consolidate results
    case lists:all(fun(R) -> R == ok end, Results) of
        true -> ok;
        false -> {error, incompatible_versions}
    end.

check_gateway_compat({R_Maj, _, _}, {G_Maj, _, _}) when R_Maj =/= G_Maj ->
    {error, {major_version_mismatch, router, c_gateway}};
check_gateway_compat({R_Maj, R_Min, _}, {G_Maj, G_Min, _}) 
  when R_Maj == G_Maj, abs(R_Min - G_Min) > 1 ->
    {warning, {version_skew_too_large, router, c_gateway}};
check_gateway_compat(_, _) ->
    ok.
```

### Runtime Validation

**Protocol version in requests**:
```erlang
% c-gateway sends version in request
Request = #{
    <<"version">> => <<"1">>,
    <<"message">> => ...
},

% Router validates
case router_protocol:validate_version(Request) of
    ok -> process_request(Request);
    {error, unsupported} -> {error, protocol_version_unsupported}
end.
```

### CI/CD Integration

**Pre-deployment check**:
```bash
#!/bin/bash
# scripts/check_compatibility.sh

ROUTER_VSN=$1
GATEWAY_VSN=$2
CAF_VSN=$3

# Parse versions
ROUTER_MAJOR=$(echo $ROUTER_VSN | cut -d. -f1)
GATEWAY_MAJOR=$(echo $GATEWAY_VSN | cut -d. -f1)

# Check major version match
if [ "$ROUTER_MAJOR" != "$GATEWAY_MAJOR" ]; then
    echo "ERROR: Major version mismatch (Router: $ROUTER_VSN, Gateway: $GATEWAY_VSN)"
    exit 1
fi

echo "✅ Compatibility check passed"
exit 0
```

**Usage in CI**:
```yaml
# .github/workflows/deploy.yml
- name: Check Compatibility
  run: |
    ./scripts/check_compatibility.sh \
      ${{ env.ROUTER_VERSION }} \
      ${{ env.GATEWAY_VERSION }} \
      ${{ env.CAF_VERSION }}
```

---

## Testing Compatibility

### Matrix Testing

**Test combinations**:
```erlang
% test/router_compatibility_SUITE.erl

compatibility_matrix_test(_Config) ->
    % Test Router 1.2.0 ↔ Gateway 1.1.0
    ok = test_combination("1.2.0", "1.1.0", "1.1.0"),
    
    % Test Router 1.2.0 ↔ Gateway 1.2.0
    ok = test_combination("1.2.0", "1.2.0", "1.2.0"),
    
    % Test incompatible (should fail)
    {error, _} = test_combination("2.0.0", "1.0.0", "1.0.0").
```

### Backward Compatibility Tests

**Ensure new version works with old components**:
```erlang
backward_compat_test(_Config) ->
    % Deploy Router 1.3.0
    start_router("1.3.0"),
    
    % Use c-gateway 1.2.0
    start_gateway("1.2.0"),
    
    % Verify requests work
    {ok, Response} = make_request(),
    ?assert(maps:is_key(<<"decision_id">>, Response)).
```

---

## Recommendations

### For Operators

1. **Keep components in sync**: Same minor version preferred
2. **Test upgrades in staging**: Never upgrade production directly
3. **Monitor after upgrades**: Watch metrics for 24-48 hours
4. **Have rollback plan**: Know how to revert each component

### For Developers

1. **Follow SemVer strictly**: Breaking changes = major bump
2. **Document breaking changes**: Update CHANGELOG.md
3. **Provide migration guides**: For major version bumps
4. **Test compatibility**: Run matrix tests before release

### For Release Managers

1. **Coordinate major releases**: Plan Router, Gateway, CAF together
2. **Phased rollouts**: Canary → staging → production
3. **Communication**: Notify teams of breaking changes 2 weeks ahead
4. **Backward compat period**: Support N-1 version for 6 months

---

## FAQ

**Q: Can I upgrade Router without upgrading c-gateway?**  
A: Yes, if within same major version and skew ≤ 1 minor version.

**Q: What happens if versions are incompatible?**  
A: Router will log error and may refuse to start or process requests.

**Q: How do I check current component versions?**  
A:
```bash
# Router
curl http://localhost:50051/version

# c-gateway
curl http://localhost:8080/_health | jq '.version'

# CAF Worker
curl http://localhost:9090/health | jq '.version'
```

**Q: Can extensions have different versions?**  
A: Yes, extensions version independently via NATS subject (v1, v2, etc.)

**Q: What's the rollback procedure for failed upgrade?**  
A: Redeploy previous version, verify health checks, monitor logs.

---

## References

- [Semantic Versioning 2.0.0](https://semver.org/)
- [Kubernetes Version Skew Policy](https://kubernetes.io/releases/version-skew-policy/)
- [gRPC Versioning Guide](https://grpc.io/docs/guides/versioning/)
- Project CHANGELOG.md (for breaking changes)

---

**Last Updated**: 2025-12-22  
**Document Version**: 1.0.0
