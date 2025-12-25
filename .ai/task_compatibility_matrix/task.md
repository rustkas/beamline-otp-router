# Task: T-INTEG-04 — Compatibility Matrix

**Created**: 2025-12-22  
**Status**: In Progress  
**Priority**: High  
**Type**: Documentation / Standards

## Objective

Document component compatibility and versioning rules for:
1. **Router ↔ c-gateway** compatibility
2. **Router ↔ CAF Worker** compatibility
3. **Versioning rules** (SemVer guidelines)
4. **Upgrade paths** (safe upgrade sequences)

## Motivation

**Problem**: Multi-component system needs clear compatibility rules

**Without this**:
- Unclear which versions work together
- Breaking changes cause production issues
- No upgrade path guidance
- Developer confusion

**With this**:
- Clear compatibility matrix
- Safe upgrade procedures
- Version enforcement
- Reduced production incidents

## Deliverables

### 1. Compatibility Matrix Document

**File**: `docs/COMPATIBILITY_MATRIX.md`

**Sections**:
1. Component Version Overview
2. Router ↔ c-gateway Compatibility
3. Router ↔ CAF Worker Compatibility
4. Protocol Versioning
5. Safe Upgrade Paths
6. Breaking Changes Log
7. Version Enforcement Tools

### 2. Versioning Rules

**Semantic Versioning** (SemVer):
- **MAJOR**: Breaking changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

**Component-specific rules**:
- Router versioning
- c-gateway versioning
- CAF Worker versioning
- Protocol versioning

### 3. Compatibility Testing

**Automated tests**:
- Version compatibility checks
- Protocol version negotiation
- Backward compatibility tests

## Key Concepts

### Protocol Versioning

**CP1 (Control Protocol v1)**:
- Router → c-gateway messages
- Versioned in message header
- Backward compatible within major version

**CAF Protocol**:
- Router → CAF Worker messages
- NATS subject versioning (caf.exec.assign.v1)
- Strict version matching

### Compatibility Levels

**Level 1: Fully Compatible** ✅
- All features work
- No warnings
- Recommended combination

**Level 2: Compatible with Warnings** ⚠️
- Works but deprecated features
- Migration path available
- Update recommended

**Level 3: Incompatible** ❌
- Does not work together
- Breaking changes present
- Must upgrade

## Compatibility Rules

### Router ↔ c-gateway

**Rule 1**: Minor version bumps are compatible
- Router 1.2.0 ↔ c-gateway 1.1.0 ✅
- Router 1.3.0 ↔ c-gateway 1.2.0 ✅

**Rule 2**: Major version must match
- Router 1.x.x ↔ c-gateway 1.x.x ✅
- Router 1.x.x ↔ c-gateway 2.x.x ❌

**Rule 3**: Patch versions always compatible
- Router 1.2.3 ↔ c-gateway 1.2.0 ✅

### Router ↔ CAF Worker

**Rule 1**: Protocol version must match
- NATS subject: caf.exec.assign.v1
- v1 → v1 ✅
- v1 → v2 ❌

**Rule 2**: Minor versions backward compatible
- Router 1.2.0 → CAF 1.1.0 ✅
- CAF 1.3.0 → Router 1.2.0 ⚠️ (works, update Router)

**Rule 3**: Major version coordinated
- Both on 1.x.x or both on 2.x.x

## Safe Upgrade Paths

### Scenario 1: Patch Release

**Example**: Router 1.2.0 → 1.2.1

**Steps**:
1. Deploy new Router version
2. No c-gateway/CAF changes needed
3. Verify health checks

**Risk**: Low ✅

### Scenario 2: Minor Release (New Features)

**Example**: Router 1.2.0 → 1.3.0

**Steps**:
1. Verify compatibility matrix
2. Deploy Router (backward compatible)
3. Optionally upgrade c-gateway/CAF for new features
4. Verify integration

**Risk**: Medium ⚠️

### Scenario 3: Major Release (Breaking Changes)

**Example**: Router 1.x.x → 2.0.0

**Steps**:
1. Review breaking changes
2. Upgrade CAF Worker first (if protocol changed)
3. Upgrade Router
4. Upgrade c-gateway
5. Run compatibility tests

**Risk**: High ⚠️

## Version Enforcement

### At Startup

**Router checks**:
```erlang
check_component_versions() ->
    GatewayVersion = get_gateway_version(),
    CAFVersion = get_caf_version(),
    
    case router_compatibility:validate(GatewayVersion, CAFVersion) of
        ok -> ok;
        {warning, Msg} -> logger:warning(Msg);
        {error, Msg} -> error({incompatible_versions, Msg})
    end.
```

### At Runtime

**Protocol negotiation**:
- c-gateway sends protocol version in request
- Router validates and responds
- CAF Worker checks NATS subject version

### In CI/CD

**Version compatibility tests**:
```bash
# Check compatibility before deploy
./scripts/check_compatibility.sh \
  --router 1.3.0 \
  --gateway 1.2.0 \
  --caf 1.2.0
```

## Timeline

- **Matrix documentation**: 2-3 hours
- **Versioning rules**: 1-2 hours
- **Examples and diagrams**: 1-2 hours
- **Tooling**: 2-3 hours
- **Total**: 1 day

## Success Criteria

- [x] Clear compatibility matrix
- [x] Documented versioning rules
- [x] Safe upgrade paths defined
- [x] Breaking changes logged
- [x] Examples for common scenarios
- [ ] Automated compatibility checks (optional)

## References

- Semantic Versioning 2.0.0 (semver.org)
- Kubernetes version skew policy
- gRPC versioning guidelines
- NATS protocol versioning
