# Proto File Management

## Single Source of Truth

**`proto/`** - Single source of truth for all components
- **Location**: `proto/beamline/flow/v1/flow.proto`
- **Used by**: Router (Erlang/OTP), Gateway (TypeScript), Integration Tests
- **Code Generation**: 
  - Router: `rebar3_gpb_plugin` (configured in `apps/otp/router/rebar.config`)
  - Gateway: `buf` (configured in `proto/buf.gen.yaml`)

## Router Configuration

The Router (Erlang/OTP) uses the root proto files via relative path:
```erlang
%% In apps/otp/router/rebar.config
{gpb_opts, [
    {i, ["../../proto"]},  % Points to root proto/ directory
    {o_erl, "src"},
    {o_hrl, "include"},
    {module_name_suffix, "_pb"},
    {strings_as_binaries, true},
    {file_suffix, "_pb"},
    {recursive, true}
]}.
```

## Usage in Applications

### Router (Erlang/OTP)
- **Proto Path**: `proto/beamline/flow/v1/flow.proto` (root)
- **Generation**: `rebar3 gpb compile`
- **Configuration**: `apps/otp/router/rebar.config`

### Gateway (TypeScript)
- **Proto Path**: `proto/beamline/flow/v1/flow.proto` (root)
- **Generation**: `buf generate --template proto/buf.gen.yaml`
- **Configuration**: `proto/buf.gen.yaml`

### Integration Tests
- **Proto Path**: `proto/beamline/flow/v1/flow.proto` (root)
- **Usage**: Absolute path in tests

## Rules

1. **Single Source**: All proto files live in root `proto/` directory
2. **No Duplicates**: No proto files in component-specific directories
3. **Consistency**: All components use the same proto definitions
4. **Documentation**: Document changes in proto files

## Code Generation

### Router (Erlang/OTP)
```bash
cd apps/otp/router
rebar3 gpb compile
```

### Gateway (TypeScript)
```bash
buf generate --template proto/buf.gen.yaml
```

## Validation

### Check Proto Files Exist
```bash
# Verify all required proto files exist
ls -la proto/beamline/*/v1/*.proto
```

### Check Router Configuration
```bash
# Verify Router points to root proto files
grep -A 10 "gpb_opts" apps/otp/router/rebar.config
```

### CI/CD Integration

**GitHub Actions / GitLab CI:**

Add validation steps to CI pipeline:
```yaml
- name: Validate Proto Files Exist
  run: |
    test -f proto/beamline/flow/v1/flow.proto
    test -f proto/beamline/provider/v1/provider.proto
    test -f proto/beamline/ingress/v1/ingress.proto

- name: Validate Router Configuration
  run: |
    grep -q "../../proto" apps/otp/router/rebar.config
```

## Historical Note

**Previous State**: Proto files were duplicated between `apps/otp/router/proto/` and `proto/`
**Current State**: Single source of truth in root `proto/` directory
**Migration**: Completed - duplicate proto files removed from router directory
