# Protobuf Code Generation Guide (Router)

Instructions for generating Erlang/OTP code from protobuf definitions for Router.

## Proto File Structure

**Single Source of Truth:**
```
apps/otp/router/proto/beamline/flow/v1/flow.proto
```

**Generated files:**
```
apps/otp/router/src/flow_pb.erl          # Erlang module for serialization/deserialization
apps/otp/router/include/flow_pb.hrl      # Header file with record definitions
```

**Important:** Proto files are located **only** in `apps/otp/router/proto/`. Do not use `proto/` in the repository root for Router.

### Proto Source Files Status

**Current Status**: Proto source files are missing. Generated code is the source of truth.

**If Proto files are missing**:
- Generated code in `src/flow_pb.erl` and `include/flow_pb.hrl` is the authoritative source for message definitions
- Proto message structure can be verified by examining generated code
- To restore Proto files, extract definitions from generated code or use original `.proto` files if available

**Verification**:
```erlang
% Check message definitions
flow_pb:get_msg_names().
% Returns: ['Message', 'RouteRequest', 'RouteDecision', ...]

% Check field definitions
flow_pb:find_msg_def('Message').
% Returns: Field list with field numbers and types
```

**Note**: Proto file restoration is deferred to CP2-LC. See `docs/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` for restoration procedures.

## Prerequisites

### Dependencies

**rebar.config** already contains the necessary dependencies:
```erlang
{plugins, [
    rebar3_gpb_plugin
]}.

{deps, [
    {grpcbox, "~> 0.17.1"},
    {jsx, "~> 3.0"},
    {telemetry, "~> 1.0"}
]}.
```

**Installation:**
```bash
cd apps/otp/router
rebar3 get-deps
```

## Code Generation

### Method 1: Using rebar3 gpb compile (recommended)

**Command:**
```bash
cd apps/otp/router
rebar3 as test gpb compile
```

**What happens:**
1. The `rebar3_gpb_plugin` reads configuration from `rebar.config`:
   ```erlang
   {gpb_opts, [
       {i, ["proto"]},                    %% Path to proto files
       {o_erl, "src"},                     %% Output path for .erl files
       {o_hrl, "include"},                 %% Output path for .hrl files
       {module_name_suffix, "_pb"},        %% Module suffix
       {strings_as_binaries, true},       %% Strings as binaries
       {file_suffix, "_pb"},              %% File suffix
       {recursive, true}                   %% Recursive search
   ]}.
   ```
2. Generates `src/flow_pb.erl` and `include/flow_pb.hrl` from `proto/beamline/flow/v1/flow.proto`

**Verification:**
```bash
# Check that files are generated
ls -la apps/otp/router/src/flow_pb.erl
ls -la apps/otp/router/include/flow_pb.hrl
```

### Method 2: Automatic generation during compilation

`rebar3 compile` automatically runs `gpb compile` if proto files have changed (thanks to `rebar3_gpb_plugin`).

**Command:**
```bash
cd apps/otp/router
rebar3 compile
```

## Configuration in rebar.config

**Current configuration:**
```erlang
{plugins, [
    rebar3_gpb_plugin
]}.

{gpb_opts, [
    %% Include paths: proto files are in apps/otp/router/proto/
    %% Single source of truth: apps/otp/router/proto/beamline/flow/v1/flow.proto
    {i, ["proto"]},
    {o_erl, "src"},
    {o_hrl, "include"},
    {module_name_suffix, "_pb"},
    {strings_as_binaries, true},
    {file_suffix, "_pb"},
    {recursive, true}
]}.

{erl_opts, [
    debug_info,
    warn_export_all,
    warn_unused_import,
    {i, ["include", "_build/default/plugins/gpb/include"]}
]}.
```

**Important parameters:**
- `{i, ["proto"]}` - path to proto files relative to `apps/otp/router/`
- `{o_erl, "src"}` - output path for Erlang modules
- `{o_hrl, "include"}` - output path for header files
- `{recursive, true}` - recursive search for proto files

## Usage in Code

**Including header file:**
```erlang
-include("flow_pb.hrl").
```

**Usage example:**
```erlang
%% Decoding RouteRequest
RouteRequestPb = flow_pb:decode_msg(Request, 'RouteRequest'),
#'RouteRequest'{message = MessagePb, policy_id = PolicyId} = RouteRequestPb,

%% Encoding RouteDecision
RouteDecisionPb = #'RouteDecision'{
    provider_id = ProviderId,
    reason = Reason,
    priority = Priority
},
Response = flow_pb:encode_msg(RouteDecisionPb, 'RouteDecision').
```

## Validation

### Checking generated code

**Compilation:**
```bash
cd apps/otp/router
rebar3 compile
```

**Dialyzer:**
```bash
cd apps/otp/router
rebar3 dialyzer
```

**Tests:**
```bash
cd apps/otp/router
rebar3 as test ct
```

## CI/CD Integration

### GitHub Actions

**Step sequence:**
```yaml
- name: Get dependencies
  working-directory: apps/otp/router
  run: rebar3 get-deps

- name: Generate Protobuf code
  working-directory: apps/otp/router
  run: rebar3 as test gpb compile

- name: Router Compile
  working-directory: apps/otp/router
  run: rebar3 compile

- name: Router Tests
  working-directory: apps/otp/router
  run: rebar3 as test ct

- name: Dialyzer
  working-directory: apps/otp/router
  run: rebar3 dialyzer
```

**Important:** Always run `rebar3 as test gpb compile` before `rebar3 compile` in CI to ensure proto files are generated.

## Troubleshooting

### Error: "undefined function flow_pb:decode_msg/2"

**Cause:** Proto files are not generated.

**Solution:**
```bash
cd apps/otp/router
rebar3 as test gpb compile
rebar3 compile
```

### Error: "cannot find proto file"

**Cause:** Incorrect path to proto files in `rebar.config`.

**Solution:** Verify that `{i, ["proto"]}` points to the correct path relative to `apps/otp/router/`.

### Error: "rebar3_gpb_plugin not found"

**Cause:** Plugin is not installed.

**Solution:**
```bash
cd apps/otp/router
rebar3 get-deps
```

## Migration from proto/ in root

**Old structure (deprecated):**
```
proto/beamline/flow/v1/flow.proto  # NOT USED for Router
```

**New structure (current):**
```
apps/otp/router/proto/beamline/flow/v1/flow.proto  # Single source of truth
```

**Important:** Router uses **only** `apps/otp/router/proto/`. Files in the root `proto/` are used by other components (Gateway, Ingress, Provider).

## References

- [gpb Documentation](https://github.com/tomas-abrahamsson/gpb)
- [rebar3_gpb_plugin](https://github.com/lrascao/rebar3_gpb_plugin)
- [Protocol Buffers Guide](https://developers.google.com/protocol-buffers)
