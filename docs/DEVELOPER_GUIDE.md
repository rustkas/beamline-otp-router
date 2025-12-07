# Developer Guide - Router Component

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: Complete

## Overview

This guide provides comprehensive developer onboarding information, development workflow, and code review process for Router component.

## Getting Started

### Prerequisites

- Erlang/OTP 25+ installed
- Rebar3 installed
- Git installed
- Basic knowledge of Erlang/OTP

### Project Structure

```
apps/otp/router/
├── src/              # Source code
├── test/             # Test suites
├── include/          # Header files
├── proto/            # Protobuf definitions
├── config/           # Configuration files
├── docs/             # Documentation
└── scripts/          # Utility scripts
```

### Building the Project

```bash
# Compile
rebar3 compile

# Run tests
rebar3 ct

# Run specific test suite
rebar3 ct --suite test/router_circuit_breaker_SUITE

# Run with coverage
rebar3 ct --cover
```

## Development Workflow

### 1. Create Feature Branch

```bash
git checkout -b feature/my-feature
```

### 2. Make Changes

- Write code following project conventions
- Add tests for new functionality
- Update documentation if needed

### 3. Run Tests

```bash
# Run all tests
rebar3 ct

# Run specific suite
rebar3 ct --suite test/router_my_feature_SUITE

# Run with Dialyzer
rebar3 dialyzer
```

### 4. Commit Changes

```bash
git add .
git commit -m "feat: Add my feature"
```

### 5. Create Pull Request

- Push branch to remote
- Create pull request
- Wait for code review

## Code Conventions

### Module Structure

```erlang
-module(my_module).

-export([public_function/1]).

-include("beamline_router.hrl").

%% @doc Public function description
-spec public_function(Arg :: term()) -> ok.
public_function(Arg) ->
    %% Implementation
    ok.
```

### Function Documentation

All public functions must have:
- `@doc` comment describing purpose
- `-spec` type specification
- Parameter descriptions in comments

### Error Handling

**Consistent Error Formats**:
- `{error, Reason}` - Simple error
- `{error, Reason, Context}` - Error with context
- Use `router_error:to_grpc/2` for gRPC errors

**Example**:
```erlang
case operation() of
    {ok, Result} -> Result;
    {error, Reason} -> {error, Reason};
    Error -> {error, unexpected_error, Error}
end.
```

### Logging

**Use `router_logger` for all logging**:
```erlang
router_logger:info(<<"Operation started">>, #{
    <<"tenant_id">> => TenantId,
    <<"operation">> => <<"my_operation">>
}).
```

**Log Levels**:
- `ERROR` - Critical errors requiring immediate attention
- `WARN` - Warnings and potential issues
- `INFO` - Informational messages
- `DEBUG` - Detailed debugging information

### Metrics

**Use `router_metrics:emit_metric/3` for metrics**:
```erlang
router_metrics:emit_metric(my_metric_name, #{count => 1}, #{
    tenant_id => TenantId,
    label => <<"value">>
}).
```

**For R10 metrics, use `router_r10_metrics`**:
```erlang
Value = router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
    tenant_id => TenantId,
    provider_id => ProviderId,
    state => <<"open">>
}).
```

## Testing Guidelines

### Test Structure

```erlang
-module(my_module_SUITE).

-include_lib("common_test/include/ct.hrl").

all() -> [test_my_function].

init_per_suite(Config) ->
    %% Setup
    Config.

end_per_suite(_Config) ->
    %% Cleanup
    ok.

test_my_function(_Config) ->
    %% Test implementation
    ?assertEqual(expected, actual),
    ok.
```

### Test Best Practices

1. **Use unique identifiers**:
   ```erlang
   TenantId = <<"test_tenant_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
   ```

2. **Clean up after tests**:
   ```erlang
   end_per_testcase(_TestCase, _Config) ->
       meck:unload(),
       router_r10_metrics:clear_metrics(),
       ok.
   ```

3. **Use proper assertions**:
   ```erlang
   ?assertEqual(expected, actual),
   ?assertMatch({ok, _}, Result),
   ?assert(is_record(Result, my_record)).
   ```

4. **Mock external dependencies**:
   ```erlang
   meck:new(external_module, [passthrough]),
   meck:expect(external_module, function, fun(_) -> {ok, result} end).
   ```

### R10 Test Guidelines

**CRITICAL**: All R10 tests must:
- Use `router_r10_metrics` for metric reading (no direct ETS access)
- Use constants from `router_r10_metrics` (no hardcoded binaries)
- Use unique tenant/provider IDs per test
- Use appropriate timeouts (3-5 seconds for metrics)
- Check state before metrics

**Example**:
```erlang
TenantId = <<"test_tenant_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
ProviderId = <<"test_provider_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

%% Check state first
{ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
?assertEqual(open, State),

%% Then check metrics
ok = router_r10_metrics:wait_for_trigger_reason(TenantId, ProviderId, [
    router_r10_metrics:trigger_reason_failure_threshold(),
    router_r10_metrics:trigger_reason_error_rate()
], 3000).
```

## Code Review Process

### Review Checklist

**Functionality**:
- [ ] Code implements requirements correctly
- [ ] Error handling is appropriate
- [ ] Edge cases are handled
- [ ] Performance is acceptable

**Code Quality**:
- [ ] Code follows project conventions
- [ ] Functions are documented
- [ ] Type specifications are correct
- [ ] No compilation warnings
- [ ] Dialyzer passes

**Testing**:
- [ ] Tests cover new functionality
- [ ] Tests use proper assertions
- [ ] Tests clean up properly
- [ ] All tests pass

**Documentation**:
- [ ] Code comments are clear
- [ ] Documentation is updated
- [ ] Examples are provided (if needed)

### Review Feedback

**When requesting changes**:
- Be specific about issues
- Provide examples of fixes
- Explain why changes are needed

**When responding to feedback**:
- Address all comments
- Ask for clarification if needed
- Update code and tests
- Re-request review when ready

## Common Patterns

### Gen Server Pattern

```erlang
-module(my_gen_server).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    {reply, ok, State}.
```

### ETS Table Pattern

```erlang
-define(TABLE, my_table).

init([]) ->
    _ = ets:new(?TABLE, [named_table, private, {read_concurrency, true}]),
    {ok, #state{}}.
```

### Metrics Pattern

```erlang
router_metrics:emit_metric(my_metric, #{count => 1}, #{
    tenant_id => TenantId,
    label => <<"value">>
}).
```

## Debugging

### Common Debugging Tools

**Erlang Shell**:
```erlang
%% Attach to running node
erl -remsh node@host

%% Check process
whereis(process_name).

%% Check ETS table
ets:info(table_name).

%% Check application
application:which_applications().
```

**Metrics Dump**:
```erlang
router_r10_metrics:dump_metrics().
```

**Process Tree**:
```erlang
supervisor:which_children(beamline_router_sup).
```

### Logging for Debugging

```erlang
router_logger:debug(<<"Debug message">>, #{
    <<"context">> => Context,
    <<"value">> => Value
}).
```

## Performance Considerations

### ETS Operations

- Use `read_concurrency` and `write_concurrency` for high-traffic tables
- Avoid frequent table scans
- Use appropriate key structures

### Process Communication

- Use `gen_server:call/2` for synchronous operations
- Use `gen_server:cast/2` for asynchronous operations
- Set appropriate timeouts

### Metrics Collection

- Batch metric updates when possible
- Use efficient data structures
- Avoid high-cardinality labels

## Security Considerations

### Secrets Management

- Never commit secrets to repository
- Use environment variables for secrets
- Mask secrets in logs and documentation

### Input Validation

- Validate all inputs
- Sanitize user-provided data
- Check permissions before operations

### Error Messages

- Don't leak sensitive information in errors
- Use generic error messages for clients
- Log detailed errors server-side only

## References

- `docs/API_CONTRACTS.md` - API contracts
- `docs/OBSERVABILITY_CONVENTIONS.md` - Observability conventions
- `docs/ERROR_REASONS_REFERENCE.md` - Error reasons reference
- `test/R10_MAINTENANCE_CHECKLIST.md` - R10 maintenance checklist
- `docs/QA_TEST_PLAN.md` - Test plan

