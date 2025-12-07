# Developer Guide

This guide provides comprehensive information for developers working on the router project, including onboarding procedures, development workflow, and code review processes.

## Table of Contents

1. [Onboarding](#onboarding)
2. [Development Workflow](#development-workflow)
3. [Code Review Process](#code-review-process)
4. [Project Structure](#project-structure)
5. [Common Tasks](#common-tasks)

## Onboarding

### Prerequisites

Before starting development on the router project, ensure you have:

- **Erlang/OTP**: Version 25.0 or later
- **Rebar3**: Version 3.20.0 or later
- **Git**: For version control
- **Editor**: Erlang-aware editor (VS Code with Erlang extension, Emacs, etc.)

### Initial Setup

1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd apps/otp/router
   ```

2. **Install dependencies**:
   ```bash
   rebar3 get-deps
   ```

3. **Compile the project**:
   ```bash
   rebar3 compile
   ```

4. **Run tests**:
   ```bash
   rebar3 as test ct
   ```

5. **Start the application** (for local development):
   ```bash
   rebar3 shell
   ```

### Development Environment

#### Recommended Tools

- **Dialyzer**: Static analysis (`rebar3 dialyzer`)
- **Xref**: Cross-reference analysis (`rebar3 xref`)
- **EUnit**: Unit testing framework (included with OTP)
- **Common Test**: Integration testing framework (included with OTP)

#### IDE Configuration

**VS Code**:
- Install "Erlang" extension
- Configure `.vscode/settings.json`:
  ```json
  {
    "erlang.erlangPath": "/path/to/erlang/bin",
    "erlang.rebarPath": "/path/to/rebar3"
  }
  ```

**Emacs**:
- Install `erlang-mode` and `edts`
- Configure `.emacs`:
  ```elisp
  (require 'erlang-start)
  (require 'edts-start)
  ```

### Key Modules to Understand

1. **`router_grpc.erl`**: Main gRPC API entry point
   - Handles `Decide` requests
   - Implements rate limiting
   - Error handling and mapping

2. **`router_decider.erl`**: Core routing logic
   - Provider selection algorithm
   - Extensions pipeline (pre → validators → provider → post)
   - Policy evaluation

3. **`router_circuit_breaker.erl`**: Circuit breaker implementation
   - State machine (closed/open/half_open)
   - Failure tracking and recovery
   - Sliding window error rate calculation

4. **`router_caf_adapter.erl`**: CAF integration
   - Assignment publishing
   - Retry logic
   - Telemetry integration

5. **`router_metrics.erl`**: Metrics infrastructure
   - ETS table management
   - Metric emission
   - Label normalization

## Development Workflow

### Branch Strategy

- **`main`**: Production-ready code
- **`develop`**: Integration branch for features
- **`feature/*`**: Feature branches
- **`fix/*`**: Bug fix branches
- **`test/*`**: Test-related branches

### Creating a Feature Branch

```bash
# Start from develop
git checkout develop
git pull origin develop

# Create feature branch
git checkout -b feature/my-feature

# Make changes, commit
git add .
git commit -m "feat: add my feature"

# Push and create PR
git push origin feature/my-feature
```

### Commit Message Format

Follow conventional commits format:

- **`feat:`**: New feature
- **`fix:`**: Bug fix
- **`docs:`**: Documentation changes
- **`test:`**: Test additions/changes
- **`refactor:`**: Code refactoring
- **`perf:`**: Performance improvements
- **`chore:`**: Maintenance tasks

**Example**:
```
feat: add circuit breaker state metrics

- Add router_circuit_breaker_state metric
- Track state transitions
- Update metrics access layer
```

### Development Cycle

1. **Create branch** from `develop`
2. **Write code** following project conventions
3. **Write tests** for new functionality
4. **Run tests** locally: `rebar3 as test ct`
5. **Check Dialyzer**: `rebar3 dialyzer`
6. **Check Xref**: `rebar3 xref`
7. **Update documentation** if needed
8. **Commit changes** with descriptive messages
9. **Push and create PR**

### Testing Workflow

#### Unit Tests

Unit tests use EUnit and are co-located with source files:

```erlang
%% In router_my_module.erl
-module(router_my_module_tests).
-include_lib("eunit/include/eunit.hrl").

my_function_test() ->
    ?assertEqual(expected, router_my_module:my_function(input)).
```

Run unit tests:
```bash
rebar3 eunit
```

#### Integration Tests

Integration tests use Common Test and are in `test/` directory:

```erlang
%% In test/router_my_module_SUITE.erl
-module(router_my_module_SUITE).
-include_lib("common_test/include/ct.hrl").

all() -> [test_my_function].

test_my_function(_Config) ->
    ok = router_test_utils:start_router_app(),
    %% Test code
    ok.
```

Run integration tests:
```bash
rebar3 as test ct --suite router_my_module_SUITE
```

#### Test Best Practices

- Use `router_test_utils` for lifecycle management
- Clear metrics between tests: `router_r10_metrics:clear_metrics()`
- Use waiters for deterministic assertions: `wait_for_metric/3`
- Reset state between tests: `router_test_utils:reset_circuit_breaker()`

See `TESTING_GUIDE.md` for detailed testing procedures.

### Code Style

#### Naming Conventions

- **Modules**: `router_<feature>.erl` (snake_case)
- **Functions**: `verb_noun/arity` (snake_case)
- **Records**: `#record_name{}` (snake_case)
- **Macros**: `?MACRO_NAME` (UPPER_SNAKE_CASE)
- **Variables**: `VariableName` (PascalCase for atoms, camelCase discouraged)

#### Function Documentation

All public functions must have `@doc` comments:

```erlang
%% @doc Brief description
%% @param Param1 Description
%% @param Param2 Description
%% @returns Description
-spec function_name(type1(), type2()) -> return_type().
function_name(Param1, Param2) ->
    %% Implementation
    ok.
```

#### Error Handling

- Return `{ok, Result}` for success
- Return `{error, Reason}` for errors
- Use `router_error:to_grpc/2` for gRPC error mapping
- Log errors with `router_logger:error/2`

#### Logging

- Use `router_logger` for all logging (production code)
- Use structured logging with maps:
  ```erlang
  router_logger:info(<<"Event description">>, #{
      <<"key1">> => Value1,
      <<"key2">> => Value2
  }).
  ```
- Never use `io:format` in production code

## Code Review Process

### PR Requirements

Before requesting review, ensure:

- [ ] All tests pass locally
- [ ] Dialyzer passes: `rebar3 dialyzer`
- [ ] Xref passes: `rebar3 xref`
- [ ] Code follows style guidelines
- [ ] Documentation is updated
- [ ] No direct ETS access in tests (use metrics access layers)
- [ ] No `io:format` in production code
- [ ] Error handling follows conventions

### Review Checklist

#### Functionality

- [ ] Code implements the intended feature/fix
- [ ] Edge cases are handled
- [ ] Error cases are handled appropriately
- [ ] Performance is acceptable

#### Code Quality

- [ ] Code is readable and maintainable
- [ ] Functions are appropriately sized (< 50 lines)
- [ ] No code duplication
- [ ] Appropriate abstractions are used
- [ ] Comments explain "why", not "what"

#### Testing

- [ ] Unit tests cover new functionality
- [ ] Integration tests cover new features
- [ ] Edge cases are tested
- [ ] Error cases are tested
- [ ] Tests use proper lifecycle management

#### Documentation

- [ ] Public functions have `@doc` comments
- [ ] Complex logic has inline comments
- [ ] README/docs updated if needed
- [ ] `@see` references added for related modules

#### Security

- [ ] Input validation is performed
- [ ] No secrets in code or logs
- [ ] RBAC checks are in place (if applicable)
- [ ] Error messages don't leak sensitive information

### Review Process

1. **Author creates PR** with description
2. **CI runs** automated checks (tests, Dialyzer, Xref)
3. **Reviewer reviews** code and comments
4. **Author addresses** review comments
5. **Reviewer approves** when satisfied
6. **PR is merged** to `develop` or `main`

### Review Guidelines

#### For Reviewers

- Be constructive and respectful
- Focus on code, not the author
- Explain reasoning for suggestions
- Approve when code meets standards
- Request changes for significant issues

#### For Authors

- Respond to all comments
- Make requested changes or explain why not
- Update PR description if scope changes
- Re-request review after addressing comments

## Project Structure

### Directory Layout

```
apps/otp/router/
├── src/                    # Source code
│   ├── router_*.erl        # Core modules
│   └── beamline_router.hrl # Common header
├── test/                   # Test suites
│   ├── router_*_SUITE.erl   # Common Test suites
│   └── router_test_utils.erl # Test utilities
├── include/                # Header files
├── priv/                   # Private resources
├── docs/                   # Documentation
└── rebar.config           # Build configuration
```

### Key Directories

- **`src/`**: Production source code
- **`test/`**: Test suites and test utilities
- **`include/`**: Header files (`.hrl`)
- **`priv/`**: Static resources (configs, scripts)

### Module Organization

- **Core modules**: `router_*.erl` in `src/`
- **Test modules**: `router_*_SUITE.erl` in `test/`
- **Metrics modules**: `router_*_metrics.erl` in `src/`
- **Utility modules**: `router_*_utils.erl` in `test/`

## Common Tasks

### Adding a New Feature

1. Create feature branch
2. Implement feature in `src/router_*.erl`
3. Add tests in `test/router_*_SUITE.erl`
4. Update documentation
5. Run tests and checks
6. Create PR

### Adding a New Metric

1. Define metric name in module
2. Emit metric using `router_metrics:emit_metric/3`
3. Create metrics access layer if needed (see `OBSERVABILITY_CONVENTIONS.md`)
4. Add tests for metric
5. Update documentation

### Debugging Issues

1. Check logs: `tail -f logs/router.log`
2. Use `router_r10_metrics:dump_metrics/0` for metrics
3. Use `router_test_utils:dump_supervisor_children/0` for process tree
4. Enable debug logging in config
5. Use `rebar3 shell` for interactive debugging

### Performance Investigation

1. Profile with `fprof` or `eprof`
2. Check metrics: `router_metrics:dump_all/0`
3. Review ETS table sizes
4. Check process queue sizes
5. See `PERFORMANCE_GUIDE.md` for details

## Related Documentation

- `TESTING_GUIDE.md` - Test execution procedures
- `OBSERVABILITY_CONVENTIONS.md` - Metrics patterns
- `INTEGRATION_GUIDE.md` - Integration procedures
- `PERFORMANCE_GUIDE.md` - Performance tuning
- `SECURITY_GUIDE.md` - Security best practices
- `API_DOCUMENTATION.md` - gRPC API reference
- `ARCHITECTURE_DOCUMENTATION.md` - System architecture

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team

