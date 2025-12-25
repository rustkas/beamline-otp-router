# Implementation Plan - Iteration 2: CT-Native Quarantine Groups

## Chosen Approach: CT Group Include/Exclude (Option A)
- Use Common Test's built-in group functionality with `{quarantine, [...]}`
- Leverage rebar3's CT options for group filtering
- Maintain backward compatibility with existing metadata-based quarantine

## Phase 1: Design Freeze

### Common Test Mechanism
We will use Common Test's built-in group functionality with the following approach:
1. **Group Definition**: Test suites will define a `quarantine` group in their `groups/0` function
2. **Group Filtering**: Use rebar3's `--group` and `--exclude-group` flags for test selection
3. **Test Suite Modification**: Add quarantine group to existing test suites without modifying their core test logic

### Command-Line Flags
- To exclude quarantine tests: `rebar3 ct --exclude-group quarantine`
- To run only quarantine tests: `rebar3 ct --group quarantine`
- To list tests in quarantine group: `rebar3 ct --list-tests --group quarantine`

### Implementation Mechanism
The implementation will work through:
1. **rebar3 ct CLI Flags**: Primary control through `--group` and `--exclude-group` flags
2. **rebar.config ct_opts**: Currently configured in `rebar.config` with test-specific settings
3. **Script Integration**: `ct-full.sh` and `ct-heavy.sh` will pass the appropriate flags to rebar3 ct

### Why Option A Works in This Repo
1. **rebar.config Analysis**:
   - The project already has `ct_opts` configured in `rebar.config`
   - Test configuration includes `sys_config` and `ct_hooks`
   - No existing group filtering that would conflict with our implementation

2. **rebar3 ct Invocation**:
   - Tests are run through `rebar3 ct` in the project's shell scripts
   - The current setup allows passing additional CT options through the command line
   - No custom test runners that would interfere with group filtering

3. **Group Filtering Compatibility**:
   - Common Test's group functionality is a core feature and well-supported
   - The filtering happens at the CT framework level, before test execution
   - Works independently of the existing metadata-based quarantine system

## Phase 2: Core Implementation
- [ ] Add quarantine group to one stable test suite as example
- [ ] Update `ct-full.sh` to exclude quarantine group at CT level
- [ ] Update `ct-heavy.sh` to include quarantine group

## Phase 3: Verification
- [ ] Verify quarantine exclusion in full/PR runs
- [ ] Verify quarantine inclusion in heavy/nightly runs
- [ ] Document verification steps and expected output

## Phase 4: Documentation
- [ ] Update test contribution guidelines
- [ ] Document migration path for existing quarantined tests
- [ ] Update progress.md with verification evidence

## Verification Requirements
- [ ] `ct-full.sh --list` shows quarantine group as excluded
- [ ] `ROUTER_TEST_LEVEL=heavy ct-heavy.sh` shows quarantine tests executing
- [ ] Existing quarantine mechanism remains functional

## Implementation Notes
- Using direct CT group filtering via rebar3's `--group`/`--exclude-group` flags
- No hooks needed - using built-in CT functionality
- Single source of truth for group definition in test suites
- Backward compatibility maintained via separate metadata files
- All changes are minimal and focused on the quarantine group functionality
