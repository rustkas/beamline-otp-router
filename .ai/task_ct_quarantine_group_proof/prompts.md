# Operator Prompts

## 1. Running Tests
```bash
# Run full test suite without quarantine
export ROUTER_TEST_LEVEL=full
./scripts/ct-full.sh

# Run only quarantine tests
rebar3 ct --suite=test/router_alerts_test_SUITE --group=quarantine -v

# Run heavy test suite with quarantine
export ROUTER_TEST_LEVEL=heavy
./scripts/ct-heavy.sh
```

## 2. Capturing Output
```bash
# Capture command output with timestamp
{
    echo "=== $(date) ==="
    echo "Command: $@"
    echo "---"
    "$@"
    echo "\n\n"
} 2>&1 | tee -a progress.log

# Example usage:
capture_output rebar3 ct -h
```

## 3. Troubleshooting "All 0 tests passed"

If you see "All 0 tests passed":
1. Verify test suite name is correct
2. Check group name matches exactly (case-sensitive)
3. Ensure test functions are properly exported
4. Check for compilation errors
5. Try running with `-v` for verbose output
6. Verify test data setup is complete
7. Check for any test environment requirements
8. Look for any test framework warnings

## 4. Environment Setup
```bash
# Make sure to set up the environment first
source scripts/setup_env.sh  # If available
export ERL_AFLAGS="+C multi_time_warp"  # If needed
```
