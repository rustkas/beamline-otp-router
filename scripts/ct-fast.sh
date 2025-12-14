#!/bin/bash
set -e

# Fast CI Run
# Runs only fast-tier tests (unit, smoke, core)
# Default for every commit

echo "=== Starting Fast CI Test Run ==="
echo "Mode: ROUTER_TEST_LEVEL=fast"

export ROUTER_TEST_LEVEL=fast

# Run all suites with fast level
# Each suite's all() function returns only fast-tier tests
rebar3 ct

echo ""
echo "=== Fast CI Complete ==="
