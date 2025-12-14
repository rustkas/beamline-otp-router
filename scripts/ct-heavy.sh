#!/bin/bash
set -e

# Heavy/Nightly Test Run
# Runs all tests including soak/stress/chaos
# For nightly or manual runs only

echo "=== Starting Heavy/Nightly Test Run ==="
echo "Mode: ROUTER_TEST_LEVEL=heavy"
echo "WARNING: This may take several hours!"
echo ""

export ROUTER_TEST_LEVEL=heavy

# Run all suites with heavy level
rebar3 ct

echo ""
echo "=== Heavy/Nightly Complete ==="
