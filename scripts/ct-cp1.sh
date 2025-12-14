#!/bin/bash
# CP1 Core Quick Run - runs essential CP1 suites for fast CI feedback
# Usage: ./scripts/ct-cp1.sh
#
# This script runs the minimal set of CP1 tests needed for CI sanity checks.
# All tests should pass without ROUTER_TEST_LEVEL or ROUTER_ENABLE_META set.

set -e

cd " /..
