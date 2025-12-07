#!/bin/bash
# @doc Check if tests are being run from the correct directory
# Prevents issues with rebar_prv_common_test option parsing when run from repo root
# Usage: source this script or run: bash scripts/check_test_run_location.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$APP_DIR/../.." && pwd)"

# Check if we're in the app directory
CURRENT_DIR="$(pwd)"

if [ "$CURRENT_DIR" != "$APP_DIR" ]; then
    echo "ERROR: Tests must be run from the application directory" >&2
    echo "Current directory: $CURRENT_DIR" >&2
    echo "Expected directory: $APP_DIR" >&2
    echo "" >&2
    echo "Please run:" >&2
    echo "  cd $APP_DIR" >&2
    echo "  rebar3 ct [options]" >&2
    echo "" >&2
    echo "This prevents issues with rebar_prv_common_test option parsing." >&2
    exit 1
fi

echo "✓ Running from correct directory: $APP_DIR"

