#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_dir="$(dirname "$script_dir")"

echo "Mode: ROUTER_TEST_LEVEL=heavy"
echo "CT quarantine group: INCLUDED"
echo "WARNING: This may take several hours!"
echo ""

export ROUTER_TEST_LEVEL=heavy

:

cd "$project_dir"

# Use the test profile so test-only helpers (erlc_paths) are consistently available.
if ! bash "$script_dir/lint/check_ct_quarantine_consistency.sh"; then
  echo "✗ Quarantine consistency lint failed" >&2
  exit 1
fi
rebar3 as test ct --dir test --readable=true

echo ""
echo "=== Heavy/Nightly Complete ==="
