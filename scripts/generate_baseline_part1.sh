#!/usr/bin/env bash
# generate_baseline.sh - Generate full tier baseline metrics JSON
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

REPORTS_DIR="$PROJECT_DIR/reports"
BASELINE_FILE="$REPORTS_DIR/full_baseline.json"
mkdir -p "$REPORTS_DIR"

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

generate_baseline() {
    local duration=$1
    local suites_count=$2
    local tests_passed=$3
    local tests_failed=$4
    local tests_skipped=$5
    local exit_status=$6
    local timestamp
    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local tests_total=$((tests_passed + tests_failed + tests_skipped))
    
    cat > "$BASELINE_FILE" <<EOF
{
  "generated_at": "$timestamp",
  "test_level": "full",
  "duration_seconds": $duration,
  "suites_count": $suites_count,
  "tests": {
    "total": $tests_total,
    "passed": $tests_passed,
    "failed": $tests_failed,
    "skipped": $tests_skipped
  },
  "exit_status": $exit_status,
  "baseline_version": "1.0.0"
}
EOF
    echo -e "${GREEN}Baseline saved: $BASEL