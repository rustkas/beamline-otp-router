#!/bin/bash
# @doc Validate alert rules using Prometheus promtool
# 
# Validates Prometheus alert rules syntax and expressions.
# Requires promtool to be installed.
# 
# Usage:
#   ./scripts/validate_alert_rules_promtool.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$ROUTER_DIR/../../.." && pwd)"
cd "$ROUTER_DIR"

ALERT_RULES_FILE="$PROJECT_ROOT/docs/observability/router-alert-rules.yaml"

echo "=== Validating Alert Rules with promtool ==="
echo ""

# Check promtool is available
if ! command -v promtool &> /dev/null; then
    echo "❌ ERROR: promtool not found"
    echo ""
    echo "Install promtool:"
    echo "  - Download from: https://prometheus.io/download/"
    echo "  - Or install via package manager:"
    echo "    Ubuntu/Debian: sudo apt-get install prometheus"
    echo "    macOS: brew install prometheus"
    echo "    Or build from source: https://github.com/prometheus/prometheus"
    exit 1
fi

echo "✓ promtool found: $(which promtool)"
echo ""

# Check file exists
if [ ! -f "$ALERT_RULES_FILE" ]; then
    echo "❌ ERROR: Alert rules file not found: $ALERT_RULES_FILE"
    exit 1
fi

echo "Validating: $ALERT_RULES_FILE"
echo ""

# Run promtool check
if promtool check rules "$ALERT_RULES_FILE"; then
    echo ""
    echo "✓ Alert rules validation passed"
    echo ""
    echo "All alert expressions are syntactically correct and valid."
    exit 0
else
    echo ""
    echo "❌ Alert rules validation failed"
    echo ""
    echo "Please fix the errors above and try again."
    exit 1
fi

