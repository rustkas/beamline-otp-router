#!/bin/bash
# Start Mock Router for testing

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Configuration
NATS_URL="${NATS_URL:-nats://localhost:4222}"
SCENARIOS_FILE="${SCENARIOS_FILE:-$PROJECT_ROOT/test/mock_scenarios.json}"
MOCK_PORT="${MOCK_PORT:-8081}"

echo "=== Starting Mock Router ==="
echo "NATS URL: $NATS_URL"
echo "Scenarios: $SCENARIOS_FILE"
echo ""

# Check if NATS is running
if ! nc -z localhost 4222 2>/dev/null; then
    echo "⚠️  Warning: NATS does not appear to be running on localhost:4222"
    echo "   Start NATS with: nats-server -js"
    echo ""
fi

# Start Erlang with mock router
cd "$PROJECT_ROOT"

erl -pa _build/default/lib/*/ebin \
    -config config/test.config \
    -eval "application:ensure_all_started(router), router_mock:start_link([{nats_url, \"$NATS_URL\"}, {scenarios_file, \"$SCENARIOS_FILE\"}])." \
    -s observer \
    -noshell

# Keep process running
wait
