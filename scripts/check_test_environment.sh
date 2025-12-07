#!/bin/bash
# @file check_test_environment.sh
# @brief Check if test environment is properly configured
# @description Verifies all dependencies and configuration for running router tests

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0
WARNINGS=0

check_command() {
    local cmd=$1
    local name=$2
    
    if command -v "$cmd" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $name found"
        return 0
    else
        echo -e "${RED}✗${NC} $name not found"
        ((ERRORS++))
        return 1
    fi
}

check_file() {
    local file=$1
    local name=$2
    
    if [ -f "$file" ]; then
        echo -e "${GREEN}✓${NC} $name found"
        return 0
    else
        echo -e "${RED}✗${NC} $name not found: $file"
        ((ERRORS++))
        return 1
    fi
}

check_optional() {
    local cmd=$1
    local name=$2
    
    if command -v "$cmd" >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $name found"
        return 0
    else
        echo -e "${YELLOW}⚠${NC} $name not found (optional)"
        ((WARNINGS++))
        return 1
    fi
}

echo "=========================================="
echo "Router Test Environment Check"
echo "=========================================="
echo ""

# Required commands
echo "Required Dependencies:"
check_command "erl" "Erlang/OTP"
check_command "rebar3" "rebar3"

# Check Erlang version
if command -v erl >/dev/null 2>&1; then
    ERL_VERSION=$(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>&1 | tr -d '\n' | tr -d '"')
    ERL_MAJOR=$(echo "$ERL_VERSION" | sed 's/[^0-9].*//' | head -c 2)
    if [ -n "$ERL_MAJOR" ] && [ "$ERL_MAJOR" -ge 25 ] 2>/dev/null; then
        echo -e "${GREEN}✓${NC} Erlang version OK: OTP $ERL_VERSION"
    else
        echo -e "${YELLOW}⚠${NC} Erlang version: OTP $ERL_VERSION (25+ recommended)"
        ((WARNINGS++))
    fi
fi

# Check rebar3 version
if command -v rebar3 >/dev/null 2>&1; then
    REBAR3_VERSION=$(rebar3 --version 2>&1 | head -1)
    echo -e "${GREEN}✓${NC} rebar3 version: $REBAR3_VERSION"
fi

echo ""
echo "Test Dependencies:"
cd "$ROUTER_DIR"

# Check if test dependencies are compiled
if [ -d "_build/test/lib" ]; then
    echo -e "${GREEN}✓${NC} Test dependencies directory exists"
    
    # Check for PropEr
    if [ -d "_build/test/lib/proper" ]; then
        echo -e "${GREEN}✓${NC} PropEr compiled"
    else
        echo -e "${YELLOW}⚠${NC} PropEr not compiled (property tests will be skipped)"
        ((WARNINGS++))
    fi
    
    # Check for meck
    if [ -d "_build/test/lib/meck" ]; then
        echo -e "${GREEN}✓${NC} meck compiled"
    else
        echo -e "${RED}✗${NC} meck not compiled (mocking tests will fail)"
        ((ERRORS++))
    fi
else
    echo -e "${RED}✗${NC} Test dependencies not compiled"
    echo "  Run: rebar3 as test get-deps && rebar3 as test compile"
    ((ERRORS++))
fi

echo ""
echo "Test Configuration:"
check_file "$ROUTER_DIR/rebar.config" "rebar.config"
check_file "$ROUTER_DIR/test/test_helpers.erl" "test_helpers.erl"

# Check for test config file (optional)
if [ -f "$ROUTER_DIR/config/test.config" ]; then
    echo -e "${GREEN}✓${NC} test.config found"
else
    echo -e "${YELLOW}⚠${NC} test.config not found (using defaults)"
    ((WARNINGS++))
fi

# Check for ct.config (optional)
if [ -f "$ROUTER_DIR/ct.config" ]; then
    echo -e "${GREEN}✓${NC} ct.config found"
else
    echo -e "${YELLOW}⚠${NC} ct.config not found (using defaults)"
    ((WARNINGS++))
fi

echo ""
echo "Test Directories:"
if [ -d "$ROUTER_DIR/test" ]; then
    TEST_COUNT=$(find "$ROUTER_DIR/test" -name "*_SUITE.erl" -type f 2>/dev/null | wc -l)
    echo -e "${GREEN}✓${NC} test/ directory found ($TEST_COUNT test suites)"
else
    echo -e "${RED}✗${NC} test/ directory not found"
    ((ERRORS++))
fi

# Check compilation (skip if takes too long - assume OK if dependencies are present)
echo ""
echo "Compilation Status:"
cd "$ROUTER_DIR"
if [ -d "_build/test/lib" ] && [ -d "_build/test/lib/meck" ]; then
    # Quick check: if dependencies are compiled, assume compilation works
    # Full compilation check can be done separately if needed
    echo -e "${GREEN}✓${NC} Test dependencies compiled (skipping full compilation check)"
    echo "  Run 'rebar3 as test compile' manually to verify full compilation"
else
    echo -e "${YELLOW}⚠${NC} Test dependencies not fully compiled"
    echo "  Run 'rebar3 as test get-deps && rebar3 as test compile'"
    ((WARNINGS++))
fi

echo ""
echo "Optional Dependencies:"
check_optional "nats-server" "NATS server (for integration tests)"
check_optional "docker" "Docker (for containerized tests)"
check_optional "docker-compose" "Docker Compose (for multi-container tests)"

echo ""
echo "=========================================="
if [ $ERRORS -eq 0 ]; then
    if [ $WARNINGS -eq 0 ]; then
        echo -e "${GREEN}✓ All checks passed!${NC}"
        echo "Test environment is ready."
        exit 0
    else
        echo -e "${YELLOW}⚠ Checks passed with $WARNINGS warning(s)${NC}"
        echo "Test environment is ready, but some optional features may be unavailable."
        exit 0
    fi
else
    echo -e "${RED}✗ Checks failed with $ERRORS error(s) and $WARNINGS warning(s)${NC}"
    echo "Please fix the errors before running tests."
    exit 1
fi

