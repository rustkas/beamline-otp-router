#!/bin/bash
# Check component version compatibility before deployment

set -e

# Colors
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Parse arguments
ROUTER_VERSION="${1:-}"
GATEWAY_VERSION="${2:-}"
CAF_VERSION="${3:-}"

usage() {
    echo "Usage: $0 <router_version> <gateway_version> <caf_version>"
    echo ""
    echo "Example:"
    echo "  $0 1.2.0 1.1.0 1.1.0"
    exit 1
}

if [ -z "$ROUTER_VERSION" ] || [ -z "$GATEWAY_VERSION" ] || [ -z "$CAF_VERSION" ]; then
    usage
fi

echo "=== Component Compatibility Check ==="
echo ""
echo "Router:    $ROUTER_VERSION"
echo "c-gateway: $GATEWAY_VERSION"
echo "CAF Worker: $CAF_VERSION"
echo ""

# Parse version components
parse_version() {
    local version=$1
    echo "$version" | awk -F. '{print $1, $2, $3}'
}

R_VSN=($(parse_version "$ROUTER_VERSION"))
G_VSN=($(parse_version "$GATEWAY_VERSION"))
C_VSN=($(parse_version "$CAF_VERSION"))

R_MAJOR=${R_VSN[0]}
R_MINOR=${R_VSN[1]}
R_PATCH=${R_VSN[2]}

G_MAJOR=${G_VSN[0]}
G_MINOR=${G_VSN[1]}
G_PATCH=${G_VSN[2]}

C_MAJOR=${C_VSN[0]}
C_MINOR=${C_VSN[1]}
C_PATCH=${C_VSN[2]}

# Track errors and warnings
ERRORS=0
WARNINGS=0

# Check Router ↔ c-gateway compatibility
echo "Checking Router ↔ c-gateway..."

if [ "$R_MAJOR" != "$G_MAJOR" ]; then
    echo -e "${RED}❌ ERROR: Major version mismatch (Router: $R_MAJOR, Gateway: $G_MAJOR)${NC}"
    echo "   Major versions must match (1.x.x ↔ 1.x.x)"
    ERRORS=$((ERRORS + 1))
else
    echo -e "${GREEN}✅ Major versions match ($R_MAJOR)${NC}"
fi

# Check version skew
SKEW=$((R_MINOR - G_MINOR))
if [ ${SKEW#-} -gt 1 ]; then
    echo -e "${YELLOW}⚠️  WARNING: Version skew too large (Router: $R_MINOR, Gateway: $G_MINOR)${NC}"
    echo "   Recommended: Keep within 1 minor version"
    WARNINGS=$((WARNINGS + 1))
elif [ "$R_MINOR" -lt "$G_MINOR" ]; then
    echo -e "${YELLOW}⚠️  WARNING: Gateway newer than Router ($G_MINOR > $R_MINOR)${NC}"
    echo "   Recommended: Update Router to at least $R_MAJOR.$G_MINOR.0"
    WARNINGS=$((WARNINGS + 1))
else
    echo -e "${GREEN}✅ Version skew acceptable${NC}"
fi

echo ""

# Check Router ↔ CAF Worker compatibility
echo "Checking Router ↔ CAF Worker..."

if [ "$R_MAJOR" != "$C_MAJOR" ]; then
    echo -e "${RED}❌ ERROR: Major version mismatch (Router: $R_MAJOR, CAF: $C_MAJOR)${NC}"
    echo "   Major versions must match for protocol compatibility"
    ERRORS=$((ERRORS + 1))
else
    echo -e "${GREEN}✅ Major versions match ($R_MAJOR)${NC}"
fi

# Check CAF version skew
CAF_SKEW=$((R_MINOR - C_MINOR))
if [ ${CAF_SKEW#-} -gt 1 ]; then
    echo -e "${YELLOW}⚠️  WARNING: Version skew too large (Router: $R_MINOR, CAF: $C_MINOR)${NC}"
    echo "   Recommended: Keep within 1 minor version"
    WARNINGS=$((WARNINGS + 1))
elif [ "$R_MINOR" -lt "$C_MINOR" ]; then
    echo -e "${YELLOW}⚠️  WARNING: CAF newer than Router ($C_MINOR > $R_MINOR)${NC}"
    echo "   Recommended: Update Router to access new CAF features"
    WARNINGS=$((WARNINGS + 1))
else
    echo -e "${GREEN}✅ Version skew acceptable${NC}"
fi

echo ""
echo "=== Summary ==="
echo -e "Errors:   ${RED}$ERRORS${NC}"
echo -e "Warnings: ${YELLOW}$WARNINGS${NC}"
echo ""

if [ $ERRORS -gt 0 ]; then
    echo -e "${RED}❌ COMPATIBILITY CHECK FAILED${NC}"
    echo ""
    echo "Action Required:"
    echo "  1. Review error messages above"
    echo "  2. Upgrade components to compatible versions"
    echo "  3. See docs/COMPATIBILITY_MATRIX.md for details"
    exit 1
elif [ $WARNINGS -gt 0 ]; then
    echo -e "${YELLOW}⚠️  COMPATIBILITY CHECK PASSED WITH WARNINGS${NC}"
    echo ""
    echo "Recommendations:"
    echo "  1. Review warnings above"
    echo "  2. Consider upgrading to latest compatible versions"
    echo "  3. Test in staging before production"
    exit 0
else
    echo -e "${GREEN}✅ COMP ATIBILITY CHECK PASSED${NC}"
    echo ""
    echo "All component versions are compatible!"
    exit 0
fi
