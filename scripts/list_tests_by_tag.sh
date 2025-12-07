#!/bin/bash
# List test suites by tag
# Parses @test_category tags from test/*.erl files and returns matching suites
#
# Usage:
#   ./scripts/list_tests_by_tag.sh <tag_expr>
#   ./scripts/list_tests_by_tag.sh fast
#   ./scripts/list_tests_by_tag.sh cp1_smoke
#   ./scripts/list_tests_by_tag.sh fast,!cp1_smoke  # fast but not cp1_smoke
#   ./scripts/list_tests_by_tag.sh cp1_smoke,cp2_plus  # intersection (both tags)
#
# Tag Expression Syntax:
#   - Single tag: fast
#   - Include + Exclude: fast,!cp1_smoke (fast but not cp1_smoke)
#   - Multiple includes: cp1_smoke,cp2_plus (intersection - suite must have both tags)
#   - Multiple excludes: fast,!cp1_smoke,!cp2_plus (fast but not cp1_smoke and not cp2_plus)
#
# Output:
#   List of suite names (without .erl extension) matching the tag expression, one per line
#
# Exit codes:
#   0 - Success
#   1 - Invalid arguments
#   2 - No suites found

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
TEST_DIR="${ROUTER_DIR}/test"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" >&2
}

# Check arguments
if [ $# -lt 1 ]; then
    log_error "Usage: $0 <tag_expr>"
    log_error "Example: $0 fast"
    log_error "Example: $0 cp1_smoke"
    log_error "Example: $0 fast,!cp1_smoke  # fast but not cp1_smoke"
    log_error "Example: $0 cp1_smoke,cp2_plus  # intersection (both tags)"
    exit 1
fi

TAG_EXPR="$1"

# Parse tag expression into includes and excludes
# Format: tag1,tag2,!exclude1,!exclude2
INCLUDE_TAGS=()
EXCLUDE_TAGS=()

IFS=',' read -ra TAGS <<< "${TAG_EXPR}"
for TAG in "${TAGS[@]}"; do
    TAG=$(echo "${TAG}" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')  # Trim spaces
    if [[ "${TAG}" =~ ^! ]]; then
        # Exclude tag (starts with !)
        EXCLUDE_TAG="${TAG#!}"  # Remove leading !
        EXCLUDE_TAG=$(echo "${EXCLUDE_TAG}" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
        # Validate exclude tag format
        if ! echo "${EXCLUDE_TAG}" | grep -qE '^[a-z0-9_-]+$'; then
            log_error "Invalid exclude tag format: ${EXCLUDE_TAG}"
            log_error "Tag must contain only lowercase letters, numbers, underscores, and hyphens"
            exit 1
        fi
        EXCLUDE_TAGS+=("${EXCLUDE_TAG}")
    else
        # Include tag
        # Validate include tag format
        if ! echo "${TAG}" | grep -qE '^[a-z0-9_-]+$'; then
            log_error "Invalid include tag format: ${TAG}"
            log_error "Tag must contain only lowercase letters, numbers, underscores, and hyphens"
            exit 1
        fi
        INCLUDE_TAGS+=("${TAG}")
    fi
done

# Check if at least one include tag is provided
if [ ${#INCLUDE_TAGS[@]} -eq 0 ]; then
    log_error "At least one include tag is required (use tag, not !tag)"
    exit 1
fi

# Check if test directory exists
if [ ! -d "${TEST_DIR}" ]; then
    log_error "Test directory not found: ${TEST_DIR}"
    exit 1
fi

# Find all .erl files in test directory
# Parse @test_category tags and match against tag expression
MATCHING_SUITES=()

while IFS= read -r -d '' FILE; do
    # Extract suite name (without path and .erl extension)
    SUITE_NAME=$(basename "${FILE}" .erl)
    
    # Check if file contains @test_category tag
    # Look for lines like: %% @test_category tag1, tag2, tag3
    # Tags are comma-separated, may have spaces
    # Only check first 50 lines (tags should be in upper part of file)
    if head -n 50 "${FILE}" | grep -qE '^\s*%%\s*@test_category'; then
        # Extract the tag line (first occurrence in first 50 lines)
        TAG_LINE=$(head -n 50 "${FILE}" | grep -E '^\s*%%\s*@test_category' | head -n 1)
        
        # Extract tags (remove %% @test_category prefix, split by comma)
        # Normalize: trim spaces, convert to lowercase for comparison
        SUITE_TAGS=$(echo "${TAG_LINE}" | sed -E 's/^\s*%%\s*@test_category\s+//' | tr ',' '\n' | sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | tr '[:upper:]' '[:lower:]')
        
        # Check include tags (all must be present for intersection)
        INCLUDE_MATCH=true
        for INCLUDE_TAG in "${INCLUDE_TAGS[@]}"; do
            INCLUDE_TAG_LOWER=$(echo "${INCLUDE_TAG}" | tr '[:upper:]' '[:lower:]')
            if ! echo "${SUITE_TAGS}" | grep -qE "^${INCLUDE_TAG_LOWER}$"; then
                INCLUDE_MATCH=false
                break
            fi
        done
        
        # Check exclude tags (suite must NOT have any exclude tag)
        EXCLUDE_MATCH=true
        for EXCLUDE_TAG in "${EXCLUDE_TAGS[@]}"; do
            EXCLUDE_TAG_LOWER=$(echo "${EXCLUDE_TAG}" | tr '[:upper:]' '[:lower:]')
            if echo "${SUITE_TAGS}" | grep -qE "^${EXCLUDE_TAG_LOWER}$"; then
                EXCLUDE_MATCH=false
                break
            fi
        done
        
        # Suite matches if it has all include tags AND none of the exclude tags
        if [ "${INCLUDE_MATCH}" = "true" ] && [ "${EXCLUDE_MATCH}" = "true" ]; then
            MATCHING_SUITES+=("${SUITE_NAME}")
        fi
    fi
done < <(find "${TEST_DIR}" -maxdepth 1 -name "*_SUITE.erl" -type f -print0)

# Check if any suites found
if [ ${#MATCHING_SUITES[@]} -eq 0 ]; then
    log_warn "No test suites found matching tag expression: ${TAG_EXPR}"
    exit 2
fi

# Output matching suites (one per line, without .erl extension)
for SUITE in "${MATCHING_SUITES[@]}"; do
    echo "${SUITE}"
done

exit 0
