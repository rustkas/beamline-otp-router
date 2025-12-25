#!/usr/bin/env bash
# ==============================================================================
# Quarantine Policy Enforcement Script
# ==============================================================================
# Validates that all quarantined suites comply with the governance policy:
#   - Every entry MUST have: suite, owner, date (ISO-8601), reason
#   - Date MUST be valid ISO-8601 format (YYYY-MM-DD)
#   - Quarantine age is checked against configurable TTL
#
# Environment Variables:
#   QUARANTINE_TTL_DAYS - days before quarantine is considered stale (default: 30)
#   QUARANTINE_STRICT   - if "true", stale quarantine is a hard error (default: false/warning)
#
# Exit Codes:
#   0 - All checks passed
#   1 - Policy violation (missing metadata, stale quarantine in strict mode)
#   2 - Configuration error (missing files, invalid data)
# ==============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

QUARANTINE_FILE="$PROJECT_DIR/config/quarantine/quarantined_suites.txt"
TEST_DIR="$PROJECT_DIR/test"

# Configuration with defaults
QUARANTINE_TTL_DAYS="${QUARANTINE_TTL_DAYS:-30}"
QUARANTINE_STRICT="${QUARANTINE_STRICT:-false}"

# ANSI color codes
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Counters
ERRORS=0
WARNINGS=0
PASSED=0

log_error() {
  echo -e "${RED}ERROR:${NC} $1" >&2
  ((ERRORS++)) || true
}

log_warning() {
  echo -e "${YELLOW}WARNING:${NC} $1" >&2
  ((WARNINGS++)) || true
}

log_pass() {
  echo -e "${GREEN}✓${NC} $1"
  ((PASSED++)) || true
}

log_info() {
  echo -e "INFO: $1"
}

trim() {
  local var="$*"
  var="${var#"${var%%[![:space:]]*}"}"
  var="${var%"${var##*[![:space:]]}"}"
  printf '%s' "$var"
}

# Validate ISO-8601 date format (YYYY-MM-DD)
validate_iso_date() {
  local date_str="$1"
  if [[ ! "$date_str" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
    return 1
  fi
  # Additional check: ensure it's a valid date
  if ! date -d "$date_str" >/dev/null 2>&1; then
    return 1
  fi
  return 0
}

# Calculate days since a given ISO date
days_since() {
  local date_str="$1"
  local now_epoch
  local target_epoch
  
  now_epoch=$(date +%s)
  target_epoch=$(date -d "$date_str" +%s 2>/dev/null) || return 1
  
  local diff_seconds=$((now_epoch - target_epoch))
  local diff_days=$((diff_seconds / 86400))
  
  echo "$diff_days"
}

# Check if suite file has quarantine CT group
has_quarantine_group() {
  local suite_file="$1"
  if grep -qE "\{\s*quarantine\s*," "$suite_file" 2>/dev/null; then
    return 0
  fi
  return 1
}

# ==============================================================================
# MAIN
# ==============================================================================
echo "=============================================="
echo "Quarantine Policy Enforcement Check"
echo "=============================================="
echo "Config:"
echo "  TTL: ${QUARANTINE_TTL_DAYS} days"
echo "  Strict mode: ${QUARANTINE_STRICT}"
echo "  Registry: $QUARANTINE_FILE"
echo ""

# Check registry file exists
if [[ ! -f "$QUARANTINE_FILE" ]]; then
  log_error "Quarantine registry file not found: $QUARANTINE_FILE"
  exit 2
fi

# Parse and validate each entry
LINE_NUM=0
while IFS= read -r raw_line || [[ -n "$raw_line" ]]; do
  ((LINE_NUM++)) || true
  line="$raw_line"
  line="${line#"${line%%[![:space:]]*}"}"
  
  # Skip empty lines and comments
  [[ -z "$line" || "${line:0:1}" == "#" ]] && continue

  # Parse 4-column format: Suite | Owner | Date | Reason
  # Use IFS to split on pipe character
  oldIFS="$IFS"
  IFS='|'
  set -- $line
  IFS="$oldIFS"
  
  raw_suite="${1:-}"
  raw_owner="${2:-}"
  raw_date="${3:-}"
  raw_reason="${4:-}"
  
  suite="$(trim "${raw_suite:-}")"
  owner="$(trim "${raw_owner:-}")"
  qdate="$(trim "${raw_date:-}")"
  reason="$(trim "${raw_reason:-}")"
  
  # Skip if suite is empty (malformed line)
  if [[ -z "$suite" ]]; then
    log_warning "Line $LINE_NUM: Empty suite name, skipping"
    continue
  fi
  
  echo ""
  echo "Checking: $suite"
  
  # ==== Validation 1: Suite file must exist ====
  suite_file="$TEST_DIR/${suite}.erl"
  if [[ ! -f "$suite_file" ]]; then
    log_error "Line $LINE_NUM: Suite file missing: $suite_file"
    continue
  fi
  
  # ==== Validation 2: Owner must be present ====
  if [[ -z "$owner" ]]; then
    log_error "Line $LINE_NUM ($suite): Missing owner"
  else
    log_pass "Owner: $owner"
  fi
  
  # ==== Validation 3: Date must be present and valid ISO-8601 ====
  if [[ -z "$qdate" ]]; then
    log_error "Line $LINE_NUM ($suite): Missing date"
  elif ! validate_iso_date "$qdate"; then
    log_error "Line $LINE_NUM ($suite): Invalid date format '$qdate' (expected YYYY-MM-DD)"
  else
    log_pass "Date: $qdate"
    
    # ==== Validation 4: Check TTL ====
    age_days=$(days_since "$qdate")
    if [[ "$age_days" -gt "$QUARANTINE_TTL_DAYS" ]]; then
      if [[ "$QUARANTINE_STRICT" == "true" ]]; then
        log_error "$suite: Quarantine is $age_days days old (TTL: $QUARANTINE_TTL_DAYS days)"
      else
        log_warning "$suite: Quarantine is $age_days days old (TTL: $QUARANTINE_TTL_DAYS days)"
      fi
    else
      log_pass "Age: $age_days days (within TTL)"
    fi
  fi
  
  # ==== Validation 5: Reason must be present ====
  if [[ -z "$reason" ]]; then
    log_error "Line $LINE_NUM ($suite): Missing reason"
  else
    log_pass "Reason: ${reason:0:60}..."
  fi
  
  # ==== Validation 6: CT quarantine group must be defined ====
  if has_quarantine_group "$suite_file"; then
    log_pass "CT group 'quarantine' defined in suite"
  else
    log_error "Line $LINE_NUM ($suite): Missing CT group 'quarantine' in $suite_file"
  fi
  
done < "$QUARANTINE_FILE"

# ==============================================================================
# Summary
# ==============================================================================
echo ""
echo "=============================================="
echo "Summary"
echo "=============================================="
echo -e "  Passed checks: ${GREEN}$PASSED${NC}"
echo -e "  Warnings:      ${YELLOW}$WARNINGS${NC}"
echo -e "  Errors:        ${RED}$ERRORS${NC}"
echo ""

# Determine exit code
if [[ "$ERRORS" -gt 0 ]]; then
  echo -e "${RED}FAILED:${NC} Policy violations detected"
  exit 1
elif [[ "$WARNINGS" -gt 0 ]]; then
  echo -e "${YELLOW}PASSED with warnings${NC}"
  exit 0
else
  echo -e "${GREEN}PASSED:${NC} All quarantine policy checks passed"
  exit 0
fi
