#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"

QUARANTINE_FILE="$PROJECT_DIR/config/quarantine/quarantined_suites.txt"
TEST_DIR="$PROJECT_DIR/test"

if [[ ! -f "$QUARANTINE_FILE" ]]; then
  echo "ERROR: quarantine metadata file not found: $QUARANTINE_FILE" >&2
  exit 1
fi

trim() { local var="$*"; var="${var#"${var%%[![:space:]]*}"}"; var="${var%"${var##*[![:space:]]}"}"; printf '%s' "$var"; }

while IFS= read -r raw_line || [[ -n "$raw_line" ]]; do
  line="$raw_line"
  line="${line#"${line%%[![:space:]]*}"}"
  [[ -z "$line" || "${line:0:1}" == "#" ]] && continue

  IFS='|' read -r raw_suite _owner _reason <<< "$line"
  suite="$(trim "${raw_suite:-}")"
  [[ -z "$suite" ]] && continue

  suite_file="$TEST_DIR/${suite}.erl"
  if [[ ! -f "$suite_file" ]]; then
    echo "ERROR: Suite listed in quarantine but file missing: $suite_file" >&2
    exit 1
  fi

  # Check for quarantine group declaration in suite file
  if grep -E "\{\s*quarantine\s*," "$suite_file" >/dev/null 2>&1; then
    continue
  fi

  # Fallback: check groups() function returns a tuple with name 'quarantine'
  if grep -E "groups\s*\(\)" "$suite_file" >/dev/null 2>&1 && \
     grep -E "\{\s*quarantine\s*," "$suite_file" >/dev/null 2>&1; then
    continue
  fi

  echo "ERROR: $suite is listed in quarantined_suites.txt but does not define CT group 'quarantine'" >&2
  exit 1
done < "$QUARANTINE_FILE"

echo "CT quarantine consistency: OK"

