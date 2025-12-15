#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROOT"

CONFIG_FILE="$ROOT/config/flaky_probe_suites.txt"
CT_PROBE="$ROOT/scripts/ct-probe-flaky.sh"
ITERATIONS="${FLAKY_PROBE_ITERS:-10}"
FLAKY_PROBE_SUITES_ENV="${FLAKY_PROBE_SUITES:-}"

if [[ -n "$FLAKY_PROBE_SUITES_ENV" ]]; then
    IFS=', ' read -ra SUITES <<< "$FLAKY_PROBE_SUITES_ENV"
else
    if [[ ! -f "$CONFIG_FILE" ]]; then
        echo "Missing config with default suites: $CONFIG_FILE" >&2
        exit 1
    fi
    mapfile -t SUITES < <(grep -vE '^\s*(#|$)' "$CONFIG_FILE")
fi

if [[ ${#SUITES[@]} -eq 0 ]]; then
    echo "No flaky probe suites provided via FLAKY_PROBE_SUITES or $CONFIG_FILE" >&2
    exit 1
fi

REPORT_DIR="$ROOT/reports"
mkdir -p "$REPORT_DIR"
REPORT_FILE="$REPORT_DIR/flaky_probe.json"

IS_NIGHTLY="false"
if [[ "${NIGHTLY:-0}" == "1" || "${CI_PIPELINE_SOURCE:-}" == "schedule" ]]; then
    IS_NIGHTLY="true"
fi

failures=0
records=()

for suite in "${SUITES[@]}"; do
    suite="${suite#"${suite%%[![:space:]]*}"}"
    suite="${suite%"${suite##*[![:space:]]}"}"
    [[ -z "$suite" ]] && continue

    printf "=== Probing %s (%s iterations) ===\n" "$suite" "$ITERATIONS"

    set +e
    output=$("$CT_PROBE" "$suite" "$ITERATIONS" 2>&1)
    rc=$?
    set -e

    echo "$output"

    if [[ $rc -eq 0 ]]; then
        status="passed"
        failure_iteration="null"
    else
        status="failed"
        failures=$((failures + 1))
        failure_iteration=$(printf '%s' "$output" | awk -F'#' '/FAILED on run #[0-9]+/ {print $2; exit}')
        failure_iteration="${failure_iteration:-null}"
    fi

    records+=("{\"suite\":\"$suite\",\"iterations\":$ITERATIONS,\"status\":\"$status\",\"failure_iteration\":${failure_iteration}}")
done

{
    printf '{\n'
    printf '  "generated_at": "%s",\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    printf '  "nightly": %s,\n' "$IS_NIGHTLY"
    printf '  "iterations": %s,\n' "$ITERATIONS"
    printf '  "suites": [\n'
    for idx in "${!records[@]}"; do
        printf '    %s' "${records[$idx]}"
        if [[ $idx -lt $((${#records[@]} - 1)) ]]; then
            printf ',\n'
        else
            printf '\n'
        fi
    done
    printf '  ],\n'
    printf '  "summary": {\n'
    printf '    "suite_count": %s,\n' "${#records[@]}"
    printf '    "failures": %s\n' "$failures"
    printf '  }\n'
    printf '}\n'
} > "$REPORT_FILE"

if [[ $failures -gt 0 ]]; then
    if [[ "$IS_NIGHTLY" == "true" ]]; then
        echo "Nightly build: $failures flaky-suite failure(s) detected. Failing the job."
        exit 1
    else
        echo "UNSTABLE: $failures flaky-suite failure(s) detected (reports/flaky_probe.json)."
    fi
else
    echo "Flaky probe detected no failures."
fi
