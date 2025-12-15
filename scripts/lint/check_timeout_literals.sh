#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

IGNORE_PHRASE="router_test_timeouts"

find_literal_issues() {
    local pattern=$1
    local -n lines=$2

    mapfile -t raw < <(rg -n --no-messages "$pattern" test)
    for entry in "${raw[@]}"; do
        [[ -z "$entry" ]] && continue
        # entry format: path:line:text
        local line_text="${entry#*:}"
        local trimmed="${line_text#"${line_text%%[![:space:]]*}"}"
        [[ "${trimmed:0:1}" == "%" ]] && continue
        [[ "$line_text" == *"$IGNORE_PHRASE"* ]] && continue
        lines+=("$entry")
    done
}

receive_issues=()
call_issues=()

find_literal_issues "after [0-9]+ ->" receive_issues
find_literal_issues "gen_server:call\\([^,]+,[[:space:]]*[0-9]+\\)" call_issues

report() {
    local title=$1
    local -n entries=$2
    if [[ ${#entries[@]} -gt 0 ]]; then
        printf "%s detected (%d occurrences):\n" "$title" "${#entries[@]}"
        for ent in "${entries[@]}"; do
            printf "  %s\n" "$ent"
        done
        return 0
    fi
    return 1
}

warned=false
if report "Hardcoded receive/after timeouts" receive_issues; then
    warned=true
fi
if report "Hardcoded gen_server:call timeouts" call_issues; then
    warned=true
fi

if [[ "$warned" == true ]]; then
    echo "Timeout literals should route through router_test_timeouts (use the helper or \"timeout/1\")." >&2
else
    echo "Timeout literal check passed (router_test_timeouts used everywhere)."
fi
