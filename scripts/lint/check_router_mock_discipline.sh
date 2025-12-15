#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." >/dev/null && pwd)"
cd "$ROOT"

PATTERN_PASSTHROUGH="meck:new\\s*\\([^)]*passthrough"
PATTERN_DIRECT="gen_server:call\\s*\\(\\s*router_nats"

SKIP_PATTERNS=(
    "/_data/"
    "/fixtures/"
    ".skip"
    ".skip2"
)

APPROVED_HELPERS=(
    "test/router_nats_test_helper.erl"
    "test/router_mock_helpers.erl"
)

PASSTHROUGH_FILES=()
DIRECT_CALL_FILES=()

should_skip_file() {
    local file="$1"
    for pattern in "${SKIP_PATTERNS[@]}"; do
        [[ "$file" == *"$pattern"* ]] && return 0
    done
    return 1
}

has_non_comment_direct_call() {
    local file="$1"
    local matches
    matches=$(rg --no-messages --no-heading --line-number "$PATTERN_DIRECT" "$file" || true)
    [[ -z "$matches" ]] && return 1
    while IFS= read -r match_line; do
        [[ -z "$match_line" ]] && continue
        local line_text="${match_line#*:}"
        local trimmed="${line_text#"${line_text%%[![:space:]]*}"}"
        [[ -z "$trimmed" ]] && continue
        [[ "${trimmed:0:1}" == "%" ]] && continue
        return 0
    done <<< "$matches"
    return 1
}

filter_direct_candidate() {
    local file="$1"
    [[ -n "$file" ]] || return 1
    should_skip_file "$file" && return 1
    for helper in "${APPROVED_HELPERS[@]}"; do
        [[ "$file" == "$helper" ]] && return 1
    done
    has_non_comment_direct_call "$file"
}

mapfile -t RAW_PASSTHROUGH_FILES < <(rg -l --no-messages -g "*.erl" "$PATTERN_PASSTHROUGH" test)
mapfile -t RAW_DIRECT_CALL_FILES < <(rg -l --no-messages -g "*.erl" "$PATTERN_DIRECT" test)

for file in "${RAW_PASSTHROUGH_FILES[@]}"; do
    [[ -z "$file" ]] && continue
    should_skip_file "$file" && continue
    PASSTHROUGH_FILES+=("$file")
done

for file in "${RAW_DIRECT_CALL_FILES[@]}"; do
    [[ -z "$file" ]] && continue
    filter_direct_candidate "$file" && DIRECT_CALL_FILES+=("$file")
done

FILTERED_DIRECT_FILES=("${DIRECT_CALL_FILES[@]}")

if [[ ${#PASSTHROUGH_FILES[@]} -gt 0 && ${#FILTERED_DIRECT_FILES[@]} -gt 0 ]]; then
    printf 'mock_discipline violation: passthrough files (%s) and direct router_nats call files (%s)\n' \
        "${PASSTHROUGH_FILES[*]}" "${FILTERED_DIRECT_FILES[*]}" >&2
    exit 1
fi

printf 'mock discipline check passed (%d passthrough, %d direct calls)\n' \
    "${#PASSTHROUGH_FILES[@]}" "${#FILTERED_DIRECT_FILES[@]}"
