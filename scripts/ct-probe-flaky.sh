#!/usr/bin/env bash
set -euo pipefail

suite_path="${1:-}"
iterations="${2:-5}"

if [[ -z "$suite_path" ]]; then
    echo "Usage: $0 <suite_path> [iterations]"
    exit 1
fi

if ! [[ "$iterations" =~ ^[0-9]+$ ]]; then
    echo "Iteration count must be a positive integer, got: $iterations" >&2
    exit 1
fi

printf "Starting flaky probe for %s (%s iterations)\n" "$suite_path" "$iterations"

for iteration in $(seq 1 "$iterations"); do
    printf "Iteration %d/%s: running suite...\n" "$iteration" "$iterations"

    if ROUTER_TEST_LEVEL=full rebar3 ct --suite "$suite_path"; then
        printf "Iteration %d/%s: PASS\n" "$iteration" "$iterations"
    else
        printf "Iteration %d/%s: FAIL\n" "$iteration" "$iterations"
        exit 1
    fi
done

echo "All $iterations iterations passed for $suite_path"
