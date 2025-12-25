#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GUARD="$SCRIPT_DIR/check_ct_suite_structure.sh"

if [ ! -x "$GUARD" ]; then
    echo "Guard script not found or not executable: $GUARD" >&2
    exit 1
fi

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

mkdir -p "$TMPDIR/valid"
cat <<'EOF' > "$TMPDIR/valid/valid_SUITE.erl"
-module(valid_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() -> [].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.
EOF

mkdir -p "$TMPDIR/missing_cb"
cat <<'EOF' > "$TMPDIR/missing_cb/missing_cb_SUITE.erl"
-module(missing_cb_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() -> [].
end_per_suite(_Config) -> ok.
init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.
EOF

mkdir -p "$TMPDIR/missing_export"
cat <<'EOF' > "$TMPDIR/missing_export/missing_export_SUITE.erl"
-module(missing_export_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() -> [].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.
EOF

run_guard() {
    local dir="$1"
    local expect="$2"
    local keyword="$3"
    local output
    if output="$("$GUARD" "$dir" 2>&1)"; then
        ret=0
    else
        ret=$?
    fi
    if [ "$ret" -ne "$expect" ]; then
        echo "Guard exit code $ret did not match expected $expect for $dir" >&2
        echo "$output" >&2
        exit 1
    fi
    if [ "$expect" -ne 0 ]; then
        echo "$output" | grep -q "$keyword" || {
            echo "Expected keyword '$keyword' not found in guard output for $dir" >&2
            echo "$output" >&2
            exit 1
        }
    fi
    echo "check_ct_suite_structure.sh ($dir) -> exit $ret"
}

run_guard "$TMPDIR/valid" 0 "checked"
run_guard "$TMPDIR/missing_cb" 1 "missing function definition"
run_guard "$TMPDIR/missing_export" 1 "missing export of init_per_suite/1"
