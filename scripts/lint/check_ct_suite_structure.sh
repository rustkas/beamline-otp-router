#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

DIRECTORY_ARGS=("$@")
SUITE_FILES=()

if [ ${#DIRECTORY_ARGS[@]} -gt 0 ]; then
    for root in "${DIRECTORY_ARGS[@]}"; do
        if [[ "$root" = /* ]]; then
            scan_root="$root"
        else
            scan_root="$PROJECT_ROOT/$root"
        fi
        if [ ! -d "$scan_root" ]; then
            echo "Guard cannot read directory $scan_root" >&2
            exit 1
        fi
        while IFS= read -r -d '' file; do
            SUITE_FILES+=("$file")
        done < <(find "$scan_root" -maxdepth 1 -name "*_SUITE.erl" -type f -print0 | sort -z)
    done
else
    cd "$PROJECT_ROOT"
    tracked=()
    if git rev-parse --verify HEAD >/dev/null 2>&1; then
        mapfile -t tracked < <(git diff --name-only --diff-filter=ACMRTUXB HEAD -- '*_SUITE.erl')
    fi
    mapfile -t untracked < <(git ls-files --others --exclude-standard -- '*_SUITE.erl')
    for path in "${tracked[@]}" "${untracked[@]}"; do
        [ -z "$path" ] && continue
        SUITE_FILES+=("$PROJECT_ROOT/$path")
    done
fi

if [ ${#SUITE_FILES[@]} -eq 0 ]; then
    echo "ct suite structure guard: no suites to check"
    exit 0
fi

CT_GUARD_FILES="$(IFS=:; echo "${SUITE_FILES[*]}")"
export CT_GUARD_FILES

python3 - <<PY
import os
import pathlib
import re
import sys

required = [
    "init_per_suite/1",
    "end_per_suite/1",
    "init_per_testcase/2",
    "end_per_testcase/2",
]

files_env = os.environ.get("CT_GUARD_FILES", "")
files = [p for p in files_env.split(os.pathsep) if p]
errors = []

for path in files:
    path_obj = pathlib.Path(path)
    if not path_obj.exists():
        errors.append(f"{path_obj}: file not found")
        continue
    text = path_obj.read_text()
    for fn in required:
        if f"{fn.split('/')[0]}(" not in text:
            errors.append(f"{path_obj}: missing function definition for {fn}")
        exports = re.findall(r"-export\((\[.*?\])\)\.", text, re.S)
        if not any(fn in block for block in exports):
            errors.append(f"{path_obj}: missing export of {fn}")

if errors:
    print("Common Test suite structure violations detected:")
    for msg in errors:
        print(f"  - {msg}")
    sys.exit(1)

print(f"ct suite structure guard: checked {len(files)} suites")
PY
