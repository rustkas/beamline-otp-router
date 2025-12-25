#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
lint_file="$repo_root/.elp_lint.toml"
vscode_file="$repo_root/.vscode/settings.json"

python3 - "$lint_file" "$vscode_file" <<'PY'
import json
import os
import re
import sys
from typing import List


def read_disabled_lints(toml_path: str) -> List[str]:
    data = open(toml_path, "r", encoding="utf-8").read()

    # Minimal TOML parse for: disabled_lints = ["W0008", "W0052"]
    m = re.search(r'^\s*disabled_lints\s*=\s*\[(.*?)\]\s*$', data, flags=re.MULTILINE | re.DOTALL)
    if m is None:
        return []

    inner = m.group(1)
    # capture "W0008" 'W0008'
    items = re.findall(r'["\']([A-Za-z0-9_]+)["\']', inner)
    out = []
    seen = set()
    for x in items:
        if x not in seen:
            seen.add(x)
            out.append(x)
    return out


def write_atomic_json(path: str, obj: object) -> None:
    tmp_path = f"{path}.tmp"
    with open(tmp_path, "w", encoding="utf-8") as f:
        json.dump(obj, f, ensure_ascii=False, indent=4)
        f.write("\n")
    os.replace(tmp_path, path)


def main() -> int:
    lint_file = sys.argv[1]
    vscode_file = sys.argv[2]

    disabled_lints = sorted(read_disabled_lints(lint_file))

    with open(vscode_file, "r", encoding="utf-8") as f:
        settings = json.load(f)

    settings["elp.diagnostics.disabled"] = disabled_lints
    write_atomic_json(vscode_file, settings)

    print(f"[OK] Synced {len(disabled_lints)} lint(s) -> {vscode_file}: elp.diagnostics.disabled")
    if disabled_lints:
        print("     " + ", ".join(disabled_lints))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
PY
