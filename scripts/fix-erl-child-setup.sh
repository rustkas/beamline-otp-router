#!/usr/bin/env bash
set -euo pipefail

root="$(erl -noshell -eval 'io:format("~s",[code:root_dir()]),halt().' 2>/dev/null)"
ecs="$(find "$root" -name erl_child_setup -type f 2>/dev/null | head -n 1)"

if [[ -z "$ecs" ]]; then
  echo "erl_child_setup not found under $root" >&2
  exit 1
fi

mode="$(stat -c '%a' "$ecs")"
perms="$(stat -c '%A' "$ecs")"

echo "erl_child_setup=$ecs mode=$mode perms=$perms"

# Expect setuid root: -rwsr-xr-x and numeric 4755
if [[ "$mode" != "4755" ]]; then
  sudo chown root:root "$ecs"
  sudo chmod 4755 "$ecs"
  echo "fixed: $(ls -l "$ecs")"
else
  echo "ok: already setuid"
fi
