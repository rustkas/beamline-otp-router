#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

exec "${script_dir}/ct-batch.sh" --batch=1 --level=all --duration-hours=0.01 "$@"
