#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

exec "${script_dir}/ct-batch.sh" --batch=1 --level=heavy --duration-hours=0.01 --resume-from=test/router_chaos_engineering_SUITE.erl "$@"
