#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

exec "${script_dir}/ct-batch.sh" --batch=2 --level=heavy --resume-from=test/router_concurrent_faults_combo_SUITE.erl "$@"
