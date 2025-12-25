#!/bin/bash
# Generate build_info.json for ELP (Erlang Language Platform)
#
# This script should be run after `rebar3 compile` to ensure
# ELP has up-to-date project information.

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_INFO_FILE="${PROJECT_ROOT}/build_info.json"

echo "Generating build_info.json for ELP..."

# Detect OTP root
OTP_ROOT=$(erl -eval 'io:format("~s", [code:root_dir()]), halt().' -noshell 2>/dev/null || echo "/usr/lib/erlang")

# Find stdlib and kernel versions
STDLIB_DIR=$(ls -d ${OTP_ROOT}/lib/stdlib-* 2>/dev/null | head -1)
KERNEL_DIR=$(ls -d ${OTP_ROOT}/lib/kernel-* 2>/dev/null | head -1)

cat > "${BUILD_INFO_FILE}" <<EOF
{
  "apps": [
    {
      "name": "beamline_router",
      "dir": "${PROJECT_ROOT}",
      "src_dirs": [
        "src",
        "test",
        "test_support"
      ],
      "extra_src_dirs": [
        "test",
        "test_support"
      ],
      "include_dirs": [
        "${PROJECT_ROOT}/include",
        "${PROJECT_ROOT}/_build/default/plugins/gpb/include",
        "${PROJECT_ROOT}/_build/default/lib/grpcbox/include",
        "${PROJECT_ROOT}/src"
      ],
      "macros": {}
    }
  ],
  "deps": [
    "${STDLIB_DIR}",
    "${KERNEL_DIR}",
    "${PROJECT_ROOT}/_build/default/lib/hpack",
    "${PROJECT_ROOT}/_build/default/lib/enats_msg",
    "${PROJECT_ROOT}/_build/default/lib/gproc",
    "${PROJECT_ROOT}/_build/default/lib/opentelemetry_api",
    "${PROJECT_ROOT}/_build/default/lib/enats",
    "${PROJECT_ROOT}/_build/default/lib/jsx",
    "${PROJECT_ROOT}/_build/default/lib/grpcbox",
    "${PROJECT_ROOT}/_build/default/lib/telemetry",
    "${PROJECT_ROOT}/_build/default/lib/chatterbox",
    "${PROJECT_ROOT}/_build/default/lib/ctx",
    "${PROJECT_ROOT}/_build/default/lib/acceptor_pool",
    "${PROJECT_ROOT}/_build/default/lib/jesse",
    "${PROJECT_ROOT}/_build/test/lib/meck",
    "${PROJECT_ROOT}/_build/test/lib/proper"
  ],
  "otp_root": "${OTP_ROOT}"
}
EOF

echo "✓ build_info.json generated at ${BUILD_INFO_FILE}"
echo "✓ OTP root: ${OTP_ROOT}"
echo "✓ stdlib: ${STDLIB_DIR}"
echo "✓ kernel: ${KERNEL_DIR}"
echo ""
echo "✓ Restart VS Code or run 'Developer: Reload Window' to refresh ELP"
