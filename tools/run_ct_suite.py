#!/usr/bin/env python3
"""Run Common Test suite via rebar3."""
import subprocess
import sys
from pathlib import Path


def main() -> int:
    """Run rebar3 ct for specified suite."""
    project_root = Path(__file__).resolve().parent.parent

    if len(sys.argv) < 2:
        print("Usage: python3 tools/run_ct_suite.py <suite_name>", file=sys.stderr)
        print("Example: python3 tools/run_ct_suite.py router_caf_adapter_load_thresholds_SUITE", file=sys.stderr)
        return 1

    suite_name = sys.argv[1]
    
    # First compile to check for errors
    print("=== Compiling...")
    compile_cmd = ["rebar3", "compile"]
    compile_result = subprocess.run(compile_cmd, cwd=project_root, check=False)
    if compile_result.returncode != 0:
        print("=== Compilation failed!", file=sys.stderr)
        return compile_result.returncode
    
    # Then run tests
    print(f"=== Running tests for {suite_name}...")
    cmd = ["rebar3", "ct", "--dir", "test", "--suite", suite_name]
    result = subprocess.run(cmd, cwd=project_root, check=False)
    return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
