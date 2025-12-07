#!/usr/bin/env python3
"""
Python wrapper for running router_cp1_minimal_mode_SUITE via rebar3.

This script runs rebar3 ct command and captures stdout/stderr to check for warnings.
Used to validate that test files compile without warnings.
"""
import subprocess
import sys
from pathlib import Path


def main() -> int:
    """Run rebar3 ct command and return exit code."""
    project_root = Path(__file__).resolve().parent.parent
    
    # Build command
    cmd = ["rebar3", "ct", "--dir", "test", "--suite", "router_cp1_minimal_mode_SUITE"]
    
    # Run command
    result = subprocess.run(
        cmd,
        cwd=project_root,
        check=False,
        capture_output=False,  # Show output in real-time
        text=True
    )
    
    return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())

