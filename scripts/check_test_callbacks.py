#!/usr/bin/env python3
"""
Check test suite files for undefined callbacks in nowarn_unused_function.

This script finds all test suite files that mention init_per_testcase/2 or
end_per_testcase/2 in nowarn_unused_function but don't define them.
"""
import re
from pathlib import Path


def check_file(filepath):
    """Check a single test suite file for callback issues."""
    issues = []
    
    try:
        content = filepath.read_text(encoding='utf-8')
    except Exception as e:
        return [f"Error reading {filepath}: {e}"]
    
    # Check for nowarn_unused_function with init_per_testcase or end_per_testcase
    nowarn_match = re.search(
        r'-compile\(\s*\{\s*nowarn_unused_function\s*,\s*\[(.*?)\]\s*\}\s*\)',
        content,
        re.DOTALL
    )
    
    if not nowarn_match:
        return []
    
    nowarn_list = nowarn_match.group(1)
    
    # Check which callbacks are mentioned
    has_init_per_testcase = 'init_per_testcase/2' in nowarn_list
    has_end_per_testcase = 'end_per_testcase/2' in nowarn_list
    
    if not (has_init_per_testcase or has_end_per_testcase):
        return []
    
    # Check if functions are defined
    defines_init = bool(re.search(r'^\s*init_per_testcase\s*\(', content, re.MULTILINE))
    defines_end = bool(re.search(r'^\s*end_per_testcase\s*\(', content, re.MULTILINE))
    
    if has_init_per_testcase and not defines_init:
        issues.append(f"{filepath.name}: init_per_testcase/2 in nowarn_unused_function but not defined")
    
    if has_end_per_testcase and not defines_end:
        issues.append(f"{filepath.name}: end_per_testcase/2 in nowarn_unused_function but not defined")
    
    return issues


def main():
    """Main function."""
    project_root = Path(__file__).resolve().parent.parent
    test_dir = project_root / "test"
    
    if not test_dir.exists():
        print(f"Test directory not found: {test_dir}")
        return 1
    
    all_issues = []
    
    for suite_file in test_dir.glob("*_SUITE.erl"):
        issues = check_file(suite_file)
        all_issues.extend(issues)
    
    if all_issues:
        print("Found issues:")
        for issue in all_issues:
            print(f"  - {issue}")
        return 1
    
    print("No issues found.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

