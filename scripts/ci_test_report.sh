#!/bin/bash
set -e

# CI Test Report Generator
# Parses Common Test suite.log files to generate JUnit XML and Performance Summary.
# Usage: ./scripts/ci_test_report.sh [output_dir]

OUTPUT_DIR="${1:-test_results}"
mkdir -p "$OUTPUT_DIR"
JUNIT_XML="$OUTPUT_DIR/junit.xml"
LOGS_ROOT="_build/test/logs"

echo "=== CI Test Report Generator ==="
echo "Logs Root: $LOGS_ROOT"
echo "Output: $JUNIT_XML"

# Find latest run dir
LATEST_RUN=$(ls -td "$LOGS_ROOT"/ct_run.* 2>/dev/null | head -n 1)

if [ -z "$LATEST_RUN" ]; then
    echo "❌ No test logs found in $LOGS_ROOT. Run tests first."
    exit 1
fi

echo "Latest Run: $LATEST_RUN"

# Python script to parse logs and generate report
# We use heredoc passed to python3 to avoid shell quoting hell
python3 - "$LATEST_RUN" "$JUNIT_XML" <<EOF
import os
import sys
import glob
import re
import xml.etree.ElementTree as ET

root_dir = sys.argv[1]
output_xml = sys.argv[2]

test_suites = []
all_tests = []

def parse_suite_log(filepath):
    suite_name = "unknown"
    tests = []
    current_test = {}
    
    # Extract suite name from path if possible, fallback to filename
    match = re.search(r"lib\.[^\.]+\.([^\.]+)\.logs", filepath)
    if match:
        suite_name = match.group(1)
    else:
        # Fallback to dirname or filename
        suite_name = os.path.basename(os.path.dirname(os.path.dirname(filepath)))

    try:
        with open(filepath, "r", errors="replace") as f:
            for line in f:
                line = line.strip()
                # Strict matching to avoid capturing "=cases" or "=elapsed_time"
                if line.startswith("=case "):
                    # Start of a test case
                    parts = line.split(maxsplit=1)
                    if len(parts) > 1:
                        name = parts[1]
                        # CT often includes module:function, split it
                        if ":" in name:
                            name = name.split(":")[-1]
                        current_test = {"name": name, "classname": suite_name, "time": 0.0, "status": "unknown"}
                elif line.startswith("=result "):
                     if current_test:
                        parts = line.split(maxsplit=1)
                        if len(parts) > 1:
                             res = parts[1]
                             if res == "ok": current_test["status"] = "passed"
                             elif "skipped" in res: current_test["status"] = "skipped"
                             else: current_test["status"] = "failed"
                elif line.startswith("=elapsed "):
                     if current_test:
                        parts = line.split(maxsplit=1)
                        if len(parts) > 1:
                             time_str = parts[1].replace("s", "")
                             try:
                                current_test["time"] = float(time_str)
                             except ValueError:
                                pass
                        # End of case info usually
                        tests.append(current_test)
                        current_test = {}
    except Exception as e:
        print(f"Error parsing {filepath}: {e}")
        return None

    return {"name": suite_name, "tests": tests}

# Walk directory finding suite.log
log_files = []
for root, dirs, files in os.walk(root_dir):
    if "suite.log" in files:
        # Skip if file is empty or just header
        log_files.append(os.path.join(root, "suite.log"))

print(f"Found {len(log_files)} suite logs.")

total_tests = 0
total_failures = 0
total_skipped = 0
total_errors = 0 

# Build XML
root = ET.Element("testsuites")

for log_file in log_files:
    suite_data = parse_suite_log(log_file)
    if not suite_data:
        continue
        
    suite_tests = suite_data["tests"]
    if not suite_tests:
        continue

    ts = ET.SubElement(root, "testsuite")
    ts.set("name", suite_data["name"])
    
    s_tests = 0
    s_failures = 0
    s_skipped = 0
    s_time = 0.0
    
    for t in suite_tests:
        total_tests += 1
        s_tests += 1
        s_time += t["time"]
        
        tc = ET.SubElement(ts, "testcase")
        tc.set("classname", t["classname"])
        tc.set("name", t["name"])
        tc.set("time", str(t["time"]))
        
        status = t["status"]
        if status == "failed":
            s_failures += 1
            total_failures += 1
            fail = ET.SubElement(tc, "failure")
            fail.set("message", "Test failed")
        elif status == "skipped":
            s_skipped += 1
            total_skipped += 1
            skip = ET.SubElement(tc, "skipped")
        
        all_tests.append(t)

    ts.set("tests", str(s_tests))
    ts.set("failures", str(s_failures))
    ts.set("skipped", str(s_skipped))
    ts.set("time", str(s_time))

# Update root stats
root.set("tests", str(total_tests))
root.set("failures", str(total_failures))
root.set("skipped", str(total_skipped))

# Write XML
tree = ET.ElementTree(root)
try:
    tree.write(output_xml, encoding="UTF-8", xml_declaration=True)
    print(f"✅ JUnit XML written to {output_xml}")
except Exception as e:
    print(f"❌ Failed to write XML: {e}")
    sys.exit(1)

# Slowest Tests Report
print("\n=== 🐢 Top 10 Slowest Tests ===")
sorted_tests = sorted(all_tests, key=lambda x: x["time"], reverse=True)
for i, t in enumerate(sorted_tests[:10]):
    print(f"{i+1}. {t['classname']}:{t['name']} - {t['time']:.4f}s")

if total_failures > 0:
    print(f"\n❌ FAILURES DETECTED: {total_failures}")
    sys.exit(1)
if total_tests == 0:
    print("\n⚠️ No tests found.")
else:
    print("\n✅ All tests passed (or skipped).")
EOF
