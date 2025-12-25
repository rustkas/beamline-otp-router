#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${repo_root}"

baseline_file="${BASELINE_FILE:-perf/baseline_cp1.json}"
policy_file="${POLICY_FILE:-perf/policy_cp1.json}"

mkdir -p _artifacts

echo "=== PERF GATE (CP1) ==="
echo "baseline=${baseline_file}"
echo "policy=${policy_file}"

# Run benchmark unless CANDIDATE_FILE is already provided
candidate_file="${CANDIDATE_FILE:-}"
if [ -z "${candidate_file}" ]; then
  echo "[1/2] Running benchmark..."
  ./scripts/bench_router.sh | tee "_artifacts/perf_gate_${CI_PIPELINE_ID:-local}_$(date +%Y%m%d_%H%M%S).log" >/dev/null
  candidate_file="$(ls -1t _artifacts/perf_baseline_*.json 2>/dev/null | head -1 || true)"
fi

if [ -z "${candidate_file}" ] || [ ! -f "${candidate_file}" ]; then
  echo "ERROR: candidate metrics json not found. Set CANDIDATE_FILE or ensure bench produces _artifacts/perf_baseline_*.json"
  exit 2
fi

echo "[2/2] Evaluating policy..."
python3 - <<'PY'
import json, os, sys, math

def load(path: str):
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)

baseline_path = os.environ.get("baseline_file")
policy_path = os.environ.get("policy_file")
candidate_path = os.environ.get("candidate_file")

baseline = load(baseline_path)
policy = load(policy_path)
candidate = load(candidate_path)

rules = policy["rules"]
gate = policy.get("gate", {})

def fail(msg: str):
    print(f"FAIL: {msg}")
    sys.exit(1)

def pct_increase(base, cand):
    if base == 0:
        return math.inf if cand > 0 else 0.0
    return (cand - base) * 100.0 / base

def pct_decrease(base, cand):
    if base == 0:
        return 0.0
    return (base - cand) * 100.0 / base

cand_ct_rc = candidate.get("run", {}).get("ct_exit_code", 0)
if gate.get("require_candidate_ct_exit_code_zero", True) and cand_ct_rc != 0:
    fail(f"candidate ct_exit_code={cand_ct_rc} (must be 0)")

b_metrics = baseline.get("metrics", {})
c_metrics = candidate.get("metrics", {})

require_all = gate.get("require_all_metrics_present", True)
missing = [k for k in rules.keys() if k not in c_metrics or c_metrics[k] is None]
if require_all and missing:
    fail(f"missing candidate metrics: {missing}")

print(f"Baseline:  {baseline_path}")
print(f"Candidate: {candidate_path}")

for name, rule in rules.items():
    if name not in c_metrics:
        continue
    base = float(b_metrics.get(name, 0))
    cand = float(c_metrics[name])

    abs_rule = rule.get("absolute")
    rel_rule = rule.get("relative")

    if abs_rule:
        if "equals" in abs_rule and cand != float(abs_rule["equals"]):
            fail(f"{name}: candidate={cand} must equal {abs_rule['equals']}")
        if "max" in abs_rule and cand > float(abs_rule["max"]):
            fail(f"{name}: candidate={cand} must be <= {abs_rule['max']}")
        if "min" in abs_rule and cand < float(abs_rule["min"]):
            fail(f"{name}: candidate={cand} must be >= {abs_rule['min']}")

    if rel_rule:
        if "max_pct_increase" in rel_rule:
            inc = pct_increase(base, cand)
            if inc > float(rel_rule["max_pct_increase"]):
                fail(f"{name}: +{inc:.2f}% > allowed +{rel_rule['max_pct_increase']}% (base={base}, cand={cand})")
        if "max_pct_decrease" in rel_rule:
            dec = pct_decrease(base, cand)
            if dec > float(rel_rule["max_pct_decrease"]):
                fail(f"{name}: -{dec:.2f}% > allowed -{rel_rule['max_pct_decrease']}% (base={base}, cand={cand})")

print("PASS: perf gate satisfied")
PY
