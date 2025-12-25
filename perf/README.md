# CP1 Performance Freeze

This directory defines the **authoritative performance baseline** and **regression policy** for **CP1 (Pre-Production Foundation)**.

Once CP1 is frozen, **any change that violates this policy is a hard regression** and **MUST NOT be merged** without an explicit baseline update and justification.

---

## Files

### baseline_cp1.json
Frozen reference metrics for CP1.

This file represents **what the system is allowed to be** at CP1 freeze time.

It is:
- Immutable during normal development
- Used by CI as the comparison anchor
- Updated **only** via an explicit, reviewed baseline bump

### policy_cp1.json
Regression policy applied to the baseline.

Defines:
- Which metrics are checked
- How regressions are measured (percentage or absolute)
- What deviation is tolerated

### Generated artifacts (`_artifacts/`)
Produced by CI or local runs:
- `perf_baseline_*.json` — raw benchmark outputs
- `perf_gate_*.log` — gate execution logs
- `perf_summary_*.md` — human-readable summary

Artifacts are **never committed**.

---

## CP1 Freeze Metrics (Authoritative)

| Metric | Meaning | Gate Rule |
|--------|---------|-----------|
| `latency_p95_ms` | p95 end-to-end routing latency | ≤ +15% |
| `latency_p99_ms` | p99 end-to-end routing latency | ≤ +20% |
| `rps` | Sustained throughput (concurrent) | ≥ −10% |
| `errors_total` | Router errors during benchmark | **0 only** |
| `backpressure_active_total` | Backpressure activations | **0 only** |
| `mem_mb_total_erlang` | BEAM memory footprint | ≤ +15% |

These metrics define **CP1 fitness**.

If any of them regress beyond policy:
→ CP1 is considered **broken**.

---

## How the Gate Works

The performance gate is implemented in:

```
scripts/perf_gate.sh
```

Behavior:
1. Runs the benchmark (`scripts/bench_router.sh`)
2. Collects JSON output from `_artifacts/perf_baseline_*.json`
3. Compares vs `baseline_cp1.json` using `policy_cp1.json`
4. Fails CI if **any metric violates policy**

---

## CI Usage

### Merge Requests (Smoke)
```bash
./scripts/perf_gate.sh
```

- Single run
- Informational
- May be allowed to fail without blocking

### Main / Scheduled (Gate)
```bash
./scripts/perf_gate.sh
```

- **Blocking**
- Defines CP1 health
- Failure = **no merge / no release**

---

## Updating the Baseline (Strict Rules)

Updating `baseline_cp1.json` is **not a casual change**.

It is allowed **only** if one of the following is true:

1. **Intentional trade-off**
   - e.g. latency ↑ for new correctness or safety guarantees

2. **Architectural improvement**
   - e.g. higher throughput at acceptable memory cost

3. **Measurement correction**
   - benchmark was wrong or incomplete

### Required for a Baseline Update PR

The PR **MUST** include:

- Updated `baseline_cp1.json`
- Link to benchmark artifacts (`perf_baseline_*.json`)
- Before / after comparison
- Written justification:
  - what changed
  - why it is acceptable
  - why policy still makes sense

Baseline bumps are:
- **Rare**
- **Explicit**
- **Reviewed like API changes**

---

## What This Is (and Is Not)

### This IS
- A **freeze gate**, not an optimization playground
- A **contract** between development and operations
- A **CP1 readiness guarantee**

### This is NOT
- A microbenchmark competition
- A promise of final production SLA
- A tuning playground for local experiments

---

## Relation to Other Readiness Layers

| Layer | Responsibility |
|-------|---------------|
| T-INFRA-01 | Deterministic NATS + test infra |
| **T-PERF-01** | **This directory — performance freeze** |
| T-SEC-01 | TLS + crypto overhead validation |
| T-OPS-01 | Rollback, recovery, runbooks |

---

## Local Usage

```bash
# Run benchmark
./scripts/bench_router.sh

# Check against baseline
./scripts/perf_gate.sh

# Or with existing candidate
CANDIDATE_FILE=_artifacts/perf_baseline_*.json ./scripts/perf_gate.sh
```

---

## CI Determinism Rules

To keep signal stable enough for gating:

1. **Pinned environment**:
   - Dedicated runner class for perf jobs
   - No parallel jobs on the same host
   - Fixed CPU/memory limits

2. **Warmup**:
   - Discard first measurements
   - Avoid cache/JIT cold start effects

3. **Bounded timeouts**:
   - Hard timeout in CI (default: 900s)
   - Failure still produces artifacts

4. **Env pinning**:
   - Git commit SHA
   - OTP release
   - CPU count
   - Kernel version

---

## Summary

If `perf_gate.sh` passes:
→ **CP1 performance contract holds**

If it fails:
→ **Something regressed**
→ Fix the code **or** justify and update the baseline

No silent regressions. No "we'll fix later".

This is the CP1 line in the sand.
