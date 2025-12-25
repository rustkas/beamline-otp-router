| Seq | Timestamp | Command | Result | Failure Signature | Patch | Next Step |
| 1 | 2025-12-17T00:00:00Z | bash scripts/lint/check_ct_quarantine_consistency.sh | PASS | - | - | Run deterministic ct |
| 2 | 2025-12-17T00:00:15Z | rebar3 as deterministic ct | PASS | - | - | Run heavy cp2 suite |
| 3 | 2025-12-17T00:00:45Z | ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_jetstream_e2e_cp2_SUITE --readable true --retry | PASS | - | Confirm determinism by re-run | Re-run heavy cp2 suite |
| 4 | 2025-12-17T00:01:10Z | ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_jetstream_e2e_cp2_SUITE --readable true --retry | PASS | - | - | Verify full CT tier |
| 5 | 2025-12-17T00:02:00Z | ./scripts/ct-full.sh | FAIL | CTS suite structure guard violations in unrelated suites | No code changes to cp2 suite (already deterministic, single lifecycle, sequence) | None (outside cp2 scope); cp2 heavy passes twice |
