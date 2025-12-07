# CI Test Strategy for JetStream Recovery Tests

## Standard CI Run

Command: `rebar3 ct --dir test`

Behavior:
- Extended SUITE: Only `smoke_test` runs (fast, ~1 second)
- Soak SUITE: Only `smoke_test` runs (fast, ~1 second)
- All other tests: Run normally

Expected duration: < 5 minutes

## Extended Recovery Tests

Command: `RUN_EXTENDED_RECOVERY=1 rebar3 ct --dir test --suite router_jetstream_extended_recovery_SUITE`

Behavior:
- Runs all extended recovery scenarios
- Duration can be scaled via `EXTENDED_TEST_SPEEDUP` env var

Example:
```bash
EXTENDED_TEST_SPEEDUP=60 RUN_EXTENDED_RECOVERY=1 rebar3 ct --dir test --suite router_jetstream_extended_recovery_SUITE
```

## Soak Tests (Nightly)

Command: `RUN_JETSTREAM_SOAK=1 rebar3 ct --dir test --suite router_jetstream_soak_SUITE`

Behavior:
- Runs all ultra-long soak scenarios
- Duration can be scaled via `EXTENDED_TEST_SPEEDUP` env var

Example:
```bash
EXTENDED_TEST_SPEEDUP=120 RUN_JETSTREAM_SOAK=1 rebar3 ct --dir test --suite router_jetstream_soak_SUITE
```

## Throughput Assertions

Default minimum throughput ratio: 90% of baseline

Override via env var:
```bash
RECOVERY_THROUGHPUT_MIN_RATIO=0.85 rebar3 ct --dir test
```

## Nightly Job Configuration

Recommended nightly job:
```bash
EXTENDED_TEST_SPEEDUP=10 \
RUN_EXTENDED_RECOVERY=1 \
RUN_JETSTREAM_SOAK=1 \
RECOVERY_THROUGHPUT_MIN_RATIO=0.85 \
rebar3 ct --dir test
```

