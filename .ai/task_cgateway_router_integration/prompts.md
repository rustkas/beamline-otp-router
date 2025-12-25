# Prompts — T-CGW-ROUTER-01

## Operating Instructions

**Mode**: Audit & Documentation (READ-ONLY)

**Rules**:
- Do NOT modify Router code (CP1 frozen)
- Do NOT modify c-gateway code (unless critical bug found, then document only)
- Document reality as-is, not "should be"
- Findings inform CP2, but don't implement CP2 here

**Evidence**:
- Every claim must have evidence (code reference, log output, test result)
- Screenshots/logs for observability validation
- Command outputs for failure mode tests

**Update frequency**:
- Update `progress.md` after each completed step
- Document findings immediately when discovered
- Flag blockers as soon as identified

## Micro-Prompts for Agent/IDE

- "Find c-gateway HTTP handler for router requests"
- "Trace request flow: HTTP → NATS → Router"
- "Document actual subject used (not assumed)"
- "Test backpressure: trigger overload, observe HTTP response"
- "Check trace_id in logs: gateway → router → gateway"
- "Simulate Router down: observe c-gateway behavior"
- "Verify Retry-After header when backpressure active"

## Quality Checks

Before marking step complete:
- [ ] Evidence provided (code/logs/output)
- [ ] Documentation clear and actionable
- [ ] Findings categorized (blocker/caveat/OK)
- [ ] No speculation (only observed facts)
