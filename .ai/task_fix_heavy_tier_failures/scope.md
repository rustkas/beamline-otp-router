## In Scope
- Diagnose and fix the failures revealed by the heavy-tier run log above, focusing on the affected modules (`router_nats_server`, `router_nats`, `router_decider`, `router_groups`, etc.).
- Keep the full-tier behavior stable; changes must not regress existing full-tier scenarios.
- Use targeted heavy-suite commands for verification (see acceptance/progress) rather than rerunning the entire heavy tier.

## Out of Scope
- Any adjustments to the quarantine mechanism or `config/quarantine/quarantined_suites.txt` unless a failure explicitly requires it.
- Broad performance tunings or resiliency efforts outside the reported failures.
- Changes that require rewriting suites outside the heavy tier unless those suites fail for the same root cause and are covered above.
