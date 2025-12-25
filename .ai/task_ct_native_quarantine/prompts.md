Guidance:
- Use CT groups as the primary mechanism for quarantine selection.
- Keep config/quarantine/quarantined_suites.txt authoritative and parsed once.
- Prefer minimal suite changes by delegating to router_ct_groups helper.
- Maintain full tier stability during staged rollout; rollback on regressions.
