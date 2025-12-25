A) CT-native proof:
- The exclusion/inclusion is done via CT group mechanisms (groups/all) and rebar3 ct group flags.
- No runner-only filtering is relied upon for quarantine semantics.

B) Full tier:
- bash scripts/ct-full.sh (or list mode) must show quarantined suites are excluded by CT group selection.
- At least one quarantined suite is proven NOT executed.

C) Heavy tier:
- ROUTER_TEST_LEVEL=heavy bash scripts/ct-heavy.sh runs quarantined suite(s) (evidence from CT logs).

D) Single source of truth:
- Adding/removing an entry in quarantined_suites.txt changes behavior without editing suites.

E) Stability:
- ./scripts/ct-full.sh runtime remains within baseline ±10%.
