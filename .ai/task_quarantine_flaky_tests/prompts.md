## Prompts

### Adding a suite to quarantine
“Add the suite entry (name + reason + owner initials) to `config/quarantine/quarantined_suites.txt`, then rerun `bash scripts/ct-full.sh --list` to confirm it now appears in the quarantined section and is omitted from the execution plan.”

### Making sure the heavy tier still covers quarantined suites
“Run `ROUTER_TEST_LEVEL=heavy bash scripts/ct-heavy.sh` (or the nightly-heavy rebar invocation) after introducing a quarantine entry and verify the logs mention that the quarantined suite executed so coverage is preserved.”

### Removing a suite from quarantine
“Once the flake is fixed and the suite passes several heavy runs, delete or comment out the entry in `config/quarantine/quarantined_suites.txt` and rerun `bash scripts/ct-full.sh --list` to confirm the suite returns to the normal full-tier scheduling.”
