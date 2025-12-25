# Progress

## router_jetstream_recovery_ext_SUITE
- Callbacks added: init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2
- Linter status: verified (`erl -noshell -pa test_support -s router_suite_linter run -s init stop`)

## router_caf_adapter_SUITE
- Callbacks added: init_per_testcase/2, end_per_testcase/2 (exports/compile attrs updated)
- Linter status: verified (`erl -noshell -pa test_support -s router_suite_linter run -s init stop`)
