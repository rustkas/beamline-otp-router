# Progress - CT Quarantine Group Proof

## Status: BLOCKED
## Blocker: rebar3 ct -h crashes with badarg in rebar_prv_common_test:merge_opts

---

## Evidence: Command Session 2025-12-15T15:24

### Command 1: pwd
```bash
cd /home/rustkas/aigroup/apps/otp/router && ( pwd; echo "exit=$?" ) 2>&1
```
**stdout:**
```
/home/rustkas/aigroup/apps/otp/router
```
**exit=0**

---

### Command 2: rebar3 --version
```bash
( rebar3 --version; echo "exit=$?" ) 2>&1
```
**stdout:**
```
rebar 3.25.1 on Erlang/OTP 27 Erts 15.2.7.1
```
**exit=0**

---

### Command 3: rebar3 ct -h
```bash
( rebar3 ct -h; echo "exit=$?" ) 2>&1
```
**stderr:**
```
===> Verifying dependencies...
===> Uncaught error in rebar_core. Run with DIAGNOSTIC=1 to see stacktrace or consult rebar3.crashdump
===> When submitting a bug report, please include the output of `rebar3 report "your command"`
```
**exit=1**

---

### Command 4: rebar3 ct -h with DIAGNOSTIC=1
```bash
cd /home/rustkas/aigroup/apps/otp/router && ( DIAGNOSTIC=1 rebar3 ct -h; echo "exit=$?" ) 2>&1
```
**stderr:**
```
===> Running provider: ct
===> Loading configuration from "/home/rustkas/aigroup/apps/otp/router/config/test.config"
===> Setting paths to [deps,plugins]
===> Uncaught error in rebar_core. Run with DIAGNOSTIC=1 to see stacktrace or consult rebar3.crashdump
===> Uncaught error: badarg
===> Stack trace to the error location:
[{erlang,element,[1,help],[{error_info,#{module => erl_erts_errors}}]},
 {lists,ukeysort,2,[{file,"lists.erl"},{line,1430}]},
 {rebar_prv_common_test,merge_opts,2,
                        [{file,"/home/runner/work/rebar3/rebar3/apps/rebar/src/rebar_prv_common_test.erl"},
                         {line,342}]},
 {rebar_prv_common_test,select_tests,4,
                        [{file,"/home/runner/work/rebar3/rebar3/apps/rebar/src/rebar_prv_common_test.erl"},
                         {line,322}]},
 {rebar_prv_common_test,do,1,
                        [{file,"/home/runner/work/rebar3/rebar3/apps/rebar/src/rebar_prv_common_test.erl"},
                         {line,44}]}]
===> When submitting a bug report, please include the output of `rebar3 report "your command"`
```
**exit=1**

---

## Root Cause
`erlang:element(1, help)` fails because `help` is an atom, not a tuple.
`rebar3 ct -h` parses `-h` as the atom `help`, passes it to `merge_opts/2`, which calls `lists:ukeysort/2` expecting proplists.
