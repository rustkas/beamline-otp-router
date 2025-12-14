# Test Environment Issues

## ✅ RESOLVED: rebar3 / Erlang OTP 27 Incompatibility

**Initial Issue Date**: 2025-12-11
**Resolution Date**: 2025-12-11T14:25+07:00

**Issue**: The installed rebar3 escript was compiled for an older Erlang version and was incompatible with OTP 27.

**Error**:
```
beam/beam_load.c(583): Error loading function rebar3:main/1:
  please re-compile this module with an Erlang/OTP 27 compiler
```

---

## Resolved Setup for OTP 27

The Router project now includes a **local rebar3 built for OTP 27** in the `bin/` directory.

### What Was Done
1. Cloned rebar3 from source: `git clone https://github.com/erlang/rebar3.git _rebar3_build`
2. Ran bootstrap: `cd _rebar3_build && ./bootstrap`
3. Copied to local bin: `cp _rebar3_build/rebar3 bin/rebar3`
4. Updated all scripts to use local rebar3 via `PATH="$(pwd)/bin:$PATH"`

### Verification
```bash
cd apps/otp/router
bin/rebar3 version
# Expected: rebar 0.0.0+build.X.refXXXXXXX on Erlang/OTP 27 Erts 15.X.X

rebar3 compile
# Expected: Compiles successfully
```

### Scripts Updated
All test scripts now automatically use the local rebar3:
- `scripts/ct-sanity.sh`
- `scripts/test_fast.sh`
- `scripts/ct-full.sh`
- Other scripts that use rebar3

### Current Environment
```bash
erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell
# Output: 27

bin/rebar3 version
# Output: rebar 0.0.0+build.1.ref83b9240 on Erlang/OTP 27 Erts 15.2.7.1
```

---

## Alternative Resolution Options (For Reference)

### Option 1: Build rebar3 from source (What we did)
```bash
git clone --depth 1 https://github.com/erlang/rebar3.git _rebar3_build
cd _rebar3_build
./bootstrap
# Copy to project bin/ or system /usr/local/bin/
```

### Option 2: Use asdf version manager
```bash
asdf plugin add rebar
asdf install rebar 3.24.0
asdf global rebar 3.24.0
```

### Option 3: Use nix
```bash
nix-shell -p rebar3
```

---

## Impact Summary

| Status | Task |
|--------|------|
| ✅ Resolved | rebar3 compilation |
| ✅ Resolved | CT test execution |
| ✅ Resolved | All test scripts |
