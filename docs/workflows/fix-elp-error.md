---
description: Fix ELP "Unknown application" error
---

# Workflow: Fix ELP "Unknown application" Error

This workflow fixes the "Unknown application" (L0003) error that ELP shows for test files.

## Steps

1. Generate correct `build_info.json`:
```bash
./scripts/generate_build_info.sh
```

2. Reload VS Code window:
   - Press `Ctrl+Shift+P` 
   - Type "Developer: Reload Window"
   - Press Enter

3. Verify the fix:
   - Open any test file (e.g., `test/router_gateway_integration_SUITE.erl`)
   - Check that the "Unknown application" error is gone
   - Look at the "Problems" panel in VS Code

## Alternative: Restart ELP server

If reloading window doesn't help:

```bash
# Kill ELP process
pkill -f "elp server"

# VS Code will automatically restart it
```

## Make it permanent

Add to your Makefile:
```makefile
compile:
	rebar3 compile
	./scripts/generate_build_info.sh
```

## Why this works

ELP was using `_build/test/lib/beamline_router` as the app directory instead of the project root `.`. This caused it to not recognize test files as part of the application.

The script ensures `build_info.json` always points to the correct directory.
