## Task: Enforce mock discipline

- Prevent `meck:new(..., [passthrough])` from silently allowing live `router_nats` calls.
- Forbid `gen_server:call(router_nats, ...)` outside the approved helper(s) that wrap router_nats interactions.
- Ensure a CI linter checks both conditions and fails when violations occur.
- The linter must demonstrably fail when we intentionally introduce a violation before it is fixed (for verification purposes).
