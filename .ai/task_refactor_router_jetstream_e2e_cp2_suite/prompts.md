# Prompts

## Mock Discipline
- Use `router_mock_helpers:ensure_mock/2` instead of `meck:new`
- Use `router_mock_helpers:set_subscribe_response/3` for subscribe mocks
- Use `router_mock_helpers:clear_calls/0` instead of manual ETS tracking
- Use `router_mock_helpers:get_call_count/1` for call verification

## Rollback Policy
- Max 3 fix attempts
- On 3rd failure → rollback + quarantine
- Never leave repo in non-compiling state
