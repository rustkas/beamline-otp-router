# Policy Schema Comments

## Field Conversion Notes

This document explains how JSON schema fields map to Proto `AdminPolicy` message.

### `weights` (Schema) → `providers` (Proto)

**Schema**: `weights` is a map `{"provider_id": 0.0-100.0}`

**Proto**: `providers` is an array `AdminProvider[]` with `id` and `weight` fields

**Conversion**: Router converts map to array:
- Map key → `AdminProvider.id`
- Map value → `AdminProvider.weight` (divide by 100 to convert 0-100 to 0.0-1.0)

**Example**:
```json
// Schema format (user input)
{"weights": {"openai": 70, "anthropic": 30}}

// Proto format (converted)
{
  "providers": [
    {"id": "openai", "weight": 0.7},
    {"id": "anthropic", "weight": 0.3}
  ]
}
```

### `sticky` (Schema) → `sticky` (Proto)

**Schema**: `sticky` is an object with `enabled`, `session_key`, `ttl`

**Proto**: `sticky` is a boolean only

**Conversion**: Router extracts `enabled` → `sticky` (bool), stores `session_key` and `ttl` in internal state

**Example**:
```json
// Schema format (user input)
{"sticky": {"enabled": true, "session_key": "user_id", "ttl": "10m"}}

// Proto format (converted)
{"sticky": true}

// Router internal state (not in Proto)
// ETS/Mnesia: sticky_sessions table with {session_key: "user_id", ttl: "10m"}
```

### `fallback` (Schema) → `rules` (Proto)

**Schema**: `fallback` is an object with `provider` field

**Proto**: `rules` is an array `AdminRule[]` with `match`, `prefer`, `fallback` fields

**Conversion**: Router converts fallback to `AdminRule` with `fallback` field set

**Example**:
```json
// Schema format (user input)
{"fallback": {"provider": "anthropic"}}

// Proto format (converted)
{
  "rules": [
    {"match": "", "prefer": [], "fallback": "anthropic"}
  ]
}
```

**Note**: See `docs/ROUTING_POLICY.md` "Policy DSL to Proto Conversion" section for complete conversion logic.

