%% Beamline Router Common Definitions
%% Version: 1.0

-ifndef(BEAMLINE_ROUTER_HRL).
-define(BEAMLINE_ROUTER_HRL, true).

%% Common types
-type tenant_id() :: binary() | string().
-type policy_id() :: binary() | string().
-type provider_id() :: binary() | string().
-type message_id() :: binary() | string().
-type trace_id() :: binary() | string().
-type session_key() :: binary() | string().

%% Policy structure
%% Quota record for tenant quota management
-record(quota, {
    tenant_id :: binary(),
    max_policies :: integer(),
    max_rules_per_policy :: integer(),
    max_providers_per_policy :: integer()
}).

-record(policy, {
    tenant_id :: tenant_id(),
    policy_id :: policy_id(),
    version = ~"1.0" :: binary(),
    defaults = #{} :: map(),
    escalate_on = [] :: list(),
    weights = #{} :: map(),  % Provider weights (0.0-1.0)
    fallback = undefined :: map() | undefined,  % Legacy: single fallback object
    fallbacks = [] :: list(),  % New: array of fallback rules with when/retry/to
    sticky = undefined :: map() | undefined,
    rate_limit = undefined :: map() | undefined,  % Rate limiting configuration
    circuit_breaker = undefined :: map() | undefined,  % Circuit breaker configuration
    pre = [] :: list(),  % Pre-processor extensions: [{id, mode, config}]
    validators = [] :: list(),  % Validator extensions: [{id, on_fail}]
    post = [] :: list(),  % Post-processor extensions: [{id, mode, config}]
    allow_rules = [] :: list(),  % Access control rules: [{subject, permission}]
    metadata = #{} :: map()
}).

%% Route request structure
-record(route_request, {
    message :: map(),
    policy_id :: policy_id() | undefined,
    context = #{} :: map()
}).

%% Route decision structure
-record(route_decision, {
    provider_id :: provider_id(),
    reason :: binary(),
    priority = 50 :: integer(),
    expected_latency_ms = 0 :: integer(),
    expected_cost = 0.0 :: float(),
    metadata = #{} :: map()
}).

%% Sticky session record
-record(sticky_session, {
    tenant_id :: tenant_id(),
    session_key :: session_key(),
    provider_id :: provider_id(),
    expires_at :: integer()
}).

%% Policy cache record
-record(policy_cache, {
    key :: {tenant_id(), policy_id()},
    policy :: #policy{},
    expires_at :: integer()
}).

%% Extension record (for Extension Registry)
-record(extension, {
    id :: binary(),
    type :: binary(),  % pre | validator | post | provider
    subject :: binary(),  % NATS subject
    timeout_ms :: integer(),
    retry :: integer(),
    enabled = true :: boolean(),
    config = #{} :: map(),
    metadata = #{} :: map()
}).

%% Alert rule record
-record(alert_rule, {
    id :: binary(),
    metric :: atom(),
    threshold :: number(),
    window_ms :: integer(),
    severity :: atom()  % warning | critical | info
}).

%% Alert record
-record(alert, {
    id :: binary(),
    rule_id :: binary(),
    severity :: atom(),
    message :: binary(),
    fired_at :: integer(),
    resolved_at :: integer() | undefined
}).

%% gRPC Status Codes
%% Note: All gRPC status codes are defined in grpcbox.hrl (included via -include_lib)
%% 
%% Authentication vs Authorization:
%% - UNAUTHENTICATED (16): client identity cannot be verified (missing/invalid key)
%% - PERMISSION_DENIED (7): client identity verified but lacks required permissions
%% 
%% Future status codes (for planning):
%% - RESOURCE_EXHAUSTED (8): rate limit, quotas exceeded

-endif.

