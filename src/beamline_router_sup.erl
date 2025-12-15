%% @doc Beamline Router Supervisor
%% 
%% Main supervisor for router core components.
%% 
%% Process Tree Structure:
%% ```
%% beamline_router_sup (supervisor)
%%   ├── router_telemetry_handler (worker) - Telemetry event handler
%%   ├── router_nats (worker) - NATS publish/subscribe interface
%%   ├── router_policy_store (worker) - ETS policy cache with Heir
%%   ├── router_extension_registry (worker) - ETS-based extension metadata cache
%%   ├── router_rate_limit_store (worker) - Token bucket for per-policy rate limiting
%%   ├── router_rbac (worker) - Role-Based Access Control
%%   ├── router_rate_limiter (worker) - Per-tenant/user rate limiting
%%   ├── router_circuit_breaker (worker) - Per-provider circuit breaker state machine
%%   ├── router_grpc_sup (supervisor) - gRPC server supervisor
%%   ├── router_decide_consumer (worker) - DecideRequest via JetStream
%%   ├── router_result_consumer (worker) - ExecResult from CAF
%%   ├── router_admin_nats_subscriber (worker) - Admin endpoints via NATS
%%   ├── [optional] router_ack_consumer (worker) - ACK consumer (if ack_enabled=true)
%%   ├── [optional] router_idempotency (worker) - Idempotency layer (if idempotency_enabled=true)
%%   └── [optional] router_metrics_http (worker) - Metrics HTTP endpoint (if metrics_export_enabled=true)
%% ```
%% 
%% Configuration Dependencies:
%% - cp2_plus_allowed: Enables CP2+ features (ack consumer, idempotency)
%% - ack_enabled: Enables ACK consumer (requires cp2_plus_allowed)
%% - idempotency_enabled: Enables idempotency layer (requires cp2_plus_allowed)
%% - telemetry_enabled: Enables telemetry handler
%% - metrics_export_enabled: Enables metrics HTTP endpoint (requires telemetry_enabled)
%% 
%% Supervisor Strategy:
%% - one_for_one: Each child is independent
%% - Max restarts: 5 per 10 seconds
%% 
%% @see router_grpc_sup for gRPC server tree
-module(beamline_router_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("beamline_router.hrl").

%% @doc Start the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initialize supervisor tree
init([]) ->
    Cp2PlusAllowed = application:get_env(beamline_router, cp2_plus_allowed, false),
    AckEnabledEnv = application:get_env(beamline_router, ack_enabled, false),
    AckEnabled = AckEnabledEnv andalso Cp2PlusAllowed,

    %% Base child specifications
    BaseChildren = [
        %% Telemetry handler
        {router_telemetry_handler,
         {router_telemetry_handler, start_link, []},
         permanent, 5000, worker, [router_telemetry_handler]},

        %% NATS core (publish/subscribe interface)
        {router_nats,
         {router_nats, start_link, []},
         permanent, 5000, worker, [router_nats]},

        %% Policy Store (ETS with Heir for fault tolerance)
        {router_policy_store,
         {router_policy_store, start_link, []},
         permanent, 5000, worker, [router_policy_store]},

        %% Extension Registry (ETS-based cache for extension metadata)
        {router_extension_registry,
         {router_extension_registry, start_link, []},
         permanent, 5000, worker, [router_extension_registry]},

        %% Rate Limit Store (token bucket for per-policy rate limiting)
        {router_rate_limit_store,
         {router_rate_limit_store, start_link, []},
         permanent, 5000, worker, [router_rate_limit_store]},

        %% RBAC (Role-Based Access Control)
        {router_rbac,
         {router_rbac, start_link, []},
         permanent, 5000, worker, [router_rbac]},

        %% Rate Limiter (per-tenant/user rate limiting)
        {router_rate_limiter,
         {router_rate_limiter, start_link, []},
         permanent, 5000, worker, [router_rate_limiter]},

        %% Circuit Breaker (per-provider circuit breaker state machine)
        {router_circuit_breaker,
         {router_circuit_breaker, start_link, []},
         permanent, 5000, worker, [router_circuit_breaker]},

        %% JetStream supervisor (manages JetStream consumer lifecycle)
        {router_jetstream_sup,
         {router_jetstream_sup, start_link, []},
         permanent, 5000, supervisor, [router_jetstream_sup]},

        %% Decide consumer (for DecideRequest via JetStream)
        %% CP2+: JetStream durable consumer with explicit ack policy
        {router_decide_consumer,
         {router_decide_consumer, start_link, []},
         permanent, 5000, worker, [router_decide_consumer]},
        
        %% Result consumer (for ExecResult from CAF)
        %% Always start - required for CP1-LC
        {router_result_consumer,
         {router_result_consumer, start_link, []},
         permanent, 5000, worker, [router_result_consumer]},
        
        %% Admin NATS subscriber (for admin endpoints via NATS request-reply)
        %% CP2+: Admin endpoints for extensions health, circuit breakers, dry-run, complexity
        {router_admin_nats_subscriber,
         {router_admin_nats_subscriber, start_link, []},
         permanent, 5000, worker, [router_admin_nats_subscriber]}
        
        %% Idempotency layer optional, start only if enabled
        %% Moved to optional children below
    ],

    %% gRPC server (optional, only if grpc_enabled=true)
    GrpcEnabled = application:get_env(beamline_router, grpc_enabled, true),
    GrpcChildren = case GrpcEnabled of
        true ->
            %% Try to start router_grpc_sup if it exists
            %% If module doesn't exist, supervisor will handle it gracefully
            [{router_grpc_sup,
              {router_grpc_sup, start_link, []},
              permanent, 5000, supervisor, [router_grpc_sup]}];
        false ->
            []
    end,

    %% Optional children
    AckChildren = case AckEnabled of
        true ->
            [{router_ack_consumer,
              {router_ack_consumer, start_link, []},
              permanent, 5000, worker, [router_ack_consumer]}];
        false ->
            []
    end,

    IdemEnabledEnv = application:get_env(beamline_router, idempotency_enabled, false),
    IdemEnabled = IdemEnabledEnv andalso Cp2PlusAllowed,
    IdemChildren = case IdemEnabled of
        true ->
            [{router_idempotency,
              {router_idempotency, start_link, []},
              permanent, 5000, worker, [router_idempotency]}];
        false ->
            []
    end,

    %% Sticky session store (optional, enabled by env)
    StickyEnabled = application:get_env(beamline_router, sticky_store_enabled, false),
    StickyChildren = case StickyEnabled of
        true ->
            [{router_sticky_store,
              {router_sticky_store, start_link, []},
              permanent, 5000, worker, [router_sticky_store]}];
        false ->
            []
    end,

    %% Metrics HTTP endpoint (/metrics) guarded
    %% Start only when telemetry_enabled is true and metrics_export_enabled is true
    %% to avoid port conflicts in tests and unnecessary HTTP server startup.
    MetricsHttpChild = case {application:get_env(beamline_router, telemetry_enabled, true),
                             application:get_env(beamline_router, metrics_export_enabled, false)} of
        {true, true} ->
            {router_metrics_http,
             {router_metrics_http, start, []},
             permanent, 5000, worker, [router_metrics_http]};
        _ ->
            undefined
    end,

    %% Filter out undefined children (metrics HTTP may be disabled)
    %% Filter out undefined children (metrics HTTP may be disabled)
    AllChildren = BaseChildren ++ GrpcChildren ++ AckChildren ++ IdemChildren ++ StickyChildren,
    Children = case MetricsHttpChild of
        undefined -> AllChildren;
        _ -> AllChildren ++ [MetricsHttpChild]
    end,

    {ok, {{one_for_one, 5, 10}, Children}}.
