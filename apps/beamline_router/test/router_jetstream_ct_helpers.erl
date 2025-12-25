%% @doc Common Test helpers for JetStream recovery test suites
%%
%% This module provides shared CT callbacks and setup/teardown logic
%% used by both router_jetstream_extended_recovery_SUITE and router_jetstream_soak_SUITE.
%%
%% @test_category helpers, ct_callbacks
-module(router_jetstream_ct_helpers).
-include_lib("common_test/include/ct.hrl").

-export([
    init_suite/1,
    end_suite/1,
    init_case/2,
    end_case/2
]).

%% @doc Initialize test suite (common setup)
-spec init_suite(ct:config()) -> ct:config().
init_suite(Config) ->
    ct:pal("### init_suite: ensure recovery store and setup", []),

    case router_jetstream_recovery_store:ensure() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_suite: failed to ensure recovery store: ~p", [Reason])
    end,

    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    
    %% Avoid starting heavy/irrelevant components
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> 
        {ok, spawn(fun() -> receive after infinity -> ok end end)} 
    end),
    
    %% Start application to ensure telemetry is running
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        {error, {already_started, beamline_router}} ->
            Config;
        Error ->
            ct:fail("init_suite: failed to start beamline_router: ~p", [Error])
    end.

%% @doc Cleanup test suite (common teardown)
-spec end_suite(ct:config()) -> ct:config().
end_suite(Config) ->
    ct:pal("### end_suite: cleaning up", []),

    application:stop(beamline_router),
    catch meck:unload(router_rate_limiter),

    _ = router_jetstream_recovery_store:reset(),

    Config.

%% @doc Initialize test case (common per-test setup)
-spec init_case(atom(), ct:config()) -> ct:config().
init_case(_TestCase, Config) ->
    %% Reset store before each test
    case router_jetstream_recovery_store:reset() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_case: failed to reset recovery store: ~p", [Reason])
    end,
    Config.

%% @doc Cleanup test case (common per-test teardown)
-spec end_case(atom(), ct:config()) -> ct:config().
end_case(_TestCase, Config) ->
    Config.

