%% @doc Common Test bootstrap helper for router suites.
%%
%% Goal: keep suite init/end small and consistent while preserving behavior.
%% This module is intentionally conservative: everything is opt-in via options.
%%
%% Options (init_per_suite/2):
%%   - app => atom() (default: beamline_router)
%%   - reset_app => boolean() (default: false) stop+unload before load
%%   - infra_mode => auto | mock | docker (default: auto)
%%   - common_env => boolean() (default: true) sets grpc_port=0, grpc_enabled=false unless overridden
%%   - app_env => map() (default: #{}) applied via application:set_env(App, K, V)
%%   - ensure_apps => [atom()] (default: []) ensure_all_started/1 before starting router app
%%   - start => router_suite | ensure_all_started | router_app | none (default: router_suite)
%%   - wait_for_app_start => [{atom(), non_neg_integer()}] (default: [])
%%
%% Options (end_per_suite/2):
%%   - app => atom() (default: beamline_router)
%%   - start => (same as above, used when stop=auto)
%%   - stop => auto | router_suite | stop_app | none (default: auto)
%%   - cleanup_mocks => boolean() (default: false)
%%
%% Options (init_per_testcase/3, end_per_testcase/3):
%%   - clear_metrics => boolean() (default: false) router_metrics_test_helper:clear_all_metrics/0
%%   - metrics_test_helper => setup | teardown | none (default: none)
%%   - clear_faults => boolean() (default: false) router_nats_fault_injection:clear_all_faults/0
%%   - ensure_router_nats_alive => boolean() (default: false) router_test_utils:ensure_router_nats_alive/0
%%   - cleanup_mocks => boolean() (default: false) router_mock_helpers:cleanup_and_verify/0
%%
%% @test_category test_infrastructure, bootstrap
%%
%% NOTE: Keep this helper free of per-suite logic; suites should pass opts.
%%
%% @end
%
% WARNING: Do not add heavy logging here (ct:pal) - it affects ct-full timing.
%
-module(router_test_bootstrap).

-include_lib("common_test/include/ct.hrl").

-export([
    init_per_suite/2,
    end_per_suite/2,
    init_per_testcase/3,
    end_per_testcase/3
]).

-define(INFRA_MODE_CACHE_KEY, router_test_bootstrap_infra_mode).

%% -------------------------------------------------------------------
%% Suite lifecycle
%% -------------------------------------------------------------------

-spec init_per_suite(list(), map()) -> list() | {skip, string()}.
init_per_suite(Config, Opts0) ->
    Opts = normalize_suite_opts(Opts0),
    App = maps:get(app, Opts),

    DetectedInfraMode = detect_infra_mode_cached(),
    RequestedInfraMode = maps:get(infra_mode, Opts),
    EffectiveInfraMode = resolve_infra_mode(RequestedInfraMode, DetectedInfraMode),
    case maybe_skip_for_infra_mode(RequestedInfraMode, DetectedInfraMode) of
        ok ->
            ok = maybe_reset_app(App, maps:get(reset_app, Opts)),
            ok = ensure_loaded(App),
            ok = apply_app_env(App, Opts),
            ok = ensure_apps_started(maps:get(ensure_apps, Opts)),
            ok = start_router(App, maps:get(start, Opts)),
            ok = wait_for_app_start(maps:get(wait_for_app_start, Opts)),
            [{infra_mode, EffectiveInfraMode} | Config];
        {skip, _} = Skip ->
            Skip
    end.

-spec end_per_suite(list(), map()) -> list().
end_per_suite(Config, Opts0) ->
    Opts = normalize_end_suite_opts(Opts0),
    App = maps:get(app, Opts),
    Start = maps:get(start, Opts),
    Stop = resolve_stop_strategy(maps:get(stop, Opts), Start),
    ok = stop_router(App, Stop),
    ok = maybe_cleanup_mocks(maps:get(cleanup_mocks, Opts)),
    Config.

%% -------------------------------------------------------------------
%% Testcase lifecycle
%% -------------------------------------------------------------------

-spec init_per_testcase(atom(), list(), map()) -> list().
init_per_testcase(_TestCase, Config, Opts0) ->
    Opts = normalize_testcase_opts(Opts0),
    ok = maybe_clear_faults(maps:get(clear_faults, Opts)),
    ok = maybe_ensure_router_nats_alive(maps:get(ensure_router_nats_alive, Opts)),
    ok = maybe_metrics_test_helper_setup(maps:get(metrics_test_helper, Opts)),
    ok = maybe_clear_metrics(maps:get(clear_metrics, Opts)),
    Config.

-spec end_per_testcase(atom(), list(), map()) -> list().
end_per_testcase(_TestCase, Config, Opts0) ->
    Opts = normalize_testcase_opts(Opts0),
    ok = maybe_clear_faults(maps:get(clear_faults, Opts)),
    ok = maybe_metrics_test_helper_teardown(maps:get(metrics_test_helper, Opts)),
    ok = maybe_cleanup_mocks(maps:get(cleanup_mocks, Opts)),
    Config.

%% -------------------------------------------------------------------
%% Internals
%% -------------------------------------------------------------------

normalize_suite_opts(Opts) when is_map(Opts) ->
    Defaults = #{
        app => beamline_router,
        reset_app => false,
        infra_mode => auto,
        common_env => true,
        app_env => #{},
        ensure_apps => [],
        start => router_suite,
        wait_for_app_start => []
    },
    maps:merge(Defaults, Opts);
normalize_suite_opts(_) ->
    normalize_suite_opts(#{}).

normalize_end_suite_opts(Opts) when is_map(Opts) ->
    Defaults = #{
        app => beamline_router,
        start => router_suite,
        stop => auto,
        cleanup_mocks => false
    },
    maps:merge(Defaults, Opts);
normalize_end_suite_opts(_) ->
    normalize_end_suite_opts(#{}).

normalize_testcase_opts(Opts) when is_map(Opts) ->
    Defaults = #{
        clear_metrics => false,
        metrics_test_helper => none,
        clear_faults => false,
        ensure_router_nats_alive => false,
        cleanup_mocks => false
    },
    maps:merge(Defaults, Opts);
normalize_testcase_opts(_) ->
    normalize_testcase_opts(#{}).

detect_infra_mode_cached() ->
    case persistent_term:get(?INFRA_MODE_CACHE_KEY, undefined) of
        undefined ->
            Mode = router_testops_helper:get_chaos_mode(),
            persistent_term:put(?INFRA_MODE_CACHE_KEY, Mode),
            Mode;
        Mode ->
            Mode
    end.

resolve_infra_mode(auto, Detected) -> Detected;
resolve_infra_mode(mock, _Detected) -> mock;
resolve_infra_mode(docker, _Detected) -> docker;
resolve_infra_mode(Other, _Detected) -> Other.

maybe_skip_for_infra_mode(docker, Detected) when Detected =/= docker ->
    {skip, "Test requires Docker mode (CHAOS_MODE=docker or Docker available)"};
maybe_skip_for_infra_mode(_, _) ->
    ok.

maybe_reset_app(_App, false) ->
    ok;
maybe_reset_app(App, true) ->
    _ = application:stop(App),
    _ = application:unload(App),
    ok.

ensure_loaded(App) ->
    _ = application:load(App),
    ok.

apply_app_env(App, Opts) ->
    DefaultEnv =
        case maps:get(common_env, Opts) of
            true -> #{grpc_port => 0, grpc_enabled => false};
            false -> #{}
        end,
    Env = maps:merge(DefaultEnv, maps:get(app_env, Opts, #{})),
    lists:foreach(
        fun({Key, Value}) -> ok = application:set_env(App, Key, Value) end,
        maps:to_list(Env)
    ),
    ok.

ensure_apps_started(Apps) when is_list(Apps) ->
    lists:foreach(
        fun(App) ->
            case application:ensure_all_started(App) of
                {ok, _} -> ok;
                {error, _} = Error -> ct:fail({cannot_start_dependency, App, Error})
            end
        end,
        Apps
    ),
    ok.

start_router(_App, none) ->
    ok;
start_router(App, ensure_all_started) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        Error -> ct:fail({cannot_start_app, App, Error})
    end;
start_router(_App, router_app) ->
    ok = router_test_utils:start_router_app();
start_router(_App, router_suite) ->
    ok = router_suite_helpers:start_router_suite();
start_router(_App, Other) ->
    ct:fail({unknown_start_strategy, Other}).

resolve_stop_strategy(auto, router_suite) -> router_suite;
resolve_stop_strategy(auto, none) -> none;
resolve_stop_strategy(auto, _Other) -> stop_app;
resolve_stop_strategy(Stop, _Start) -> Stop.

stop_router(_App, none) ->
    ok;
stop_router(App, stop_app) ->
    _ = application:stop(App),
    ok;
stop_router(_App, router_suite) ->
    ok = router_suite_helpers:stop_router_suite();
stop_router(_App, Other) ->
    ct:fail({unknown_stop_strategy, Other}).

wait_for_app_start(Waits) when is_list(Waits) ->
    lists:foreach(
        fun({Name, TimeoutMs}) ->
            _ = test_helpers:wait_for_app_start(Name, TimeoutMs),
            ok
        end,
        Waits
    ),
    ok.

maybe_clear_metrics(false) -> ok;
maybe_clear_metrics(true) ->
    ok = router_metrics_test_helper:clear_all_metrics().

maybe_metrics_test_helper_setup(setup) ->
    ok = router_metrics_test_helper:setup();
maybe_metrics_test_helper_setup(_) ->
    ok.

maybe_metrics_test_helper_teardown(teardown) ->
    ok = router_metrics_test_helper:teardown();
maybe_metrics_test_helper_teardown(_) ->
    ok.

maybe_clear_faults(false) -> ok;
maybe_clear_faults(true) ->
    catch router_nats_fault_injection:clear_all_faults(),
    ok.

maybe_ensure_router_nats_alive(false) -> ok;
maybe_ensure_router_nats_alive(true) ->
    ok = router_test_utils:ensure_router_nats_alive().

maybe_cleanup_mocks(false) -> ok;
maybe_cleanup_mocks(true) ->
    ok = router_mock_helpers:cleanup_and_verify().

