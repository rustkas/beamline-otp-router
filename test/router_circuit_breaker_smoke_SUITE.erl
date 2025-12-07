%% @doc Minimal smoke test for router_circuit_breaker startup in CT environment
%% Purpose: Isolate the problem - verify if CB starts in minimal CT setup
-module(router_circuit_breaker_smoke_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, smoke_starts_and_is_alive/1]).
-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    smoke_starts_and_is_alive/1
]}).

all() -> [smoke_starts_and_is_alive].

init_per_suite(Config) ->
    %% Minimal setup - just start application
    _ = application:load(beamline_router),
    
    %% Set minimal test environment
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, metrics_export_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, Started} ->
            ct:pal("beamline_router started, apps: ~p", [Started]),
            %% Wait a bit for supervisor to start children
            timer:sleep(500),
            Config;
        Error ->
            ct:fail({cannot_start_beamline_router, Error})
    end.

end_per_suite(_Config) ->
    _ = application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

smoke_starts_and_is_alive(_Config) ->
    %% Check supervisor
    case whereis(beamline_router_sup) of
        undefined ->
            ct:fail(beamline_router_supervisor_not_found);
        SupPid ->
            ct:pal("Supervisor PID: ~p", [SupPid])
    end,
    
    %% Get supervisor children
    Children = supervisor:which_children(beamline_router_sup),
    ct:pal("Supervisor children count: ~p", [length(Children)]),
    ct:pal("Supervisor children: ~p", [Children]),
    
    %% Check if CB child is in the list
    CBChild = lists:keyfind(router_circuit_breaker, 1, Children),
    case CBChild of
        false ->
            ct:fail({circuit_breaker_child_not_in_supervisor, children, Children});
        {router_circuit_breaker, undefined, worker, [router_circuit_breaker]} ->
            ct:fail({circuit_breaker_child_failed_to_start, child_spec, CBChild});
        {router_circuit_breaker, CBChildPid, worker, [router_circuit_breaker]} when is_pid(CBChildPid) ->
            ct:pal("Circuit breaker child PID: ~p", [CBChildPid]),
            case is_process_alive(CBChildPid) of
                true -> ct:pal("Circuit breaker child is alive");
                false -> ct:fail({circuit_breaker_child_not_alive, pid, CBChildPid})
            end;
        CBChildOther ->
            ct:fail({unexpected_circuit_breaker_child_format, CBChildOther})
    end,
    
    %% Check process registry
    CBProc = whereis(router_circuit_breaker),
    ct:pal("router_circuit_breaker whereis: ~p", [CBProc]),
    
    case CBProc of
        undefined ->
            ct:fail(router_circuit_breaker_not_registered);
        CBProcPid when is_pid(CBProcPid) ->
            case is_process_alive(CBProcPid) of
                true ->
                    ct:pal("router_circuit_breaker is alive and registered"),
                    ok;
                false ->
                    ct:fail({router_circuit_breaker_registered_but_not_alive, pid, CBProcPid})
            end
    end,
    
    %% Try a simple call to verify process is functional
    case catch gen_server:call(router_circuit_breaker, {get_state, <<"test">>, <<"test">>}, 1000) of
        {ok, _State} ->
            ct:pal("Circuit breaker responded to call");
        {error, not_found} ->
            ct:pal("Circuit breaker responded (state not found, expected)");
        {'EXIT', {noproc, _}} ->
            ct:fail({circuit_breaker_call_failed_noproc});
        {'EXIT', Reason} ->
            ct:fail({circuit_breaker_call_failed, Reason});
        Other ->
            ct:pal("Circuit breaker call returned: ~p", [Other])
    end,
    
    %% Check code paths
    SupCodePath = code:which(beamline_router_sup),
    CBCodePath = code:which(router_circuit_breaker),
    ct:pal("beamline_router_sup code path: ~p", [SupCodePath]),
    ct:pal("router_circuit_breaker code path: ~p", [CBCodePath]),
    
    ok.

