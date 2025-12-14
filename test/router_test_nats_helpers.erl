-module(router_test_nats_helpers).
-include_lib("common_test/include/ct.hrl").

-export([
    init_nats_config/0,
    wait_for_connection/1,
    restart_nats/0,
    ensure_disconnected/1
]).

init_nats_config() ->
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_reconnect_attempts, 5),
    ok = application:set_env(beamline_router, nats_reconnect_delay_ms, 500),
    ok = application:set_env(beamline_router, nats_max_reconnect_delay_ms, 2000),
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    ok = application:unset_env(beamline_router, nats_url).

wait_for_connection(Timeout) ->
    test_helpers:wait_for_condition(fun() ->
        case router_nats:get_connection_status() of
            {ok, Map} when is_map(Map) ->
                maps:get(state, Map) =:= connected;
            _ -> false
        end
    end, Timeout).

restart_nats() ->
    supervisor:terminate_child(beamline_router_sup, router_nats),
    timer:sleep(500),
    {ok, _} = supervisor:restart_child(beamline_router_sup, router_nats),
    timer:sleep(500).

ensure_disconnected(Timeout) ->
    test_helpers:wait_for_condition(fun() ->
        case router_nats:get_connection_status() of
            {ok, Map} when is_map(Map) ->
                State = maps:get(state, Map),
                State =:= disconnected orelse State =:= reconnecting orelse State =:= connecting;
            _ -> false
        end
    end, Timeout).
