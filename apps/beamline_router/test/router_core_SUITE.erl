-module(router_core_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([groups/0]).
-export([route_missing_tenant_emits_error_metric/1]).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [{core_route_tests, [sequence], [route_missing_tenant_emits_error_metric]}].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

route_missing_tenant_emits_error_metric(_Config) ->
    HandlerId = {?MODULE, core_missing_tenant},
    try
        _ = application:load(beamline_router),
        ok = application:set_env(beamline_router, grpc_port, 0),
        ok = application:set_env(beamline_router, grpc_enabled, false),
        ok = application:set_env(beamline_router, telemetry_enabled, true),
        case application:ensure_all_started(beamline_router) of
            {ok, _} -> ok;
            Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
        end,
        telemetry:attach(HandlerId,
                         [router_core, errors_total],
                         fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
                             erlang:put(core_error_metadata, Metadata)
                         end,
                         #{}),
        RouteReq = #route_request{
            message = #{~"message_id" => ~"m-1"},
            policy_id = ~"default",
            context = #{}
        },
        _ = router_core:route(RouteReq, #{}),
        test_helpers:wait_for_condition(fun() ->
                                            case erlang:get(core_error_metadata) of
                                                undefined -> false;
                                                _ -> true
                                            end
                                        end, 3000),
        case erlang:get(core_error_metadata) of
            undefined ->
                ct:fail("No errors_total telemetry captured for missing tenant_id");
            Metadata ->
                Err = maps:get(error, Metadata, undefined),
                true = Err =/= undefined,
                missing_tenant_id = Err,
                ok
        end,
        telemetry:detach(HandlerId),
        ok
    after
        _ = try telemetry:detach(HandlerId) catch _:_ -> ok end,
        _ = application:stop(beamline_router)
    end.
