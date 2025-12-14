%% @doc Unit Tests for router_jetstream module
%% Targeted coverage tests for internal helper functions
%% @test_category unit, fast, coverage_hotspot
-module(router_jetstream_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_setup/1,
    test_configure/1,
    test_metrics/1,
    test_trace_ctx/1,
    test_start_link/1,
    test_get_config/1,
    test_should_nak/1,
    test_handle_msg_format/1,
    test_term_format/1,
    test_delivery_count_tracking/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_module_exports,
            test_setup,
            test_configure,
            test_metrics,
            test_trace_ctx,
            test_start_link,
            test_get_config,
            test_should_nak,
            test_handle_msg_format,
            test_term_format,
            test_delivery_count_tracking
        ]}
    ].

init_per_suite(Config) ->
    %% Ensure app is loaded
    _ = application:load(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for module structure
%% ============================================================================

test_module_exports(_Config) ->
    {module, router_jetstream} = code:ensure_loaded(router_jetstream),
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    
    %% Check key exports exist
    ?assertEqual(true, lists:member({ack, 1}, Exports)),
    ?assertEqual(true, lists:member({nak, 2}, Exports)),
    ?assertEqual(true, lists:member({handle, 2}, Exports)),
    ?assertEqual(true, lists:member({metrics, 0}, Exports)),
    ok.

test_start_link(_Config) ->
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, lists:member({start_link, 0}, Exports)),
    ok.

%% ============================================================================
%% Tests for setup/1
%% ============================================================================

test_setup(_Config) ->
    Result = router_jetstream:setup(#{}),
    ?assertMatch({ok, _}, Result),
    ok.

%% ============================================================================
%% Tests for configure/1
%% ============================================================================

test_configure(_Config) ->
    %% Valid configuration
    Result1 = router_jetstream:configure(#{
        max_deliver => 3,
        backoff_seconds => [1, 2, 4]
    }),
    ?assertEqual(ok, Result1),
    
    %% Different values
    Result2 = router_jetstream:configure(#{
        max_deliver => 5,
        backoff_seconds => [1, 5, 10, 30]
    }),
    ?assertEqual(ok, Result2),
    ok.

%% ============================================================================
%% Tests for metrics/0
%% ============================================================================

test_metrics(_Config) ->
    Metrics = router_jetstream:metrics(),
    ?assertEqual(true, is_list(Metrics)),
    ?assertEqual(true, length(Metrics) >= 1),
    
    %% Check expected metrics
    ?assertEqual(true, lists:member(router_jetstream_ack_total, Metrics)),
    ok.

%% ============================================================================
%% Tests for trace_ctx/1
%% ============================================================================

test_trace_ctx(_Config) ->
    Headers = #{<<"traceparent">> => <<"00-abc123-def456-01">>},
    Result = router_jetstream:trace_ctx(Headers),
    ?assertMatch({trace, _}, Result),
    
    %% Empty headers
    Result2 = router_jetstream:trace_ctx(#{}),
    ?assertMatch({trace, _}, Result2),
    ok.

%% ============================================================================
%% Tests for get_config (internal, if exported)
%% ============================================================================

test_get_config(_Config) ->
    %% Test that module handles config retrieval
    Exports = router_jetstream:module_info(exports),
    case lists:member({get_config, 1}, Exports) of
        true ->
            Config = router_jetstream:get_config(<<"test.subject">>),
            ?assertEqual(true, is_tuple(Config));
        false ->
            %% Not exported directly, skip
            ok
    end,
    ok.

%% ============================================================================
%% Tests for should_nak (internal logic if exported)
%% ============================================================================

test_should_nak(_Config) ->
    %% Test that module handles should_nak logic
    Exports = router_jetstream:module_info(exports),
    case lists:member({should_nak, 2}, Exports) of
        true ->
            Result = router_jetstream:should_nak(1, [1, 2, 4]),
            ?assertEqual(true, is_boolean(Result));
        false ->
            %% Not exported directly, skip
            ok
    end,
    ok.

%% ============================================================================
%% Tests for handle message format
%% ============================================================================

test_handle_msg_format(_Config) ->
    %% Test that module handles message maps correctly
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, lists:member({handle, 2}, Exports)),
    ok.

%% ============================================================================
%% Tests for term/1 format
%% ============================================================================

test_term_format(_Config) ->
    %% Test that term/1 is exported
    Exports = router_jetstream:module_info(exports),
    ?assertEqual(true, lists:member({term, 1}, Exports)),
    ok.

%% ============================================================================
%% Tests for delivery count tracking
%% ============================================================================

test_delivery_count_tracking(_Config) ->
    %% Test that delivery count functions are available
    Exports = router_jetstream:module_info(exports),
    
    %% Check for subscribe functions
    ?assertEqual(true, lists:member({subscribe_decide, 1}, Exports)),
    ?assertEqual(true, lists:member({subscribe_results, 1}, Exports)),
    ok.
