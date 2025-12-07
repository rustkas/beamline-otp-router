%% @doc Performance Tests for Metrics Labels
%%
%% Tests performance impact of label extraction and metric emission.
%% Verifies:
%% - Label extraction doesn't add significant overhead
%% - Metric emission with labels is performant
%% - ETS table size remains manageable
%%
%% @test_category performance, metrics, labels
-module(router_metrics_labels_performance_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile({nowarn_unused_function, [all/0, init_per_suite/1, end_per_suite/1,
                                    init_per_testcase/2, end_per_testcase/2]}).

all() ->
    [
        test_label_extraction_performance,
        test_metric_emission_performance,
        test_ets_table_size_growth
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    _ = application:set_env(beamline_router, test_mode, true),
    ok = router_metrics:ensure(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ets:delete_all_objects(router_metrics),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Performance Tests
%% ============================================================================

test_label_extraction_performance(_Config) ->
    %% Test performance of label extraction functions
    Subject = <<"beamline.router.v1.decide">>,
    Msg = #{
        id => <<"msg-123">>,
        subject => Subject,
        headers => #{
            <<"tenant_id">> => <<"tenant-456">>,
            <<"request_id">> => <<"req-789">>
        }
    },
    
    %% Measure extraction time
    Iterations = 10000,
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(_) ->
        _ = router_jetstream:extract_assignment_id(Subject),
        _ = router_jetstream:extract_tenant_id(Msg),
        _ = router_jetstream:extract_request_id(Msg)
    end, lists:seq(1, Iterations)),
    
    EndTime = erlang:monotonic_time(microsecond),
    Duration = EndTime - StartTime,
    AvgTime = Duration / (Iterations * 3),  %% 3 extractions per iteration
    
    %% Average time per extraction should be < 10 microseconds
    true = (AvgTime < 10),
    ct:log("Average extraction time: ~p microseconds", [AvgTime]),
    ok.

test_metric_emission_performance(_Config) ->
    %% Test performance of metric emission with labels
    Iterations = 10000,
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(I) ->
        router_metrics:emit_metric(router_dlq_total, #{count => 1}, #{
            assignment_id => <<"decide">>,
            reason => <<"maxdeliver_exhausted">>,
            tenant_id => <<"tenant-", (integer_to_binary(I))/binary>>,
            source => <<"maxdeliver_exhausted">>,
            msg_id => <<"msg-", (integer_to_binary(I))/binary>>,
            request_id => <<"req-", (integer_to_binary(I))/binary>>
        })
    end, lists:seq(1, Iterations)),
    
    EndTime = erlang:monotonic_time(microsecond),
    Duration = EndTime - StartTime,
    AvgTime = Duration / Iterations,
    
    %% Average time per emission should be < 50 microseconds
    true = (AvgTime < 50),
    ct:log("Average emission time: ~p microseconds", [AvgTime]),
    ok.

test_ets_table_size_growth(_Config) ->
    %% Test that ETS table size doesn't grow unbounded
    InitialSize = ets:info(router_metrics, size),
    
    %% Emit metrics with many different label combinations
    Iterations = 1000,
    lists:foreach(fun(I) ->
        router_metrics:emit_metric(router_dlq_total, #{count => 1}, #{
            assignment_id => <<"decide">>,
            reason => <<"maxdeliver_exhausted">>,
            tenant_id => <<"tenant-", (integer_to_binary(I))/binary>>,
            source => <<"maxdeliver_exhausted">>,
            msg_id => <<"msg-", (integer_to_binary(I))/binary>>,
            request_id => <<"req-", (integer_to_binary(I))/binary>>
        })
    end, lists:seq(1, Iterations)),
    
    FinalSize = ets:info(router_metrics, size),
    Growth = FinalSize - InitialSize,
    
    %% Growth should be approximately equal to number of unique label combinations
    %% (each unique combination creates one ETS entry)
    true = (Growth =< Iterations),
    ct:log("ETS table size: ~p -> ~p (growth: ~p)", [InitialSize, FinalSize, Growth]),
    ok.

