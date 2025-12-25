-module(router_perf_baseline_tests).

-include_lib("eunit/include/eunit.hrl").

validate_suite_passes_test() ->
    Baseline = baseline_fixture(),
    Observed = #{
        <<"latency_ms">> => #{<<"p50">> => 60, <<"p95">> => 130, <<"p99">> => 175},
        <<"throughput_rps">> => #{<<"avg">> => 22},
        <<"error_rate">> => #{<<"fraction">> => 0.0}
    },
    ?assertEqual(ok, router_perf_baseline:validate_suite(Baseline, <<"router_ext_load_baseline_SUITE">>, Observed)).

validate_suite_fails_on_p95_regression_test() ->
    Baseline = baseline_fixture(),
    Observed = #{
        <<"latency_ms">> => #{<<"p50">> => 60, <<"p95">> => 200, <<"p99">> => 175},
        <<"throughput_rps">> => #{<<"avg">> => 22},
        <<"error_rate">> => #{<<"fraction">> => 0.0}
    },
    {error, Errors} = router_perf_baseline:validate_suite(Baseline, <<"router_ext_load_baseline_SUITE">>, Observed),
    ?assert(lists:any(fun(E) -> is_threshold_failed(E, <<"latency_ms">>, <<"p95">>) end, Errors)).

validate_suite_fails_on_throughput_drop_test() ->
    Baseline = baseline_fixture(),
    Observed = #{
        <<"latency_ms">> => #{<<"p50">> => 60, <<"p95">> => 130, <<"p99">> => 175},
        <<"throughput_rps">> => #{<<"avg">> => 10},
        <<"error_rate">> => #{<<"fraction">> => 0.0}
    },
    {error, Errors} = router_perf_baseline:validate_suite(Baseline, <<"router_ext_load_baseline_SUITE">>, Observed),
    ?assert(lists:any(fun(E) -> is_threshold_failed(E, <<"throughput_rps">>, <<"avg">>) end, Errors)).

baseline_fixture() ->
    #{
        <<"suites">> => [
            #{
                <<"suite">> => <<"router_ext_load_baseline_SUITE">>,
                <<"metrics">> => #{
                    <<"latency_ms">> => #{<<"p50">> => 55, <<"p95">> => 120, <<"p99">> => 160},
                    <<"throughput_rps">> => #{<<"avg">> => 25},
                    <<"error_rate">> => #{<<"fraction">> => 0.0}
                },
                <<"tolerance">> => #{
                    <<"latency_ms">> => #{
                        <<"p95">> => #{<<"mode">> => <<"relative">>, <<"max_increase_fraction">> => 0.15},
                        <<"p99">> => #{<<"mode">> => <<"relative">>, <<"max_increase_fraction">> => 0.20},
                        <<"p50">> => #{<<"mode">> => <<"absolute">>, <<"max_increase_ms">> => 20}
                    },
                    <<"throughput_rps">> => #{
                        <<"avg">> => #{<<"mode">> => <<"relative">>, <<"max_drop_fraction">> => 0.15}
                    },
                    <<"error_rate">> => #{
                        <<"fraction">> => #{<<"mode">> => <<"absolute">>, <<"max_value">> => 0.001}
                    }
                }
            }
        ]
    }.

is_threshold_failed({threshold_failed, Group, Key, _}, Group, Key) ->
    true;
is_threshold_failed(_, _, _) ->
    false.
