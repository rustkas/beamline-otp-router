#!/usr/bin/env escript
%%! -noshell
-mode(compile).

main(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} ->
            case run(Opts) of
                ok -> halt(0);
                {error, Reason} ->
                    io:format(standard_error, "ERROR: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, help} ->
            usage(),
            halt(0);
        {error, Reason} ->
            io:format(standard_error, "ERROR: ~p~n", [Reason]),
            usage(),
            halt(2)
    end.

usage() ->
    io:format(
        "Usage:\n"
        "  scripts/perf_baseline_capture.escript --in <observations.json> --out <baseline.json> \\\n"
        "    [--env-name <name>] [--command <string>] [--warmup <seconds>] [--sustained <seconds>] [--sample-min <n>]\n"
    ).

run(Opts) ->
    InPath = maps:get(in, Opts),
    OutPath = maps:get(out, Opts),

    ObsBin = read_file(InPath),
    Obs = jsx:decode(ObsBin, [return_maps]),

    Suites0 = maps:get(<<"suites">>, Obs, []),
    Suites1 = [add_default_tolerance(S) || S <- Suites0],

    NowUtc = iso8601_utc(),
    EnvName = list_to_binary(maps:get(env_name, Opts, "ci-heavy")),
    Command = list_to_binary(maps:get(command, Opts, "ct-batch.sh --batch=3 --level=heavy")),

    Baseline = #{
        <<"schema_version">> => <<"1.0">>,
        <<"captured_at_utc">> => NowUtc,
        <<"captured_by">> => #{
            <<"runner">> => <<"ct-batch.sh">>,
            <<"command">> => Command,
            <<"git">> => #{
                <<"commit">> => git_commit_bin(),
                <<"branch">> => git_branch_bin()
            }
        },
        <<"environment">> => build_env(EnvName),
        <<"global_defaults">> => #{
            <<"warmup_seconds">> => maps:get(warmup, Opts, 10),
            <<"sustained_seconds">> => maps:get(sustained, Opts, 60),
            <<"sample_count_min">> => maps:get(sample_min, Opts, 200)
        },
        <<"suites">> => Suites1
    },

    OutBin = jsx:encode(Baseline),
    ok = filelib:ensure_dir(OutPath),
    ok = file:write_file(OutPath, OutBin),
    io:format("Wrote baseline: ~s~n", [OutPath]),
    ok.

add_default_tolerance(SuiteEntry) ->
    T0 = maps:get(<<"tolerance">>, SuiteEntry, #{}),
    case map_size(T0) of
        0 ->
            SuiteEntry#{<<"tolerance">> => default_tolerance()};
        _ ->
            SuiteEntry
    end.

default_tolerance() ->
    #{
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
    }.

build_env(EnvName) ->
    #{
        <<"name">> => EnvName,
        <<"os">> => os_type_bin(),
        <<"cpu_cores">> => cpu_cores(),
        <<"memory_mb">> => mem_total_mb(),
        <<"container_limits">> => #{<<"cpu_quota">> => null, <<"memory_mb">> => null},
        <<"notes">> => <<"">>
    }.

os_type_bin() ->
    {OsFamily, OsName} = os:type(),
    list_to_binary(atom_to_list(OsFamily) ++ "-" ++ atom_to_list(OsName)).

cpu_cores() ->
    case os:cmd("getconf _NPROCESSORS_ONLN 2>/dev/null") of
        "" -> 0;
        S -> to_int(S)
    end.

mem_total_mb() ->
    case file:read_file("/proc/meminfo") of
        {ok, Bin} ->
            case re:run(Bin, <<"MemTotal:\\s+(\\d+)\\s+kB">>, [{capture, [1], binary}]) of
                {match, [KbBin]} -> (binary_to_integer(KbBin) div 1024);
                _ -> 0
            end;
        _ -> 0
    end.

git_commit_bin() ->
    to_bin(os:cmd("git rev-parse HEAD 2>/dev/null")).

git_branch_bin() ->
    to_bin(os:cmd("git rev-parse --abbrev-ref HEAD 2>/dev/null")).

iso8601_utc() ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:universal_time(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y,Mo,D,H,Mi,S])).

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> Bin;
        {error, Reason} -> erlang:error({read_failed, Path, Reason})
    end.

to_bin(S) when is_list(S) ->
    B = list_to_binary(string:trim(S)),
    case B of
        <<>> -> <<"unknown">>;
        _ -> B
    end.

to_int(S) ->
    list_to_integer(string:trim(S)).

parse_args([], Acc) ->
    case {maps:is_key(in, Acc), maps:is_key(out, Acc)} of
        {true, true} -> {ok, Acc};
        _ -> {error, missing_required_args}
    end;
parse_args(["--in", V | Rest], Acc) ->
    parse_args(Rest, Acc#{in => V});
parse_args(["--out", V | Rest], Acc) ->
    parse_args(Rest, Acc#{out => V});
parse_args(["--env-name", V | Rest], Acc) ->
    parse_args(Rest, Acc#{env_name => V});
parse_args(["--command", V | Rest], Acc) ->
    parse_args(Rest, Acc#{command => V});
parse_args(["--warmup", V | Rest], Acc) ->
    parse_args(Rest, Acc#{warmup => list_to_integer(V)});
parse_args(["--sustained", V | Rest], Acc) ->
    parse_args(Rest, Acc#{sustained => list_to_integer(V)});
parse_args(["--sample-min", V | Rest], Acc) ->
    parse_args(Rest, Acc#{sample_min => list_to_integer(V)});
parse_args(["--help" | _], _Acc) ->
    {error, help};
parse_args([Unknown | _], _Acc) ->
    {error, {unknown_arg, Unknown}}.
