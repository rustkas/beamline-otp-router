%% @doc Common Test helpers for bounded waits and polling
%% Standardized bounded polling functions for all test suites
-module(test_helpers).

-include_lib("common_test/include/ct.hrl").

%% Suppress warnings for helper functions that may be used dynamically
-compile({nowarn_unused_function, [wait_for_ets_entry/3]}).

-export([
    wait_for_app_start/2,
    wait_for_app_start/3,
    wait_for_meck_call/4,
    wait_for_meck_call/5,
    wait_for_no_meck_call/4,
    wait_for_no_meck_call/5,
    wait_for_condition/2,
    wait_for_condition/3,
    wait_for_process/2,
    wait_for_process/3,
    get_proper_options/0,
    wait_for_metric/4,
    wait_for_log/4,
    wait_for_ets_entry/3,
    wait_for_ets_entry/4
]).

%% @doc Wait for application process to be ready (bounded polling)
%% Default interval: 10ms
-spec wait_for_app_start(atom(), non_neg_integer()) -> ok | no_return().
wait_for_app_start(ProcessName, MaxWaitMs) ->
    wait_for_app_start(ProcessName, MaxWaitMs, 10).

-spec wait_for_app_start(atom(), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_app_start(ProcessName, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    case whereis(ProcessName) of
        undefined ->
            timer:sleep(IntervalMs),
            wait_for_app_start(ProcessName, MaxWaitMs - IntervalMs, IntervalMs);
        Pid when is_pid(Pid) ->
            %% Process exists, verify it's alive
            case is_process_alive(Pid) of
                true -> ok;
                false ->
                    timer:sleep(IntervalMs),
                    wait_for_app_start(ProcessName, MaxWaitMs - IntervalMs, IntervalMs)
            end
    end;
wait_for_app_start(ProcessName, _MaxWaitMs, _IntervalMs) ->
    ct:fail("Timeout waiting for ~p to start", [ProcessName]).

%% @doc Wait for meck function to be called (bounded polling)
%% Args can be a list or '_' for any arguments
-spec wait_for_meck_call(atom(), atom(), '_' | list(), non_neg_integer()) -> ok | no_return().
wait_for_meck_call(Module, Function, Args, MaxWaitMs) ->
    wait_for_meck_call(Module, Function, Args, MaxWaitMs, 10).

-spec wait_for_meck_call(atom(), atom(), '_' | list(), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_meck_call(Module, Function, Args, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    %% Check if function was called with matching arguments
    case Args of
        '_' ->
            %% Check if function was called with any arguments
            case meck:called(Module, Function, '_') of
                true -> ok;
                false ->
                    timer:sleep(IntervalMs),
                    wait_for_meck_call(Module, Function, Args, MaxWaitMs - IntervalMs, IntervalMs)
            end;
        _ when is_list(Args) ->
            %% Check if function was called with specific arguments
            case meck:called(Module, Function, Args) of
                true -> ok;
                false ->
                    timer:sleep(IntervalMs),
                    wait_for_meck_call(Module, Function, Args, MaxWaitMs - IntervalMs, IntervalMs)
            end
    end;
wait_for_meck_call(Module, Function, Args, _MaxWaitMs, _IntervalMs) ->
    case Args of
        '_' ->
            ct:fail("Timeout waiting for ~p:~p to be called", [Module, Function]);
        _ when is_list(Args) ->
            ct:fail("Timeout waiting for ~p:~p/~p to be called", [Module, Function, length(Args)]);
        _ ->
            ct:fail("Timeout waiting for ~p:~p with args ~p to be called", [Module, Function, Args])
    end.

%% @doc Wait and verify that meck function was NOT called (bounded polling)
%% Args can be a list or '_' for any arguments
-spec wait_for_no_meck_call(atom(), atom(), '_' | list(), non_neg_integer()) -> ok | no_return().
wait_for_no_meck_call(Module, Function, Args, MaxWaitMs) ->
    wait_for_no_meck_call(Module, Function, Args, MaxWaitMs, 10).

-spec wait_for_no_meck_call(atom(), atom(), '_' | list(), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_no_meck_call(Module, Function, Args, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    %% Check if function was called with matching arguments
    case Args of
        '_' ->
            %% Check if function was called with any arguments
            case meck:called(Module, Function, '_') of
                true ->
                    N = meck:num_calls(Module, Function, '_'),
                    ct:fail("Unexpected call to ~p:~p (called ~p times)", [Module, Function, N]);
                false ->
                    timer:sleep(IntervalMs),
                    wait_for_no_meck_call(Module, Function, Args, MaxWaitMs - IntervalMs, IntervalMs)
            end;
        _ when is_list(Args) ->
            %% Check if function was called with specific arguments
            case meck:called(Module, Function, Args) of
                true ->
                    ct:fail("Unexpected call to ~p:~p/~p", [Module, Function, length(Args)]);
                false ->
                    timer:sleep(IntervalMs),
                    wait_for_no_meck_call(Module, Function, Args, MaxWaitMs - IntervalMs, IntervalMs)
            end
    end;
wait_for_no_meck_call(_Module, _Function, _Args, _MaxWaitMs, _IntervalMs) ->
    %% Timeout reached, function was not called (expected)
    ok.

%% @doc Wait for a condition to become true (bounded polling)
%% Fun() should return true when condition is met, false otherwise
-spec wait_for_condition(fun(() -> boolean()), non_neg_integer()) -> ok | no_return().
wait_for_condition(Fun, MaxWaitMs) ->
    wait_for_condition(Fun, MaxWaitMs, 10).

-spec wait_for_condition(fun(() -> boolean()), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_condition(Fun, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    case Fun() of
        true -> ok;
        false ->
            timer:sleep(IntervalMs),
            wait_for_condition(Fun, MaxWaitMs - IntervalMs, IntervalMs)
    end;
wait_for_condition(_Fun, _MaxWaitMs, _IntervalMs) ->
    ct:fail("Timeout waiting for condition").

%% @doc Wait for a process to be alive (bounded polling)
-spec wait_for_process(pid() | atom(), non_neg_integer()) -> ok | no_return().
wait_for_process(Process, MaxWaitMs) ->
    wait_for_process(Process, MaxWaitMs, 10).

-spec wait_for_process(pid() | atom(), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_process(Process, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    Pid = case Process of
        Pid0 when is_pid(Pid0) -> Pid0;
        Name when is_atom(Name) -> whereis(Name)
    end,
    case Pid of
        undefined ->
            timer:sleep(IntervalMs),
            wait_for_process(Process, MaxWaitMs - IntervalMs, IntervalMs);
        Pid1 when is_pid(Pid1) ->
            case is_process_alive(Pid1) of
                true -> ok;
                false ->
                    timer:sleep(IntervalMs),
                    wait_for_process(Process, MaxWaitMs - IntervalMs, IntervalMs)
            end
    end;
wait_for_process(Process, _MaxWaitMs, _IntervalMs) ->
    ct:fail("Timeout waiting for process ~p", [Process]).

%% @doc Get PropEr options from environment variables (for nightly runs)
%% Returns options list with numtests and seed if set in environment
%% PropEr seed format: {A, B, C} tuple for exs1024, or integer for other generators
-spec get_proper_options() -> list().
get_proper_options() ->
    NumTests = case os:getenv("PROPERTY_TEST_NUMTESTS") of
        false -> 100;  % Default
        NumStr -> 
            case string:to_integer(NumStr) of
                {Num, _} when Num > 0 -> Num;
                _ -> 100
            end
    end,
    Options = [{numtests, NumTests}],
    case os:getenv("PROPERTY_TEST_SEED") of
        false -> Options;
        SeedStr ->
            %% PropEr accepts seed as integer or {A, B, C} tuple
            %% For reproducibility, we use integer seed (PropEr will convert internally)
            case string:to_integer(SeedStr) of
                {Seed, _} when Seed > 0 -> 
                    %% Convert integer seed to exs1024 tuple format for reproducibility
                    %% Use seed to generate deterministic tuple: {Seed, Seed*2, Seed*3}
                    SeedTuple = {Seed, Seed * 2, Seed * 3},
                    [{seed, SeedTuple} | Options];
                _ -> Options
            end
    end.

%% @doc Wait for metric to be emitted (bounded polling)
%% MetricTable: ETS table tracking metric calls
%% MetricName: atom() - name of the metric to wait for
%% MaxWaitMs: maximum wait time in milliseconds
%% Returns: ok when metric found, fails on timeout
-spec wait_for_metric(ets:tid(), atom(), atom(), non_neg_integer()) -> ok | no_return().
wait_for_metric(MetricTable, MetricName, Function, MaxWaitMs) ->
    wait_for_metric(MetricTable, MetricName, Function, MaxWaitMs, 10).

-spec wait_for_metric(ets:tid(), atom(), atom(), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_metric(MetricTable, MetricName, Function, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    case ets:match_object(MetricTable, {metric, Function, MetricName, '_'}) of
        [] ->
            timer:sleep(IntervalMs),
            wait_for_metric(MetricTable, MetricName, Function, MaxWaitMs - IntervalMs, IntervalMs);
        _ ->
            ok
    end;
wait_for_metric(_MetricTable, MetricName, _Function, _MaxWaitMs, _IntervalMs) ->
    ct:fail("Timeout waiting for metric ~p", [MetricName]).

%% @doc Wait for log entry to be written (bounded polling)
%% LogTable: ETS table tracking log calls
%% LogLevel: atom() - log level (error, warn, info, debug)
%% MessagePattern: binary() - pattern to match in log message
%% MaxWaitMs: maximum wait time in milliseconds
%% Returns: ok when log found, fails on timeout
-spec wait_for_log(ets:tid(), atom(), binary(), non_neg_integer()) -> ok | no_return().
wait_for_log(LogTable, LogLevel, MessagePattern, MaxWaitMs) ->
    wait_for_log(LogTable, LogLevel, MessagePattern, MaxWaitMs, 10).

-spec wait_for_log(ets:tid(), atom(), binary(), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_log(LogTable, LogLevel, MessagePattern, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    case ets:match_object(LogTable, {log, LogLevel, '_', '_'}) of
        [] ->
            timer:sleep(IntervalMs),
            wait_for_log(LogTable, LogLevel, MessagePattern, MaxWaitMs - IntervalMs, IntervalMs);
        LogEntries ->
            %% Check if any log entry contains the pattern
            case lists:any(fun({log, _Level, Message, _Context}) ->
                binary:match(Message, MessagePattern) =/= nomatch
            end, LogEntries) of
                true -> ok;
                false ->
                    timer:sleep(IntervalMs),
                    wait_for_log(LogTable, LogLevel, MessagePattern, MaxWaitMs - IntervalMs, IntervalMs)
            end
    end;
wait_for_log(_LogTable, _LogLevel, MessagePattern, _MaxWaitMs, _IntervalMs) ->
    ct:fail("Timeout waiting for log entry with pattern ~p", [MessagePattern]).

%% @doc Wait for ETS entry to appear (bounded polling)
%% Table: ETS table to check
%% Key: key to look for
%% MaxWaitMs: maximum wait time in milliseconds
%% Returns: ok when entry found, fails on timeout
-spec wait_for_ets_entry(ets:tid(), term(), non_neg_integer()) -> ok | no_return().
wait_for_ets_entry(Table, Key, MaxWaitMs) ->
    wait_for_ets_entry(Table, Key, MaxWaitMs, 10).

-spec wait_for_ets_entry(ets:tid(), term(), non_neg_integer(), non_neg_integer()) -> ok | no_return().
wait_for_ets_entry(Table, Key, MaxWaitMs, IntervalMs) when MaxWaitMs > 0 ->
    case ets:lookup(Table, Key) of
        [] ->
            timer:sleep(IntervalMs),
            wait_for_ets_entry(Table, Key, MaxWaitMs - IntervalMs, IntervalMs);
        _ ->
            ok
    end;
wait_for_ets_entry(_Table, Key, _MaxWaitMs, _IntervalMs) ->
    ct:fail("Timeout waiting for ETS entry with key ~p", [Key]).

