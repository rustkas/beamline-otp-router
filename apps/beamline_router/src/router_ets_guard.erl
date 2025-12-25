-module(router_ets_guard).

-doc "ETS table invariant verification and guard helpers".
%%
%% This module provides centralized validation of ETS table invariants
%% to catch configuration errors early in integration tests.
%%
%% Usage:
%%   Spec = #{type => set, keypos => 1, read_concurrency => true, compressed => false},
%%   router_ets_guard:ensure_table(Table, Spec).

-export([ensure_table/2, verify_table/2, check_spec/1, validate_spec/1]).

%% Returns: {ok, Checked} | {error, [{table, Name}, {reason, Reason}]}
-spec ensure_table(Table :: ets:tid() | atom(), Spec :: map()) -> {ok, map()} | {error, list()}.
ensure_table(Table, Spec) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Validate spec keys first (fail-fast)
    case check_spec(Spec) of
        ok ->
            case verify_table(Table, Spec) of
                ok ->
                    EndTime = erlang:monotonic_time(microsecond),
                    CheckLatencyUs = router_duration:duration_us(StartTime, EndTime),
                    
                    %% Emit metrics for guard execution
                    router_telemetry_helper:execute(
                        [router_ets_guard, check_duration],
                        #{duration_us => CheckLatencyUs},
                        #{
                            table => table_name(Table),
                            status => ~"ok"
                        }
                    ),
                    
                    {ok, #{checked => true, latency_us => CheckLatencyUs}};
                {error, Violations} ->
                    EndTime = erlang:monotonic_time(microsecond),
                    _CheckLatencyUs = router_duration:duration_us(StartTime, EndTime),
                    
                    %% Emit violation counter
                    router_telemetry_helper:execute(
                        [router_ets_guard, invariant_violation_total],
                        #{count => length(Violations)},
                        #{
                            table => table_name(Table)
                        }
                    ),
                    
                    log_violations(Table, Violations),
                    
                    ErrorReason = [
                        {table, case is_atom(Table) of true -> Table; false -> tid end},
                        {reason, Violations}
                    ],
                    {error, ErrorReason}
            end;
        {error, InvalidKeys} ->
            ErrorReason = [
                {table, case is_atom(Table) of true -> Table; false -> tid end},
                {reason, {invalid_spec_keys, InvalidKeys}}
            ],
            {error, ErrorReason}
    end.

-spec verify_table(Table :: ets:tid() | atom(), Spec :: map()) -> ok | {error, list()}.
verify_table(Table, Spec) ->
    Violations = [],
    Violations1 = check_type(Table, Spec, Violations),
    Violations2 = check_keypos(Table, Spec, Violations1),
    Violations3 = check_read_concurrency(Table, Spec, Violations2),
    Violations4 = check_write_concurrency(Table, Spec, Violations3),
    Violations5 = check_compressed(Table, Spec, Violations4),
    case Violations5 of
        [] -> ok;
        List -> {error, List}
    end.

check_type(Table, Spec, Violations) ->
    case maps:get(type, Spec, undefined) of
        undefined -> Violations;
        ExpectedType ->
            case ets:info(Table, type) of
                ExpectedType -> Violations;
                ActualType ->
                    [{type, ExpectedType, ActualType} | Violations]
            end
    end.

check_keypos(Table, Spec, Violations) ->
    case maps:get(keypos, Spec, undefined) of
        undefined -> Violations;
        ExpectedKeypos ->
            case ets:info(Table, keypos) of
                ExpectedKeypos -> Violations;
                ActualKeypos ->
                    [{keypos, ExpectedKeypos, ActualKeypos} | Violations]
            end
    end.

check_read_concurrency(Table, Spec, Violations) ->
    case maps:get(read_concurrency, Spec, undefined) of
        undefined -> Violations;
        ExpectedRC ->
            case ets:info(Table, read_concurrency) of
                ExpectedRC -> Violations;
                ActualRC ->
                    [{read_concurrency, ExpectedRC, ActualRC} | Violations]
            end
    end.

check_write_concurrency(Table, Spec, Violations) ->
    case maps:get(write_concurrency, Spec, undefined) of
        undefined -> Violations;
        ExpectedWC ->
            case ets:info(Table, write_concurrency) of
                ExpectedWC -> Violations;
                ActualWC ->
                    [{write_concurrency, ExpectedWC, ActualWC} | Violations]
            end
    end.

check_compressed(Table, Spec, Violations) ->
    case maps:get(compressed, Spec, undefined) of
        undefined -> Violations;
        ExpectedCompressed ->
            case ets:info(Table, compressed) of
                ExpectedCompressed -> Violations;
                ActualCompressed ->
                    [{compressed, ExpectedCompressed, ActualCompressed} | Violations]
            end
    end.

log_violations(Table, Violations) ->
    TelemetryEnabled = application:get_env(beamline_router, telemetry_enabled, true),
    case TelemetryEnabled andalso erlang:function_exported(router_logger, error, 2) of
        true ->
            lists:foreach(fun({Field, Expected, Actual}) ->
                router_logger:error("ETS table invariant violated", #{
                    ~"table" => case is_atom(Table) of true -> atom_to_binary(Table, utf8); false -> ~"tid" end,
                    ~"field" => atom_to_binary(Field, utf8),
                    ~"expected" => format_value(Expected),
                    ~"actual" => format_value(Actual)
                })
            end, Violations);
        false ->
            ok
    end.

%% Validates that spec only contains known keys
-spec check_spec(Spec :: map()) -> ok | {error, list()}.
check_spec(Spec) when is_map(Spec) ->
    AllowedKeys = [type, keypos, read_concurrency, write_concurrency, compressed],
    SpecKeys = maps:keys(Spec),
    InvalidKeys = [Key || Key <- SpecKeys, not lists:member(Key, AllowedKeys)],
    case InvalidKeys of
        [] -> ok;
        _ -> {error, InvalidKeys}
    end;
check_spec(_) ->
    {error, not_a_map}.

%% Returns: ok | {error, Reason}
%% Alias for check_spec/1 for consistency
-spec validate_spec(Spec :: map()) -> ok | {error, term()}.
validate_spec(Spec) ->
    check_spec(Spec).

-spec table_name(ets:tid() | atom()) -> binary().
table_name(Table) when is_atom(Table) ->
    atom_to_binary(Table, utf8);
table_name(_) ->
    ~"tid".

format_value(V) when is_boolean(V) -> atom_to_binary(V, utf8);
format_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_value(V) when is_integer(V) -> integer_to_binary(V);
format_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).
