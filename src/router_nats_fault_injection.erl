%% @doc Fault Injection Module for router_nats Testing
%% 
%% Provides controlled fault injection for NATS/JetStream operations during testing.
%% Allows tests to simulate various failure scenarios:
%% - Connection failures
%% - Operation timeouts
%% - Operation errors
%% - Connection drops
%% 
%% Usage in tests:
%%   router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
%%   router_nats_fault_injection:enable_fault(publish, {error, timeout}),
%%   router_nats_fault_injection:disable_fault(publish),
-module(router_nats_fault_injection).
-export([enable_fault/2, disable_fault/1, clear_all_faults/0, should_fail/2, get_fault/1, inject_faults/1]).

-ignore_xref([
  {router_nats_fault_injection, enable_fault, 2},
  {router_nats_fault_injection, disable_fault, 1},
  {router_nats_fault_injection, clear_all_faults, 0},
  {router_nats_fault_injection, get_fault, 1},
  {router_nats_fault_injection, inject_faults, 1}
]).

-define(FAULT_TABLE, router_nats_faults).

%% @doc Initialize fault injection table
%% Handles race condition where another process creates table between check and create
ensure_table() ->
    case ets:whereis(?FAULT_TABLE) of
        undefined ->
            try
                ets:new(?FAULT_TABLE, [named_table, public, set, {read_concurrency, true}])
            catch
                error:badarg ->
                    %% Table was created by another process between whereis and new
                    ?FAULT_TABLE
            end;
        _ ->
            ?FAULT_TABLE
    end.

%% @doc Enable fault injection for an operation
%% Operation: connect | publish | publish_with_ack | ack | nak | subscribe
%% Fault: {error, Reason} | timeout | close_connection | {delay, Milliseconds} | {intermittent, Fault, Probability}
%%   - Probability: 0.0 to 1.0 (0.0 = never fails, 1.0 = always fails)
-spec enable_fault(atom(), term()) -> ok.
enable_fault(Operation, Fault) when is_atom(Operation) ->
    ensure_table(),
    ets:insert(?FAULT_TABLE, {Operation, Fault}),
    ok.

%% @doc Disable fault injection for an operation
-spec disable_fault(atom()) -> ok.
disable_fault(Operation) when is_atom(Operation) ->
    ensure_table(),
    ets:delete(?FAULT_TABLE, Operation),
    ok.

%% @doc Clear all fault injections
-spec clear_all_faults() -> ok.
clear_all_faults() ->
    ensure_table(),
    ets:delete_all_objects(?FAULT_TABLE),
    ok.

%% @doc Check if operation should fail
%% Returns: {true, Fault} | false
%% Handles intermittent faults with probability-based failure
-spec should_fail(atom(), term()) -> {true, term()} | false.
should_fail(Operation, _Context) when is_atom(Operation) ->
    ensure_table(),
    case ets:lookup(?FAULT_TABLE, Operation) of
        [{Operation, {intermittent, Fault, Probability}}] when is_float(Probability), Probability >= 0.0, Probability =< 1.0 ->
            %% Intermittent fault: check probability
            Random = rand:uniform(),
            case Random =< Probability of
                true ->
                    {true, Fault};
                false ->
                    false
            end;
        [{Operation, Fault}] ->
            %% Persistent fault: always fail
            {true, Fault};
        [] ->
            false
    end.

%% @doc Get fault configuration for operation
-spec get_fault(atom()) -> {ok, term()} | {error, not_found}.
get_fault(Operation) when is_atom(Operation) ->
    ensure_table(),
    case ets:lookup(?FAULT_TABLE, Operation) of
        [{Operation, Fault}] ->
            {ok, Fault};
        [] ->
            {error, not_found}
    end.

%% @doc Inject multiple faults at once
%% @param Faults Map of Operation => Fault
-spec inject_faults(map()) -> ok.
inject_faults(Faults) ->
    maps:fold(fun(Operation, Fault, _) ->
        enable_fault(Operation, Fault)
    end, ok, Faults),
    ok.
