-module(router_ets_metrics).

-doc "ETS operation metrics helper".
%% Emits telemetry for ETS operations (insert, delete, lookup, match) for observability

-export([emit_ets_op/4]).

-define(TELEMETRY_PREFIX, [router_ets]).

%% Op: insert | delete | lookup | match | select
%% Table: atom() | ets:tid()
%% LatencyUs: integer() (microseconds)
%% Metadata: map() with additional context
-spec emit_ets_op(Op :: atom(), Table :: atom() | ets:tid(), LatencyUs :: integer(), Metadata :: map()) -> ok.
emit_ets_op(Op, Table, LatencyUs, Metadata) ->
    TableName = case is_atom(Table) of
        true -> atom_to_binary(Table, utf8);
        false -> ~"tid"
    end,
    
    %% Emit operation counter
    router_telemetry_helper:execute(
        ?TELEMETRY_PREFIX ++ [ops_total],
        #{count => 1},
        maps:merge(#{
            table => TableName,
            op => atom_to_binary(Op, utf8)
        }, Metadata)
    ),
    
    %% Emit latency metric
    LatencySeconds = LatencyUs / 1_000_000.0,
    router_telemetry_helper:execute(
        ?TELEMETRY_PREFIX ++ [latency_seconds],
        #{duration_seconds => LatencySeconds},
        maps:merge(#{
            table => TableName,
            op => atom_to_binary(Op, utf8)
        }, Metadata)
    ),
    
    ok.

