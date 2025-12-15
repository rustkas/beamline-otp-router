-module(router_prometheus).
-export([dump/0, dump/1, render/0]).

-ignore_xref([
  {router_prometheus, dump, 0},
  {router_prometheus, dump, 1}
]).

-include("beamline_router.hrl").

%% @doc Dump metrics to default path
dump() -> dump("metrics_dump/metrics.prom").

%% @doc Dump metrics to specified path
dump(Path) ->
  Dir = filename:dirname(Path),
  ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
  Body = render(),
  file:write_file(Path, Body).

%% @doc Render metrics in Prometheus text format
%% Returns binary with Prometheus text format (RFC 4180)
%% Format: # HELP metric_name Description
%%         # TYPE metric_name counter|gauge|histogram
%%         metric_name{label1="value1"} value
render() ->
  Metrics = metrics_snapshot(),
  %% Group metrics by name for HELP/TYPE headers
  Grouped = group_metrics_by_name(Metrics),
  Lines = lists:flatmap(fun(GroupEntry) ->
    format_metric_group(GroupEntry)
  end, lists:sort(Grouped)),
  unicode:characters_to_binary(string:join(Lines, "\n")).

%% @doc Get metrics snapshot from ETS
%% Returns list of metric tuples: {Name, Val} or {{Name, LabelsKey}, Val}
metrics_snapshot() ->
  case ets:info(router_metrics) of
    undefined -> [];
    _ -> lists:filter(fun is_metric_tuple/1, ets:tab2list(router_metrics))
  end.

%% @doc Check if tuple is a valid metric
%% Supports both formats:
%% - {Name, Val} - metric without labels (backward compatible)
%% - {{Name, LabelsKey}, Val} - metric with labels
is_metric_tuple({Name, Val}) when is_atom(Name), (is_integer(Val) orelse is_float(Val)) -> true;
is_metric_tuple({{Name, LabelsKey}, Val}) when is_atom(Name), is_list(LabelsKey), (is_integer(Val) orelse is_float(Val)) -> true;
is_metric_tuple(_) -> false.

%% @doc Group metrics by name and labels
%% Returns list of {Name, [{LabelsKey, Val}]} tuples for labeled metrics
%% or {Name, [Val]} for unlabeled metrics
group_metrics_by_name(Metrics) ->
  GroupedMap = lists:foldl(fun
    %% Metric without labels: {Name, Val}
    ({Name, Val}, Acc) when is_atom(Name) ->
      Key = {Name, no_labels},
      case maps:get(Key, Acc, undefined) of
        undefined -> maps:put(Key, [Val], Acc);
        Existing -> maps:put(Key, [{no_labels, Val} | Existing], Acc)
      end;
    %% Metric with labels: {{Name, LabelsKey}, Val}
    ({{Name, LabelsKey}, Val}, Acc) when is_atom(Name), is_list(LabelsKey) ->
      Key = {Name, with_labels},
      case maps:get(Key, Acc, undefined) of
        undefined -> maps:put(Key, [{LabelsKey, Val}], Acc);
        Existing -> maps:put(Key, [{LabelsKey, Val} | Existing], Acc)
      end
  end, #{}, Metrics),
  maps:to_list(GroupedMap).

%% @doc Format metric group with HELP and TYPE
%% GroupEntry is {Key, Values} where:
%% - Key = {Name, no_labels} or {Name, with_labels}
%% - Values = [{no_labels, Val}] or [{LabelsKey, Val}]
format_metric_group({{Name, _LabelType}, Values}) ->
  MetricName = atom_to_list(Name),
  {Type, Description} = get_metric_metadata(Name),
  HelpLine = io_lib:format("# HELP ~s ~s", [MetricName, Description]),
  TypeLine = io_lib:format("# TYPE ~s ~s", [MetricName, Type]),
  ValueLines = [format_metric_value(Name, ValueEntry) || ValueEntry <- Values],
  [HelpLine, TypeLine | ValueLines].

%% @doc Format single metric value
%% ValueEntry can be:
%% - Val (integer or float) for unlabeled metrics
%% - {no_labels, Val} for unlabeled metrics (from grouping)
%% - {LabelsKey, Val} for labeled metrics
format_metric_value(Name, Val) when is_integer(Val) ->
  MetricName = atom_to_list(Name),
  io_lib:format("~s ~b", [MetricName, Val]);
format_metric_value(Name, Val) when is_float(Val) ->
  MetricName = atom_to_list(Name),
  io_lib:format("~s ~.6f", [MetricName, Val]);
format_metric_value(Name, {no_labels, Val}) when is_integer(Val) ->
  MetricName = atom_to_list(Name),
  io_lib:format("~s ~b", [MetricName, Val]);
format_metric_value(Name, {no_labels, Val}) when is_float(Val) ->
  MetricName = atom_to_list(Name),
  io_lib:format("~s ~.6f", [MetricName, Val]);
format_metric_value(Name, {LabelsKey, Val}) when is_list(LabelsKey) ->
  MetricName = atom_to_list(Name),
  LabelsStr = format_labels(LabelsKey),
  case is_integer(Val) of
    true -> io_lib:format("~s~s ~b", [MetricName, LabelsStr, Val]);
    false -> io_lib:format("~s~s ~.6f", [MetricName, LabelsStr, Val])
  end.

%% @doc Format labels for Prometheus text format
%% LabelsKey is a list of {Key, Value} tuples
%% Output format: {label1="value1",label2="value2"}
-spec format_labels(list({atom() | binary(), binary() | atom() | integer() | float()})) -> string().
format_labels([]) ->
  "";
format_labels(LabelsKey) ->
  LabelsStr = lists:foldl(fun({Key, Value}, Acc) ->
    KeyStr = label_key_to_string(Key),
    ValueStr = label_value_to_string(Value),
    LabelPair = io_lib:format("~s=\"~s\"", [KeyStr, ValueStr]),
    case Acc of
      "" -> LabelPair;
      _ -> io_lib:format("~s,~s", [Acc, LabelPair])
    end
  end, "", LabelsKey),
  io_lib:format("{~s}", [LabelsStr]).

%% @doc Convert label key to string
-spec label_key_to_string(atom() | binary()) -> string().
label_key_to_string(Key) when is_atom(Key) ->
  atom_to_list(Key);
label_key_to_string(Key) when is_binary(Key) ->
  binary_to_list(Key);
label_key_to_string(Key) ->
  lists:flatten(io_lib:format("~p", [Key])).

%% @doc Convert label value to string (escape quotes and backslashes)
-spec label_value_to_string(binary() | atom() | integer() | float()) -> string().
label_value_to_string(Value) when is_binary(Value) ->
  %% Escape quotes and backslashes in Prometheus label values
  Escaped = binary:replace(
    binary:replace(Value, <<"\\">>, <<"\\\\">>, [global]),
    <<"\"">>, <<"\\\"">>, [global]
  ),
  binary_to_list(Escaped);
label_value_to_string(Value) when is_atom(Value) ->
  atom_to_list(Value);
label_value_to_string(Value) when is_integer(Value) ->
  integer_to_list(Value);
label_value_to_string(Value) when is_float(Value) ->
  float_to_list(Value, [{decimals, 6}, compact]);
label_value_to_string(Value) ->
  lists:flatten(io_lib:format("~p", [Value])).

%% @doc Get metric metadata (type and description)
%% Returns: {Type, Description}
%% Type: counter, gauge, histogram
get_metric_metadata(Name) ->
  case Name of
    %% JetStream metrics
    router_jetstream_ack_total -> 
      {"counter", "Total number of JetStream message acknowledgements"};
    router_jetstream_redelivery_total -> 
      {"counter", "Total number of JetStream message redeliveries (NAK operations)"};
    router_redelivery_total -> 
      {"counter", "Total number of JetStream message redeliveries (deprecated, use router_jetstream_redelivery_total)"};
    router_dlq_total -> 
      {"counter", "Total number of messages sent to Dead Letter Queue"};
    
    %% Idempotency metrics
    router_idem_hits_total -> 
      {"counter", "Total number of idempotency cache hits"};
    router_idem_miss_total -> 
      {"counter", "Total number of idempotency cache misses"};
    router_idem_evictions_total -> 
      {"counter", "Total number of idempotency entries evicted due to TTL expiration"};
    
    %% ACL metrics
    router_acl_allowed_total -> 
      {"counter", "Total number of ACL allowed decisions"};
    router_acl_denied_total -> 
      {"counter", "Total number of ACL denied decisions"};
    
    %% Headers propagation metrics
    ctx_missing_headers_total -> 
      {"counter", "Total number of requests with missing context headers (trace_id, span_id, tenant_id)"};
    
    %% Span metrics
    router_span_duration_seconds ->
      {"histogram", "Duration of router spans in seconds"};
    
    %% Error metrics
    router_error_total ->
      {"counter", "Total number of router errors"};
    
    %% ============================================================
    %% Circuit Breaker metrics (R10)
    %% Emitted by: router_circuit_breaker.erl
    %% Labels: tenant_id, provider_id, state/reason/from/to
    %% ============================================================
    router_circuit_breaker_state ->
      {"gauge", "Current circuit breaker state (0=closed, 0.5=half_open, 1=open)"};
    router_circuit_breaker_state_transitions_total ->
      {"counter", "Total circuit breaker state transitions"};
    router_circuit_breaker_trigger_reason ->
      {"counter", "Count of circuit breaker openings by trigger reason"};
    
    %% NATS publish latency (used by latency threshold CB)
    router_nats_publish_latency_seconds ->
      {"gauge", "Latest NATS publish latency in seconds"};
    
    %% Default (unknown metric)
    _ -> 
      {"counter", "Router metric"}
  end.
