-module(router_metrics_http).
-export([start/0, stop/0, metrics/3]).

-ignore_xref([
  {router_metrics_http, start, 0},
  {router_metrics_http, stop, 0},
  {router_metrics_http, metrics, 3}
]).

-include("beamline_router.hrl").

-doc "Start HTTP server for Prometheus metrics export".
%% Port: 9001 (CP2 specification, separate from gRPC port 9000)
%% Path: GET /metrics
%% Format: Prometheus text format (RFC 4180)
start() ->
  case application:get_env(beamline_router, telemetry_enabled, true) of
    false -> ok;
    _ -> ok
  end,
  case application:ensure_all_started(inets) of
    {ok, _} -> ok;
    {error, _} -> ok
  end,
  %% CP2 specification: Router metrics on port 9001 (separate from gRPC port 9000)
  MetricsPort = application:get_env(beamline_router, metrics_port, 9001),
  case lists:member(httpd, application:which_applications()) of
    true -> ok;
    false -> ok
  end,
  case inets:start(httpd, [
    {port, MetricsPort},
    {server_name, "beamline-router"},
    {server_root, "."},
    {document_root, "."},
    {modules, [mod_esi]},
    {erl_script_alias, {"/metrics", [router_metrics_http]}}
  ]) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, _}} -> {ok, undefined};
    {error, {badarg, _}} -> {ok, undefined};
    Other -> Other
  end.

stop() ->
  MetricsPort = application:get_env(beamline_router, metrics_port, 9001),
  %% Stop HTTP server by port and default name
  case inets:stop(httpd, {any, MetricsPort, default}) of
    ok -> ok;
    {error, {already_stopped, _}} -> ok;
    {error, not_started} -> ok;
    Other -> Other
  end.

%% Returns Prometheus text format metrics
%% Content-Type: text/plain; version=0.0.4; charset=utf-8
metrics(SessionId, _Env, _Input) ->
  ok = router_metrics:ensure(),
  Body = router_prometheus:render(),
  %% Prometheus text format content type
  mod_esi:deliver(SessionId, "Content-Type: text/plain; version=0.0.4; charset=utf-8\r\n\r\n"),
  mod_esi:deliver(SessionId, Body).
