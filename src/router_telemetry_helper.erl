%% @doc Telemetry helper module
%% Provides safe telemetry execution with test mode support
%% Checks telemetry_enabled configuration before emitting events
-module(router_telemetry_helper).
-export([
    execute/3
]).

%% @doc Check if telemetry is enabled
%% Returns true if telemetry is enabled, false otherwise
%% Defaults to true (enabled) unless explicitly disabled
is_enabled() ->
    case application:get_env(beamline_router, telemetry_enabled, true) of
        false -> false;
        _ -> true
    end.

%% @doc Execute telemetry event (safe wrapper)
%% Only emits event if telemetry is enabled
%% Usage: router_telemetry_helper:execute([router_core, route], #{duration_us => 100}, #{tenant_id => <<"t1">>})
-spec execute([atom()], map(), map()) -> ok.
execute(Event, Measurements, Metadata) ->
    case is_enabled() of
        true ->
            telemetry:execute(Event, Measurements, Metadata);
        false ->
            ok  %% Silently skip telemetry in test mode
    end.

%% @doc Execute telemetry span (safe wrapper)
%% Only emits span if telemetry is enabled
%% Usage: router_telemetry_helper:span([router_core, route], #{tenant_id => <<"t1">>}, fun() -> ... end)
%% Removed span/3; not used and caused xref warnings
