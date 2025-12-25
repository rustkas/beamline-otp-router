-module(router_telemetry_handler).

-doc "Telemetry Handler for Router Core".
%% Collects and aggregates telemetry events for metrics
%% CP1: Simple in-memory aggregation (no Prometheus/OTel)
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([attach_handlers/0, detach_handlers/0]).
-export([get_metrics/0, reset_metrics/0]).
-ignore_xref([
    {router_telemetry_handler, start_link, 0},
    {router_telemetry_handler, attach_handlers, 0},
    {router_telemetry_handler, detach_handlers, 0},
    {router_telemetry_handler, get_metrics, 0},
    {router_telemetry_handler, reset_metrics, 0}
]).
%% Export telemetry callback used by telemetry:attach_many/4
-export([handle_telemetry_event/4]).

-include("beamline_router.hrl").

-record(state, {
    metrics :: #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    attach_handlers(),
    {ok, #state{metrics = #{}}}.

attach_handlers() ->
    %% Router Core events
    telemetry:attach_many(
        ~"router_core_handlers",
        [
            [router_core, route, start],
            [router_core, route, stop],
            [router_core, route, exception],
            [router_core, routes_total],
            [router_core, resolutions_total],
            [router_core, errors_total]
        ],
        fun ?MODULE:handle_telemetry_event/4,
        undefined
    ),
    
    %% Policy Store events
    telemetry:attach_many(
        ~"router_policy_store_handlers",
        [
            [router_policy_store, load_policy],
            [router_policy_store, upsert_policy],
            [router_policy_store, delete_policy],
            [router_policy_store, list_policies]
        ],
        fun ?MODULE:handle_telemetry_event/4,
        undefined
    ),
    
    ok.

detach_handlers() ->
    telemetry:detach(~"router_core_handlers"),
    telemetry:detach(~"router_policy_store_handlers"),
    ok.

handle_telemetry_event([router_core, route, start], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {event, route_start, Measurements, Metadata});
handle_telemetry_event([router_core, route, stop], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {event, route_stop, Measurements, Metadata});
handle_telemetry_event([router_core, route, exception], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {event, route_exception, Measurements, Metadata});
handle_telemetry_event([router_core, routes_total], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {event, routes_total, Measurements, Metadata});
handle_telemetry_event([router_core, resolutions_total], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {event, resolutions_total, Measurements, Metadata});
handle_telemetry_event([router_core, errors_total], Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {event, errors_total, Measurements, Metadata});
handle_telemetry_event([router_policy_store | _] = Event, Measurements, Metadata, _Config) ->
    gen_server:cast(?MODULE, {event, Event, Measurements, Metadata});
handle_telemetry_event(_Event, _Measurements, _Metadata, _Config) ->
    ok.

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};
handle_call(reset_metrics, _From, _State) ->
    {reply, ok, #state{metrics = #{}}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({event, EventName, Measurements, Metadata}, State) ->
    NewMetrics = update_metrics(EventName, Measurements, Metadata, State#state.metrics),
    {noreply, State#state{metrics = NewMetrics}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    detach_handlers(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%% Internal: Update metrics
update_metrics(routes_total, #{count := Count}, Metadata, Metrics) ->
    Key = {routes_total, maps:get(result, Metadata, all)},
    Current = maps:get(Key, Metrics, 0),
    maps:put(Key, Current + Count, Metrics);
update_metrics(resolutions_total, #{count := Count}, _Metadata, Metrics) ->
    Key = resolutions_total,
    Current = maps:get(Key, Metrics, 0),
    maps:put(Key, Current + Count, Metrics);
update_metrics(errors_total, #{count := Count}, Metadata, Metrics) ->
    ErrorReason = maps:get(error, Metadata, unknown),
    Key = {errors_total, ErrorReason},
    Current = maps:get(Key, Metrics, 0),
    maps:put(Key, Current + Count, Metrics);
update_metrics(route_stop, Measurements, Metadata, Metrics) ->
    Duration = maps:get(duration, Measurements, 0),
    ResultKey = case is_map(Metadata) of
        true -> maps:get(result, Metadata, all);
        false -> all
    end,
    Key = {route_duration, ResultKey},
    Durations = maps:get(Key, Metrics, []),
    NewDurations = [Duration | lists:sublist(Durations, 99)],  %% Keep last 100
    maps:put(Key, NewDurations, Metrics);
update_metrics(_Event, _Measurements, _Metadata, Metrics) ->
    Metrics.
