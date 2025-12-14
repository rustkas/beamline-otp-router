%% @doc Test helper module for router_caf_adapter tests
%% Provides common utilities for:
%% - Creating test requests and decisions
%% - Managing telemetry handler lifecycle
%% - Mock setup and teardown for router_nats
%%
%% Usage:
%% - Include in test SUITE with: -include("router_caf_test_helper.hrl").
%% - Or call functions directly via router_caf_test_helper:function().
%%
%% @test_category test_helper, caf
-module(router_caf_test_helper).

-include("../include/beamline_router.hrl").

-export([
    %% Request/Decision builders
    make_test_request/1,
    make_test_request/2,
    make_test_decision/0,
    make_test_decision/1,
    
    %% Telemetry helpers
    setup_telemetry_handler/2,
    setup_telemetry_handlers/1,
    cleanup_telemetry_handler/1,
    cleanup_telemetry_handlers/1,
    get_captured_telemetry/1,
    clear_captured_telemetry/0,
    
    %% Mock helpers
    setup_router_nats_mock/0,
    setup_router_nats_mock/1,
    teardown_router_nats_mock/0,
    
    %% Config helpers
    set_caf_config/1,
    reset_caf_config/0,
    
    %% Telemetry event names (matching router_caf_adapter.erl)
    telemetry_event/1
]).

%% Telemetry event prefix (must match router_caf_adapter.erl)
-define(TELEMETRY_PREFIX, [router_caf_adapter]).

%% ============================================================================
%% Request/Decision Builders
%% ============================================================================

%% @doc Create test request with given request_id
-spec make_test_request(binary()) -> map().
make_test_request(RequestId) ->
    make_test_request(RequestId, #{}).

%% @doc Create test request with given request_id and extra fields
-spec make_test_request(binary(), map()) -> map().
make_test_request(RequestId, ExtraFields) ->
    BaseRequest = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => RequestId,
        <<"tenant_id">> => <<"test-tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    maps:merge(BaseRequest, ExtraFields).

%% @doc Create default test decision
-spec make_test_decision() -> #route_decision{}.
make_test_decision() ->
    make_test_decision(#{}).

%% @doc Create test decision with overrides
-spec make_test_decision(map()) -> #route_decision{}.
make_test_decision(Overrides) ->
    #route_decision{
        provider_id = maps:get(provider_id, Overrides, <<"openai">>),
        reason = maps:get(reason, Overrides, <<"weighted">>),
        priority = maps:get(priority, Overrides, 50),
        expected_latency_ms = maps:get(expected_latency_ms, Overrides, 850),
        expected_cost = maps:get(expected_cost, Overrides, 0.012),
        metadata = maps:get(metadata, Overrides, #{})
    }.

%% ============================================================================
%% Telemetry Helpers
%% ============================================================================

%% @doc Get the full telemetry event name for a counter
%% These names must match what router_caf_adapter.erl emits
-spec telemetry_event(atom()) -> [atom()].
telemetry_event(skipped) ->
    ?TELEMETRY_PREFIX ++ [router_assignment_skipped_total];
telemetry_event(blocked) ->
    ?TELEMETRY_PREFIX ++ [router_assignment_blocked_total];
telemetry_event(published) ->
    ?TELEMETRY_PREFIX ++ [router_assignment_published_total];
telemetry_event(retry) ->
    ?TELEMETRY_PREFIX ++ [router_assignment_retry_total];
telemetry_event(failed) ->
    ?TELEMETRY_PREFIX ++ [router_assignment_publish_failures_total];
telemetry_event(exhausted) ->
    ?TELEMETRY_PREFIX ++ [router_retry_exhausted_total];
telemetry_event(span_start) ->
    ?TELEMETRY_PREFIX ++ [publish_assignment, start];
telemetry_event(span_stop) ->
    ?TELEMETRY_PREFIX ++ [publish_assignment, stop].

%% @doc Setup a telemetry handler for a specific event
%% HandlerId should be an atom (e.g., test_handler_retry)
%% EventType is one of: skipped, blocked, published, retry, failed, exhausted, span_start, span_stop
-spec setup_telemetry_handler(atom(), atom()) -> ok.
setup_telemetry_handler(HandlerId, EventType) when is_atom(HandlerId), is_atom(EventType) ->
    Event = telemetry_event(EventType),
    Handler = make_capture_handler(EventType),
    %% Detach first if exists (idempotent)
    catch telemetry:detach(HandlerId),
    telemetry:attach(HandlerId, Event, Handler, #{}),
    ok.

%% @doc Setup multiple telemetry handlers
%% Handlers is a list of {HandlerId, EventType} tuples
-spec setup_telemetry_handlers([{atom(), atom()}]) -> ok.
setup_telemetry_handlers(Handlers) ->
    lists:foreach(fun({HandlerId, EventType}) ->
        setup_telemetry_handler(HandlerId, EventType)
    end, Handlers),
    ok.

%% @doc Cleanup a single telemetry handler
-spec cleanup_telemetry_handler(atom()) -> ok.
cleanup_telemetry_handler(HandlerId) ->
    catch telemetry:detach(HandlerId),
    ok.

%% @doc Cleanup multiple telemetry handlers
-spec cleanup_telemetry_handlers([atom()]) -> ok.
cleanup_telemetry_handlers(HandlerIds) ->
    lists:foreach(fun(Id) -> cleanup_telemetry_handler(Id) end, HandlerIds),
    ok.

%% @doc Get captured telemetry data for an event type
%% Returns undefined if no data captured
-spec get_captured_telemetry(atom()) -> {map(), map()} | undefined.
get_captured_telemetry(EventType) ->
    get({telemetry, EventType}).

%% @doc Clear all captured telemetry data
-spec clear_captured_telemetry() -> ok.
clear_captured_telemetry() ->
    lists:foreach(fun(EventType) ->
        erase({telemetry, EventType})
    end, [skipped, blocked, published, retry, failed, exhausted, span_start, span_stop]),
    ok.

%% Internal: Create capture handler for event type
-spec make_capture_handler(atom()) -> fun().
make_capture_handler(EventType) ->
    fun(_Event, Measurements, Metadata, _HandlerConfig) ->
        put({telemetry, EventType}, {Measurements, Metadata})
    end.

%% ============================================================================
%% Mock Helpers
%% ============================================================================

%% @doc Setup router_nats mock with default success behavior
%% Delegates to router_mock_helpers for consistency across all test suites.
-spec setup_router_nats_mock() -> ok | {skip, string()}.
setup_router_nats_mock() ->
    setup_router_nats_mock(fun(_, _, _) -> {ok, <<"ack-success">>} end).

%% @doc Setup router_nats mock with custom publish_with_ack function
%% Delegates to router_mock_helpers for consistency.
-spec setup_router_nats_mock(fun()) -> ok | {skip, string()}.
setup_router_nats_mock(PublishFun) ->
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            ok = router_mock_helpers:setup_router_nats_mock(#{
                publish_with_ack => PublishFun
            }),
            ok
    end.

%% @doc Teardown router_nats mock
%% Delegates to router_mock_helpers for consistency.
-spec teardown_router_nats_mock() -> ok.
teardown_router_nats_mock() ->
    router_mock_helpers:unload(router_nats).

%% ============================================================================
%% Config Helpers
%% ============================================================================

%% @doc Set CAF adapter configuration
%% Config is a map of {key, value} pairs
-spec set_caf_config(map()) -> ok.
set_caf_config(Config) when is_map(Config) ->
    maps:foreach(fun(Key, Value) ->
        application:set_env(beamline_router, Key, Value)
    end, Config),
    ok.

%% @doc Reset CAF adapter configuration to defaults
-spec reset_caf_config() -> ok.
reset_caf_config() ->
    ConfigKeys = [
        caf_max_retries,
        caf_retry_base_ms,
        caf_deadline_min_ms,
        caf_deadline_max_ms,
        caf_deadline_multiplier,
        caf_push_assignment_enabled,
        caf_push_assignment_allowed_tenants
    ],
    lists:foreach(fun(Key) ->
        application:unset_env(beamline_router, Key)
    end, ConfigKeys),
    ok.
