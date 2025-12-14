%% @doc Test helper for router_decide_consumer test suites
%% Contains shared setup, mock functions, and state builders
%% @test_category test_helper
-module(router_decide_test_helper).

-export([
    setup_basic_state/0,
    setup_basic_state/1,
    with_standard_mocks/1,
    make_decide_request/1,
    make_decide_request/2
]).

-include_lib("common_test/include/ct.hrl").

%% Include state record definition from router_decide_consumer
-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}
}).

%% @doc Create basic state record for direct handle_info testing
-spec setup_basic_state() -> #state{}.
setup_basic_state() ->
    setup_basic_state(#{}).

-spec setup_basic_state(map()) -> #state{}.
setup_basic_state(Overrides) ->
    #state{
        connection = maps:get(connection, Overrides, undefined),
        decide_subject = maps:get(decide_subject, Overrides, <<"beamline.router.v1.decide">>),
        js_durable_group = maps:get(js_durable_group, Overrides, <<"test-group">>),
        publication_monitors = maps:get(publication_monitors, Overrides, #{})
    }.

%% @doc Run function with standard mocks, cleanup guaranteed even on failure
-spec with_standard_mocks(fun(() -> term())) -> term().
with_standard_mocks(Fun) ->
    %% Setup mocks - no passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(router_intake_backpressure, [passthrough]),
    meck:new(router_intake_validator, [passthrough]),
    meck:new(router_core, [passthrough]),
    meck:new(router_tenant_validator, [passthrough]),
    
    %% NOTE: Do NOT call ensure_router_nats_started() with mock - start_link not stubbed
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tenant_validation_enabled, true),
    
    %% Setup default expectations
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, 
                fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> 
                    {ok, <<"consumer-1">>} 
                end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    
    meck:expect(router_intake_backpressure, check_backpressure, 
                fun(_Subject) -> {backpressure_inactive, #{}} end),
    
    meck:expect(router_intake_validator, validate_intake_message, 
                fun(_Subject, Payload, _Headers, _Type) ->
                    case jsx:decode(Payload, [return_maps]) of
                        Message when is_map(Message) -> {ok, Message};
                        _ -> {error, {schema_validation_failed, <<"Invalid JSON">>, #{}}}
                    end
                end),
    
    meck:expect(router_core, route, fun(_RouteRequest, _Context) ->
        {ok, #{
            provider_id => <<"test-provider">>,
            reason => <<"test">>,
            priority => 50,
            expected_latency_ms => 100,
            metadata => #{}
        }}
    end),
    
    meck:expect(router_tenant_validator, validate_tenant, 
                fun(TenantId, _Context) -> {ok, TenantId} end),
    
    try
        Fun()
    after
        %% Guaranteed cleanup
        catch meck:unload(router_nats),
        catch meck:unload(router_intake_backpressure),
        catch meck:unload(router_intake_validator),
        catch meck:unload(router_core),
        catch meck:unload(router_tenant_validator)
    end.

%% @doc Create a standard decide request map
-spec make_decide_request(binary()) -> map().
make_decide_request(RequestId) ->
    make_decide_request(RequestId, #{}).

-spec make_decide_request(binary(), map()) -> map().
make_decide_request(RequestId, Overrides) ->
    Base = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => RequestId,
        <<"trace_id">> => <<"tr-", RequestId/binary>>,
        <<"tenant_id">> => maps:get(tenant_id, Overrides, <<"default_tenant">>),
        <<"task">> => maps:get(task, Overrides, #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        }),
        <<"policy_id">> => maps:get(policy_id, Overrides, <<"default">>)
    },
    maps:merge(Base, maps:without([tenant_id, task, policy_id], Overrides)).
