%% @doc Test helper for router_decide_consumer test suites
%% Contains shared setup and state builders
%% @test_category test_helper
-module(router_decide_test_helper).

-export([
    setup_basic_state/0,
    setup_basic_state/1,
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
        publication_monitors = maps:get(publication_monitors, Overrides, #{{} => #{}})
    }.

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
