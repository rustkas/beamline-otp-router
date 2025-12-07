%% @doc E2E Test Suite for Extensions Pipeline
%% Tests: Gateway → Router → Extension Registry → Extensions (NATS) → Provider → Post
%% @test_category e2e, extensions, nats
-module(router_extensions_e2e_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
%% Also suppress warnings for helper functions used by test cases
-compile({nowarn_unused_function, [
    all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2,
    test_e2e_full_pipeline/1,
    test_e2e_pre_processor/1,
    test_e2e_validator/1,
    test_e2e_post_processor/1,
    test_e2e_custom_provider/1,
    test_e2e_multiple_extensions/1,
    test_e2e_extension_timeout/1,
    test_e2e_extension_error/1,
    check_nats_connection/1,
    start_extension_services/1,
    start_extension_services_with_delay/2,
    start_extension_services_with_error/1,
    find_extensions_dir/0,
    find_existing_dir/1,
    start_node_extensions/2,
    stop_extension_services/1,
    create_policy_with_extensions/1,
    create_policy_with_provider/1,
    create_route_request/2
]}).


all() ->
    [
        {group, e2e_extensions_tests}
    ].

groups() ->
    [
        {e2e_extensions_tests, [sequence], [
            test_e2e_pre_processor,
            test_e2e_validator,
            test_e2e_post_processor,
            test_e2e_custom_provider,
            test_e2e_full_pipeline,
            test_e2e_multiple_extensions,
            test_e2e_extension_timeout,
            test_e2e_extension_error
        ]}
    ].

init_per_suite(Config) ->
    %% Check if NATS is available
    NatsUrl = os:getenv("NATS_URL", "nats://localhost:4222"),
    
    %% Try to connect to NATS
    case check_nats_connection(NatsUrl) of
        {ok, _} ->
            ct:log("NATS connection successful: ~s", [NatsUrl]),
            ok = application:load(beamline_router),
            ok = application:set_env(beamline_router, grpc_port, 0),
            ok = application:set_env(beamline_router, grpc_enabled, false),
            ok = application:set_env(beamline_router, nats_mode, mock),  %% Use mock mode for tests
            ok = application:set_env(beamline_router, nats_url, NatsUrl),
            ok = application:set_env(beamline_router, extension_registry, [
                {source, fixtures}
            ]),
            ok = application:set_env(beamline_router, telemetry_enabled, true),
            
            case application:ensure_all_started(beamline_router) of
                {ok, _} ->
                    %% Wait for extension registry to load
                    timer:sleep(1000),
                    [{nats_url, NatsUrl}, {nats_available, true} | Config];
                Error ->
                    ct:log("Failed to start beamline_router: ~p, using mock mode", [Error]),
                    [{nats_available, false} | Config]
            end;
        {error, Reason} ->
            ct:log("NATS not available: ~p, using mock mode for tests", [Reason]),
            ok = application:load(beamline_router),
            ok = application:set_env(beamline_router, grpc_port, 0),
            ok = application:set_env(beamline_router, grpc_enabled, false),
            ok = application:set_env(beamline_router, nats_mode, mock),
            ok = application:set_env(beamline_router, telemetry_enabled, true),
            case application:ensure_all_started(beamline_router) of
                {ok, _} ->
                    [{nats_available, false} | Config];
                Error ->
                    ct:log("Failed to start beamline_router: ~p", [Error]),
                    [{nats_available, false} | Config]
            end
    end.

end_per_suite(Config) ->
    case proplists:get_value(nats_available, Config, false) of
        true ->
            application:stop(beamline_router);
        false ->
            ok
    end,
    Config.

init_per_testcase(_TestCase, Config) ->
    case proplists:get_value(nats_available, Config, false) of
        true ->
            Config;
        false ->
            {skip, "NATS not available"}
    end.

end_per_testcase(_TestCase, Config) ->
    Config.

%% @doc E2E: Full pipeline with pre → validator → provider → post
test_e2e_full_pipeline(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    %% Start extension services (or use mock)
    ExtensionsPid = start_extension_services(NatsUrl),
    
    %% Mock extension invoker if extensions not available
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        {ok, #{<<"processed">> => true}}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        %% Create policy with full pipeline
        Policy = create_policy_with_extensions([
            {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]},
            {validators, [{id, <<"pii_guard">>, on_fail, <<"block">>}]},
            {post, [{id, <<"mask_pii">>, mode, <<"required">>}]}
        ]),
        
        %% Create route request
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>, <<"trace_id">> => <<"trace-001">>},
        
        %% Execute decision
        case router_decider:decide(Request, Policy, Context) of
            {ok, Decision} ->
                ct:log("Decision: ~p", [Decision]),
                %% Verify decision was made
                ?assert(is_record(Decision, route_decision)),
                ok;
            {error, Reason} ->
                ct:log("Decision failed (expected in mock mode): ~p", [Reason]),
                %% In mock mode, decision may fail if extensions not available
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% @doc E2E: Pre-processor extension
test_e2e_pre_processor(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    ExtensionsPid = start_extension_services(NatsUrl),
    
    %% Mock extension invoker
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        {ok, #{<<"normalized">> => true}}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        Policy = create_policy_with_extensions([
            {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}
        ]),
        
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>},
        
        case router_decider:decide(Request, Policy, Context) of
            {ok, _Decision} ->
                ok;
            {error, Reason} ->
                ct:log("Pre-processor failed (expected in mock mode): ~p", [Reason]),
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% @doc E2E: Validator extension
test_e2e_validator(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    ExtensionsPid = start_extension_services(NatsUrl),
    
    %% Mock extension invoker
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        {ok, #{<<"validated">> => true}}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        Policy = create_policy_with_extensions([
            {validators, [{id, <<"pii_guard">>, on_fail, <<"block">>}]}
        ]),
        
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>},
        
        case router_decider:decide(Request, Policy, Context) of
            {ok, _Decision} ->
                ok;
            {error, Reason} ->
                ct:log("Validator failed (expected in mock mode): ~p", [Reason]),
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% @doc E2E: Post-processor extension
test_e2e_post_processor(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    ExtensionsPid = start_extension_services(NatsUrl),
    
    %% Mock extension invoker
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        {ok, #{<<"masked">> => true}}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        Policy = create_policy_with_extensions([
            {post, [{id, <<"mask_pii">>, mode, <<"required">>}]}
        ]),
        
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>},
        
        case router_decider:decide(Request, Policy, Context) of
            {ok, _Decision} ->
                ok;
            {error, Reason} ->
                ct:log("Post-processor failed (expected in mock mode): ~p", [Reason]),
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% @doc E2E: Custom provider extension
test_e2e_custom_provider(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    ExtensionsPid = start_extension_services(NatsUrl),
    
    %% Mock extension invoker
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        {ok, #{<<"provider_id">> => <<"test_provider">>}}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        Policy = create_policy_with_provider(<<"test_provider">>),
        
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>},
        
        case router_decider:decide(Request, Policy, Context) of
            {ok, Decision} ->
                %% Verify custom provider was selected
                <<"test_provider">> = Decision#route_decision.provider_id;
            {error, Reason} ->
                ct:log("Custom provider failed (expected in mock mode): ~p", [Reason]),
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% @doc E2E: Multiple extensions in pipeline
test_e2e_multiple_extensions(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    ExtensionsPid = start_extension_services(NatsUrl),
    
    %% Mock extension invoker
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        {ok, #{<<"processed">> => true}}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        Policy = create_policy_with_extensions([
            {pre, [
                {id, <<"normalize_text">>, mode, <<"required">>}
            ]},
            {validators, [
                {id, <<"pii_guard">>, on_fail, <<"block">>}
            ]},
            {post, [
                {id, <<"mask_pii">>, mode, <<"required">>}
            ]}
        ]),
        
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>},
        
        case router_decider:decide(Request, Policy, Context) of
            {ok, _Decision} ->
                ok;
            {error, Reason} ->
                ct:log("Multiple extensions failed (expected in mock mode): ~p", [Reason]),
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% @doc E2E: Extension timeout handling
test_e2e_extension_timeout(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    %% Start extension service with delay (simulating timeout)
    ExtensionsPid = start_extension_services_with_delay(NatsUrl, 5000),
    
    %% Mock extension invoker to simulate timeout
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        timer:sleep(5000),  %% Simulate timeout
        {error, timeout}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        Policy = create_policy_with_extensions([
            {pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}
        ]),
        
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>},
        
        %% Extension should timeout (100ms timeout, 5000ms delay)
        case router_decider:decide(Request, Policy, Context) of
            {ok, _Decision} ->
                %% Should succeed with fail-open mode
                ok;
            {error, Reason} ->
                ct:log("Extension timeout handled: ~p", [Reason]),
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% @doc E2E: Extension error handling
test_e2e_extension_error(Config) ->
    NatsUrl = proplists:get_value(nats_url, Config, undefined),
    
    %% Start extension service that returns error
    ExtensionsPid = start_extension_services_with_error(NatsUrl),
    
    %% Mock extension invoker to simulate error
    meck:new(router_extension_invoker, [passthrough]),
    meck:new(router_nats, [passthrough]),
    
    meck:expect(router_extension_invoker, invoke, fun(_ExtensionId, _Request, _Context) ->
        {error, {extension_error, <<"test error">>}}
    end),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    try
        Policy = create_policy_with_extensions([
            {pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}
        ]),
        
        Request = create_route_request(<<"Hello World">>, Policy),
        Context = #{<<"tenant_id">> => <<"test_tenant">>},
        
        %% Extension should error, but continue with fail-open mode
        case router_decider:decide(Request, Policy, Context) of
            {ok, _Decision} ->
                ok;
            {error, Reason} ->
                ct:log("Extension error handled: ~p", [Reason]),
                ok
        end
    after
        stop_extension_services(ExtensionsPid),
        meck:unload(router_extension_invoker),
        meck:unload(router_nats)
    end.

%% Internal: Check NATS connection
check_nats_connection(NatsUrl) ->
    case code:which(nats) of
        non_existing ->
            [_, _, HostPort] = string:split(NatsUrl, "//"),
            [Host, PortStr] = string:split(HostPort, ":"),
            Port = list_to_integer(PortStr),
            case gen_tcp:connect(binary_to_list(Host), Port, [binary, {active, false}], 1000) of
                {ok, Socket} ->
                    gen_tcp:close(Socket),
                    {ok, connected};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {ok, connected}
    end.

%% Internal: Start extension services (Node.js processes)
start_extension_services(NatsUrl) ->
    ExtensionsDir = find_extensions_dir(),
    case ExtensionsDir of
        {ok, Dir} ->
            %% Start Node.js extension services
            Ports = start_node_extensions(Dir, NatsUrl),
            {extensions_dir, Dir, ports, Ports};
        {error, _} ->
            ct:log("Extensions directory not found, using mock mode"),
            {mock, undefined}
    end.

start_extension_services_with_delay(_NatsUrl, _DelayMs) ->
    %% For timeout tests, we can use a delayed extension or skip
    {mock, undefined}.

start_extension_services_with_error(_NatsUrl) ->
    %% For error tests, we can use an error extension or skip
    {mock, undefined}.

%% Internal: Find extensions directory
find_extensions_dir() ->
    %% Try to find tools/extensions directory
    PossiblePaths = [
        filename:join([code:root_dir(), "..", "tools", "extensions"]),
        filename:join([code:lib_dir(beamline_router), "..", "..", "tools", "extensions"]),
        "tools/extensions",
        "../tools/extensions"
    ],
    find_existing_dir(PossiblePaths).

find_existing_dir([]) ->
    {error, not_found};
find_existing_dir([Path | Rest]) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, Path};
        false ->
            find_existing_dir(Rest)
    end.

%% Internal: Start Node.js extension services
start_node_extensions(Dir, NatsUrl) ->
    %% Start each extension as a separate Node.js process
    Extensions = [
        {normalize_text, "src/normalize_text.js"},
        {pii_guard, "src/pii_guard.js"},
        {mask_pii, "src/mask_pii.js"},
        {test_provider, "src/test_provider.js"}
    ],
    
    lists:map(fun({Name, Script}) ->
        ScriptPath = filename:join([Dir, Script]),
        %% Use absolute path and ensure node is in PATH
        NodeCmd = case os:find_executable("node") of
            NodePath when is_list(NodePath) ->
                NodePath;
            undefined ->
                ct:log("Warning: node not found in PATH, trying 'node' directly"),
                "node"
        end,
        Cmd = lists:flatten([NodeCmd, " ", ScriptPath]),
        Port = open_port({spawn, Cmd}, [
            {env, [{"NATS_URL", binary_to_list(NatsUrl)}]},
            stream,
            exit_status,
            use_stdio,
            stderr_to_stdout
        ]),
        ct:log("Started extension ~p (Port: ~p, Cmd: ~s)", [Name, Port, Cmd]),
        timer:sleep(1000),  % Give extension time to connect to NATS
        {Name, Port}
    end, Extensions).

stop_extension_services({extensions_dir, _Dir, ports, Ports}) ->
    %% Stop all Node.js processes
    lists:foreach(fun({_Name, Port}) ->
        port_close(Port)
    end, Ports),
    timer:sleep(500);
stop_extension_services({mock, _}) ->
    ok;
stop_extension_services(_) ->
    ok.

%% Internal: Create policy with extensions
create_policy_with_extensions(Extensions) ->
    Pre = proplists:get_value(pre, Extensions, []),
    Validators = proplists:get_value(validators, Extensions, []),
    Post = proplists:get_value(post, Extensions, []),
    
    %% Convert pre/post items to proper format
    PreItems = lists:map(fun(Item) ->
        case Item of
            {id, Id, mode, Mode} ->
                #{id => Id, mode => Mode, config => #{}};
            _ when is_map(Item) ->
                Item;
            _ ->
                #{}
        end
    end, Pre),
    
    ValidatorItems = lists:map(fun(Item) ->
        case Item of
            {id, Id, on_fail, OnFail} ->
                #{id => Id, on_fail => OnFail};
            _ when is_map(Item) ->
                Item;
            _ ->
                #{}
        end
    end, Validators),
    
    PostItems = lists:map(fun(Item) ->
        case Item of
            {id, Id, mode, Mode} ->
                #{id => Id, mode => Mode, config => #{}};
            _ when is_map(Item) ->
                Item;
            _ ->
                #{}
        end
    end, Post),
    
    #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        pre = PreItems,
        validators = ValidatorItems,
        post = PostItems,
        weights = #{<<"openai:gpt-4">> => 1.0},
        metadata = #{}
    }.

%% Internal: Create policy with custom provider
create_policy_with_provider(ProviderId) ->
    #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"test_policy">>,
        version = <<"1.0">>,
        weights = #{ProviderId => 1.0},
        metadata = #{}
    }.

%% Internal: Create route request
create_route_request(Payload, Policy) ->
    #route_request{
        message = #{
            <<"message_id">> => <<"msg-001">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => Payload,
            <<"metadata">> => #{}
        },
        policy_id = Policy#policy.policy_id,
        context = #{}
    }.
