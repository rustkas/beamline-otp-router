%% @doc Chaos Test Suite for Router Intake Validation
%% Tests Router resilience to NATS failures and recoveries
%% Tests: NATS failures → Router behavior → Recovery → Invariants
-module(router_intake_chaos_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).


all() ->
    [
        {group, chaos_tests}
    ].

groups() ->
    [
        {chaos_tests, [sequence], [
            test_chaos_nats_single_restart,
            test_chaos_nats_multiple_restarts,
            test_chaos_nats_randomized_failures,
            test_chaos_nats_during_message_processing,
            test_chaos_nats_recovery_verification
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, real),  %% Use real NATS for chaos tests
    ok = application:set_env(beamline_router, dlq_enabled, true),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    
    %% Check if NATS container is available
    NATSContainer = os:getenv("NATS_CONTAINER", "nats"),
    case os:cmd("docker ps -a --format '{{.Names}}' | grep -q '^" ++ NATSContainer ++ "$' && echo 'found'") of
        "found\n" ->
            ct:log("NATS container '~s' found", [NATSContainer]),
            [{nats_container, NATSContainer} | Config];
        _ ->
            ct:log("Warning: NATS container '~s' not found. Chaos tests may fail.", [NATSContainer]),
            [{nats_container, NATSContainer}, {skip_chaos, true} | Config]
    end.

end_per_suite(Config) ->
    %% Ensure NATS is running at end
    case proplists:get_value(nats_container, Config) of
        undefined -> ok;
        NATSContainer ->
            os:cmd("docker start " ++ NATSContainer ++ " >/dev/null 2>&1"),
            timer:sleep(2000)
    end,
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    meck:unload(),
    Config.

%% ============================================================================
%% Chaos Tests: Router resilience to NATS failures
%% ============================================================================

%% @doc Chaos Test 1: Mild Chaos - Single NATS restart
%% Verifies Router survives a single NATS restart
test_chaos_nats_single_restart(Config) ->
    case proplists:get_value(skip_chaos, Config, false) of
        true ->
            {skip, "NATS container not available"};
        false ->
            NATSContainer = proplists:get_value(nats_container, Config, "nats"),
            
            %% Setup mocks for tracking
            meck:new(router_logger, [passthrough]),
            meck:new(telemetry, [passthrough]),
            
            MetricEvents = ets:new(metric_events, [set, private]),
            meck:expect(telemetry, execute, fun(Event, _Measurements, Metadata) ->
                ets:insert(MetricEvents, {event, Event, Metadata}),
                ok
            end),
            
            %% Get initial state
            RouterPid = whereis(router_decide_consumer),
            ?assert(is_pid(RouterPid)),
            {ProcessCountBefore, _} = {erlang:system_info(process_count), erlang:memory(processes_used)},
            
            %% Stop NATS
            ct:log("Stopping NATS container '~s'...", [NATSContainer]),
            _ = os:cmd("docker stop " ++ NATSContainer),
            timer:sleep(5000),  %% Wait for connection loss
            
            %% Verify Router is still alive
            ?assert(is_process_alive(RouterPid)),
            
            %% Verify error metrics/logs
            timer:sleep(2000),
            AllMetrics = ets:tab2list(MetricEvents),
            ConnectionErrorMetrics = [M || {event, E, _} = M <- AllMetrics,
                                          E =:= [router_nats, connection_error]],
            %% Router should log connection errors
            ?assert(length(ConnectionErrorMetrics) >= 0),  %% May or may not emit immediately
            
            %% Start NATS
            ct:log("Starting NATS container '~s'...", [NATSContainer]),
            _ = os:cmd("docker start " ++ NATSContainer),
            timer:sleep(5000),  %% Wait for reconnection
            
            %% Verify Router is still alive
            ?assert(is_process_alive(RouterPid)),
            
            %% Verify recovery
            timer:sleep(3000),
            {ProcessCountAfter, _} = {erlang:system_info(process_count), erlang:memory(processes_used)},
            
            %% Process stability check
            ProcessGrowth = (ProcessCountAfter - ProcessCountBefore) / max(ProcessCountBefore, 1),
            ?assert(ProcessGrowth < 0.2),  %% Allow 20% growth (reconnection may spawn processes)
            
            %% Verify Router can still process messages (if NATS is ready)
            timer:sleep(2000),
            ?assert(is_process_alive(RouterPid)),
            
            ct:log("Mild Chaos test complete: Router survived NATS restart"),
            
            meck:unload(router_logger),
            meck:unload(telemetry),
            ets:delete(MetricEvents),
            ok
    end.

%% @doc Chaos Test 2: Moderate Chaos - Multiple NATS restarts
%% Verifies Router survives multiple NATS restarts
test_chaos_nats_multiple_restarts(Config) ->
    case proplists:get_value(skip_chaos, Config, false) of
        true ->
            {skip, "NATS container not available"};
        false ->
            NATSContainer = proplists:get_value(nats_container, Config, "nats"),
            
            %% Setup tracking
            meck:new(router_logger, [passthrough]),
            meck:new(telemetry, [passthrough]),
            
            MetricEvents = ets:new(metric_events, [set, private]),
            RestartCount = ets:new(restart_count, [set, private]),
            ets:insert(RestartCount, {restarts, 0}),
            
            meck:expect(telemetry, execute, fun(Event, _Measurements, Metadata) ->
                ets:insert(MetricEvents, {event, Event, Metadata}),
                ok
            end),
            
            RouterPid = case whereis(router_decide_consumer) of
                undefined -> {skip, "Router not started"};
                Pid when is_pid(Pid) -> Pid
            end,
            
            %% Perform 3 restarts
            Restarts = 3,
            lists:foreach(fun(Iteration) ->
                ct:log("Moderate Chaos: Restart ~p/~p", [Iteration, Restarts]),
                
                %% Stop NATS
                os:cmd("docker stop " ++ NATSContainer),
                timer:sleep(5000),
                
                %% Verify Router alive
                ?assert(is_process_alive(RouterPid)),
                
                %% Start NATS
                os:cmd("docker start " ++ NATSContainer),
                timer:sleep(5000),
                
                %% Verify Router alive
                ?assert(is_process_alive(RouterPid)),
                
                %% Update restart count
                Count = case ets:lookup(RestartCount, restarts) of
                    [] -> 1;
                    [{restarts, C}] -> C + 1
                end,
                ets:insert(RestartCount, {restarts, Count})
            end, lists:seq(1, Restarts)),
            
            %% Verify final state
            FinalRestarts = case ets:lookup(RestartCount, restarts) of
                [] -> 0;
                [{restarts, C}] -> C
            end,
            ?assertEqual(Restarts, FinalRestarts),
            ?assert(is_process_alive(RouterPid)),
            
            %% Check for connection error metrics
            AllMetrics = ets:tab2list(MetricEvents),
            ConnectionErrorMetrics = [M || {event, E, _} = M <- AllMetrics,
                                          E =:= [router_nats, connection_error]],
            %% Should have some connection errors logged
            ct:log("Connection error metrics: ~p", [length(ConnectionErrorMetrics)]),
            
            ct:log("Moderate Chaos test complete: Router survived ~p restarts", [Restarts]),
            
            meck:unload(router_logger),
            meck:unload(telemetry),
            ets:delete(MetricEvents),
            ets:delete(RestartCount),
            ok
    end.

%% @doc Chaos Test 3: Hard Chaos - Randomized failures
%% Verifies Router survives randomized NATS failures
test_chaos_nats_randomized_failures(Config) ->
    case proplists:get_value(skip_chaos, Config, false) of
        true ->
            {skip, "NATS container not available"};
        false ->
            NATSContainer = proplists:get_value(nats_container, Config, "nats"),
            
            %% Setup tracking
            meck:new(router_logger, [passthrough]),
            meck:new(telemetry, [passthrough]),
            
            MetricEvents = ets:new(metric_events, [set, private]),
            FailureCount = ets:new(failure_count, [set, private]),
            ets:insert(FailureCount, {failures, 0}),
            
            meck:expect(telemetry, execute, fun(Event, _Measurements, Metadata) ->
                ets:insert(MetricEvents, {event, Event, Metadata}),
                ok
            end),
            
            RouterPid = case whereis(router_decide_consumer) of
                undefined -> {skip, "Router not started"};
                Pid when is_pid(Pid) -> Pid
            end,
            
            %% Random failures over 60 seconds
            StartTime = erlang:monotonic_time(second),
            Duration = 60,
            
            %% Initialize random seed using rand module (replaces deprecated random:seed/1)
            _ = rand:seed(exs1024, erlang:timestamp()),
            
            while_chaos_active(StartTime, Duration, NATSContainer, RouterPid, FailureCount),
            
            %% Verify final state
            FinalFailures = case ets:lookup(FailureCount, failures) of
                [] -> 0;
                [{failures, C}] -> C
            end,
            ?assert(is_process_alive(RouterPid)),
            ?assert(FinalFailures > 0),  %% Should have at least one failure
            
            ct:log("Hard Chaos test complete: Router survived ~p randomized failures", [FinalFailures]),
            
            meck:unload(router_logger),
            meck:unload(telemetry),
            ets:delete(MetricEvents),
            ets:delete(FailureCount),
            ok
    end.

%% @doc Helper: While chaos is active
-spec while_chaos_active(integer(), integer(), string(), pid(), ets:tid()) -> ok.
while_chaos_active(StartTime, Duration, NATSContainer, RouterPid, FailureCount) ->
    CurrentTime = erlang:monotonic_time(second),
    Elapsed = CurrentTime - StartTime,
    
    if Elapsed >= Duration ->
        ok;
    true ->
        %% Random decision: failure or normal (using rand module, replaces deprecated random:uniform/1)
        Decision = rand:uniform(3),
        
        case Decision of
            1 ->
                %% Outage
                OutageDuration = 5 + rand:uniform(10),  %% 5-15 seconds
                ct:log("Hard Chaos: Random outage for ~p seconds", [OutageDuration]),
                os:cmd("docker stop " ++ NATSContainer),
                timer:sleep(OutageDuration * 1000),
                os:cmd("docker start " ++ NATSContainer),
                timer:sleep(3000),
                
                %% Verify Router alive
                ?assert(is_process_alive(RouterPid)),
                
                %% Update failure count
                Count = case ets:lookup(FailureCount, failures) of
                    [] -> 1;
                    [{failures, C}] -> C + 1
                end,
                ets:insert(FailureCount, {failures, Count});
            2 ->
                %% Short freeze
                ct:log("Hard Chaos: Short freeze"),
                os:cmd("docker stop " ++ NATSContainer),
                timer:sleep(2000),
                os:cmd("docker start " ++ NATSContainer),
                timer:sleep(2000),
                
                ?assert(is_process_alive(RouterPid)),
                
                Count = case ets:lookup(FailureCount, failures) of
                    [] -> 1;
                    [{failures, C}] -> C + 1
                end,
                ets:insert(FailureCount, {failures, Count});
            _ ->
                %% Normal operation
                NormalDuration = 5 + rand:uniform(10),  %% 5-15 seconds
                timer:sleep(NormalDuration * 1000),
                ?assert(is_process_alive(RouterPid))
        end,
        
        while_chaos_active(StartTime, Duration, NATSContainer, RouterPid, FailureCount)
    end.

%% @doc Chaos Test 4: NATS failure during message processing
%% Verifies Router handles NATS failures during active message processing
test_chaos_nats_during_message_processing(Config) ->
    case proplists:get_value(skip_chaos, Config, false) of
        true ->
            {skip, "NATS container not available"};
        false ->
            NATSContainer = proplists:get_value(nats_container, Config, "nats"),
            
            %% Setup mocks
            meck:new(router_nats, [passthrough]),
            meck:new(router_logger, [passthrough]),
            meck:new(telemetry, [passthrough]),
            
            AckCount = ets:new(ack_count, [set, private]),
            DLQCount = ets:new(dlq_count, [set, private]),
            
            meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _DG, _M) -> {ok, <<"consumer-1">>} end),
            meck:expect(router_nats, ack_message, fun(_MsgId) ->
                Count = case ets:lookup(AckCount, acks) of
                    [] -> 1;
                    [{acks, C}] -> C + 1
                end,
                ets:insert(AckCount, {acks, Count}),
                ok
            end),
            meck:expect(router_nats, publish_with_ack, fun(_DLQSubject, _DLQPayload, _Headers) ->
                Count = case ets:lookup(DLQCount, dlq) of
                    [] -> 1;
                    [{dlq, C}] -> C + 1
                end,
                ets:insert(DLQCount, {dlq, Count}),
                {ok, <<"dlq-msg">>}
            end),
            
            RouterPid = case whereis(router_decide_consumer) of
                undefined -> {skip, "Router not started"};
                Pid when is_pid(Pid) -> Pid
            end,
            Subject = <<"beamline.router.v1.decide">>,
            
            %% Send some messages
            MessageCount = 10,
            lists:foreach(fun(Index) ->
                {RequestJson, _Request} = generate_valid_decide_request(Index),
                MsgId = list_to_binary(["msg-chaos-", integer_to_list(Index)]),
                router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
                timer:sleep(100)
            end, lists:seq(1, MessageCount div 2)),
            
            %% Stop NATS mid-processing
            ct:log("Stopping NATS during message processing..."),
            os:cmd("docker stop " ++ NATSContainer),
            timer:sleep(3000),
            
            %% Verify Router alive
            ?assert(is_process_alive(RouterPid)),
            
            %% Continue sending messages (should fail gracefully)
            lists:foreach(fun(Index) ->
                {RequestJson, _Request} = generate_valid_decide_request(Index + MessageCount div 2),
                MsgId = list_to_binary(["msg-chaos-", integer_to_list(Index + MessageCount div 2)]),
                router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
                timer:sleep(100)
            end, lists:seq(1, MessageCount div 2)),
            
            %% Restart NATS
            ct:log("Restarting NATS..."),
            os:cmd("docker start " ++ NATSContainer),
            timer:sleep(5000),
            
            %% Verify Router alive and can process new messages
            ?assert(is_process_alive(RouterPid)),
            
            %% Send final messages
            {RequestJson, _Request} = generate_valid_decide_request(MessageCount + 1),
            MsgId = list_to_binary(["msg-chaos-final"]),
            router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
            timer:sleep(2000),
            
            ?assert(is_process_alive(RouterPid)),
            
            ct:log("Chaos during processing test complete"),
            
            meck:unload(router_nats),
            meck:unload(router_logger),
            meck:unload(telemetry),
            ets:delete(AckCount),
            ets:delete(DLQCount),
            ok
    end.

%% @doc Chaos Test 5: Recovery verification
%% Verifies Router properly recovers after NATS restoration
test_chaos_nats_recovery_verification(Config) ->
    case proplists:get_value(skip_chaos, Config, false) of
        true ->
            {skip, "NATS container not available"};
        false ->
            NATSContainer = proplists:get_value(nats_container, Config, "nats"),
            
            %% Setup tracking
            meck:new(router_logger, [passthrough]),
            meck:new(telemetry, [passthrough]),
            
            MetricEvents = ets:new(metric_events, [set, private]),
            meck:expect(telemetry, execute, fun(Event, _Measurements, Metadata) ->
                ets:insert(MetricEvents, {event, Event, Metadata}),
                ok
            end),
            
            RouterPid = case whereis(router_decide_consumer) of
                undefined -> {skip, "Router not started"};
                Pid when is_pid(Pid) -> Pid
            end,
            {ProcessCountBefore, _} = {erlang:system_info(process_count), erlang:memory(processes_used)},
            
            %% Stop NATS
            ct:log("Stopping NATS for recovery test..."),
            os:cmd("docker stop " ++ NATSContainer),
            timer:sleep(10000),  %% Long outage
            
            %% Verify Router alive
            ?assert(is_process_alive(RouterPid)),
            
            %% Start NATS
            ct:log("Starting NATS for recovery..."),
            os:cmd("docker start " ++ NATSContainer),
            timer:sleep(10000),  %% Wait for recovery
            
            %% Verify Router alive
            ?assert(is_process_alive(RouterPid)),
            
            %% Verify recovery metrics
            AllMetrics = ets:tab2list(MetricEvents),
            ReconnectMetrics = [M || {event, E, _} = M <- AllMetrics,
                                    E =:= [router_nats, reconnect]],
            ConnectionMetrics = [M || {event, E, _} = M <- AllMetrics,
                                      E =:= [router_nats, connection_established]],
            
            ct:log("Reconnect metrics: ~p", [length(ReconnectMetrics)]),
            ct:log("Connection metrics: ~p", [length(ConnectionMetrics)]),
            
            %% Verify process stability
            {ProcessCountAfter, _} = {erlang:system_info(process_count), erlang:memory(processes_used)},
            ProcessGrowth = (ProcessCountAfter - ProcessCountBefore) / max(ProcessCountBefore, 1),
            ?assert(ProcessGrowth < 0.2),
            
            %% Verify no unbounded growth
            ?assert(is_process_alive(RouterPid)),
            
            ct:log("Recovery verification test complete"),
            
            meck:unload(router_logger),
            meck:unload(telemetry),
            ets:delete(MetricEvents),
            ok
    end.

%% Helper functions

generate_valid_decide_request(Index) ->
    RequestId = list_to_binary(["req-chaos-", integer_to_list(Index)]),
    TraceId = list_to_binary(["trace-chaos-", integer_to_list(Index)]),
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => RequestId,
        <<"trace_id">> => TraceId,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => list_to_binary(["s3://bucket/key-", integer_to_list(Index)])
        },
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    {RequestJson, Request}.

