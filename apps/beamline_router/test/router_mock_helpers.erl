-module(router_mock_helpers).

-export([ensure_mock/2, unload/1, unload_all/0, unload_all/1, ensure_test_deps/0, reset/1]).
-export([expect/3, expect/4, reset_all/1]).
-export([assert_no_mocks/0, get_mocked_modules/0]).
-export([cleanup_and_verify/0]).
-export([setup_router_nats_mock/0, setup_router_nats_mock/1]).
%% Helper functions for fault injection - must be exported for meck callbacks
-export([check_fault_injection/2, emit_mock_failure_metric/3]).
%% Helper functions for fail-open mode - must be exported for meck callbacks
-export([apply_fail_open_mode/1, apply_fail_open_mode_ack/1]).
%% Mock state management functions
-export([get_mock_connection_state/0, set_mock_connection_state/1, init_mock_state_table/0]).

%% @doc Convenience wrapper for meck:expect/3
expect(Module, Function, Fun) when is_function(Fun) ->
    ok = ensure_test_deps(),
    meck:expect(Module, Function, Fun).

%% @doc Convenience wrapper for meck:expect/4
expect(Module, Function, Arity, Fun) when is_integer(Arity), is_function(Fun) ->
    ok = ensure_test_deps(),
    meck:expect(Module, Function, Arity, Fun).

%% @doc Reset multiple modules at once (safe)
reset_all(Modules) when is_list(Modules) ->
    lists:foreach(fun reset/1, Modules),
    ok.

%% @doc Ensure test dependencies (meck, proper) are loaded
ensure_test_deps() ->
    case code:ensure_loaded(meck) of
        {module, meck} -> ok;
        {error, _} ->
            %% Try multiple strategies to find test libs
            load_test_deps_from_build_dir(),
            %% Verify meck is now available
            case code:ensure_loaded(meck) of
                {module, meck} -> ok;
                {error, Reason} -> {error, {meck_not_found, Reason}}
            end
    end.

load_test_deps_from_build_dir() ->
    %% Strategy 1: Try from current working directory
    {ok, Cwd} = file:get_cwd(),
    TestLibDir1 = filename:join([Cwd, "_build", "test", "lib"]),
    case filelib:is_dir(TestLibDir1) of
        true ->
            load_libs_from_dir(TestLibDir1);
        false ->
            %% Strategy 2: Hardcoded project path as fallback
            HardcodedPath = "/home/rustkas/aigroup/apps/otp/router/_build/test/lib",
            case filelib:is_dir(HardcodedPath) of
                true ->
                    load_libs_from_dir(HardcodedPath);
                false ->
                    %% Strategy 3: Use beamline_router app dir
                    case code:lib_dir(beamline_router) of
                        {error, _} ->
                            ok;
                        AppDir ->
                            TestLibDir2 = filename:dirname(AppDir),
                            load_libs_from_dir(TestLibDir2)
                    end
            end
    end.

load_libs_from_dir(LibDir) ->
    case filelib:is_dir(LibDir) of
        true ->
            Pattern = filename:join([LibDir, "*", "ebin"]),
            Libs = filelib:wildcard(Pattern),
            lists:foreach(fun(P) -> code:add_pathz(P) end, Libs);
        false ->
            ok
    end.

%% Ensure meck is started for Module only once
ensure_mock(Module, Options) ->
    ok = ensure_test_deps(),
    %% meck:mocked() returns list of mocked modules
    case lists:member(Module, meck:mocked()) of
        true -> ok;
        false -> meck:new(Module, Options)
    end.

reset(Module) ->
    ok = ensure_test_deps(),
    case lists:member(Module, meck:mocked()) of
        true ->
            catch meck:reset(Module),
            ok;
        false ->
            ok
    end.

unload(Module) ->
    case lists:member(Module, meck:mocked()) of
        true -> catch meck:unload(Module), ok;
        false -> ok
    end.

unload_all() ->
    catch meck:unload(),
    ok.

unload_all(Modules) when is_list(Modules) ->
    lists:foreach(fun unload/1, Modules),
    ok.

%% @doc Get list of currently mocked modules
get_mocked_modules() ->
    ok = ensure_test_deps(),
    meck:mocked().

%% @doc Assert no mocks are active - useful in end_per_testcase/end_per_suite
%% Returns ok if no mocks, throws error with list of stray mocks otherwise
assert_no_mocks() ->
    ok = ensure_test_deps(),
    case meck:mocked() of
        [] -> ok;
        StrayMocks ->
            %% Clean them up first
            catch meck:unload(),
            error({stray_mocks_found, StrayMocks})
    end.

%% @doc Setup router_nats mock without passthrough to avoid noproc on gen_server:call.
%% This is the SAFE way to mock router_nats in tests.
%% IMPORTANT: Do NOT use passthrough for router_nats because:
%% - publish_with_ack, nak_message, ack_message, subscribe_jetstream, request
%%   all call gen_server:call(router_nats, ...) which fails if the real process is down.
-spec setup_router_nats_mock() -> ok.
setup_router_nats_mock() ->
    setup_router_nats_mock(#{}).

-spec setup_router_nats_mock(map()) -> ok.
setup_router_nats_mock(Overrides) ->
    ok = ensure_test_deps(),
    %% Robust cleanup: unload meck if exists (with retry)
    unload_router_nats_mock(),
    %% Kill and unregister any stale router_nats process
    cleanup_router_nats_process(),
    %% Create mock WITHOUT passthrough (with robust retry)
    create_router_nats_mock(),
    %% Initialize mock state table for connection state tracking
    init_mock_state_table(),
    %% Stub start_link/0 - creates a NEW process each time for supervisor compatibility
    %% The supervisor expects start_link to return a new pid each time
    %% IMPORTANT: The process must handle gen_server protocol for fallback scenarios
    meck:expect(router_nats, start_link,
        maps:get(start_link, Overrides, fun() ->
            %% Cleanup any existing process first
            case whereis(router_nats) of
                undefined -> ok;
                OldPid ->
                    _ = (catch unregister(router_nats)),
                    _ = (catch exit(OldPid, kill)),
                    timer:sleep(10)
            end,
            %% Create and register new process that handles gen_server protocol
            %% This is a fallback for when meck is unloaded but process still exists
            NewPid = spawn(fun() -> mock_gen_server_loop() end),
            register(router_nats, NewPid),
            {ok, NewPid}
        end)),
    %% Setup safe stubs for all functions that call gen_server:call(router_nats,...)
    %% These stubs check fault injection first, then return mock response
    meck:expect(router_nats, publish_with_ack,
        maps:get(publish_with_ack, Overrides,
            fun(Subject, Payload, _Headers) -> 
                %% First check connection state
                case router_mock_helpers:get_mock_connection_state() of
                    State when State =:= disconnected; State =:= reconnecting ->
                        %% Not connected - emit failure and apply fail-open
                        router_mock_helpers:emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, not_connected),
                        router_mock_helpers:apply_fail_open_mode_ack(not_connected);
                    _ ->
                        %% Connected - check fault injection
                        case router_mock_helpers:check_fault_injection(publish_with_ack, #{subject => Subject, payload => Payload}) of
                            {true, {error, Reason}} ->
                                router_mock_helpers:emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, Reason),
                                router_mock_helpers:apply_fail_open_mode_ack(Reason);
                            {true, timeout} ->
                                router_mock_helpers:emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, timeout),
                                router_mock_helpers:apply_fail_open_mode_ack(timeout);
                            {true, close_connection} ->
                                router_mock_helpers:emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, connection_closed),
                                router_mock_helpers:apply_fail_open_mode_ack(connection_closed);
                            false ->
                                {ok, <<"mock-msg-id">>}
                        end
                end
            end)),
    meck:expect(router_nats, nak_message, 
        maps:get(nak_message, Overrides, fun(MsgId) -> 
            case router_mock_helpers:check_fault_injection(nak, #{msg_id => MsgId}) of
                {true, {error, Reason}} -> {error, Reason};
                {true, _} -> {error, fault_injected};
                false -> ok
            end
        end)),
    meck:expect(router_nats, ack_message, 
        maps:get(ack_message, Overrides, fun(MsgId) -> 
            case router_mock_helpers:check_fault_injection(ack, #{msg_id => MsgId}) of
                {true, {error, Reason}} -> {error, Reason};
                {true, _} -> {error, fault_injected};
                false -> ok
            end
        end)),
    meck:expect(router_nats, subscribe_jetstream, 
        maps:get(subscribe_jetstream, Overrides, 
            fun(Subj, _Stream, _Ack, _Dur, _Mode) -> 
                case router_mock_helpers:check_fault_injection(subscribe, #{subject => Subj}) of
                    {true, {error, Reason}} -> {error, Reason};
                    {true, _} -> {error, fault_injected};
                    false -> {ok, <<"mock-consumer">>}
                end
            end)),
    meck:expect(router_nats, publish, 
        maps:get(publish, Overrides, fun(Subject, Payload) -> 
            %% First check connection state
            case router_mock_helpers:get_mock_connection_state() of
                State when State =:= disconnected; State =:= reconnecting ->
                    %% Not connected - emit failure and apply fail-open
                    router_mock_helpers:emit_mock_failure_metric(router_nats_publish_failures_total, Subject, not_connected),
                    router_mock_helpers:apply_fail_open_mode(not_connected);
                _ ->
                    %% Connected - check fault injection
                    case router_mock_helpers:check_fault_injection(publish, #{subject => Subject, payload => Payload}) of
                        {true, {error, Reason}} ->
                            router_mock_helpers:emit_mock_failure_metric(router_nats_publish_failures_total, Subject, Reason),
                            router_mock_helpers:apply_fail_open_mode(Reason);
                        {true, timeout} ->
                            router_mock_helpers:emit_mock_failure_metric(router_nats_publish_failures_total, Subject, timeout),
                            router_mock_helpers:apply_fail_open_mode(timeout);
                        {true, close_connection} ->
                            router_mock_helpers:emit_mock_failure_metric(router_nats_publish_failures_total, Subject, connection_closed),
                            router_mock_helpers:apply_fail_open_mode(connection_closed);
                        false ->
                            ok
                    end
            end
        end)),
    meck:expect(router_nats, request, 
        maps:get(request, Overrides, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end)),
    meck:expect(router_nats, subscribe, 
        maps:get(subscribe, Overrides, fun(_Subj, _Callback, _Timeout) -> ok end)),
    meck:expect(router_nats, get_connection_status, 
        maps:get(get_connection_status, Overrides, fun() ->
            FailOpenMode = application:get_env(beamline_router, nats_fail_open_mode, false),
            State = router_mock_helpers:get_mock_connection_state(),
            {ok, #{state => State, fail_open_mode => FailOpenMode}}
        end)),
    %% Additional stubs for functions used in tests
    meck:expect(router_nats, reconnect, 
        maps:get(reconnect, Overrides, fun() -> ok end)),
    meck:expect(router_nats, get_connection_health, 
        maps:get(get_connection_health, Overrides, fun() -> #{status => healthy, latency_ms => 1} end)),
    meck:expect(router_nats, get_jetstream_consumers, 
        maps:get(get_jetstream_consumers, Overrides, fun() -> [] end)),
    case erlang:function_exported(router_nats, simulate_connection_lost, 0) of
        true ->
            meck:expect(router_nats, simulate_connection_lost,
                maps:get(simulate_connection_lost, Overrides, fun() -> ok end));
        false ->
            ok
    end,
    ok.

%% @doc Create router_nats mock with robust retry
%% Uses no_link to prevent mock from dying when init_per_suite process exits
create_router_nats_mock() ->
    create_router_nats_mock(3).

create_router_nats_mock(0) ->
    error(failed_to_create_router_nats_mock);
create_router_nats_mock(Retries) ->
    try
        meck:new(router_nats, [no_link])
    catch
        error:{already_started, _} ->
            %% Force cleanup and retry
            force_cleanup_router_nats_mock(),
            create_router_nats_mock(Retries - 1);
        _:_ ->
            %% Any other error - try cleanup and retry
            force_cleanup_router_nats_mock(),
            create_router_nats_mock(Retries - 1)
    end.

%% @doc Force cleanup of router_nats mock and process
force_cleanup_router_nats_mock() ->
    %% Kill the meck process directly if it exists
    case whereis(router_nats_meck) of
        undefined -> ok;
        MeckPid ->
            exit(MeckPid, kill),
            timer:sleep(20)
    end,
    %% Unload via meck API
    _ = (catch meck:unload(router_nats)),
    timer:sleep(30),
    %% Clean up the registered process
    cleanup_router_nats_process().

%% @doc Unload router_nats mock safely
unload_router_nats_mock() ->
    case lists:member(router_nats, catch meck:mocked()) of
        true ->
            _ = (catch meck:unload(router_nats)),
            timer:sleep(30);
        false ->
            ok
    end,
    %% Double-check and force unload if still there
    case lists:member(router_nats, catch meck:mocked()) of
        true ->
            _ = (catch meck:unload(router_nats)),
            timer:sleep(30);
        false ->
            ok
    end.

%% @doc Cleanup any stale router_nats process
cleanup_router_nats_process() ->
    case whereis(router_nats) of
        undefined -> ok;
        OldPid ->
            _ = (catch unregister(router_nats)),
            _ = (catch exit(OldPid, kill)),
            %% Wait for process to die
            wait_for_process_death(OldPid, 100)
    end.

%% @doc Wait for a process to terminate
wait_for_process_death(Pid, TimeoutMs) when TimeoutMs > 0 ->
    case is_process_alive(Pid) of
        false -> ok;
        true ->
            timer:sleep(10),
            wait_for_process_death(Pid, TimeoutMs - 10)
    end;
wait_for_process_death(_, _) -> ok.

%% @doc Safe register with cleanup if name already taken
safe_register(Name, Pid) ->
    case whereis(Name) of
        undefined ->
            try
                register(Name, Pid),
                ok
            catch
                error:badarg ->
                    %% Name was taken between check and register - retry
                    cleanup_router_nats_process(),
                    register(Name, Pid),
                    ok
            end;
        ExistingPid when ExistingPid =:= Pid ->
            %% Already registered with this pid
            ok;
        _OtherPid ->
            %% Different pid registered - clean up and register ours
            cleanup_router_nats_process(),
            register(Name, Pid),
            ok
    end.

%% @doc Mock gen_server loop that handles gen_server protocol
%% This is a fallback when meck is unloaded but the registered process still exists.
%% Handles common router_nats gen_server calls with safe mock responses.
%% IMPORTANT: Must check fault injection for operations to support tests.
%% IMPORTANT: Must respect fail_open_mode from application env for fail-open tests.
%% IMPORTANT: Must track connection state for fail-open tests that simulate connection loss.
mock_gen_server_loop() ->
    receive
        {'$gen_call', From, get_connection_status} ->
            %% Read connection state from ETS table, default to connected
            State = get_mock_connection_state(),
            FailOpenMode = application:get_env(beamline_router, nats_fail_open_mode, false),
            gen_server:reply(From, {ok, #{state => State, fail_open_mode => FailOpenMode}}),
            mock_gen_server_loop();
        {'$gen_call', From, {publish, Subject, Payload}} ->
            Result = handle_publish_with_fault_injection(Subject, Payload),
            gen_server:reply(From, Result),
            mock_gen_server_loop();
        {'$gen_call', From, {publish_with_ack, Subject, Payload, _Headers}} ->
            Result = handle_publish_with_ack_with_fault_injection(Subject, Payload),
            gen_server:reply(From, Result),
            mock_gen_server_loop();
        {'$gen_call', From, {ack_message, MsgId}} ->
            Result = case check_fault_injection(ack, #{msg_id => MsgId}) of
                {true, {error, Reason}} -> {error, Reason};
                {true, _} -> {error, fault_injected};
                false -> ok
            end,
            gen_server:reply(From, Result),
            mock_gen_server_loop();
        {'$gen_call', From, {nak_message, MsgId}} ->
            Result = case check_fault_injection(nak, #{msg_id => MsgId}) of
                {true, {error, Reason}} -> {error, Reason};
                {true, _} -> {error, fault_injected};
                false -> ok
            end,
            gen_server:reply(From, Result),
            mock_gen_server_loop();
        {'$gen_call', From, reconnect} ->
            %% Reconnect resets connection state to connected
            set_mock_connection_state(connected),
            gen_server:reply(From, {ok, connected}),
            mock_gen_server_loop();
        {'$gen_call', From, {get_connection_health}} ->
            gen_server:reply(From, {ok, healthy}),
            mock_gen_server_loop();
        {'$gen_call', From, _Request} ->
            %% Catch-all for unknown calls - return ok
            gen_server:reply(From, ok),
            mock_gen_server_loop();
        {'$gen_cast', {connection_lost, _Reason}} ->
            %% Handle connection_lost cast - update mock state to disconnected
            set_mock_connection_state(disconnected),
            mock_gen_server_loop();
        {'$gen_cast', {connection_restored, _Pid}} ->
            %% Handle connection_restored cast - update mock state to connected
            set_mock_connection_state(connected),
            mock_gen_server_loop();
        {'$gen_cast', _Msg} ->
            mock_gen_server_loop();
        stop ->
            ok;
        _Other ->
            mock_gen_server_loop()
    end.

%% @doc Handle publish with fault injection for gen_server fallback
%% Also checks connection state - if disconnected, treats as not_connected error
-spec handle_publish_with_fault_injection(binary(), binary()) -> ok | {error, term()}.
handle_publish_with_fault_injection(Subject, Payload) ->
    %% First check connection state
    case get_mock_connection_state() of
        State when State =:= disconnected; State =:= reconnecting ->
            %% Not connected - emit failure and apply fail-open
            emit_mock_failure_metric(router_nats_publish_failures_total, Subject, not_connected),
            apply_fail_open_mode(not_connected);
        _ ->
            %% Connected - check fault injection
            case check_fault_injection(publish, #{subject => Subject, payload => Payload}) of
                {true, {error, Reason}} ->
                    emit_mock_failure_metric(router_nats_publish_failures_total, Subject, Reason),
                    apply_fail_open_mode(Reason);
                {true, timeout} ->
                    emit_mock_failure_metric(router_nats_publish_failures_total, Subject, timeout),
                    apply_fail_open_mode(timeout);
                {true, close_connection} ->
                    emit_mock_failure_metric(router_nats_publish_failures_total, Subject, connection_closed),
                    apply_fail_open_mode(connection_closed);
                false ->
                    ok
            end
    end.

%% @doc Handle publish_with_ack with fault injection for gen_server fallback
%% Also checks connection state - if disconnected, treats as not_connected error
-spec handle_publish_with_ack_with_fault_injection(binary(), binary()) -> {ok, binary()} | {error, term()}.
handle_publish_with_ack_with_fault_injection(Subject, Payload) ->
    %% First check connection state
    case get_mock_connection_state() of
        State when State =:= disconnected; State =:= reconnecting ->
            %% Not connected - emit failure and apply fail-open
            emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, not_connected),
            apply_fail_open_mode_ack(not_connected);
        _ ->
            %% Connected - check fault injection
            case check_fault_injection(publish_with_ack, #{subject => Subject, payload => Payload}) of
                {true, {error, Reason}} ->
                    emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, Reason),
                    apply_fail_open_mode_ack(Reason);
                {true, timeout} ->
                    emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, timeout),
                    apply_fail_open_mode_ack(timeout);
                {true, close_connection} ->
                    emit_mock_failure_metric(router_nats_publish_with_ack_failures_total, Subject, connection_closed),
                    apply_fail_open_mode_ack(connection_closed);
                false ->
                    {ok, <<"mock-msg-id">>}
            end
    end.

%% @doc Apply fail-open mode for publish: return ok if fail_open_mode enabled, else error
-spec apply_fail_open_mode(term()) -> ok | {error, term()}.
apply_fail_open_mode(Reason) ->
    case application:get_env(beamline_router, nats_fail_open_mode, false) of
        true -> ok;
        false -> {error, Reason}
    end.

%% @doc Apply fail-open mode for publish_with_ack: return {ok, stub-msg-id} if fail_open_mode enabled
-spec apply_fail_open_mode_ack(term()) -> {ok, binary()} | {error, term()}.
apply_fail_open_mode_ack(Reason) ->
    case application:get_env(beamline_router, nats_fail_open_mode, false) of
        true -> {ok, <<"stub-msg-id">>};
        false -> {error, Reason}
    end.

%% @doc Safe cleanup for end_per_testcase: unload all and verify none remain
%% Returns ok always, but logs warning if stray mocks were found.
%% This is the recommended cleanup function for CP1 suites.
cleanup_and_verify() ->
    ok = ensure_test_deps(),
    catch meck:unload(),
    case meck:mocked() of
        [] -> ok;
        Remaining ->
            io:format(user, "[ERROR] router_mock_helpers: failed to unload mocks ~p~n", 
                      [Remaining]),
            ok
    end.

%% ============================================================================
%% Mock State Management
%% ============================================================================

-define(MOCK_STATE_TABLE, router_mock_state).

%% @doc Initialize mock state ETS table
-spec init_mock_state_table() -> ok.
init_mock_state_table() ->
    case ets:whereis(?MOCK_STATE_TABLE) of
        undefined ->
            ets:new(?MOCK_STATE_TABLE, [public, named_table, set]),
            ets:insert(?MOCK_STATE_TABLE, {connection_state, connected}),
            ok;
        _Tid ->
            %% Table exists, reset to connected
            ets:insert(?MOCK_STATE_TABLE, {connection_state, connected}),
            ok
    end.

%% @doc Get mock connection state (for meck callbacks)
-spec get_mock_connection_state() -> atom().
get_mock_connection_state() ->
    case ets:whereis(?MOCK_STATE_TABLE) of
        undefined -> connected;  %% Default to connected if table doesn't exist
        _Tid ->
            case ets:lookup(?MOCK_STATE_TABLE, connection_state) of
                [{connection_state, State}] -> State;
                [] -> connected
            end
    end.

%% @doc Set mock connection state
-spec set_mock_connection_state(atom()) -> ok.
set_mock_connection_state(State) when is_atom(State) ->
    case ets:whereis(?MOCK_STATE_TABLE) of
        undefined ->
            %% Initialize table if it doesn't exist
            init_mock_state_table(),
            ets:insert(?MOCK_STATE_TABLE, {connection_state, State}),
            ok;
        _Tid ->
            ets:insert(?MOCK_STATE_TABLE, {connection_state, State}),
            ok
    end.

%% ============================================================================
%% Fault Injection Helpers for Mock Mode
%% ============================================================================

%% @doc Check fault injection for an operation
%% Returns: {true, Fault} | false
%% Uses try/catch to handle case when module isn't loaded
-spec check_fault_injection(atom(), map()) -> {true, term()} | false.
check_fault_injection(Operation, Context) ->
    try
        router_nats_fault_injection:should_fail(Operation, Context)
    catch
        error:undef -> false;
        _:_ -> false
    end.

%% @doc Emit mock failure metric (delegates to router_metrics if available)
-spec emit_mock_failure_metric(atom(), binary(), term()) -> ok.
emit_mock_failure_metric(MetricName, Subject, Reason) ->
    ReasonBin = case Reason of
        R when is_binary(R) -> R;
        R when is_atom(R) -> atom_to_binary(R, utf8);
        _ -> <<"unknown">>
    end,
    Stream = extract_stream_from_subject(Subject),
    %% Emit metric via router_metrics
    %% No try-catch to ensure errors propagate and metrics are captured by test helper
    router_metrics:emit_metric(MetricName, #{count => 1}, #{
        reason => ReasonBin,
        subject => Subject,
        stream => Stream,
        source => <<"mock">>
    }).

%% @doc Extract stream name from subject (simplified version for mock)
-spec extract_stream_from_subject(binary()) -> binary().
extract_stream_from_subject(Subject) when is_binary(Subject) ->
    case binary:split(Subject, <<".">>) of
        [FirstPart, _] -> FirstPart;
        [FirstPart] -> FirstPart;
        _ -> <<"unknown">>
    end;
extract_stream_from_subject(_) ->
    <<"unknown">>.
