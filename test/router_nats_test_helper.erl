%% @doc Centralized helper module for mocking router_nats in tests
%% 
%% This module provides a unified interface for setting up and tearing down
%% router_nats mocks across all test suites. It encapsulates the meck lifecycle
%% and provides common mock configurations for different test scenarios.
%%
%% == When to Use This Helper vs Direct meck Calls ==
%%
%% USE THIS HELPER FOR:
%% - Standard success/failure publish scenarios
%% - Setup/teardown lifecycle management
%% - Simple sequence-based mock responses
%% - Connection state mocking
%%
%% USE DIRECT meck CALLS FOR:
%% - Argument-sensitive corner cases (use expect_custom/2 or meck:expect directly)
%% - Complex stateful mocks with process dictionary
%% - Mocking functions not covered by this helper
%% - When you need meck:history/1 for call verification
%%
%% Example of corner-case with direct meck after helper setup:
%% ```
%% router_nats_test_helper:setup_mock(),
%% %% Custom behavior based on payload content
%% meck:expect(router_nats, publish_with_ack, fun(Subject, Payload, _Headers) ->
%%     case binary:match(Payload, <<"error_trigger">>) of
%%         nomatch -> {ok, <<"ack">>};
%%         _ -> {error, custom_error}
%%     end
%% end),
%% ```
%%
%% Usage:
%% 1. In init_per_suite or init_per_testcase:
%%    ok = router_nats_test_helper:setup_mock() 
%%    OR
%%    ok = router_nats_test_helper:setup_mock(#{publish_response => ok})
%%
%% 2. To set specific expectations:
%%    router_nats_test_helper:expect_publish(fun(_, _, _) -> {ok, <<"ack">>} end)
%%    OR
%%    router_nats_test_helper:expect_publish_failure({error, timeout})
%%
%% 3. In end_per_suite or end_per_testcase:
%%    ok = router_nats_test_helper:teardown_mock()
%%
%% NOTE: This module does NOT start/stop the beamline_router application.
%% The SUITE is responsible for application lifecycle.
%%
%% NOTE: nats_mode should be set to 'mock' in init_per_suite:
%%    ok = application:set_env(beamline_router, nats_mode, mock)
%%
%% @test_category test_helper, nats
-module(router_nats_test_helper).

-export([
    %% Setup/teardown
    setup_mock/0,
    setup_mock/1,
    teardown_mock/0,
    is_mocked/0,
    
    %% Metric table lifecycle (unique per testcase)
    setup_metric_table/2,
    teardown_metric_table/1,
    get_metric_table/1,
    wait_for_metric/3,
    wait_for_publish_failure/1,
    wait_for_publish_failure/2,
    wait_for_publish_with_ack_failure/1,
    wait_for_publish_with_ack_failure/2,
    wait_for_publish_total_inc/1,
    wait_for_publish_total_inc/2,
    
    %% Expectations - basic
    expect_publish/1,
    expect_publish_success/0,
    expect_publish_success/1,
    expect_publish_failure/1,
    expect_publish_sequence/1,
    
    %% Expectations - advanced (for corner cases)
    expect_publish_with_args/1,  %% receives Subject, Payload, Headers
    expect_custom/2,             %% expect any function
    
    %% ACK expectations
    expect_ack/1,
    expect_ack_success/0,
    expect_nak/1,
    
    %% Connection expectations
    expect_connect_failure/1,
    expect_connect_success/0,
    
    %% Verification
    get_call_count/1,
    get_call_history/1,         %% for detailed debugging
    get_last_call_args/1,       %% get last call arguments
    reset_mock/0,
    
    %% Discipline helpers (encourage proper mock usage)
    verify_publish_called_with/1,        %% assert publish was called with specific args
    check_corner_case_coverage/0,        %% hint mode (strict if STRICT_MOCK_DISCIPLINE=true)
    check_corner_case_coverage_strict/0, %% always fails on basic_only (for CI)
    
    %% Sanity tests (can be called from test suites)
    sanity_test_publish/0,
    sanity_test_ack/0,
    sanity_test_failure/0
]).

%% Default mock options - NO passthrough to avoid noproc on gen_server:call(router_nats,...)
%% IMPORTANT: passthrough is dangerous for router_nats because publish_with_ack, nak_message,
%% ack_message, subscribe_jetstream, and request all call gen_server:call(router_nats,...)
%% which fails if the real router_nats process is not running.
-define(DEFAULT_MOCK_OPTIONS, [no_link]).

%% ============================================================================
%% Setup/Teardown
%% ============================================================================

%% @doc Setup router_nats mock with default options
%% Returns ok if successful, {skip, Reason} if meck not available
-spec setup_mock() -> ok | {skip, string()}.
setup_mock() ->
    setup_mock(#{}).

%% @doc Setup router_nats mock with custom options
%% Options:
%%   - passthrough: boolean, default true (pass through non-mocked calls)
%%   - no_link: boolean, default true (don't link mock process to test)
%%   - publish_response: term, default {ok, <<"ack">>} (default publish response)
-spec setup_mock(map()) -> ok | {skip, string()}.
setup_mock(Options) ->
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            %% Unload any existing mock first
            catch meck:unload(router_nats),
            
            %% Build meck options
            MeckOptions = build_meck_options(Options),
            
            %% Create mock
            meck:new(router_nats, MeckOptions),
            
            %% ALWAYS set safe stubs for all functions that call gen_server:call(router_nats,...)
            %% This prevents noproc errors when the real process is not running.
            DefaultPublishResponse = maps:get(publish_response, Options, {ok, <<"ack">>}),
            meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> DefaultPublishResponse end),
            meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
            meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
            meck:expect(router_nats, subscribe_jetstream, 
                fun(_Subj, _Stream, _Ack, _Dur, _Mode) -> {error, connection_closed} end),
            meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
            meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
            meck:expect(router_nats, subscribe, fun(_Subj, _Callback, _Timeout) -> {ok, self()} end),
            meck:expect(router_nats, get_connection_status, fun() -> connected end),
            %% Additional stubs for functions used in tests
            meck:expect(router_nats, reconnect, fun() -> ok end),
            meck:expect(router_nats, get_connection_health, fun() -> #{status => healthy, latency_ms => 1} end),
            meck:expect(router_nats, get_jetstream_consumers, fun() -> [] end),
            meck:expect(router_nats, simulate_connection_lost, fun() -> ok end),
            
            ok
    end.

%% @doc Teardown router_nats mock
%% Safe to call even if mock is not active
-spec teardown_mock() -> ok.
teardown_mock() ->
    case code:which(meck) of
        non_existing ->
            ok;
        _ ->
            catch meck:unload(router_nats),
            ok
    end.

%% @doc Check if router_nats is currently mocked
-spec is_mocked() -> boolean().
is_mocked() ->
    case code:which(meck) of
        non_existing -> false;
        _ ->
            try
                meck:validate(router_nats),
                true
            catch
                _:_ -> false
            end
    end.

%% ============================================================================
%% Expectations - Publish
%% ============================================================================

%% @doc Set custom publish_with_ack expectation
-spec expect_publish(fun()) -> ok.
expect_publish(Fun) when is_function(Fun, 3) ->
    meck:expect(router_nats, publish_with_ack, Fun),
    ok.

%% @doc Set successful publish expectation with default ack
-spec expect_publish_success() -> ok.
expect_publish_success() ->
    expect_publish_success(<<"ack-test">>).

%% @doc Set successful publish expectation with custom ack
-spec expect_publish_success(binary()) -> ok.
expect_publish_success(AckId) ->
    meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> {ok, AckId} end),
    ok.

%% @doc Set failing publish expectation
-spec expect_publish_failure(term()) -> ok.
expect_publish_failure(Error) ->
    meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> {error, Error} end),
    ok.

%% @doc Set sequence of publish responses (first call returns first item, etc.)
%% After all items consumed, returns last item for any subsequent calls
-spec expect_publish_sequence([term()]) -> ok.
expect_publish_sequence(Responses) when is_list(Responses), length(Responses) > 0 ->
    meck:expect(router_nats, publish_with_ack, fun(_, _, _) ->
        case get(router_nats_publish_sequence) of
            undefined ->
                put(router_nats_publish_sequence, Responses),
                get_next_sequence_response(Responses);
            CurrentResponses ->
                get_next_sequence_response(CurrentResponses)
        end
    end),
    ok.

%% ============================================================================
%% Expectations - Advanced (for Corner Cases)
%% ============================================================================

%% @doc Set publish expectation that receives all arguments
%% Use this when you need to inspect or vary behavior based on Subject, Payload, or Headers
%% Example:
%%   expect_publish_with_args(fun(Subject, Payload, Headers) ->
%%       case Subject of
%%           <<"test.error">> -> {error, test_error};
%%           _ -> {ok, <<"ack">>}
%%       end
%%   end)
-spec expect_publish_with_args(fun((binary(), binary(), map()) -> term())) -> ok.
expect_publish_with_args(Fun) when is_function(Fun, 3) ->
    meck:expect(router_nats, publish_with_ack, Fun),
    ok.

%% @doc Set expectation for any router_nats function
%% Use this for corner-case mocking not covered by other helpers
%% Example:
%%   expect_custom(subscribe_jetstream, fun(Subject, Durable, AckPolicy, DeliverGroup, Mode) ->
%%       {ok, <<"consumer-custom">>}
%%   end)
-spec expect_custom(atom(), fun()) -> ok.
expect_custom(FunctionName, Fun) when is_atom(FunctionName), is_function(Fun) ->
    meck:expect(router_nats, FunctionName, Fun),
    ok.

%% ============================================================================
%% Expectations - ACK/NAK
%% ============================================================================

%% @doc Set custom ack_message expectation
-spec expect_ack(fun()) -> ok.
expect_ack(Fun) when is_function(Fun, 1) ->
    meck:expect(router_nats, ack_message, Fun),
    ok.

%% @doc Set successful ack expectation
-spec expect_ack_success() -> ok.
expect_ack_success() ->
    meck:expect(router_nats, ack_message, fun(_) -> ok end),
    ok.

%% @doc Set custom nak expectation
-spec expect_nak(fun()) -> ok.
expect_nak(Fun) when is_function(Fun, 2) ->
    meck:expect(router_nats, nak_message, Fun),
    ok.

%% ============================================================================
%% Expectations - Connection
%% ============================================================================

%% @doc Set connection failure expectation
-spec expect_connect_failure(term()) -> ok.
expect_connect_failure(Error) ->
    meck:expect(router_nats, connect, fun() -> {error, Error} end),
    meck:expect(router_nats, is_connected, fun() -> false end),
    ok.

%% @doc Set successful connection expectation
-spec expect_connect_success() -> ok.
expect_connect_success() ->
    meck:expect(router_nats, connect, fun() -> ok end),
    meck:expect(router_nats, is_connected, fun() -> true end),
    ok.

%% ============================================================================
%% Verification
%% ============================================================================

%% @doc Get call count for a function
-spec get_call_count(atom()) -> non_neg_integer().
get_call_count(Function) ->
    try
        meck:num_calls(router_nats, Function, '_')
    catch
        _:_ -> 0
    end.

%% @doc Get full call history for a function (for detailed debugging)
%% Returns list of {Pid, {Module, Function, Args}, Result} tuples
-spec get_call_history(atom()) -> list().
get_call_history(Function) ->
    try
        History = meck:history(router_nats),
        [Call || {_Pid, {_Mod, Fun, _Args}, _Result} = Call <- History, Fun =:= Function]
    catch
        _:_ -> []
    end.

%% @doc Get arguments from the last call to a function (for assertions)
%% Returns {ok, Args} or {error, no_calls}
-spec get_last_call_args(atom()) -> {ok, list()} | {error, no_calls}.
get_last_call_args(Function) ->
    case get_call_history(Function) of
        [] -> {error, no_calls};
        Calls ->
            {_Pid, {_Mod, _Fun, Args}, _Result} = lists:last(Calls),
            {ok, Args}
    end.

%% @doc Reset mock (clear history but keep expectations)
-spec reset_mock() -> ok.
reset_mock() ->
    catch meck:reset(router_nats),
    ok.

%% ============================================================================
%% Discipline Helpers
%% ============================================================================

%% @doc Verify that publish_with_ack was called with specific arguments
%% This encourages developers to write specific assertions rather than just checking call counts
%% Matcher is a map with optional keys: subject, payload_contains, headers_key
%% Example:
%%   verify_publish_called_with(#{subject => <<"caf.exec.assign.v1">>, payload_contains => <<"tenant_id">>})
-spec verify_publish_called_with(map()) -> ok | {error, term()}.
verify_publish_called_with(Matcher) ->
    History = get_call_history(publish_with_ack),
    case find_matching_call(History, Matcher) of
        {ok, _MatchedCall} -> ok;
        {error, not_found} ->
            {error, {no_matching_call, Matcher, length(History)}}
    end.

%% @doc Check if test used only basic mocks and log a reminder
%% Call this in end_per_testcase to encourage corner-case testing
%%
%% Behavior:
%%   - Default: logs hint if basic_only, returns atom
%%   - STRICT_MOCK_DISCIPLINE=true: calls ct:fail if basic_only
%%
%% Returns: 
%%   - basic_only: only simple success/failure mocks were used
%%   - advanced: custom expectations or argument verification was used
-spec check_corner_case_coverage() -> basic_only | advanced.
check_corner_case_coverage() ->
    Result = analyze_mock_coverage(),
    
    %% Check for strict mode
    StrictMode = os:getenv("STRICT_MOCK_DISCIPLINE", "false") =:= "true",
    
    case {Result, StrictMode} of
        {basic_only, true} ->
            %% STRICT MODE: fail the test
            ct:pal("~n"
                   "╔════════════════════════════════════════════════════════════════╗~n"
                   "║ ❌ STRICT MOCK DISCIPLINE VIOLATION                            ║~n"
                   "║                                                                ║~n"
                   "║ STRICT_MOCK_DISCIPLINE=true is set.                           ║~n"
                   "║ This test used only BASIC mock patterns.                      ║~n"
                   "║                                                                ║~n"
                   "║ Required: Use advanced mock features:                         ║~n"
                   "║   - expect_publish_with_args/1                                ║~n"
                   "║   - expect_publish_sequence/1                                 ║~n"
                   "║   - verify_publish_called_with/1                              ║~n"
                   "╚════════════════════════════════════════════════════════════════╝~n", []),
            ct:fail("STRICT_MOCK_DISCIPLINE: basic_only mock usage not allowed");
        {basic_only, false} ->
            %% Default mode: hint only
            ct:pal("~n"
                   "╔════════════════════════════════════════════════════════════════╗~n"
                   "║ 💡 MOCK COVERAGE HINT                                          ║~n"
                   "║                                                                ║~n"
                   "║ This test used only BASIC mock patterns (single response).    ║~n"
                   "║ Consider testing corner cases with:                           ║~n"
                   "║   - expect_publish_with_args/1 (behavior based on input)      ║~n"
                   "║   - expect_publish_sequence/1 (fail-then-succeed patterns)    ║~n"
                   "║   - verify_publish_called_with/1 (specific argument checks)   ║~n"
                   "╚════════════════════════════════════════════════════════════════╝~n", []),
            basic_only;
        {advanced, _} ->
            advanced
    end.

%% @doc Strict version that always fails on basic_only (for CI discipline checks)
%% Does not depend on env var - use this in dedicated discipline test suite
-spec check_corner_case_coverage_strict() -> ok | no_return().
check_corner_case_coverage_strict() ->
    case analyze_mock_coverage() of
        basic_only ->
            ct:fail("Mock discipline violation: only basic mock patterns used");
        advanced ->
            ok
    end.

%% Internal: analyze mock coverage without side effects
-spec analyze_mock_coverage() -> basic_only | advanced.
analyze_mock_coverage() ->
    %% Check if any argument-aware calls were made
    History = try meck:history(router_nats) catch _:_ -> [] end,
    
    %% Analyze if any calls had varied arguments
    UniqueSubjects = lists:usort([S || {_Pid, {_, publish_with_ack, [S, _, _]}, _} <- History]),
    UniqueResults = lists:usort([R || {_Pid, {_, publish_with_ack, _}, R} <- History]),
    
    CallCount = length(History),
    SubjectCount = length(UniqueSubjects),
    ResultCount = length(UniqueResults),
    
    case {CallCount, SubjectCount, ResultCount} of
        {0, _, _} ->
            %% No calls at all - considered basic
            basic_only;
        {_, 1, 1} when CallCount > 0 ->
            %% All calls had same subject and same result: likely basic mock
            basic_only;
        _ ->
            %% Multiple subjects or results: test used varied expectations
            advanced
    end.

%% Internal: find a call matching the given criteria
find_matching_call([], _Matcher) ->
    {error, not_found};
find_matching_call([{_Pid, {_, publish_with_ack, [Subject, Payload, Headers]}, _Result} | Rest], Matcher) ->
    SubjectMatch = case maps:get(subject, Matcher, undefined) of
        undefined -> true;
        ExpSubject -> Subject =:= ExpSubject
    end,
    PayloadMatch = case maps:get(payload_contains, Matcher, undefined) of
        undefined -> true;
        Pattern -> binary:match(Payload, Pattern) =/= nomatch
    end,
    HeadersMatch = case maps:get(headers_key, Matcher, undefined) of
        undefined -> true;
        Key -> maps:is_key(Key, Headers)
    end,
    case SubjectMatch andalso PayloadMatch andalso HeadersMatch of
        true -> {ok, {Subject, Payload, Headers}};
        false -> find_matching_call(Rest, Matcher)
    end;
find_matching_call([_ | Rest], Matcher) ->
    find_matching_call(Rest, Matcher).

%% ============================================================================
%% Sanity Tests
%% ============================================================================

%% @doc Sanity test for publish mock
%% Returns ok if mock works correctly, {error, Reason} otherwise
-spec sanity_test_publish() -> ok | {error, term()}.
sanity_test_publish() ->
    case setup_mock(#{publish_response => {ok, <<"sanity-ack">>}}) of
        {skip, Reason} -> {error, {meck_not_available, Reason}};
        ok ->
            Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
            teardown_mock(),
            case Result of
                {ok, <<"sanity-ack">>} -> ok;
                Other -> {error, {unexpected_result, Other}}
            end
    end.

%% @doc Sanity test for ACK mock
-spec sanity_test_ack() -> ok | {error, term()}.
sanity_test_ack() ->
    case setup_mock() of
        {skip, Reason} -> {error, {meck_not_available, Reason}};
        ok ->
            expect_ack_success(),
            Result = router_nats:ack_message(<<"msg-id">>),
            teardown_mock(),
            case Result of
                ok -> ok;
                Other -> {error, {unexpected_result, Other}}
            end
    end.

%% @doc Sanity test for failure mock
-spec sanity_test_failure() -> ok | {error, term()}.
sanity_test_failure() ->
    case setup_mock() of
        {skip, Reason} -> {error, {meck_not_available, Reason}};
        ok ->
            expect_publish_failure(timeout),
            Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
            teardown_mock(),
            case Result of
                {error, timeout} -> ok;
                Other -> {error, {unexpected_result, Other}}
            end
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% Build meck options from map
-spec build_meck_options(map()) -> list().
build_meck_options(Options) ->
    %% NEVER use passthrough for router_nats - it causes noproc crashes when
    %% the real router_nats process is not running. Ignore passthrough option.
    case maps:get(no_link, Options, true) of
        true -> [no_link];
        false -> []
    end.

%% Get next response from sequence
-spec get_next_sequence_response([term()]) -> term().
get_next_sequence_response([Response]) ->
    %% Last response - keep returning it
    Response;
get_next_sequence_response([Response | Rest]) ->
    put(router_nats_publish_sequence, Rest),
    Response.

%% ============================================================================
%% Metric Table Lifecycle (Unique Per Testcase)
%% ============================================================================

%% @doc Setup unique metric table for a testcase
%% Creates ETS table with unique name to prevent conflicts between parallel tests
%% Returns: Config with {metric_table, Tid}, {metric_table_name, Name}, {unique_id, Id}
-spec setup_metric_table(atom(), list()) -> list().
setup_metric_table(TestCase, Config) ->
    %% Create unique table name per testcase
    UniqueId = erlang:unique_integer([positive]),
    TableName = list_to_atom("metric_calls_" ++ atom_to_list(TestCase) ++ "_" ++ integer_to_list(UniqueId)),
    MetricTable = ets:new(TableName, [set, public]),
    [{metric_table, MetricTable}, {metric_table_name, TableName}, {unique_id, UniqueId} | Config].

%% @doc Teardown metric table and cleanup
%% Safe to call even if table doesn't exist
-spec teardown_metric_table(list()) -> ok.
teardown_metric_table(Config) ->
    case proplists:get_value(metric_table, Config) of
        undefined -> ok;
        Tid -> catch ets:delete(Tid)
    end,
    ok.

%% @doc Get metric table from config
-spec get_metric_table(list()) -> ets:tid() | undefined.
get_metric_table(Config) ->
    proplists:get_value(metric_table, Config).

%% @doc Wait for a specific metric to appear in the ETS table (bounded polling)
%% This replaces ad-hoc timer:sleep calls with deterministic waiting
-spec wait_for_metric(list(), atom(), non_neg_integer()) -> ok | {error, timeout}.
    wait_for_metric(Config, MetricName, TimeoutMs) ->
        MetricTable = get_metric_table(Config),
        case MetricTable of
            undefined ->
                {error, no_metric_table};
            Tid ->
                wait_for_metric_loop(Tid, MetricName, TimeoutMs, 10)
        end.

%% @doc Convenience: wait for publish failure metric using shared harness
-spec wait_for_publish_failure(pos_integer()) -> ok.
wait_for_publish_failure(ExpectedCount) ->
    router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, ExpectedCount).

%% @doc Convenience with custom timeout
-spec wait_for_publish_failure(pos_integer(), pos_integer()) -> ok.
wait_for_publish_failure(ExpectedCount, TimeoutMs) ->
    router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, ExpectedCount, TimeoutMs).

%% @doc Convenience: wait for publish_with_ack failure metric
-spec wait_for_publish_with_ack_failure(pos_integer()) -> ok.
wait_for_publish_with_ack_failure(ExpectedCount) ->
    router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, ExpectedCount).

%% @doc Convenience with custom timeout
-spec wait_for_publish_with_ack_failure(pos_integer(), pos_integer()) -> ok.
wait_for_publish_with_ack_failure(ExpectedCount, TimeoutMs) ->
    router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, ExpectedCount, TimeoutMs).

%% @doc Wait for publish_total inc
-spec wait_for_publish_total_inc(pos_integer()) -> ok.
wait_for_publish_total_inc(ExpectedCount) ->
    router_metrics_test_helper:wait_for_inc(router_nats_publish_total, ExpectedCount).

%% @doc Wait for publish_total inc with timeout
-spec wait_for_publish_total_inc(pos_integer(), pos_integer()) -> ok.
wait_for_publish_total_inc(ExpectedCount, TimeoutMs) ->
    router_metrics_test_helper:wait_for_inc(router_nats_publish_total, ExpectedCount, TimeoutMs).

%% Internal: Bounded polling loop for metric
wait_for_metric_loop(_Tid, MetricName, TimeoutMs, _IntervalMs) when TimeoutMs =< 0 ->
    {error, {timeout, MetricName}};
wait_for_metric_loop(Tid, MetricName, TimeoutMs, IntervalMs) ->
    try
        AllMetrics = ets:tab2list(Tid),
        Matching = [M || {metric, inc, Name} = M <- AllMetrics, Name =:= MetricName],
        case length(Matching) > 0 of
            true -> ok;
            false ->
                timer:sleep(IntervalMs),
                wait_for_metric_loop(Tid, MetricName, TimeoutMs - IntervalMs, IntervalMs)
        end
    catch
        _:_ ->
            timer:sleep(IntervalMs),
            wait_for_metric_loop(Tid, MetricName, TimeoutMs - IntervalMs, IntervalMs)
    end.
