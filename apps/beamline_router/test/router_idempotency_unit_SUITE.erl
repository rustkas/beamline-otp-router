%% @doc Unit Tests for router_idempotency module
%% @test_category unit, fast, coverage_hotspot
-module(router_idempotency_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_check_and_mark_new_message/1,
    test_check_and_mark_duplicate/1,
    test_check_and_mark_different_key_types/1,
    test_check_and_mark_with_additional_data/1,
    test_check_and_mark_same_id_different_type/1,
    test_idempotency_isolation/1,
    test_high_volume_messages/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_check_and_mark_new_message/1,
    test_check_and_mark_duplicate/1,
    test_check_and_mark_different_key_types/1,
    test_check_and_mark_with_additional_data/1,
    test_check_and_mark_same_id_different_type/1,
    test_idempotency_isolation/1,
    test_high_volume_messages/1
]}).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) ->
    [{group, unit_tests}].
groups() ->
    [
        {unit_tests, [sequence], [
            test_check_and_mark_new_message,
            test_check_and_mark_duplicate,
            test_check_and_mark_different_key_types,
            test_check_and_mark_with_additional_data,
            test_check_and_mark_same_id_different_type,
            test_idempotency_isolation,
            test_high_volume_messages
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Start router_idempotency gen_server if not running
    case whereis(router_idempotency) of
        undefined ->
            case router_idempotency:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _}} -> ok;
                StartError -> ct:fail("Failed to start router_idempotency: ~p", [StartError])
            end;
        _Pid ->
            ok
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for check_and_mark/2, check_and_mark/3
%% ============================================================================

test_check_and_mark_new_message(_Config) ->
    KeyType = <<"assignment_id">>,
    MessageId = <<"msg_new_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    
    Result = router_idempotency:check_and_mark(KeyType, MessageId),
    
    ?assertEqual({ok, not_seen}, Result),
    
    ok.

test_check_and_mark_duplicate(_Config) ->
    KeyType = <<"assignment_id">>,
    MessageId = <<"msg_dup_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    
    %% First call - should be not_seen
    Result1 = router_idempotency:check_and_mark(KeyType, MessageId),
    ?assertEqual({ok, not_seen}, Result1),
    
    %% Second call with same ID - should be seen
    Result2 = router_idempotency:check_and_mark(KeyType, MessageId),
    ?assertEqual({ok, seen}, Result2),
    
    %% Third call - should still be seen
    Result3 = router_idempotency:check_and_mark(KeyType, MessageId),
    ?assertEqual({ok, seen}, Result3),
    
    ok.

test_check_and_mark_different_key_types(_Config) ->
    Unique = integer_to_binary(erlang:unique_integer([positive])),
    
    %% Test different key types
    KeyTypes = [<<"assignment_id">>, <<"request_id">>, <<"ack_id">>, <<"usage_id">>],
    
    lists:foreach(fun(KeyType) ->
        MessageId = <<"msg_", KeyType/binary, "_", Unique/binary>>,
        
        Result = router_idempotency:check_and_mark(KeyType, MessageId),
        ?assertEqual({ok, not_seen}, Result, 
                     io_lib:format("Failed for key type: ~p", [KeyType]))
    end, KeyTypes),
    
    ok.

test_check_and_mark_with_additional_data(_Config) ->
    KeyType = <<"request_id">>,
    MessageId = <<"msg_data_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    AdditionalData = #{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"status">> => <<"processed">>,
        <<"provider">> => <<"openai">>
    },
    
    %% First call with additional data - should be not_seen
    Result1 = router_idempotency:check_and_mark(KeyType, MessageId, AdditionalData),
    ?assertEqual({ok, not_seen}, Result1),
    
    %% Second call - should be seen
    Result2 = router_idempotency:check_and_mark(KeyType, MessageId, AdditionalData),
    ?assertEqual({ok, seen}, Result2),
    
    ok.

test_check_and_mark_same_id_different_type(_Config) ->
    MessageId = <<"shared_msg_id_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    
    %% Same message ID with different key types should be independent
    Result1 = router_idempotency:check_and_mark(<<"assignment_id">>, MessageId),
    ?assertEqual({ok, not_seen}, Result1),
    
    Result2 = router_idempotency:check_and_mark(<<"request_id">>, MessageId),
    ?assertEqual({ok, not_seen}, Result2),
    
    Result3 = router_idempotency:check_and_mark(<<"ack_id">>, MessageId),
    ?assertEqual({ok, not_seen}, Result3),
    
    %% But same type should be seen
    Result4 = router_idempotency:check_and_mark(<<"assignment_id">>, MessageId),
    ?assertEqual({ok, seen}, Result4),
    
    ok.

test_idempotency_isolation(_Config) ->
    %% Test that different messages are isolated
    Unique = integer_to_binary(erlang:unique_integer([positive])),
    
    Msg1 = <<"iso_msg_1_", Unique/binary>>,
    Msg2 = <<"iso_msg_2_", Unique/binary>>,
    Msg3 = <<"iso_msg_3_", Unique/binary>>,
    
    %% All should be not_seen initially
    ?assertEqual({ok, not_seen}, router_idempotency:check_and_mark(<<"assignment_id">>, Msg1)),
    ?assertEqual({ok, not_seen}, router_idempotency:check_and_mark(<<"assignment_id">>, Msg2)),
    ?assertEqual({ok, not_seen}, router_idempotency:check_and_mark(<<"assignment_id">>, Msg3)),
    
    %% Each should be seen on second call
    ?assertEqual({ok, seen}, router_idempotency:check_and_mark(<<"assignment_id">>, Msg1)),
    ?assertEqual({ok, seen}, router_idempotency:check_and_mark(<<"assignment_id">>, Msg2)),
    ?assertEqual({ok, seen}, router_idempotency:check_and_mark(<<"assignment_id">>, Msg3)),
    
    ok.

test_high_volume_messages(_Config) ->
    %% Test with many messages to ensure scalability
    Unique = integer_to_binary(erlang:unique_integer([positive])),
    KeyType = <<"assignment_id">>,
    NumMessages = 100,
    
    %% Generate and check many messages
    Results = lists:map(fun(I) ->
        MessageId = <<"high_vol_", Unique/binary, "_", (integer_to_binary(I))/binary>>,
        router_idempotency:check_and_mark(KeyType, MessageId)
    end, lists:seq(1, NumMessages)),
    
    %% All should be not_seen
    NotSeenCount = length([R || R <- Results, R =:= {ok, not_seen}]),
    ?assertEqual(NumMessages, NotSeenCount),
    
    %% Now check again - all should be seen
    SeenResults = lists:map(fun(I) ->
        MessageId = <<"high_vol_", Unique/binary, "_", (integer_to_binary(I))/binary>>,
        router_idempotency:check_and_mark(KeyType, MessageId)
    end, lists:seq(1, NumMessages)),
    
    SeenCount = length([R || R <- SeenResults, R =:= {ok, seen}]),
    ?assertEqual(NumMessages, SeenCount),
    
    ok.
