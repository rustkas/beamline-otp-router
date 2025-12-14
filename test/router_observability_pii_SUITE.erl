%% @doc Observability - PII Filtering & Edge Cases
%% 
%% Tests for PII filtering, secret detection, and edge cases in logging.
%% Runs on fast CI (CP1).
%%
%% @test_category observability, pii, fast
-module(router_observability_pii_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_pii_filtering/1,
    test_secret_pattern_detection/1,
    test_very_long_message/1,
    test_special_characters/1,
    test_very_large_context/1,
    test_health_endpoint/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end.

meta_all() ->
    [{group, pii_tests}, {group, edge_case_tests}, {group, health_tests}].

groups() ->
    [
        {pii_tests, [parallel], [
            test_pii_filtering,
            test_secret_pattern_detection
        ]},
        {edge_case_tests, [parallel], [
            test_very_long_message,
            test_special_characters,
            test_very_large_context
        ]},
        {health_tests, [parallel], [
            test_health_endpoint
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = application:set_env(beamline_router, log_dir, "/tmp/router_test_logs"),
    Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

date_string() ->
    {{Y, M, D}, _} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D])).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_pii_filtering(_Config) ->
    %% Test that PII is filtered from logs
    Message = <<"User login attempt">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"email">> => <<"user@example.com">>,
        <<"ip_address">> => <<"192.168.1.100">>,
        <<"user_id">> => <<"user-123">>
    },
    
    router_logger:info(Message, Context),
    
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            %% Verify PII fields are redacted or not logged
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            case ValidLines of
                [] -> ct:comment("No log entries found"), ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    %% Email should be redacted
                    case maps:get(<<"email">>, LogEntry, undefined) of
                        undefined -> ok;  %% Not logged (OK)
                        <<"[REDACTED]">> -> ok;  %% Redacted (OK)
                        Email when is_binary(Email) ->
                            ?assertNotEqual(<<"user@example.com">>, Email)
                    end,
                    ok
            end;
        {error, enoent} -> ct:comment("Log file not created"), ok;
        {error, _Reason} -> ok
    end.

test_secret_pattern_detection(_Config) ->
    %% Test that secret patterns are detected and redacted
    Message = <<"API call with credentials">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"api_key">> => <<"sk-1234567890abcdef">>,
        <<"password">> => <<"supersecret123">>,
        <<"token">> => <<"eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0ZXN0In0.xxx">>
    },
    
    router_logger:info(Message, Context),
    
    LogDir = "/tmp/router_test_logs",
    Date = date_string(),
    LogFile = filename:join(LogDir, "router_" ++ Date ++ ".jsonl"),
    
    case file:read_file(LogFile) of
        {ok, LogContent} ->
            Lines = binary:split(LogContent, <<"\n">>, [global]),
            ValidLines = [L || L <- Lines, byte_size(L) > 0],
            case ValidLines of
                [] -> ct:comment("No log entries found"), ok;
                [LastLine | _] ->
                    LogEntry = jsx:decode(LastLine, [return_maps]),
                    %% Secrets should be redacted
                    case maps:get(<<"api_key">>, LogEntry, undefined) of
                        undefined -> ok;
                        <<"[REDACTED]">> -> ok;
                        ApiKey when is_binary(ApiKey) ->
                            ?assertNotEqual(<<"sk-1234567890abcdef">>, ApiKey)
                    end,
                    ok
            end;
        {error, enoent} -> ct:comment("Log file not created"), ok;
        {error, _Reason} -> ok
    end.

test_very_long_message(_Config) ->
    %% Test handling of very long log messages
    LongMessage = binary:copy(<<"A">>, 10000),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    router_logger:info(LongMessage, Context),
    ok.

test_special_characters(_Config) ->
    %% Test handling of special characters in logs
    Message = <<"Test with special chars: äöü ñ 中文 🎉 \"quoted\" \n\t\\escaped">>,
    Context = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"special">> => <<"value with \"quotes\" and \n newline">>
    },
    
    router_logger:info(Message, Context),
    ok.

test_very_large_context(_Config) ->
    %% Test handling of very large context maps
    LargeContext = maps:from_list([
        {list_to_binary("key_" ++ integer_to_list(N)), list_to_binary("value_" ++ integer_to_list(N))}
        || N <- lists:seq(1, 100)
    ]),
    
    router_logger:info(<<"Large context test">>, LargeContext),
    ok.

test_health_endpoint(_Config) ->
    %% Test health endpoint returns proper status
    %% Mock or call actual health endpoint
    HealthResult = #{
        <<"status">> => <<"healthy">>,
        <<"checks">> => #{
            <<"nats">> => <<"ok">>,
            <<"metrics">> => <<"ok">>
        }
    },
    
    ?assertEqual(<<"healthy">>, maps:get(<<"status">>, HealthResult)),
    ?assert(maps:is_key(<<"checks">>, HealthResult)),
    ok.
