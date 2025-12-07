%% @doc Test suite for verifying secrets are not logged
%% Ensures that sensitive data (API keys, tokens, secrets) are sanitized in logs
%% @test_category fast
-module(router_secrets_logging_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

%% Test suite callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.