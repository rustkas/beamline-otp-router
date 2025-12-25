%% Centralizes tier-aware timeout values used by test suites/helpers.
-module(router_test_timeouts).

-export([
    tier/0,
    timeout/1,
    very_short_wait/0,
    short_wait/0,
    default_wait/0,
    long_wait/0,
    call_timeout/0,
    receive_timeout/0
]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

tier() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "fast" -> fast;
        "full" -> full;
        "heavy" -> heavy;
        _ -> full
    end.

timeout(Key) ->
    Tier = tier(),
    Definitions = timeouts(),
    case maps:find(Key, Definitions) of
        {ok, TierMap} ->
            case maps:find(Tier, TierMap) of
                {ok, Value} -> Value;
                error -> fallback()
            end;
        error -> fallback()
    end.

very_short_wait() ->
    timeout(very_short).

short_wait() ->
    timeout(short).

default_wait() ->
    timeout(default).

long_wait() ->
    timeout(long).

call_timeout() ->
    default_wait().

receive_timeout() ->
    default_wait().

fallback() ->
    5000.

%% ------------------------------------------------------------------
%% Internal helpers
%% ------------------------------------------------------------------

timeouts() ->
    #{
        very_short => #{sanity => 100, fast => 200, full => 400, heavy => 800},
        short => #{sanity => 500, fast => 1000, full => 2000, heavy => 4000},
        default => #{sanity => 2000, fast => 4000, full => 5000, heavy => 10000},
        long => #{sanity => 8000, fast => 12000, full => 20000, heavy => 40000}
    }.
