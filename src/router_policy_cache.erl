%% @doc Policy Cache Manager (Stub Implementation)
%%
%% This module is a stub implementation to preserve interfaces if referenced.
%% The actual policy caching is handled by router_policy_store (ETS-based).
%%
%% ⚠️ NOTE: This module is intentionally minimal and does not perform actual caching.
%% For policy caching functionality, see router_policy_store.erl
%%
%% @see router_policy_store.erl For actual policy caching implementation
-module(router_policy_cache).

-export([start_link/0]).

-include("beamline_router.hrl").

%% @doc Start policy cache manager (stub)
%% Returns: {ok, Pid} where Pid is the current process
%% This is a stub to preserve interfaces - actual caching is done by router_policy_store
-spec start_link() -> {ok, pid()}.
start_link() ->
    try
        router_logger:info(<<"Policy cache manager started (stub)">>, #{
            <<"event">> => <<"policy_cache_stub_started">>,
            <<"note">> => <<"Actual caching handled by router_policy_store">>
        }),
        Pid = spawn(fun() -> 
            receive
                stop -> ok
            after
                infinity -> ok
            end
        end),
        {ok, Pid}
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to start policy cache manager">>, #{
                <<"error">> => {Class, Reason},
                <<"event">> => <<"policy_cache_start_failed">>
            }),
            {error, Reason}
    end.
