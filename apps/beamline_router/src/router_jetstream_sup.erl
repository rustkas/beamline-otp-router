-module(router_jetstream_sup).

-doc "JetStream Supervisor".
%%
%% Supervisor for JetStream consumers and related processes.
%% Manages the lifecycle of JetStream subscriptions and message processing.
%%
%% @module router_jetstream_sup

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("beamline_router.hrl").

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Supervisor strategy: one_for_one - restart only failed child
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% JetStream worker processes
    Children = [
        %% JetStream consumer manager
        #{
            id => router_jetstream_consumer_manager,
            start => {router_jetstream_consumer_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [router_jetstream_consumer_manager]
        }
    ],

    {ok, {SupFlags, Children}}.
