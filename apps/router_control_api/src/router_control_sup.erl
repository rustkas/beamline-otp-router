%% Router control API supervisor.
-module(router_control_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {router_control,
         {router_control, start_link, []},
         permanent, 5000, worker, [router_control]},
        {router_control_nats,
         {router_control_nats, start_link, []},
         permanent, 5000, worker, [router_control_nats]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
