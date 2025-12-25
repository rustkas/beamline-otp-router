%% Router Control API application entrypoint.
-module(router_control_api_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    router_control_sup:start_link().

stop(_State) ->
    ok.
