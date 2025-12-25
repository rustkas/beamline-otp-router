-module(beamline_router_app).

-doc "Beamline Router Application".
%% Main application module for router core
-behaviour(application).

-export([start/2, stop/1]).

-include("beamline_router.hrl").
 
start(_StartType, _StartArgs) ->
    beamline_router_sup:start_link().

stop(_State) ->
    ok.

