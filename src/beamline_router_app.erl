%% @doc Beamline Router Application
%% Main application module for router core
-module(beamline_router_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("beamline_router.hrl").
 
%% @doc Start the application   
start(_StartType, _StartArgs) ->
    beamline_router_sup:start_link().

%% @doc Stop the application
stop(_State) ->
    ok.

