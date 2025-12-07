%% DevState integration removed intentionally – use application env flags instead.
-module(router_state).
-export([get_current_cp/0]).

-ignore_xref([{router_state, get_current_cp, 0}]).

get_current_cp() ->
    application:get_env(beamline_router, current_cp, <<"CP1-baseline">>). 
