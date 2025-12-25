%% DevState integration removed intentionally – use application env flags instead.
-module(router_state).
-export([get_current_cp/0, is_cp2_plus_allowed/0]).

-ignore_xref([{router_state, get_current_cp, 0}, {router_state, is_cp2_plus_allowed, 0}]).

get_current_cp() ->
    application:get_env(beamline_router, current_cp, ~"CP1-baseline").

is_cp2_plus_allowed() ->
    application:get_env(beamline_router, cp2_plus_allowed, false). 
