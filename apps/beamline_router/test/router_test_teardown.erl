-module(router_test_teardown).

-export([cleanup_ets/1]).

%% Safe ETS cleanup that tolerates missing tables
cleanup_ets(Name) ->
    case ets:info(Name) of
        undefined -> ok;
        _ -> catch ets:delete(Name), ok
    end.
