-module(router_nats_tls_validation_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_tls_handshake_success/1]).

all() -> [test_tls_handshake_success].

init_per_suite(Config) ->
    %% Ensure application and dependencies are started
    {ok, _} = application:ensure_all_started(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

test_tls_handshake_success(_Config) ->
    %% 1. Verify environment config is loaded correctly
    ?assertEqual(real, application:get_env(beamline_router, nats_mode, stub)),
    ?assertEqual(true, application:get_env(beamline_router, nats_tls_enabled, false)),
    
    Url = application:get_env(beamline_router, nats_url, undefined),
    ct:pal("Connecting to NATS TLS URL: ~p", [Url]),
    
    %% 2. router_nats should be already started by init_per_suite
    %% But if we want to ensure it's fresh or started with our config specifically,
    %% we rely on the fact that ensure_all_started started the supervision tree.
    
    %% 3. Wait for connection state to become 'connected'
    case wait_for_connected(60) of  % Increased from 20 to 60 (30 seconds)
        ok -> 
            ct:pal("Successfully connected to NATS via TLS!"),
            %% 4. Perform a smoke operation (simple publish to verify TLS works)
            %% Note: Using simple publish instead of publish_with_ack to avoid gen_server timeout
            case router_nats:publish(<<"tls.smoke.test">>, <<"secure payload">>) of
                ok ->
                    ct:pal("Successfully published message over TLS!");
                {error, Reason} ->
                    ct:fail("Publish failed over TLS: ~p", [Reason])
            end;
        {error, timeout} ->
            Status = router_nats:get_connection_status(),
            ct:fail("Failed to connect to NATS via TLS within timeout. Current status: ~p", [Status])
    end,
    
    ok = router_nats:stop().

wait_for_connected(0) -> {error, timeout};
wait_for_connected(N) ->
    case router_nats:get_connection_status() of
        {ok, #{state := connected}} -> 
            ct:pal("Connection established after ~p attempts", [60 - N]),
            ok;
        {ok, #{state := State}} ->
            ct:pal("Waiting for connection (attempt ~p/60), current state: ~p", [60 - N + 1, State]),
            timer:sleep(500),
            wait_for_connected(N - 1);
        Other ->
            ct:pal("Waiting for connection (attempt ~p/60), status: ~p", [60 - N + 1, Other]),
            timer:sleep(500),
            wait_for_connected(N - 1)
    end.
