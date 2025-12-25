-module(router_r10_client_utils).

-doc "R10 Client Utilities for E2E Tests".
%%
%% Provides client generation strategies (sync/concurrent) for R10 E2E scenarios.
%% Supports configurable load profiles (ci/heavy) via CT config.
%%
%% @test_category utils, r10, e2e

-export([
    spawn_clients/2,
    spawn_clients/3,
    spawn_clients_sync/2,
    spawn_clients_concurrent/2,
    wait_for_clients/2,
    get_r10_config/0,
    get_r10_profile/0
]).

-spec spawn_clients(non_neg_integer(), non_neg_integer()) -> [pid()].
spawn_clients(NumClients, RequestsPerClient) ->
    spawn_clients(NumClients, RequestsPerClient, concurrent).

-spec spawn_clients(non_neg_integer(), non_neg_integer(), sync | concurrent) -> [pid()].
spawn_clients(NumClients, RequestsPerClient, Strategy) ->
    case Strategy of
        sync ->
            spawn_clients_sync(NumClients, RequestsPerClient);
        concurrent ->
            spawn_clients_concurrent(NumClients, RequestsPerClient)
    end.

%% Each client runs sequentially, one after another (inside a single worker process).
-spec spawn_clients_sync(non_neg_integer(), non_neg_integer()) -> [pid()].
spawn_clients_sync(NumClients, RequestsPerClient) ->
    Pid = spawn(fun() ->
        lists:foreach(
            fun(ClientId) ->
                lists:foreach(
                    fun(ReqId) ->
                        Subject =
                            <<"test.subject.",
                              (integer_to_binary(ClientId))/binary,
                              ".",
                              (integer_to_binary(ReqId))/binary>>,
                        Payload = <<"payload-", (integer_to_binary(ReqId))/binary>>,
                        _ = router_nats:publish(Subject, Payload),
                        receive after 0 -> ok end
                    end,
                    lists:seq(1, RequestsPerClient)
                )
            end,
            lists:seq(1, NumClients)
        )
    end),
    [Pid].

%% Each client runs in parallel, generating concurrent load.
-spec spawn_clients_concurrent(non_neg_integer(), non_neg_integer()) -> [pid()].
spawn_clients_concurrent(NumClients, RequestsPerClient) ->
    lists:map(
        fun(ClientId) ->
            spawn(fun() ->
                lists:foreach(
                    fun(ReqId) ->
                        Subject =
                            <<"test.subject.",
                              (integer_to_binary(ClientId))/binary,
                              ".",
                              (integer_to_binary(ReqId))/binary>>,
                        Payload = <<"payload-", (integer_to_binary(ReqId))/binary>>,
                        _ = router_nats:publish(Subject, Payload),
                        receive after 0 -> ok end
                    end,
                    lists:seq(1, RequestsPerClient)
                )
            end)
        end,
        lists:seq(1, NumClients)
    ).

-spec wait_for_clients([pid()], integer()) -> ok.
wait_for_clients([], _Timeout) ->
    ok;
wait_for_clients(Pids, Timeout) ->
    Start = erlang:monotonic_time(millisecond),
    wait_for_clients_loop(Pids, Start, Timeout).

wait_for_clients_loop([], _Start, _Timeout) ->
    ok;
wait_for_clients_loop(Pids, Start, Timeout) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - Start,
    case Elapsed >= Timeout of
        true ->
            ct:fail({clients_timeout, Pids});
        false ->
            Alive = [P || P <- Pids, is_process_alive(P)],
            case Alive of
                [] ->
                    ok;
                _ ->
                    receive after 50 -> ok end,
                    wait_for_clients_loop(Alive, Start, Timeout)
            end
    end.

%% Returns map with r10_* parameters.
-spec get_r10_config() -> #{atom() => term()}.
get_r10_config() ->
    Profile = get_r10_profile(),
    {DefaultClients, DefaultRequests} =
        case Profile of
            heavy ->
                {50, 100};
            _ ->
                {10, 20}
        end,
    #{
        load_clients => ct:get_config(r10_load_clients, DefaultClients),
        requests_per_client => ct:get_config(r10_requests_per_client, DefaultRequests),
        failure_type => ct:get_config(r10_failure_type, error),
        profile => Profile
    }.

-spec get_r10_profile() -> ci | heavy.
get_r10_profile() ->
    case ct:get_config(r10_profile, ci) of
        heavy -> heavy;
        _ -> ci
    end.
