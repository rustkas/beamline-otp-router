%% Common test helpers (gRPC requests, port discovery, etc.)
%%
%% NOTE:
%% - Avoid module-level warning suppression.
%% - Avoid `catch`; use try/catch.
%%
-module(router_test_helpers).

-export([
    grpc_port/0,
    grpc_port/1,
    grpc_client_defaults/0,
    grpc_metadata/0,
    grpc_metadata/1,
    grpc_tenant_metadata/1,
    new_request_id/0,
    make_test_request/2,
    make_test_request/3,
    make_test_message/2
]).

-define(DEFAULT_TIMEOUT_MS, 5000).

-spec grpc_port() -> non_neg_integer().
grpc_port() ->
    grpc_port(?DEFAULT_TIMEOUT_MS).

-spec grpc_port(non_neg_integer()) -> non_neg_integer().
grpc_port(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs > 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    grpc_port_loop(TimeoutMs, Deadline).

-spec grpc_client_defaults() -> map().
grpc_client_defaults() ->
    #{
            host => "127.0.0.1",
            port => grpc_port(),
            tls => false,
            timeout => ?DEFAULT_TIMEOUT_MS
        }.

-spec grpc_metadata() -> map().
grpc_metadata() ->
    grpc_metadata(#{}).

-spec grpc_metadata(map()) -> map().
grpc_metadata(Extra) when is_map(Extra) ->
    maps:merge(#{
            <<"x-api-key">> => <<"test-admin-key">>,
            <<"x-request-id">> => new_request_id(),
            <<"x-tenant-id">> => <<"default_tenant">>
        },
        Extra
    ).

-spec grpc_tenant_metadata(binary()) -> map().
grpc_tenant_metadata(TenantId) when is_binary(TenantId) ->
    grpc_metadata(#{<<"x-tenant-id">> => TenantId}).

-spec new_request_id() -> binary().
new_request_id() ->
    Int = erlang:unique_integer([positive, monotonic]),
    list_to_binary(io_lib:format("req-~p", [Int])).

%% Internal

-spec grpc_port_loop(non_neg_integer(), integer()) -> non_neg_integer().
grpc_port_loop(TimeoutMs, Deadline) ->
    Now = erlang:monotonic_time(millisecond),
    if
        Now >= Deadline ->
            ct:fail("gRPC server port not available within ~p ms", [TimeoutMs]);
        true ->
            Port = case erlang:function_exported(grpcbox, get_port, 1) of
                true ->
                    try apply(grpcbox, get_port, [default]) of
                        P when is_integer(P), P > 0 -> P;
                        _ -> undefined
                    catch
                        _:_ -> undefined
                    end;
                false ->
                    case application:get_env(beamline_router, grpc_port) of
                        {ok, P} when is_integer(P), P > 0 -> P;
                        _ -> undefined
                    end
            end,
            case Port of
                PortVal when is_integer(PortVal), PortVal > 0 ->
                    PortVal;
                _ ->
                    timer:sleep(20),
                    grpc_port_loop(TimeoutMs, Deadline)
            end
    end.

%% ============================================================================
%% Soak Testing Helpers
%% ============================================================================

%% @doc Make a simple test routing request
-spec make_test_request(binary(), binary()) -> ok | {error, term()}.
make_test_request(TenantId, PolicyId) ->
    make_test_request(TenantId, PolicyId, #{}).

%% @doc Make a test routing request with custom options
-spec make_test_request(binary(), binary(), map()) -> ok | {error, term()}.
make_test_request(TenantId, PolicyId, Opts) ->
    Message = make_test_message(TenantId, Opts),
    
    %% Attempt to route (may fail if router_core not started)
    try
        case whereis(router_core) of
            undefined ->
                ok;  % Router not running, skip
            _ ->
                router_core:route(#{
                    message => Message,
                    policy_id => PolicyId,
                    context => #{}
                }, #{}),
                ok
        end
    catch
        _:_ -> ok
    end.

%% @doc Create a test message map
-spec make_test_message(binary(), map()) -> map().
make_test_message(TenantId, Opts) ->
    RequestId = maps:get(request_id, Opts, 
                         list_to_binary(io_lib:format("req_~p", [erlang:unique_integer([positive])]))),
    MessageType = maps:get(message_type, Opts, <<"text.generate">>),
    
    #{
        <<"message_id">> => RequestId,
        <<"tenant_id">> => TenantId,
        <<"message_type">> => MessageType,
        <<"payload">> => #{
            <<"prompt">> => <<"Test prompt">>,
            <<"model">> => <<"test-model">>
        },
        <<"metadata">> => #{},
        <<"timestamp_ms">> => erlang:system_time(millisecond)
    }.

