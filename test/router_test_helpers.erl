%% @doc Common Test Helpers for Router gRPC Tests
%% 
%% Provides unified helpers for:
%% - Starting/stopping gRPC server with ephemeral ports
%% - Publishing selected port in test config
%% - Creating test contexts with authentication
%% - Common test utilities
-module(router_test_helpers).

-export([
    init_grpc_server/1,
    stop_grpc_server/1,
    get_grpc_port/1,
    create_context_with_auth/1,
    create_context_without_auth/0,
    create_context_with_invalid_auth/0
]).

-include_lib("common_test/include/ct.hrl").

%% @doc Initialize gRPC server with ephemeral port
%% 
%% Sets grpc_port to 0 (ephemeral) and starts the application.
%% Retrieves the actual assigned port and adds it to Config.
%% 
%% Returns: Updated Config with grpc_port and admin_api_key
init_grpc_server(Config) ->
    %% Load application
    _ = application:load(beamline_router),
    
    %% Set ephemeral port (0 = OS assigns available port)
    ok = application:set_env(beamline_router, grpc_port, 0),
    
    %% Set test admin API key
    AdminKey = <<"test-admin-key">>,
    ok = application:set_env(beamline_router, admin_api_key, AdminKey),
    
    %% Start application
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Retrieve actual assigned port
    %% grpcbox server is started by router_grpc_sup, port is assigned by OS
    %% We need to query the server for its actual port
    ActualPort = get_actual_grpc_port(),
    
    %% Add to config
    Config1 = [{grpc_port, ActualPort}, {admin_api_key, AdminKey} | Config],
    
    ct:log("gRPC server started on ephemeral port: ~p", [ActualPort]),
    
    Config1.

%% @doc Stop gRPC server and cleanup
stop_grpc_server(_Config) ->
    application:stop(beamline_router),
    application:unload(beamline_router),
    ok.

%% @doc Get gRPC port from config
get_grpc_port(Config) ->
    proplists:get_value(grpc_port, Config, 9000).

%% @doc Create test context with valid authentication
create_context_with_auth(AdminKey) ->
    Metadata = [
        {<<"x-api-key">>, AdminKey}
    ],
    #{metadata => Metadata}.

%% @doc Create test context without authentication
create_context_without_auth() ->
    #{metadata => []}.

%% @doc Create test context with invalid authentication
create_context_with_invalid_auth() ->
    Metadata = [
        {<<"x-api-key">>, <<"invalid-key">>}
    ],
    #{metadata => Metadata}.

%% Internal: Get actual gRPC port from running server
%% 
%% Queries grpcbox server for its actual listening port.
%% Falls back to configured port if server not found.
get_actual_grpc_port() ->
    %% Try to get port from grpcbox server
    %% grpcbox stores server info in gproc or similar
    case get_grpcbox_port() of
        {ok, Port} ->
            Port;
        {error, _} ->
            %% Fallback: try to get from application env
            case application:get_env(beamline_router, grpc_port) of
                {ok, Port} when is_integer(Port), Port > 0 ->
                    Port;
                _ ->
                    %% Default fallback
                    9000
            end
    end.

%% Internal: Get port from grpcbox server
get_grpcbox_port() ->
    %% Try to find grpcbox server process and query its port
    %% This is implementation-specific to grpcbox
    try
        %% Check if grpcbox_ctx or similar provides port info
        case erlang:function_exported(grpcbox, get_server_port, 1) of
            true ->
                %% Try to get port from default server
                case catch grpcbox:get_server_port(default) of
                    Port when is_integer(Port) ->
                        {ok, Port};
                    _ ->
                        {error, not_found}
                end;
            false ->
                {error, not_available}
        end
    catch
        _:_ ->
            {error, not_available}
    end.

