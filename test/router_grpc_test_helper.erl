%% @doc Common test helper for gRPC server setup/teardown
%% Provides unified functions for starting/stopping gRPC server in test suites
-module(router_grpc_test_helper).

-export([start_grpc_server/0, start_grpc_server/1, stop_grpc_server/0, get_grpc_port/1]).
-export([create_context_with_auth/1, create_context_without_auth/0]).

-include_lib("common_test/include/ct.hrl").

%% @doc Start gRPC server with ephemeral port (port = 0)
start_grpc_server() ->
    start_grpc_server([]).

%% @doc Start gRPC server with configuration
%% Options:
%%   - {port, Port} - specific port (default: 0 for ephemeral)
%%   - {admin_api_key, Key} - admin API key (default: <<"test-admin-key">>)
start_grpc_server(Options) ->
    %% Load application
    _ = application:load(beamline_router),
    
    %% Set ephemeral port (0 = OS assigns available port)
    Port = proplists:get_value(port, Options, 0),
    ok = application:set_env(beamline_router, grpc_port, Port),
    
    %% Set admin API key
    AdminKey = proplists:get_value(admin_api_key, Options, <<"test-admin-key">>),
    ok = application:set_env(beamline_router, admin_api_key, AdminKey),
    
    %% Start application
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get actual assigned port (if ephemeral)
    ActualPort = get_grpc_port([]),
    
    ct:log("gRPC server started on port ~p", [ActualPort]),
    
    {ok, ActualPort}.

%% @doc Stop gRPC server
stop_grpc_server() ->
    application:stop(beamline_router),
    application:unload(beamline_router),
    ok.

%% @doc Get actual gRPC port from running server
%% Returns the port number assigned by OS (for ephemeral ports)
get_grpc_port(_Config) ->
    %% Try to get port from grpcbox server configuration
    case application:get_env(beamline_router, grpc_port) of
        {ok, Port} when Port =/= 0 ->
            Port;
        {ok, 0} ->
            %% Ephemeral port - need to query grpcbox for actual port
            %% This is a simplified approach; in production, you might need
            %% to query grpcbox server directly
            case whereis(router_grpc_sup) of
                undefined ->
                    %% Fallback: try to get from supervisor children
                    get_port_from_supervisor();
                Pid when is_pid(Pid) ->
                    get_port_from_supervisor()
            end;
        undefined ->
            %% Default port
            9000
    end.

%% Internal: Get port from supervisor children
get_port_from_supervisor() ->
    try
        %% Query grpcbox server for actual port
        %% This is implementation-specific and may need adjustment
        case erlang:function_exported(grpcbox_server, get_port, 1) of
            true ->
                %% Try to get port from grpcbox server
                case catch grpcbox_server:get_port(router_grpc_sup) of
                    Port when is_integer(Port) ->
                        Port;
                    _ ->
                        %% Fallback to default
                        9000
                end;
            false ->
                %% Fallback: check if we can get from application env after start
                case application:get_env(beamline_router, grpc_port) of
                    {ok, Port} when is_integer(Port) ->
                        Port;
                    _ ->
                        9000
                end
        end
    catch
        _:_ ->
            %% Fallback to default
            9000
    end.

%% @doc Create test context with authentication
create_context_with_auth(AdminKey) ->
    Metadata = [
        {<<"x-api-key">>, AdminKey}
    ],
    #{metadata => Metadata}.

%% @doc Create test context without authentication
create_context_without_auth() ->
    #{metadata => []}.

