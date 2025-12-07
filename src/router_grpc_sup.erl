%% @doc gRPC Server Supervisor
%% 
%% Supervisor for gRPC server components.
%% This is a placeholder supervisor that can be extended when gRPC server is implemented.
%% 
%% Currently, gRPC functionality is handled via router_grpc module directly,
%% so this supervisor has no children. It exists to satisfy the supervisor tree
%% structure when grpc_enabled=true.
-module(router_grpc_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @doc Start the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initialize supervisor tree
%% Currently empty - no gRPC server children
%% This can be extended when gRPC server implementation is added
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
