-module(router_gen_server_lifecycle_template).

-doc "Gen Server Lifecycle Pattern Template".
%%
%% This is a template for implementing the reset/lifecycle pattern in gen_server modules.
%% Copy this file and customize it for your gen_server module.
%%
%% Pattern: Safe reset via handle_call(reset_all, ...) - clears ETS tables but keeps process alive
%% This is called from test utilities, should not kill the process
%%
%% Reference implementations:
%% - router_circuit_breaker.erl (simple single table)
%% - router_rbac.erl (multiple tables with safe deletion)
%% - router_rate_limit_store.erl (single table)
%%
%% @see DESIGN_PATTERNS.md#lifecycle-patterns For lifecycle pattern documentation
%%
%% ⚠️ TEMPLATE FILE - DO NOT USE DIRECTLY
%% Copy this file and customize for your gen_server module.

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([reset/0]).

-include("beamline_router.hrl").

-record(state, {
    table :: ets:tid() | atom()
}).

-define(TABLE, module_table).

%% ============================================================================
%% Public API
%% ============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Safe reset via handle_call(reset_all, ...) - clears ETS table but keeps process alive
%% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
-spec reset() -> ok | {error, term()}.
reset() ->
    try
        gen_server:call(?MODULE, reset_all, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(~"Module server not running for reset", #{
                ~"event" => ~"module_reset"
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(~"Module reset timeout", #{
                ~"event" => ~"module_reset"
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(~"Module reset error", #{
                ~"event" => ~"module_reset",
                ~"error" => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

%% ============================================================================
%% Gen Server Callbacks
%% ============================================================================

init([]) ->
    Table = ets:new(?TABLE, [
        set,
        named_table,
        protected,
        {read_concurrency, true}
    ]),
    {ok, #state{table = Table}}.

handle_call(reset_all, _From, State = #state{table = Table}) ->
    %% Safe reset: clear all states but keep process and ETS table alive
    %% This is called from test utilities, should not kill the process
    %% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(~"Module reset_all: ETS table undefined", #{
                ~"event" => ~"module_reset_all"
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(~"Module reset_all: table cleared", #{
                ~"event" => ~"module_reset_all"
            }),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

