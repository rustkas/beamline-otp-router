%% Router control API orchestrator (skeleton).
-module(router_control).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("beamline_router.hrl").

-record(state, {
    started_at_ms :: integer()
}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #state{}}.
init([]) ->
    Now = erlang:system_time(millisecond),
    router_logger:info(~"Router control orchestrator started", #{
        ~"component" => ~"router_control"
    }),
    {ok, #state{started_at_ms = Now}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.
