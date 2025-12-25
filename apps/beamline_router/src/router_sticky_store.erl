-module(router_sticky_store).

-doc "Sticky Session Store".
%% Persistent storage for sticky session mappings (session_key -> provider_id)
%% CP1: ETS-based storage with TTL
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_provider/2, set_provider/3, clear_session/2, clear_expired/0]).
-export([get_table_size/0, get_table_memory/0, check_size_limit/0]).
-ignore_xref([
    {router_sticky_store, start_link, 0},
    {router_sticky_store, clear_expired, 0},
    {router_sticky_store, clear_session, 2}
]).

-include("beamline_router.hrl").

-define(TABLE, sticky_sessions).
-define(DEFAULT_TTL_SEC, 3600).  %% 1 hour default TTL

-record(state, {
    table :: ets:tid(),
    ttl_sec :: integer()
}).

-record(session, {
    key :: {tenant_id(), binary()},  %% {tenant_id, session_key}
    provider_id :: binary(),
    expires_at :: integer()  %% Unix timestamp
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Table = ets:new(?TABLE, [
        set,
        named_table,
        {keypos, #session.key},
        protected,
        {read_concurrency, true}
    ]),
    
    %% Schedule periodic cleanup
    schedule_cleanup(),
    
    {ok, #state{table = Table, ttl_sec = ?DEFAULT_TTL_SEC}}.

get_provider(TenantId, SessionKey) ->
    gen_server:call(?MODULE, {get_provider, TenantId, SessionKey}).

set_provider(TenantId, SessionKey, ProviderId) ->
    gen_server:call(?MODULE, {set_provider, TenantId, SessionKey, ProviderId}).

clear_session(TenantId, SessionKey) ->
    gen_server:call(?MODULE, {clear_session, TenantId, SessionKey}).

clear_expired() ->
    gen_server:call(?MODULE, clear_expired).

handle_call({get_provider, TenantId, SessionKey}, _From, State) ->
    Key = {TenantId, SessionKey},
    Now = erlang:system_time(second),
    
    case ets:lookup(?TABLE, Key) of
        [#session{provider_id = ProviderId, expires_at = ExpiresAt}] when ExpiresAt > Now ->
            {reply, {ok, ProviderId}, State};
        [#session{}] ->
            %% Expired, remove it
            ets:delete(?TABLE, Key),
            {reply, {error, not_found}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({set_provider, TenantId, SessionKey, ProviderId}, _From, State) ->
    Key = {TenantId, SessionKey},
    ExpiresAt = erlang:system_time(second) + State#state.ttl_sec,
    
    Session = #session{
        key = Key,
        provider_id = ProviderId,
        expires_at = ExpiresAt
    },
    
    ets:insert(?TABLE, Session),
    {reply, ok, State};
handle_call({clear_session, TenantId, SessionKey}, _From, State) ->
    Key = {TenantId, SessionKey},
    ets:delete(?TABLE, Key),
    {reply, ok, State};
handle_call(clear_expired, _From, State) ->
    Now = erlang:system_time(second),
    Deleted = clear_expired_sessions(Now),
    {reply, {ok, Deleted}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    Now = erlang:system_time(second),
    _Deleted = clear_expired_sessions(Now),
    schedule_cleanup(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal: Clear expired sessions
clear_expired_sessions(Now) ->
    MatchSpec = [
        {
            #session{key = '$1', expires_at = '$2', _ = '_'},
            [{'<', '$2', Now}],
            ['$1']
        }
    ],
    Keys = ets:select(?TABLE, MatchSpec),
    [ets:delete(?TABLE, Key) || Key <- Keys],
    length(Keys).

%% Internal: Schedule cleanup
schedule_cleanup() ->
    erlang:send_after(60000, self(), cleanup_expired).  %% Every minute

-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(?TABLE, size) of
        undefined -> undefined;
        Size -> Size
    end.

-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(?TABLE, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, sticky_store_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(?TABLE, Limit);
        _ -> {error, invalid_limit}
    end.
