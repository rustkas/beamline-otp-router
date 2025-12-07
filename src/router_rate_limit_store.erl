%% @doc Rate Limit Store
%% Token bucket implementation for per-policy and per-tenant rate limiting
%% CP2: ETS-based storage with token bucket algorithm
-module(router_rate_limit_store).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([check_rate_limit/3, reset_rate_limit/2, get_rate_limit_status/2, reset/0]).
-export([get_table_size/0, get_table_memory/0, check_size_limit/0]).

-ignore_xref([
    {router_rate_limit_store, start_link, 0},
    {router_rate_limit_store, reset_rate_limit, 2},
    {router_rate_limit_store, get_rate_limit_status, 2}
]).

-include("beamline_router.hrl").

-define(TABLE, rate_limits).
-define(CLEANUP_INTERVAL_MS, 60000).  %% Cleanup expired buckets every minute
-define(DEFAULT_MAX_SIZE, 100000).  %% Default maximum table size (100k entries)

-record(state, {
    table :: ets:tid()
}).

-record(rate_limit_bucket, {
    key :: {tenant_id(), policy_id()} | tenant_id(),  %% {TenantId, PolicyId} or TenantId
    tokens :: integer(),  %% Current tokens in bucket
    last_refill :: integer(),  %% Unix timestamp (seconds) of last refill
    requests_per_second :: integer(),  %% Rate limit: requests per second
    burst :: integer()  %% Burst capacity
}).

%% @doc Start rate limit store
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize rate limit store
init([]) ->
    Table = ets:new(?TABLE, [
        set,
        named_table,
        {keypos, #rate_limit_bucket.key},
        protected,
        {read_concurrency, true}
    ]),
    
    %% Schedule periodic cleanup
    schedule_cleanup(),
    
    {ok, #state{table = Table}}.

%% @doc Check rate limit
%% Args:
%%   Scope: policy | tenant
%%   Identifier: {TenantId, PolicyId} | TenantId
%%   Config: #{enabled => boolean(), requests_per_second => integer(), burst => integer()}
%% Returns:
%%   {ok, allow} | {error, {rate_limit_exceeded, Details}}
check_rate_limit(Scope, Identifier, Config) ->
    gen_server:call(?MODULE, {check_rate_limit, Scope, Identifier, Config}).

%% @doc Reset rate limit for identifier
reset_rate_limit(Scope, Identifier) ->
    gen_server:call(?MODULE, {reset_rate_limit, Scope, Identifier}).

%% @doc Get rate limit status
get_rate_limit_status(Scope, Identifier) ->
    gen_server:call(?MODULE, {get_status, Scope, Identifier}).

%% @doc Reset all rate limit state (for testing)
%% Safe reset via handle_call(reset_all, ...) - clears ETS table but keeps process alive
%% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
-spec reset() -> ok | {error, term()}.
reset() ->
    try
        gen_server:call(?MODULE, reset_all, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(<<"Rate limit store server not running for reset">>, #{
                <<"event">> => <<"rate_limit_store_reset">>
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(<<"Rate limit store reset timeout">>, #{
                <<"event">> => <<"rate_limit_store_reset">>
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(<<"Rate limit store reset error">>, #{
                <<"event">> => <<"rate_limit_store_reset">>,
                <<"error">> => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

%% @doc Handle check_rate_limit request
handle_call({check_rate_limit, Scope, Identifier, Config}, _From, State) ->
    Enabled = maps:get(<<"enabled">>, Config, false),
    case Enabled of
        false ->
            {reply, {ok, allow}, State};
        true ->
            RequestsPerSecond = maps:get(<<"requests_per_second">>, Config, 100),
            Burst = maps:get(<<"burst">>, Config, 50),
            
            Key = build_key(Scope, Identifier),
            Now = erlang:system_time(second),
            
            Result = case ets:lookup(?TABLE, Key) of
                [#rate_limit_bucket{
                    tokens = Tokens,
                    last_refill = LastRefill,
                    requests_per_second = RPS,
                    burst = BurstSize
                }] ->
                    %% Refill tokens based on time elapsed
                    %% Protect against clock skew (clock going backward)
                    TimeElapsed = max(0, Now - LastRefill),
                    TokensToAdd = TimeElapsed * RPS,
                    NewTokens = min(Tokens + TokensToAdd, BurstSize),
                    
                    if
                        NewTokens >= 1 ->
                            %% Token available, consume one
                            UpdatedBucket = #rate_limit_bucket{
                                key = Key,
                                tokens = NewTokens - 1,
                                last_refill = Now,
                                requests_per_second = RPS,
                                burst = BurstSize
                            },
                            ets:insert(?TABLE, UpdatedBucket),
                            
                            %% Emit metrics
                            router_metrics:emit_metric(router_rate_limit_allowed_total, #{count => 1}, #{
                                scope => atom_to_binary(Scope, utf8),
                                tenant_id => extract_tenant_id(Identifier)
                            }),
                            
                            {ok, allow};
                        true ->
                            %% No tokens available
                            RemainingTime = case NewTokens < 0 of
                                true -> 1;  %% At least 1 second
                                false -> 1 - (NewTokens / RPS)
                            end,
                            
                            %% Emit metrics
                            router_metrics:emit_metric(router_rate_limit_exceeded_total, #{count => 1}, #{
                                scope => atom_to_binary(Scope, utf8),
                                tenant_id => extract_tenant_id(Identifier)
                            }),
                            
                            {error, {rate_limit_exceeded, #{
                                scope => Scope,
                                identifier => Identifier,
                                limit => RPS,
                                burst => BurstSize,
                                retry_after_seconds => erlang:ceil(RemainingTime)
                            }}}
                    end;
                [] ->
                    %% First request: create bucket with full burst capacity
                    Bucket = #rate_limit_bucket{
                        key = Key,
                        tokens = Burst - 1,  %% Consume one token
                        last_refill = Now,
                        requests_per_second = RequestsPerSecond,
                        burst = Burst
                    },
                    ets:insert(?TABLE, Bucket),
                    
                    %% Emit metrics
                    router_metrics:emit_metric(router_rate_limit_allowed_total, #{count => 1}, #{
                        scope => atom_to_binary(Scope, utf8),
                        tenant_id => extract_tenant_id(Identifier)
                    }),
                    
                    {ok, allow}
            end,
            
            {reply, Result, State}
    end;
handle_call({reset_rate_limit, Scope, Identifier}, _From, State) ->
    Key = build_key(Scope, Identifier),
    ets:delete(?TABLE, Key),
    {reply, ok, State};
handle_call({get_status, Scope, Identifier}, _From, State) ->
    Key = build_key(Scope, Identifier),
    case ets:lookup(?TABLE, Key) of
        [#rate_limit_bucket{
            tokens = Tokens,
            last_refill = LastRefill,
            requests_per_second = RPS,
            burst = Burst
        }] ->
            Now = erlang:system_time(second),
            TimeElapsed = Now - LastRefill,
            TokensToAdd = TimeElapsed * RPS,
            CurrentTokens = min(Tokens + TokensToAdd, Burst),
            
            {reply, {ok, #{
                <<"tokens">> => CurrentTokens,
                <<"limit">> => RPS,
                <<"burst">> => Burst,
                <<"last_refill">> => LastRefill
            }}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call(reset_all, _From, State = #state{table = Table}) ->
    %% Safe reset: clear all states but keep process and ETS table alive
    %% This is called from test utilities, should not kill the process
    %% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(<<"Rate limit store reset_all: ETS table undefined">>, #{
                <<"event">> => <<"rate_limit_store_reset_all">>
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(<<"Rate limit store reset_all: table cleared">>, #{
                <<"event">> => <<"rate_limit_store_reset_all">>
            }),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Periodic cleanup of expired buckets
handle_info(cleanup_expired, State) ->
    Now = erlang:system_time(second),
    _Deleted = clear_expired_buckets(Now),
    %% Check and enforce size limit if configured
    enforce_size_limit_if_needed(),
    schedule_cleanup(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal: Build key from scope and identifier
build_key(policy, {TenantId, PolicyId}) ->
    {TenantId, PolicyId};
build_key(tenant, TenantId) ->
    TenantId;
build_key(_, Identifier) ->
    Identifier.

%% Internal: Extract tenant_id from identifier
extract_tenant_id({TenantId, _PolicyId}) ->
    TenantId;
extract_tenant_id(TenantId) when is_binary(TenantId) ->
    TenantId;
extract_tenant_id(_) ->
    <<"unknown">>.

%% Internal: Clear expired buckets (buckets not used for 1 hour)
clear_expired_buckets(Now) ->
    ExpiryThreshold = Now - 3600,  %% 1 hour
    MatchSpec = [
        {
            #rate_limit_bucket{key = '$1', last_refill = '$2', _ = '_'},
            [{'<', '$2', ExpiryThreshold}],
            ['$1']
        }
    ],
    Keys = ets:select(?TABLE, MatchSpec),
    [ets:delete(?TABLE, Key) || Key <- Keys],
    length(Keys).

%% Internal: Schedule cleanup
schedule_cleanup() ->
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired).

%% @doc Get current table size (number of entries)
-spec get_table_size() -> integer() | undefined.
get_table_size() ->
    case ets:info(?TABLE, size) of
        undefined -> undefined;
        Size -> Size
    end.

%% @doc Get current table memory usage in bytes
-spec get_table_memory() -> integer() | undefined.
get_table_memory() ->
    case ets:info(?TABLE, memory) of
        undefined -> undefined;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

%% @doc Check if table size exceeds configured limit
-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, rate_limit_store_max_size, ?DEFAULT_MAX_SIZE),
    router_resource_monitor:check_table_size_limit(?TABLE, MaxSize).

%% Internal: Enforce size limit by evicting oldest entries if needed
enforce_size_limit_if_needed() ->
    MaxSize = application:get_env(beamline_router, rate_limit_store_max_size, ?DEFAULT_MAX_SIZE),
    case router_resource_monitor:check_table_size_limit(?TABLE, MaxSize) of
        {ok, _Size} -> ok;
        {error, exceeded, CurrentSize, Limit} ->
            CountToEvict = CurrentSize - Limit,
            EvictedCount = evict_oldest_buckets(CountToEvict),
            case EvictedCount > 0 of
                true ->
                    router_logger:warn(<<"Rate limit store size limit enforced">>, #{
                        <<"current_size">> => CurrentSize,
                        <<"limit">> => Limit,
                        <<"evicted">> => EvictedCount
                    });
                false ->
                    ok
            end
    end.

%% Internal: Evict oldest buckets (by last_refill time)
evict_oldest_buckets(Count) when Count =< 0 -> 0;
evict_oldest_buckets(Count) ->
    %% Get all buckets sorted by last_refill (oldest first)
    AllBuckets = ets:foldl(fun(Bucket, Acc) -> [Bucket | Acc] end, [], ?TABLE),
    SortedBuckets = lists:sort(fun(B1, B2) ->
        B1#rate_limit_bucket.last_refill =< B2#rate_limit_bucket.last_refill
    end, AllBuckets),
    
    %% Evict oldest N buckets
    BucketsToEvict = lists:sublist(SortedBuckets, Count),
    lists:foreach(fun(Bucket) ->
        ets:delete(?TABLE, Bucket#rate_limit_bucket.key)
    end, BucketsToEvict),
    length(BucketsToEvict).
