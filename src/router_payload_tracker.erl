%% @doc Payload Size Distribution Tracker
%% Tracks payload sizes per tenant/endpoint for abuse detection
%% CP2+: Abuse detection for heavy payload patterns
-module(router_payload_tracker).

-export([
    init/0,
    track_payload/3,
    check_abuse_pattern/2,
    cleanup_old_entries/0,
    get_stats/1
]).

-include("beamline_router.hrl").

%% ETS table for payload size tracking
-define(PAYLOAD_TRACKING_TABLE, router_payload_size_tracking).

%% Configuration
-define(DEFAULT_LARGE_PAYLOAD_THRESHOLD, 524288).  %% 500KB
-define(DEFAULT_LARGE_PAYLOAD_RATIO_THRESHOLD, 80).  %% 80%
-define(DEFAULT_MIN_REQUESTS_FOR_PATTERN, 10).  %% Minimum requests to detect pattern
-define(DEFAULT_RETENTION_WINDOW_SECONDS, 300).  %% 5 minutes

%% Tracking entry structure
-record(payload_stats, {
    tenant_id :: binary(),
    endpoint :: atom(),
    first_seen :: integer(),  %% Unix timestamp (seconds)
    last_seen :: integer(),  %% Unix timestamp (seconds)
    total_requests :: non_neg_integer(),
    large_payload_count :: non_neg_integer(),
    total_payload_size :: non_neg_integer(),
    min_payload_size :: non_neg_integer(),
    max_payload_size :: non_neg_integer()
}).

%% @doc Initialize payload tracking
-spec init() -> ok.
init() ->
    %% Create ETS table for payload tracking
    _Table = ets:new(?PAYLOAD_TRACKING_TABLE, [
        set,
        named_table,
        public,
        {write_concurrency, true},
        {read_concurrency, true},
        {keypos, #payload_stats.tenant_id}
    ]),
    
    %% Log initialization
    router_logger:info(<<"Payload size tracking initialized">>, #{
        <<"table">> => atom_to_binary(?PAYLOAD_TRACKING_TABLE, utf8)
    }),
    
    ok.

%% @doc Track payload size for tenant/endpoint
-spec track_payload(binary(), atom(), non_neg_integer()) -> ok.
track_payload(TenantId, Endpoint, PayloadSize) when is_binary(TenantId), is_atom(Endpoint), is_integer(PayloadSize) ->
    try
        Key = {TenantId, Endpoint},
        Now = erlang:system_time(second),
        
        %% Get or create stats entry
        Stats = case ets:lookup(?PAYLOAD_TRACKING_TABLE, Key) of
            [] ->
                %% Create new entry
                #payload_stats{
                    tenant_id = TenantId,
                    endpoint = Endpoint,
                    first_seen = Now,
                    last_seen = Now,
                    total_requests = 1,
                    large_payload_count = case PayloadSize > get_large_payload_threshold() of true -> 1; false -> 0 end,
                    total_payload_size = PayloadSize,
                    min_payload_size = PayloadSize,
                    max_payload_size = PayloadSize
                };
            [{Key, ExistingStats}] ->
                %% Update existing entry
                LargePayloadThreshold = get_large_payload_threshold(),
                IsLarge = PayloadSize > LargePayloadThreshold,
                
                ExistingStats#payload_stats{
                    last_seen = Now,
                    total_requests = ExistingStats#payload_stats.total_requests + 1,
                    large_payload_count = case IsLarge of
                        true -> ExistingStats#payload_stats.large_payload_count + 1;
                        false -> ExistingStats#payload_stats.large_payload_count
                    end,
                    total_payload_size = ExistingStats#payload_stats.total_payload_size + PayloadSize,
                    min_payload_size = min(ExistingStats#payload_stats.min_payload_size, PayloadSize),
                    max_payload_size = max(ExistingStats#payload_stats.max_payload_size, PayloadSize)
                }
        end,
        
        %% Store updated stats
        ets:insert(?PAYLOAD_TRACKING_TABLE, {Key, Stats}),
        
        ok
    catch
        Class:Reason ->
            %% Log error but don't fail
            router_logger:error(<<"Failed to track payload size">>, #{
                <<"error">> => {Class, Reason},
                <<"tenant_id">> => TenantId,
                <<"endpoint">> => atom_to_binary(Endpoint, utf8)
            }),
            ok
    end.

%% @doc Check for abuse patterns
-spec check_abuse_pattern(binary(), atom()) -> {ok, normal} | {abuse, heavy_payload, map()}.
check_abuse_pattern(TenantId, Endpoint) when is_binary(TenantId), is_atom(Endpoint) ->
    try
        Key = {TenantId, Endpoint},
        
        case ets:lookup(?PAYLOAD_TRACKING_TABLE, Key) of
            [] ->
                {ok, normal};
            [{Key, Stats}] ->
                %% Check if we have enough requests to detect pattern
                MinRequests = get_min_requests_for_pattern(),
                case Stats#payload_stats.total_requests >= MinRequests of
                    false ->
                        {ok, normal};
                    true ->
                        %% Calculate large payload ratio
                        LargePayloadRatio = (Stats#payload_stats.large_payload_count * 100) div Stats#payload_stats.total_requests,
                        LargePayloadRatioThreshold = get_large_payload_ratio_threshold(),
                        
                        case LargePayloadRatio > LargePayloadRatioThreshold of
                            true ->
                                %% Abuse pattern detected
                                AbuseContext = #{
                                    <<"tenant_id">> => TenantId,
                                    <<"endpoint">> => atom_to_binary(Endpoint, utf8),
                                    <<"total_requests">> => Stats#payload_stats.total_requests,
                                    <<"large_payload_count">> => Stats#payload_stats.large_payload_count,
                                    <<"large_payload_ratio">> => LargePayloadRatio,
                                    <<"large_payload_threshold">> => get_large_payload_threshold(),
                                    <<"avg_payload_size">> => Stats#payload_stats.total_payload_size div Stats#payload_stats.total_requests,
                                    <<"min_payload_size">> => Stats#payload_stats.min_payload_size,
                                    <<"max_payload_size">> => Stats#payload_stats.max_payload_size
                                },
                                {abuse, heavy_payload, AbuseContext};
                            false ->
                                {ok, normal}
                        end
                end
        end
    catch
        Class:Reason ->
            %% Log error but return normal
            router_logger:error(<<"Failed to check abuse pattern">>, #{
                <<"error">> => {Class, Reason},
                <<"tenant_id">> => TenantId,
                <<"endpoint">> => atom_to_binary(Endpoint, utf8)
            }),
            {ok, normal}
    end.

%% @doc Cleanup old entries (based on retention window)
-spec cleanup_old_entries() -> {ok, non_neg_integer()}.
cleanup_old_entries() ->
    try
        Now = erlang:system_time(second),
        RetentionWindow = get_retention_window_seconds(),
        CutoffTime = Now - RetentionWindow,
        
        %% Find all entries older than cutoff
        OldEntries = ets:foldl(fun({Key, Stats}, Acc) ->
            case Stats#payload_stats.last_seen < CutoffTime of
                true -> [Key | Acc];
                false -> Acc
            end
        end, [], ?PAYLOAD_TRACKING_TABLE),
        
        %% Delete old entries
        lists:foreach(fun(Key) ->
            ets:delete(?PAYLOAD_TRACKING_TABLE, Key)
        end, OldEntries),
        
        RemovedCount = length(OldEntries),
        {ok, RemovedCount}
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to cleanup old entries">>, #{
                <<"error">> => {Class, Reason}
            }),
            {ok, 0}
    end.

%% @doc Get stats for tenant/endpoint
-spec get_stats(binary()) -> {ok, list()} | {error, term()}.
get_stats(TenantId) when is_binary(TenantId) ->
    try
        %% Find all entries for tenant
        Stats = ets:foldl(fun({Key, EntryStats}, Acc) ->
            {TId, _Endpoint} = Key,
            case TId =:= TenantId of
                true -> [EntryStats | Acc];
                false -> Acc
            end
        end, [], ?PAYLOAD_TRACKING_TABLE),
        
        {ok, Stats}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% Internal: Get large payload threshold
get_large_payload_threshold() ->
    application:get_env(beamline_router, abuse_large_payload_threshold, ?DEFAULT_LARGE_PAYLOAD_THRESHOLD).

%% Internal: Get large payload ratio threshold
get_large_payload_ratio_threshold() ->
    application:get_env(beamline_router, abuse_large_payload_ratio_threshold, ?DEFAULT_LARGE_PAYLOAD_RATIO_THRESHOLD).

%% Internal: Get minimum requests for pattern detection
get_min_requests_for_pattern() ->
    application:get_env(beamline_router, abuse_min_requests_for_pattern, ?DEFAULT_MIN_REQUESTS_FOR_PATTERN).

%% Internal: Get retention window
get_retention_window_seconds() ->
    application:get_env(beamline_router, abuse_retention_window_seconds, ?DEFAULT_RETENTION_WINDOW_SECONDS).

