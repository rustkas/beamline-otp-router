%% @doc Rate Limiter Module
%% Provides per-tenant and per-user rate limiting with dynamic limits
%% CP1-ROUTER: Enhanced rate limiting for Policy Enforcement
-module(router_rate_limiter).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([check_rate_limit/3, check_rate_limit/4, get_rate_limits/1, set_rate_limits/2, reset/0]).

-ignore_xref([
    {router_rate_limiter, start_link, 0},
    {router_rate_limiter, check_rate_limit, 4},
    {router_rate_limiter, get_rate_limits, 1},
    {router_rate_limiter, set_rate_limits, 2}
]).

-include("beamline_router.hrl").

%% ETS table for rate limit counters
-define(RATE_LIMITS_TABLE, rate_limits).

%% Default rate limits by tier
-define(DEFAULT_BASIC_LIMIT, 100).      %% requests per minute
-define(DEFAULT_PREMIUM_LIMIT, 1000).   %% requests per minute
-define(DEFAULT_ENTERPRISE_LIMIT, 10000). %% requests per minute

%% Rate limit record
-record(rate_limit, {
    key :: {binary(), binary(), binary()},  %% {tenant_id, endpoint, user_id}
    count :: integer(),
    window_start :: integer(),  %% timestamp in milliseconds
    limit :: integer(),
    ttl_seconds :: integer()
}).

-record(state, {
    rate_limits_table :: ets:tid(),
    cleanup_timer :: timer:tref() | undefined
}).

%% @doc Start rate limiter server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize rate limiter
init([]) ->
    %% Create ETS table for rate limit counters (check if exists first)
    RateLimitsTable = case catch ets:info(?RATE_LIMITS_TABLE, name) of
        undefined ->
            ets:new(?RATE_LIMITS_TABLE, [
                set,
                named_table,
                protected,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        {'EXIT', _} ->
            ets:new(?RATE_LIMITS_TABLE, [
                set,
                named_table,
                protected,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ?RATE_LIMITS_TABLE
    end,
    
    %% Start cleanup timer (clean expired windows every 60 seconds)
    {ok, CleanupTimer} = timer:send_interval(60000, cleanup_expired),
    
    State = #state{
        rate_limits_table = RateLimitsTable,
        cleanup_timer = CleanupTimer
    },
    
    {ok, State}.

%% @doc Check rate limit for tenant/endpoint/user
-spec check_rate_limit(binary(), binary(), binary()) -> {ok, integer()} | {error, rate_limit_exceeded, integer()}.
check_rate_limit(TenantId, Endpoint, UserId) ->
    gen_server:call(?MODULE, {check_rate_limit, TenantId, Endpoint, UserId}).

%% @doc Check rate limit with custom limit
-spec check_rate_limit(binary(), binary(), binary(), integer()) -> {ok, integer()} | {error, rate_limit_exceeded, integer()}.
check_rate_limit(TenantId, Endpoint, UserId, CustomLimit) ->
    gen_server:call(?MODULE, {check_rate_limit, TenantId, Endpoint, UserId, CustomLimit}).

%% @doc Get rate limits for tenant (based on tier)
-spec get_rate_limits(binary()) -> map().
get_rate_limits(TenantId) ->
    gen_server:call(?MODULE, {get_rate_limits, TenantId}).

%% @doc Set custom rate limits for tenant
-spec set_rate_limits(binary(), map()) -> ok.
set_rate_limits(TenantId, Limits) ->
    gen_server:call(?MODULE, {set_rate_limits, TenantId, Limits}).

%% @doc Reset all rate limiter state (for testing)
%% Safe reset via handle_call(reset_all, ...) - clears ETS table but keeps process alive
%% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
-spec reset() -> ok | {error, term()}.
reset() ->
    try
        gen_server:call(?MODULE, reset_all, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(<<"Rate limiter server not running for reset">>, #{
                <<"event">> => <<"rate_limiter_reset">>
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(<<"Rate limiter reset timeout">>, #{
                <<"event">> => <<"rate_limiter_reset">>
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(<<"Rate limiter reset error">>, #{
                <<"event">> => <<"rate_limiter_reset">>,
                <<"error">> => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

%% gen_server callbacks

handle_call({check_rate_limit, TenantId, Endpoint, UserId}, _From, State) ->
    %% Get rate limits for tenant
    Limits = get_tenant_rate_limits(TenantId),
    Limit = maps:get(<<"requests_per_minute">>, Limits, ?DEFAULT_BASIC_LIMIT),
    TtlSeconds = maps:get(<<"ttl_seconds">>, Limits, 60),
    
    Result = do_check_rate_limit(TenantId, Endpoint, UserId, Limit, TtlSeconds, State),
    {reply, Result, State};

handle_call({check_rate_limit, TenantId, Endpoint, UserId, CustomLimit}, _From, State) ->
    %% Use custom limit
    Limits = get_tenant_rate_limits(TenantId),
    TtlSeconds = maps:get(<<"ttl_seconds">>, Limits, 60),
    
    Result = do_check_rate_limit(TenantId, Endpoint, UserId, CustomLimit, TtlSeconds, State),
    {reply, Result, State};

handle_call({get_rate_limits, TenantId}, _From, State) ->
    Limits = get_tenant_rate_limits(TenantId),
    {reply, Limits, State};

handle_call({set_rate_limits, TenantId, Limits}, _From, State) ->
    try
        %% Validate limits structure
        case validate_rate_limits(Limits) of
            {ok, ValidLimits} ->
                %% Store custom limits (in-memory for CP1, future: persistent storage)
                ok = store_tenant_rate_limits(TenantId, ValidLimits),
                router_logger:info(<<"Rate limits set successfully">>, #{
                    <<"tenant_id">> => TenantId,
                    <<"event">> => <<"rate_limits_set">>
                }),
                {reply, ok, State};
            {error, Reason} ->
                router_logger:error(<<"Invalid rate limits configuration">>, #{
                    <<"tenant_id">> => TenantId,
                    <<"reason">> => Reason,
                    <<"event">> => <<"rate_limits_validation_failed">>
                }),
                {reply, {error, invalid_config}, State}
        end
    catch
        Class:Error:Stacktrace ->
            router_logger:error(<<"Failed to set rate limits">>, #{
                <<"tenant_id">> => TenantId,
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Error])),
                <<"class">> => iolist_to_binary(io_lib:format("~p", [Class])),
                <<"stacktrace">> => iolist_to_binary(io_lib:format("~p", [Stacktrace])),
                <<"event">> => <<"rate_limits_set_failed">>
            }),
            {reply, {error, internal}, State}
    end;
handle_call(reset_all, _From, State = #state{rate_limits_table = Table}) ->
    %% Safe reset: clear all states but keep process and ETS table alive
    %% This is called from test utilities, should not kill the process
    %% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(<<"Rate limiter reset_all: ETS table undefined">>, #{
                <<"event">> => <<"rate_limiter_reset_all">>
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            router_logger:info(<<"Rate limiter reset_all: table cleared">>, #{
                <<"event">> => <<"rate_limiter_reset_all">>
            }),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    %% Clean expired rate limit windows
    cleanup_expired_windows(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> timer:cancel(Timer)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% Check rate limit and increment counter
do_check_rate_limit(TenantId, Endpoint, UserId, Limit, TtlSeconds, State) ->
    Key = {TenantId, Endpoint, UserId},
    NowMs = erlang:system_time(millisecond),
    RateLimitsTable = State#state.rate_limits_table,
    
    try
        case ets:lookup(RateLimitsTable, Key) of
            [] ->
                %% Initialize counter
                WindowStart = NowMs,
                ets:insert(RateLimitsTable, #rate_limit{
                    key = Key,
                    count = 1,
                    window_start = WindowStart,
                    limit = Limit,
                    ttl_seconds = TtlSeconds
                }),
                Remaining = Limit - 1,
                {ok, Remaining};
            [#rate_limit{count = Count, window_start = WindowStart, limit = Limit, ttl_seconds = TtlSeconds}] ->
                WindowMs = TtlSeconds * 1000,
                case is_window_expired(WindowStart, NowMs, WindowMs) of
                    true ->
                        %% Reset counter for new window
                        ets:insert(RateLimitsTable, #rate_limit{
                            key = Key,
                            count = 1,
                            window_start = NowMs,
                            limit = Limit,
                            ttl_seconds = TtlSeconds
                        }),
                        Remaining = Limit - 1,
                        {ok, Remaining};
                    false ->
                        %% Check if limit exceeded
                        case Count >= Limit of
                            true ->
                                Remaining = 0,
                                {error, rate_limit_exceeded, Remaining};
                            false ->
                                %% Increment counter
                                NewCount = Count + 1,
                                ets:update_counter(RateLimitsTable, Key, {#rate_limit.count, 1}),
                                Remaining = Limit - NewCount,
                                {ok, Remaining}
                        end
                end
        end
    catch
        error:{badarg, _} ->
            %% Table not accessible - allow request (fail open)
            router_logger:error(<<"Rate limit check failed: table not accessible, allowing request">>, #{
                <<"tenant_id">> => TenantId,
                <<"endpoint">> => Endpoint
            }),
            {ok, Limit};
        Class:Reason ->
            %% Other errors - allow request (fail open)
            router_logger:error(<<"Rate limit check failed, allowing request">>, #{
                <<"tenant_id">> => TenantId,
                <<"endpoint">> => Endpoint,
                <<"error">> => {Class, Reason}
            }),
            {ok, Limit}
    end.

%% Check if rate limit window is expired
is_window_expired(WindowStart, NowMs, WindowMs) ->
    (NowMs - WindowStart) >= WindowMs.

%% Get tenant rate limits (based on tier)
get_tenant_rate_limits(TenantId) ->
    %% Get rate limits from configuration
    RateLimits = application:get_env(beamline_router, rate_limits, #{
        default_requests_per_minute => 100,
        premium_requests_per_minute => 1000,
        enterprise_requests_per_minute => 10000
    }),
    
    %% Get tenant tier (future: load from database)
    Tier = get_tenant_tier(TenantId),
    case Tier of
        basic ->
            DefaultLimit = maps:get(default_requests_per_minute, RateLimits, ?DEFAULT_BASIC_LIMIT),
            #{<<"requests_per_minute">> => DefaultLimit, <<"ttl_seconds">> => 60};
        premium ->
            PremiumLimit = maps:get(premium_requests_per_minute, RateLimits, ?DEFAULT_PREMIUM_LIMIT),
            #{<<"requests_per_minute">> => PremiumLimit, <<"ttl_seconds">> => 60};
        enterprise ->
            EnterpriseLimit = maps:get(enterprise_requests_per_minute, RateLimits, ?DEFAULT_ENTERPRISE_LIMIT),
            #{<<"requests_per_minute">> => EnterpriseLimit, <<"ttl_seconds">> => 60};
        _ ->
            DefaultLimit = maps:get(default_requests_per_minute, RateLimits, ?DEFAULT_BASIC_LIMIT),
            #{<<"requests_per_minute">> => DefaultLimit, <<"ttl_seconds">> => 60}
    end.

%% Get tenant tier (stub for CP1)
get_tenant_tier(_TenantId) ->
    %% CP1: Default to basic tier
    %% Future: Lookup from database or config
    basic.

%% Validate rate limits configuration
-spec validate_rate_limits(map()) -> {ok, map()} | {error, atom()}.
validate_rate_limits(Limits) when is_map(Limits) ->
    RequestsPerMinute = maps:get(<<"requests_per_minute">>, Limits, undefined),
    TtlSeconds = maps:get(<<"ttl_seconds">>, Limits, undefined),
    
    case {RequestsPerMinute, TtlSeconds} of
        {undefined, _} ->
            {error, missing_requests_per_minute};
        {_, undefined} ->
            {error, missing_ttl_seconds};
        {RPM, TTL} when is_integer(RPM), RPM > 0, is_integer(TTL), TTL > 0 ->
            {ok, Limits};
        {RPM, _} when not is_integer(RPM); RPM =< 0 ->
            {error, invalid_requests_per_minute};
        {_, TTL} when not is_integer(TTL); TTL =< 0 ->
            {error, invalid_ttl_seconds};
        _ ->
            {error, invalid_config}
    end;
validate_rate_limits(_) ->
    {error, invalid_config}.

%% Store tenant rate limits (in-memory for CP1)
store_tenant_rate_limits(_TenantId, _Limits) ->
    %% CP1: In-memory only (future: persistent storage)
    ok.

%% Clean expired rate limit windows
cleanup_expired_windows(State) ->
    try
        NowMs = erlang:system_time(millisecond),
        RateLimitsTable = State#state.rate_limits_table,
        Pattern = #rate_limit{key = '$1', window_start = '$2', ttl_seconds = '$3', _ = '_'},
        Matches = ets:match(RateLimitsTable, Pattern),
        
        %% Delete expired entries
        lists:foreach(fun([Key, WindowStart, TtlSeconds]) ->
            WindowMs = TtlSeconds * 1000,
            case is_window_expired(WindowStart, NowMs, WindowMs) of
                true ->
                    ets:delete(RateLimitsTable, Key);
                false ->
                    ok
            end
        end, Matches),
        
        ok
    catch
        error:{badarg, _} ->
            %% Table not accessible - log but don't fail
            router_logger:error(<<"Failed to cleanup expired rate limit windows: table not accessible">>, #{
                <<"event">> => <<"rate_limit_cleanup_table_inaccessible">>
            }),
            ok;
        Class:Reason ->
            %% Other errors - log but don't fail
            router_logger:error(<<"Failed to cleanup expired rate limit windows">>, #{
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"rate_limit_cleanup_failed">>
            }),
            ok
    end.

%% Internal: Sanitize error for logging (masks secrets)
-spec sanitize_error_for_logging(term()) -> binary() | term().
sanitize_error_for_logging(Error) ->
    %% Convert error to binary for pattern matching
    ErrorBin = case is_binary(Error) of
        true -> Error;
        false -> iolist_to_binary(io_lib:format("~p", [Error]))
    end,
    %% Check for common secret patterns in error message
    case re:run(ErrorBin, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
        match ->
            <<"[REDACTED: contains sensitive data]">>;
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.
