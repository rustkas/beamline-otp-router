-module(router_idempotency).

-doc "Idempotency Layer".
%% Provides idempotency checks for processed messages using ETS with TTL
%% Prevents duplicate processing of results, ACKs, and usage events
-behaviour(gen_server).

-export([start_link/0]).
-export([check_and_mark/2, check_and_mark/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ignore_xref([
  {router_idempotency, start_link, 0},
  {router_idempotency, check_and_mark, 2}
]).

-include("beamline_router.hrl").

-define(TABLE_NAME, router_idempotency).
-define(DEFAULT_TTL_SECONDS, 3600).  %% 1 hour default TTL
-define(TELEMETRY_PREFIX, [router_idempotency]).

-record(state, {
    table :: ets:tid(),
    ttl_seconds :: integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Returns: {ok, not_seen} | {ok, seen} | {error, Reason}
-spec check_and_mark(binary(), binary()) -> {ok, not_seen} | {ok, seen} | {error, term()}.
check_and_mark(KeyType, MessageId) ->
    check_and_mark(KeyType, MessageId, undefined).

%% KeyType: assignment_id | request_id | ack_id | usage_id
%% MessageId: The message identifier
%% AdditionalData: Optional additional data to store (e.g., timestamp, status)
-spec check_and_mark(binary(), binary(), map() | undefined) -> {ok, not_seen} | {ok, seen} | {error, term()}.
check_and_mark(KeyType, MessageId, AdditionalData) ->
    gen_server:call(?MODULE, {check_and_mark, KeyType, MessageId, AdditionalData}).

init([]) ->
    %% Get TTL from configuration (3-arity returns value directly)
    TtlSeconds = application:get_env(beamline_router, idempotency_ttl_seconds, ?DEFAULT_TTL_SECONDS),
    
    %% Create ETS table with ordered_set for efficient lookups
    Table = ets:new(?TABLE_NAME, [
        ordered_set,
        named_table,
        public,  %% Allow other processes to read
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    
    State = #state{
        table = Table,
        ttl_seconds = TtlSeconds
    },
    
    %% Start cleanup timer
    CleanupInterval = min(TtlSeconds div 10, 60),  %% Cleanup every 10% of TTL or max 60 seconds
    {ok, _Timer} = timer:send_interval(CleanupInterval * 1000, cleanup),
    
    router_logger:info(~"Idempotency layer started", #{
        ~"table" => ?TABLE_NAME,
        ~"ttl_seconds" => TtlSeconds,
        ~"cleanup_interval_seconds" => CleanupInterval
    }),
    
    {ok, State}.

handle_call({check_and_mark, KeyType, MessageId, AdditionalData}, _From, #state{table = Table, ttl_seconds = TtlSeconds} = State) ->
    Key = {KeyType, MessageId},
    Now = erlang:system_time(second),
    ExpiresAt = Now + TtlSeconds,
    
    case ets:lookup(Table, Key) of
        [] ->
            %% Not seen before - mark as processed
            Value = #{
                message_id => MessageId,
                key_type => KeyType,
                processed_at => Now,
                expires_at => ExpiresAt,
                additional_data => AdditionalData
            },
            ets:insert(Table, {Key, Value}),
            %% Emit miss metric (not seen)
            emit_counter(router_idempotency_miss_total, #{
                key_type => KeyType,
                message_id => MessageId
            }),
            {reply, {ok, not_seen}, State};
        [{Key, ExistingValue}] ->
            %% Check if expired
            ExistingExpiresAt = maps:get(expires_at, ExistingValue, 0),
            case Now < ExistingExpiresAt of
                true ->
                    %% Still valid - already seen (hit)
                    emit_counter(router_idempotency_hit_total, #{
                        key_type => KeyType,
                        message_id => MessageId
                    }),
                    {reply, {ok, seen}, State};
                false ->
                    %% Expired - treat as not seen and update
                    NewValue = ExistingValue#{
                        processed_at => Now,
                        expires_at => ExpiresAt,
                        additional_data => AdditionalData
                    },
                    ets:insert(Table, {Key, NewValue}),
                    %% Emit miss metric (expired, treated as not seen)
                    emit_counter(router_idempotency_miss_total, #{
                        key_type => KeyType,
                        message_id => MessageId,
                        reason => ~"expired"
                    }),
                    {reply, {ok, not_seen}, State}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, #state{table = Table} = State) ->
    %% Cleanup expired entries
    Now = erlang:system_time(second),
    ExpiredKeys = ets:foldl(fun({Key, Value}, Acc) ->
        ExpiresAt = maps:get(expires_at, Value, 0),
        case Now >= ExpiresAt of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], Table),
    
    %% Delete expired entries
    lists:foreach(fun(Key) ->
        ets:delete(Table, Key)
    end, ExpiredKeys),
    
    case length(ExpiredKeys) > 0 of
        true ->
            router_logger:debug(~"Idempotency cleanup", #{
                ~"expired_count" => length(ExpiredKeys),
                ~"remaining_count" => ets:info(Table, size)
            });
        false ->
            ok
    end,
    
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{table = Table}) ->
    ets:delete(Table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal: Emit counter metric
-spec emit_counter(atom(), map()) -> ok.
emit_counter(MetricName, Metadata) ->
    telemetry:execute(?TELEMETRY_PREFIX ++ [MetricName], #{
        count => 1
    }, Metadata).
