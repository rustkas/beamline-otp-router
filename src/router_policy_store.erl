%% @doc Policy Store
%% In-memory ETS cache for routing policies, loads from fixtures (CP1: no PostgreSQL)
-module(router_policy_store).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load_policy/2, reload_fixtures/0, get_policy/2, get_policy/3]).
-export([upsert_policy/2, upsert_policy/3, delete_policy/2, delete_policy/3, list_policies/1, list_policies/2]).
-export([exec_with_telemetry/3, wait_for_table_owner/3]).  %% Internal helpers (used internally)
-export([parse_policy_map/3]).  %% Exported for use by router_policy.erl
-export([get_table_size/0, get_table_memory/0, get_index_table_size/0, check_size_limit/0, reset/0]).
%% Silence xref warnings for internal helpers kept for future use
-ignore_xref([
    {router_policy_store, upsert_policy, 2},
    {router_policy_store, upsert_policy, 3},
    {router_policy_store, wait_for_table_owner, 3},
    {router_policy_store, list_policies, 2},
    {router_policy_store, reload_fixtures, 0},
    {router_policy_store, start_link, 0}
]).

-include("beamline_router.hrl").

%% Use error sanitization from router_jetstream
-import(router_jetstream, [sanitize_error_for_logging/1]).

%% Use centralized ETS guard for invariant verification

%% Telemetry events
-define(TELEMETRY_PREFIX, [router_policy_store]).

-define(TABLE, policy_store).
-define(INDEX_TABLE, policy_store_index).
-define(FIXTURES_DIR, "priv/fixtures/policies").
-define(TRANSFER_TIMEOUT_MS, 1000).
-define(TRANSFER_BACKOFF_MS, 25).
-define(TRANSFER_RETRY_MS, 500).  %% Additional retry before fallback table creation

-record(state, {
    table :: ets:tid(),
    index_table :: ets:tid() | undefined,  %% Secondary index: tenant_id -> [policy_id]
    thresholds :: #{latency_warn_us => integer(),
                    latency_crit_us => integer(),
                    list_latency_warn_us => integer(),
                    queue_warn => integer(),
                    queue_crit => integer(),
                    policy_count_warn => integer()}
}).

%% @doc Start policy store
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize policy store
init([]) ->
    %% Check if heir/transfer logic should be disabled (for test mode)
    DisableHeir = case application:get_env(beamline_router, disable_heir, false) of
        true -> true;
        _ -> false
    end,
    
    %% Ensure heir process is running (unless disabled)
    HeirPid = if
        DisableHeir ->
            undefined;
        true ->
            case whereis(router_policy_store_heir) of
                undefined ->
                    %% Start heir process if not running
                    case router_policy_store_heir:start_link() of
                        {ok, Pid} -> Pid;
                        {error, {already_started, Pid}} -> Pid;
                        Error -> 
                            %% Log error but continue (heir is optional for fault tolerance)
                            case erlang:function_exported(router_logger, warn, 2) of
                                true ->
                                    router_logger:warn("Failed to start heir process", #{
                                        <<"error">> => erlang:term_to_binary(Error)
                                    });
                                false ->
                                    ok
                            end,
                            undefined
                    end;
                Pid ->
                    Pid
            end
    end,
    
    %% Check if table exists and who owns it
    SelfPid = self(),
    OwnerInfo = ets:info(?TABLE, owner),
    Table = if
        DisableHeir ->
            %% Test mode: create table without heir/transfer logic
            case OwnerInfo of
                undefined ->
                    ets:new(?TABLE, [
                        set,
                        named_table,
                        protected,
                        {read_concurrency, true}
                    ]);
                _ when is_pid(OwnerInfo) andalso OwnerInfo =:= SelfPid ->
                    ?TABLE;
                _ ->
                    %% Table exists but owned by another process, create new one
                    ets:new(?TABLE, [
                        set,
                        named_table,
                        {keypos, 1},
                        protected,
                        {read_concurrency, true}
                    ])
            end;
        OwnerInfo =:= undefined ->
            %% Table doesn't exist, create new one with heir
            case HeirPid of
                undefined ->
                    %% No heir available, create without heir
                    ets:new(?TABLE, [
                        set,
                        named_table,
                        protected,
                        {read_concurrency, true}
                    ]);
                _ ->
                    %% Create with heir for fault tolerance
                    ets:new(?TABLE, [
                        set,
                        named_table,
                        protected,
                        {heir, HeirPid, none},
                        {read_concurrency, true}
                    ])
            end;
        is_pid(OwnerInfo) andalso OwnerInfo =:= SelfPid ->
            %% We already own the table (shouldn't happen in normal startup)
            ?TABLE;
        is_pid(OwnerInfo) ->
            %% Table exists but owned by another process (likely heir after crash)
            %% Request transfer of ownership
            case HeirPid of
                undefined ->
                    %% No heir available, create new table
                    ets:new(?TABLE, [
                        set,
                        named_table,
                        protected,
                        {read_concurrency, true}
                    ]);
                _ ->
                    %% Emit telemetry for transfer attempt
                    case erlang:function_exported(telemetry, execute, 3) of
                        true ->
                            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [transfer_attempt], #{}, #{
                                table => ?TABLE
                            });
                        false ->
                            ok
                    end,
                    %% Request heir to transfer table to us
                    case router_policy_store_heir:claim(SelfPid) of
                        ok ->
                            %% Wait for ownership transfer with timeout, backoff, and retry
                            case wait_for_table_owner_with_retry(?TABLE, SelfPid, ?TRANSFER_TIMEOUT_MS) of
                                {ok, WaitDurationUs} ->
                                    %% Emit telemetry for successful transfer with wait duration
                                    case erlang:function_exported(telemetry, execute, 3) of
                                        true ->
                                            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [transfer_success], 
                                                #{wait_duration_us => WaitDurationUs}, 
                                                #{table => ?TABLE});
                                        false ->
                                            ok
                                    end,
                                    ?TABLE;
                                {error, timeout, WaitDurationUs} ->
                                    %% Emit telemetry for timeout with wait duration
                                    case erlang:function_exported(telemetry, execute, 3) of
                                        true ->
                                            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [transfer_timeout], 
                                                #{wait_duration_us => WaitDurationUs}, 
                                                #{table => ?TABLE});
                                        false ->
                                            ok
                                    end,
                                    %% Transfer failed or not completed, create new table
                                    ets:new(?TABLE, [
                                        set,
                                        named_table,
                                        {keypos, 1},
                                        protected,
                                        {heir, HeirPid, none},
                                        {read_concurrency, true}
                                    ])
                            end;
                        ClaimError ->
                            %% Claim failed, create new table
                            case erlang:function_exported(router_logger, warn, 2) of
                                true ->
                                    router_logger:warn("Failed to claim ETS table from heir", #{
                                        <<"error">> => erlang:term_to_binary(ClaimError)
                                    });
                                false ->
                                    ok
                            end,
                            ets:new(?TABLE, [
                                set,
                                named_table,
                                {keypos, 1},
                                protected,
                                {heir, HeirPid, none},
                                {read_concurrency, true}
                            ])
                    end
            end;
        true ->
            %% Unexpected owner type, create new table
            ets:new(?TABLE, [
                set,
                named_table,
                {keypos, 1},
                protected,
                {read_concurrency, true}
            ])
    end,
    
    %% Ensure heir is set (in case table was created without heir)
    %% Skip dynamic heir change to avoid badarg in restricted environments
    ok,
    
    %% Verify table invariants using centralized guard
    %% This catches configuration errors early in integration tests
    TableSpec = #{
        type => set,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    router_ets_guard:ensure_table(Table, TableSpec),
    
    %% Handle index table: check if it exists and claim if needed
    IndexTable = if
        DisableHeir ->
            %% Test mode: create index table without heir/transfer logic
            case ets:info(?INDEX_TABLE, owner) of
                undefined ->
                    ets:new(?INDEX_TABLE, [
                        bag,
                        named_table,
                        protected,
                        {read_concurrency, true}
                    ]);
                Owner when is_pid(Owner) andalso Owner =:= SelfPid ->
                    ?INDEX_TABLE;
                _ ->
                    %% Table exists but owned by another process, create new one
                    ets:new(?INDEX_TABLE, [
                        bag,
                        named_table,
                        {keypos, 1},
                        protected,
                        {read_concurrency, true}
                    ])
            end;
        true ->
            case ets:info(?INDEX_TABLE, owner) of
                undefined ->
                    %% Index table doesn't exist, create new one with heir
                    case HeirPid of
                        undefined ->
                            %% No heir available, create without heir
                            ets:new(?INDEX_TABLE, [
                                bag,
                                named_table,
                                {keypos, 1},
                                protected,
                                {read_concurrency, true}
                            ]);
                        _ ->
                            %% Create with heir for fault tolerance
                            ets:new(?INDEX_TABLE, [
                                bag,
                                named_table,
                                {keypos, 1},
                                protected,
                                {heir, HeirPid, none},
                                {read_concurrency, true}
                            ])
                    end;
                IndexOwnerInfo ->
                    if
                        IndexOwnerInfo =:= undefined ->
                            %% Index table doesn't exist, create new
                            case HeirPid of
                                undefined ->
                                    ets:new(?INDEX_TABLE, [
                                        bag,
                                        named_table,
                                        {keypos, 1},
                                        protected,
                                        {read_concurrency, true}
                                    ]);
                                _ ->
                                    ets:new(?INDEX_TABLE, [
                                        bag,
                                        named_table,
                                        {keypos, 1},
                                        protected,
                                        {heir, HeirPid, none},
                                        {read_concurrency, true}
                                    ])
                            end;
                        is_pid(IndexOwnerInfo) andalso IndexOwnerInfo =:= SelfPid ->
                            %% We already own the index table
                            ?INDEX_TABLE;
                        is_pid(IndexOwnerInfo) ->
                            %% Index table exists but owned by another process (likely heir after crash)
                            %% Request transfer of ownership
                            case HeirPid of
                                undefined ->
                                    %% No heir available, create new index table
                                    ets:new(?INDEX_TABLE, [
                                        bag,
                                        named_table,
                                        {keypos, 1},
                                        protected,
                                        {read_concurrency, true}
                                    ]);
                                _ ->
                                    %% Emit telemetry for transfer attempt
                                    case erlang:function_exported(telemetry, execute, 3) of
                                        true ->
                                            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [transfer_attempt], #{}, #{
                                                table => ?INDEX_TABLE
                                            });
                                        false ->
                                            ok
                                    end,
                                    %% Request heir to transfer index table to us
                                    case router_policy_store_heir:claim(SelfPid) of
                                        ok ->
                                            %% Wait for ownership transfer with timeout, backoff, and retry
                                            case wait_for_table_owner_with_retry(?INDEX_TABLE, SelfPid, ?TRANSFER_TIMEOUT_MS) of
                                                {ok, WaitDurationUsIdx} ->
                                                    %% Emit telemetry for successful transfer with wait duration
                                                    case erlang:function_exported(telemetry, execute, 3) of
                                                        true ->
                                                            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [transfer_success], 
                                                                #{wait_duration_us => WaitDurationUsIdx}, 
                                                                #{table => ?INDEX_TABLE});
                                                        false ->
                                                            ok
                                                    end,
                                                    ?INDEX_TABLE;
                                                {error, timeout, WaitDurationUsIdx} ->
                                                    %% Emit telemetry for timeout with wait duration
                                                    case erlang:function_exported(telemetry, execute, 3) of
                                                        true ->
                                                            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [transfer_timeout], 
                                                                #{wait_duration_us => WaitDurationUsIdx}, 
                                                                #{table => ?INDEX_TABLE});
                                                        false ->
                                                            ok
                                                    end,
                                                    %% Create new index table as fallback
                                                    ets:new(?INDEX_TABLE, [
                                                        bag,
                                                        named_table,
                                                        {keypos, 1},
                                                        protected,
                                                        {heir, HeirPid, none},
                                                        {read_concurrency, true}
                                                    ])
                                            end;
                                        ClaimErr ->
                                            %% Claim failed, create new index table
                                            case erlang:function_exported(router_logger, warn, 2) of
                                                true ->
                                                    router_logger:warn("Failed to claim index table from heir", #{
                                                        <<"error">> => erlang:term_to_binary(ClaimErr)
                                                    });
                                                false ->
                                                    ok
                                            end,
                                            ets:new(?INDEX_TABLE, [
                                                bag,
                                                named_table,
                                                {keypos, 1},
                                                protected,
                                                {heir, HeirPid, none},
                                                {read_concurrency, true}
                                            ])
                                    end
                            end;
                        true ->
                            %% Unexpected owner type, create new index table
                            ets:new(?INDEX_TABLE, [
                                bag,
                                named_table,
                                {keypos, 1},
                                protected,
                                {read_concurrency, true}
                            ])
                    end
            end
    end,
    
    %% Ensure heir is set for index table
    %% Skip dynamic heir change to avoid badarg in restricted environments
    ok,
    
    %% Verify index table invariants using centralized guard
    %% This catches configuration errors early in integration tests
    IndexTableSpec = #{
        type => bag,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    router_ets_guard:ensure_table(IndexTable, IndexTableSpec),
    
    %% Check if table is empty (fresh start or after crash)
    TableSize = case ets:info(Table, size) of
        undefined -> 0;
        Size -> Size
    end,
    
    case TableSize of
        0 ->
            %% Table is empty, load fixtures
            load_fixtures(Table);
        _ ->
            %% Table has data (from heir transfer or previous run)
            %% Reload fixtures to ensure consistency (fixtures may have changed)
            %% This ensures fixtures are always loaded, even after crash recovery
            load_fixtures(Table)
    end,
    
    %% Build initial index from existing policies
    rebuild_index(Table, IndexTable),
    
    %% read_concurrency already set at creation; skip dynamic change
    
    %% Load performance thresholds from application environment
    Thresholds = load_thresholds(),
    
    {ok, #state{table = Table, index_table = IndexTable, thresholds = Thresholds}}.

%% @doc Handle calls
handle_call({load_policy, TenantId, PolicyId}, _From, State) ->
    Result = do_load_policy(TenantId, PolicyId, State#state.table),
    {reply, Result, State};
handle_call({get_policy, TenantId, PolicyId, CorrelationId}, _From, State) ->
    Meta = build_meta(#{tenant_id => TenantId, table => ?TABLE, policy_id => PolicyId}, CorrelationId),
    Result = exec_with_telemetry(get_policy, Meta, fun() ->
        do_get_policy(TenantId, PolicyId, State#state.table)
    end),
    %% Log if thresholds exceeded
    case Result of
        {ok, _Policy} ->
            ok;  %% Telemetry already emitted
        {error, _Reason} ->
            ok   %% Telemetry already emitted
    end,
    {reply, Result, State};
handle_call({upsert_policy, TenantId, Policy, CorrelationId}, _From, State) ->
    %% Use element/2 to avoid badrecord issues with record access
    %% policy_id is at position 3 in the policy tuple
    PolicyId = element(3, Policy),
    Meta = build_meta(#{tenant_id => TenantId, table => ?TABLE, policy_id => PolicyId}, CorrelationId),
    Result = exec_with_telemetry(upsert, Meta, fun() ->
        do_upsert_policy(TenantId, Policy, State#state.table, State#state.index_table)
    end),
    {reply, Result, State};
handle_call({delete_policy, TenantId, PolicyId, CorrelationId}, _From, State) ->
    Meta = build_meta(#{tenant_id => TenantId, table => ?TABLE, policy_id => PolicyId}, CorrelationId),
    Result = exec_with_telemetry(delete, Meta, fun() ->
        do_delete_policy(TenantId, PolicyId, State#state.table, State#state.index_table)
    end),
    {reply, Result, State};
handle_call({list_policies, TenantId, CorrelationId}, _From, State) ->
    Meta = build_meta(#{tenant_id => TenantId, table => ?TABLE}, CorrelationId),
    Result = exec_with_telemetry(list, Meta, fun() ->
        do_list_policies(TenantId, State#state.table, State#state.index_table)
    end),
    %% Add policy count to metadata if successful
    case Result of
        {ok, Policies} ->
            PolicyCount = length(Policies),
            %% Emit additional telemetry with count
            %% count: number of entities in response (number of policies in response)
            case erlang:function_exported(telemetry, execute, 3) of
                true ->
                    router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [list], #{count => PolicyCount}, Meta);
                false ->
                    ok
            end;
        _ ->
            ok
    end,
    {reply, Result, State};
handle_call(reload_fixtures, _From, State) ->
    load_fixtures(State#state.table),
    {reply, ok, State};
handle_call(reset_all, _From, State = #state{table = Table, index_table = IndexTable}) ->
    %% Safe reset: clear all states but keep process and ETS tables alive
    %% This is called from test utilities, should not kill the process
    %% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
    case ets:info(Table) of
        undefined ->
            %% Table lost - log warning but continue
            router_logger:warn(<<"Policy store reset_all: ETS table undefined">>, #{
                <<"event">> => <<"policy_store_reset_all">>
            }),
            {reply, ok, State};
        _ ->
            ets:delete_all_objects(Table),
            case IndexTable of
                undefined -> ok;
                _ -> ets:delete_all_objects(IndexTable)
            end,
            %% Reload fixtures after reset
            load_fixtures(Table),
            %% Rebuild index after reset
            case IndexTable of
                undefined -> ok;
                _ -> rebuild_index(Table, IndexTable)
            end,
            router_logger:info(<<"Policy store reset_all: tables cleared and fixtures reloaded">>, #{
                <<"event">> => <<"policy_store_reset_all">>
            }),
            {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handle casts
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info({'ETS-TRANSFER', Table, FromPid, _HeirData}, State) ->
    %% Received ETS table from crashed process (heir mechanism)
    %% This should not happen in normal operation, but handle gracefully
    case Table =:= State#state.table of
        true ->
            %% Same table, no action needed
            {noreply, State};
        false ->
            %% Different table - this is unexpected, log and continue
            case erlang:function_exported(router_logger, warn, 2) of
                true ->
                    router_logger:warn("Received ETS table transfer", #{
                        <<"from_pid">> => erlang:pid_to_list(FromPid),
                        <<"table">> => erlang:ref_to_list(Table)
                    });
                false ->
                    ok
            end,
            {noreply, State#state{table = Table}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
terminate(_Reason, State) ->
    %% ETS tables will be automatically deleted when owner process dies
    %% If heir is set, tables will be transferred to heir before deletion
    %% Clean up index table explicitly (main table is named and will be auto-deleted)
    case State#state.index_table of
        undefined ->
            ok;
        IndexTable ->
            %% Index table is also named, but clean up explicitly for clarity
            ets:delete(IndexTable)
    end,
    ok.

%% @doc Code change callback
%% Rebuild index after code change to ensure consistency
code_change(_OldVsn, State, _Extra) ->
    case State#state.index_table of
        undefined ->
            {ok, State};
        IndexTable ->
            %% Rebuild index to ensure consistency after code change
            rebuild_index(State#state.table, IndexTable),
            {ok, State}
    end.

%% @doc Load performance thresholds from application environment
%% Returns a map with threshold values in microseconds (for latency) or counts
%% Converts ms → us once at load time
load_thresholds() ->
    App = beamline_router,
    LatencyWarnUs = application:get_env(App, latency_warn_ms, 10) * 1000,
    LatencyCritUs = application:get_env(App, latency_crit_ms, 50) * 1000,
    ListLatencyWarnUs = application:get_env(App, list_policies_latency_warn_ms, 5) * 1000,
    QueueWarn = application:get_env(App, queue_warn, 10),
    QueueCrit = application:get_env(App, queue_crit, 100),
    PolicyCountWarn = application:get_env(App, policy_count_warn, 100),
    #{
        latency_warn_us => LatencyWarnUs,
        latency_crit_us => LatencyCritUs,
        list_latency_warn_us => ListLatencyWarnUs,
        queue_warn => QueueWarn,
        queue_crit => QueueCrit,
        policy_count_warn => PolicyCountWarn
    }.

%% @doc Build metadata map with optional correlation_id
%% Adds correlation_id to metadata if provided
build_meta(Meta, undefined) ->
    Meta;
build_meta(Meta, CorrelationId) when is_binary(CorrelationId) ->
    Meta#{correlation_id => CorrelationId};
build_meta(Meta, _) ->
    Meta.

%% @doc Wait for table ownership transfer with timeout and backoff
%% Helper function to wait for ETS table ownership after claim request
wait_for_table_owner(Tab, SelfPid, TimeoutMs) ->
    BackoffMs = ?TRANSFER_BACKOFF_MS,
    wait_for_table_owner_loop(Tab, SelfPid, TimeoutMs, BackoffMs).

wait_for_table_owner_loop(Tab, SelfPid, Remain, BackoffMs) ->
    case ets:info(Tab, owner) of
        SelfPid ->
            ok;
        _ ->
            if
                Remain =< 0 ->
                    {error, timeout};
                true ->
                    timer:sleep(BackoffMs),
                    wait_for_table_owner_loop(Tab, SelfPid, Remain - BackoffMs, BackoffMs)
            end
    end.

%% @doc Wait for table ownership with additional retry before fallback
%% This reduces the probability of unnecessary table recreation due to ETS-TRANSFER delays
%% Returns: ok | {error, timeout} | {ok, WaitDurationUs} for telemetry
wait_for_table_owner_with_retry(Tab, SelfPid, TimeoutMs) ->
    StartWait = erlang:monotonic_time(),
    case wait_for_table_owner(Tab, SelfPid, TimeoutMs) of
        ok ->
            EndWait = erlang:monotonic_time(),
            WaitDurationUs = erlang:convert_time_unit(EndWait - StartWait, native, microsecond),
            {ok, WaitDurationUs};
        {error, timeout} ->
            %% Additional short retry before fallback
            case wait_for_table_owner(Tab, SelfPid, ?TRANSFER_RETRY_MS) of
                ok ->
                    EndWaitRetry = erlang:monotonic_time(),
                    WaitDurationUsRetry = erlang:convert_time_unit(EndWaitRetry - StartWait, native, microsecond),
                    {ok, WaitDurationUsRetry};
                {error, timeout} ->
                    EndWaitTimeout = erlang:monotonic_time(),
                    WaitDurationUsTimeout = erlang:convert_time_unit(EndWaitTimeout - StartWait, native, microsecond),
                    {error, timeout, WaitDurationUsTimeout}
            end
    end.

%% @doc Execute operation with telemetry
%% Helper function to wrap operations with telemetry events
%% Standardized events: [router_policy_store, upsert|delete|list|get_policy|rebuild_index|transfer_*]
%% Measurements: duration_us, queue_len
%% Metadata: tenant_id, table, policy_id (if applicable), result (ok|error), error (if applicable), correlation_id (if provided)
%% 
%% Correlation ID can be passed in Meta map to link operations end-to-end (from gRPC metadata)
%% Uses router_telemetry_helper for safe telemetry execution (respects telemetry_enabled setting)
exec_with_telemetry(Op, Meta, Fun) ->
    Start = erlang:monotonic_time(),
    try
        Res = Fun(),
        EndOk = erlang:monotonic_time(),
        DurationUsOk = erlang:convert_time_unit(EndOk - Start, native, microsecond),
        QueueLenOk = case process_info(self(), message_queue_len) of
            {message_queue_len, LenOk} -> LenOk;
            _ -> 0
        end,
        router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [Op],
                                        #{duration_us => DurationUsOk, queue_len => QueueLenOk},
                                        Meta#{result => ok}),
        Res
    catch
        Class:Reason:Stack ->
            %% Log error with router_logger before raising
            SanitizedReason = sanitize_error_for_logging(Reason),
            router_logger:error(<<"Policy store operation failed">>, #{
                <<"operation">> => Op,
                <<"error">> => Class,
                <<"reason">> => SanitizedReason,
                <<"event">> => <<"policy_store_operation_failed">>
            }),
            EndErr = erlang:monotonic_time(),
            DurationUsErr = erlang:convert_time_unit(EndErr - Start, native, microsecond),
            QueueLenErr = case process_info(self(), message_queue_len) of
                {message_queue_len, LenErr} -> LenErr;
                _ -> 0
            end,
            router_telemetry_helper:execute(?TELEMETRY_PREFIX ++ [Op],
                                            #{duration_us => DurationUsErr, queue_len => QueueLenErr},
                                            Meta#{result => error, error => Reason}),
            erlang:raise(Class, Reason, Stack)
    end.

%% @doc Load policy (public API)
load_policy(TenantId, PolicyId) ->
    gen_server:call(?MODULE, {load_policy, TenantId, PolicyId}).

%% @doc Get policy from cache (public API)
%% CorrelationId is optional and can be passed for end-to-end tracing
get_policy(TenantId, PolicyId) ->
    get_policy(TenantId, PolicyId, undefined).
get_policy(TenantId, PolicyId, CorrelationId) ->
    gen_server:call(?MODULE, {get_policy, TenantId, PolicyId, CorrelationId}).

%% @doc Reload fixtures (public API)
reload_fixtures() ->
    gen_server:call(?MODULE, reload_fixtures).

%% @doc Reset all policy store state (for testing)
%% Safe reset via handle_call(reset_all, ...) - clears ETS tables but keeps process alive
%% Pattern: Reuse reset/lifecycle pattern from router_circuit_breaker
-spec reset() -> ok | {error, term()}.
reset() ->
    try
        gen_server:call(?MODULE, reset_all, 5000)
    catch
        exit:{noproc, _} ->
            router_logger:error(<<"Policy store server not running for reset">>, #{
                <<"event">> => <<"policy_store_reset">>
            }),
            {error, service_unavailable};
        exit:{timeout, _} ->
            router_logger:error(<<"Policy store reset timeout">>, #{
                <<"event">> => <<"policy_store_reset">>
            }),
            {error, timeout};
        Class:Reason ->
            router_logger:error(<<"Policy store reset error">>, #{
                <<"event">> => <<"policy_store_reset">>,
                <<"error">> => {Class, Reason}
            }),
            {error, {Class, Reason}}
    end.

%% @doc Upsert policy (public API)
%% CorrelationId is optional and can be passed for end-to-end tracing
upsert_policy(TenantId, Policy) ->
    upsert_policy(TenantId, Policy, undefined).
upsert_policy(TenantId, Policy, CorrelationId) ->
    gen_server:call(?MODULE, {upsert_policy, TenantId, Policy, CorrelationId}).

%% @doc Delete policy (public API)
%% CorrelationId is optional and can be passed for end-to-end tracing
delete_policy(TenantId, PolicyId) ->
    delete_policy(TenantId, PolicyId, undefined).
delete_policy(TenantId, PolicyId, CorrelationId) ->
    gen_server:call(?MODULE, {delete_policy, TenantId, PolicyId, CorrelationId}).

%% @doc List policies for tenant (public API)
%% CorrelationId is optional and can be passed for end-to-end tracing
list_policies(TenantId) ->
    list_policies(TenantId, undefined).
list_policies(TenantId, CorrelationId) ->
    gen_server:call(?MODULE, {list_policies, TenantId, CorrelationId}).

%% Internal: Load policy with fallback to fixtures
do_load_policy(TenantId, PolicyId, Table) ->
    Key = {TenantId, PolicyId},
    
    StartTime = erlang:monotonic_time(microsecond),
    LookupResult = ets:lookup(Table, Key),
    LookupLatency = erlang:monotonic_time(microsecond) - StartTime,
    
    %% Emit ETS metrics
    router_ets_metrics:emit_ets_op(lookup, Table, LookupLatency, #{
        tenant_id => TenantId,
        policy_id => PolicyId
    }),
    
    case LookupResult of
        [{Key, Policy}] ->
            %% Cache hit: policy found in ETS table
            router_metrics:emit_metric(router_policy_store_cache_hits_total, #{count => 1}, #{
                tenant_id => TenantId
            }),
            {ok, Policy};
        [] ->
            %% Cache miss: policy not found in ETS table, try to load from fixtures
            router_metrics:emit_metric(router_policy_store_cache_misses_total, #{count => 1}, #{
                tenant_id => TenantId
            }),
            case load_from_fixture(TenantId, PolicyId) of
                {ok, Policy} ->
                    InsertStart = erlang:monotonic_time(microsecond),
                    ets:insert(Table, {Key, Policy}),
                    InsertLatency = erlang:monotonic_time(microsecond) - InsertStart,
                    router_ets_metrics:emit_ets_op(insert, Table, InsertLatency, #{
                        tenant_id => TenantId,
                        policy_id => PolicyId
                    }),
                    {ok, Policy};
                Error ->
                    Error
            end
    end.

%% Internal: Get policy from cache only
do_get_policy(TenantId, PolicyId, Table) ->
    Key = {TenantId, PolicyId},
    
    StartTime = erlang:monotonic_time(microsecond),
    LookupResult = ets:lookup(Table, Key),
    LookupLatency = erlang:monotonic_time(microsecond) - StartTime,
    LookupLatencyMs = LookupLatency div 1000,  %% Convert microseconds to milliseconds
    
    %% Emit ETS metrics
    router_ets_metrics:emit_ets_op(lookup, Table, LookupLatency, #{
        tenant_id => TenantId,
        policy_id => PolicyId
    }),
    
    %% Emit policy store lookup latency metric
    router_metrics:emit_metric(router_policy_store_lookup_latency_ms, #{value => LookupLatencyMs}, #{
        tenant_id => TenantId,
        policy_id => PolicyId
    }),
    
    case LookupResult of
        [{Key, Policy}] ->
            %% Cache hit: policy found in ETS table
            router_metrics:emit_metric(router_policy_store_cache_hits_total, #{count => 1}, #{
                tenant_id => TenantId
            }),
            {ok, Policy};
        [] ->
            %% Cache miss: policy not found in ETS table
            router_metrics:emit_metric(router_policy_store_cache_misses_total, #{count => 1}, #{
                tenant_id => TenantId
            }),
            {error, not_found}
    end.

%% Internal: Load all fixtures
load_fixtures(Table) ->
    %% Clear table
    ets:delete_all_objects(Table),
    
    %% Load default policy fixture
    case load_from_fixture(<<"default_tenant">>, <<"default">>) of
        {ok, Policy} ->
            Key = {<<"default_tenant">>, <<"default">>},
            ets:insert(Table, {Key, Policy});
        _ ->
            %% If fixture not found, create default policy
            DefaultPolicy = create_default_policy(),
            Key = {<<"default_tenant">>, <<"default">>},
            ets:insert(Table, {Key, DefaultPolicy})
    end,
    
    ok.

%% Internal: Load policy from fixture file
load_from_fixture(TenantId, PolicyId) ->
    %% Try to read fixture file
    PrivDir = case code:priv_dir(beamline_router) of
        {error, bad_name} ->
            %% Development mode - use relative path
            filename:join([code:lib_dir(beamline_router), "..", "priv"]);
        Dir ->
            Dir
    end,
    FixturePath = filename:join([PrivDir, "fixtures", "policies", 
                                  binary_to_list(TenantId), binary_to_list(PolicyId) ++ ".json"]),
    
    case file:read_file(FixturePath) of
        {ok, JsonData} ->
            case jsx:decode(JsonData, [return_maps]) of
                PolicyMap when is_map(PolicyMap) ->
                    Policy = parse_policy_map(TenantId, PolicyId, PolicyMap),
                    {ok, Policy};
                Error ->
                    {error, {invalid_json, Error}}
            end;
        {error, enoent} ->
            %% Fixture not found - return error (default policy only for default_tenant/default)
            {error, not_found};
        Error ->
            {error, Error}
    end.

%% @doc Parse policy map to record (exported for use by router_policy.erl)
%% Supports both legacy format (weights map, fallback object) and new JSON-DSL format (providers array, fallbacks array)
parse_policy_map(TenantId, PolicyId, PolicyMap) ->
    %% Parse weights: support both legacy "weights" map and new "providers" array
    Weights = parse_weights(PolicyMap),
    
    %% Parse fallbacks: support both legacy "fallback" object and new "fallbacks" array
    Fallback = maps:get(<<"fallback">>, PolicyMap, undefined),  % Legacy format
    Fallbacks = parse_fallbacks(PolicyMap),  % New format
    
    %% Parse sticky: support both legacy format and new JSON-DSL format
    Sticky = parse_sticky(maps:get(<<"sticky">>, PolicyMap, undefined)),
    
    %% Parse rate_limit: CP2 rate limiting configuration
    RateLimit = parse_rate_limit(maps:get(<<"rate_limit">>, PolicyMap, undefined)),
    
    %% Parse circuit_breaker configuration (if present)
    CircuitBreaker = maps:get(<<"circuit_breaker">>, PolicyMap, undefined),
    
    %% Parse extensions: pre, validators, post
    Pre = parse_pre_extensions(maps:get(<<"pre">>, PolicyMap, [])),
    Validators = parse_validator_extensions(maps:get(<<"validators">>, PolicyMap, [])),
    Post = parse_post_extensions(maps:get(<<"post">>, PolicyMap, [])),
    
    #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = maps:get(<<"version">>, PolicyMap, <<"1.0">>),
        defaults = maps:get(<<"defaults">>, PolicyMap, #{}),
        escalate_on = maps:get(<<"escalate_on">>, PolicyMap, []),
        weights = Weights,
        fallback = Fallback,  % Legacy format (for backward compatibility)
        fallbacks = Fallbacks,  % New format
        sticky = Sticky,
        rate_limit = RateLimit,
        circuit_breaker = CircuitBreaker,
        pre = Pre,
        validators = Validators,
        post = Post,
        metadata = maps:get(<<"metadata">>, PolicyMap, #{})
    }.

%% Internal: Parse weights from policy map
%% Supports both legacy "weights" map (0.0-1.0) and new "providers" array (0-100 → 0.0-1.0)
parse_weights(PolicyMap) ->
    case maps:get(<<"providers">>, PolicyMap, undefined) of
        undefined ->
            %% Legacy format: use weights map directly (already 0.0-1.0)
            maps:get(<<"weights">>, PolicyMap, #{});
        Providers when is_list(Providers) ->
            %% New format: convert providers array to weights map
            convert_providers_to_weights(Providers);
        _ ->
            %% Invalid format, fallback to legacy
            maps:get(<<"weights">>, PolicyMap, #{})
    end.

%% Internal: Convert providers array to weights map
%% Input: [{"name": "provider_a", "weight": 70}, ...]
%% Output: #{<<"provider_a">> => 0.7, ...}
convert_providers_to_weights(Providers) ->
    lists:foldl(fun(Provider, Acc) ->
        case Provider of
            #{<<"name">> := Name, <<"weight">> := Weight} when is_integer(Weight) ->
                %% Convert 0-100 → 0.0-1.0
                NormalizedWeight = Weight / 100.0,
                maps:put(Name, NormalizedWeight, Acc);
            #{<<"name">> := Name, <<"weight">> := Weight} when is_float(Weight) ->
                %% Already normalized (0.0-1.0), use as-is
                maps:put(Name, Weight, Acc);
            _ ->
                %% Invalid provider entry, skip
                Acc
        end
    end, #{}, Providers).

%% Internal: Parse fallbacks from policy map
%% Supports both legacy "fallback" object and new "fallbacks" array
parse_fallbacks(PolicyMap) ->
    case maps:get(<<"fallbacks">>, PolicyMap, undefined) of
        undefined ->
            %% Legacy format: convert single fallback to fallbacks array (for compatibility)
            case maps:get(<<"fallback">>, PolicyMap, undefined) of
                undefined ->
                    [];
                Fallback when is_map(Fallback) ->
                    %% Convert legacy fallback to new format
                    [convert_legacy_fallback(Fallback)]
            end;
        Fallbacks when is_list(Fallbacks) ->
            %% New format: parse fallbacks array
            lists:map(fun parse_fallback_rule/1, Fallbacks);
        _ ->
            []
    end.

%% Internal: Convert legacy fallback object to new fallback rule format
convert_legacy_fallback(Fallback) ->
    Provider = maps:get(<<"provider">>, Fallback, undefined),
    Conditions = maps:get(<<"conditions">>, Fallback, []),
    
    %% Convert conditions array to when object
    When = case Conditions of
        [] ->
            #{<<"status">> => [<<"all_providers_failed">>]};
        [Condition | _] when is_binary(Condition) ->
            %% Simple condition → status array
            #{<<"status">> => Conditions};
        _ ->
            #{<<"status">> => Conditions}
    end,
    
    #{
        <<"when">> => When,
        <<"retry">> => 1,  % Default retry count for legacy fallback
        <<"to">> => Provider
    }.

%% Internal: Parse single fallback rule from fallbacks array
parse_fallback_rule(FallbackRule) when is_map(FallbackRule) ->
    When = maps:get(<<"when">>, FallbackRule, #{}),
    Retry = maps:get(<<"retry">>, FallbackRule, 1),
    To = maps:get(<<"to">>, FallbackRule, undefined),
    Backoff = maps:get(<<"backoff">>, FallbackRule, undefined),
    
    %% Convert when condition to match expression format
    MatchExpr = convert_when_to_match(When),
    
    FallbackMap = #{
        <<"when">> => When,  % Keep original for condition evaluation
        <<"match">> => MatchExpr,  % Match expression for proto conversion
        <<"retry">> => Retry,
        <<"to">> => To
    },
    
    %% Add backoff if present
    case Backoff of
        undefined -> FallbackMap;
        BackoffMap when is_map(BackoffMap) -> maps:put(<<"backoff">>, BackoffMap, FallbackMap);
        _ -> FallbackMap
    end;
parse_fallback_rule(_) ->
    #{}.

%% Internal: Convert when condition to match expression
%% Input: {"status": ["timeout", "5xx"]}
%% Output: "status:timeout|5xx"
convert_when_to_match(When) when is_map(When) ->
    %% Convert when object to match expression string
    lists:foldl(fun({Key, Values}, Acc) ->
        KeyStr = binary_to_list(Key),
        ValuesStr = case Values of
            ValuesList when is_list(ValuesList) ->
                %% Join values with |
                lists:join("|", [binary_to_list(V) || V <- ValuesList, is_binary(V)]);
            Value when is_binary(Value) ->
                binary_to_list(Value);
            _ ->
                ""
        end,
        MatchPart = KeyStr ++ ":" ++ ValuesStr,
        case Acc of
            "" -> MatchPart;
            _ -> Acc ++ "|" ++ MatchPart
        end
    end, "", maps:to_list(When));
convert_when_to_match(_) ->
    "".

%% Internal: Parse sticky configuration
%% Supports both legacy format (ttl_seconds) and new JSON-DSL format (ttl string)
parse_sticky(undefined) ->
    undefined;
parse_sticky(Sticky) when not(is_map(Sticky)) ->
    undefined;
parse_sticky(Sticky) ->
    %% Parse TTL: support both "ttl" string ("10m") and "ttl_seconds" integer
    TTLSeconds = case maps:get(<<"ttl">>, Sticky, undefined) of
        undefined ->
            %% Legacy format: use ttl_seconds directly
            maps:get(<<"ttl_seconds">>, Sticky, 3600);
        TTLString when is_binary(TTLString) ->
            %% New format: parse duration string ("10m", "5m", "1h")
            parse_ttl_duration(TTLString);
        TTLString when is_list(TTLString) ->
            %% String as list, convert to binary
            parse_ttl_duration(list_to_binary(TTLString));
        _ ->
            %% Invalid format, use default
            3600
    end,
    
    %% Ensure session_key is present (default to "session_id" if not specified)
    SessionKey = maps:get(<<"session_key">>, Sticky, <<"session_id">>),
    
    maps:merge(Sticky, #{
        <<"ttl_seconds">> => TTLSeconds,
        <<"session_key">> => SessionKey
    }).

%% @doc Parse circuit breaker configuration from policy JSON
%% NOTE: This function is not used in CP1 as circuit_breaker is not part of #policy{} record
%% It will be used in future CP versions when circuit_breaker field is added to #policy{} record
%% parse_circuit_breaker(undefined) ->
%%     undefined;
%% parse_circuit_breaker(CircuitBreaker) when not(is_map(CircuitBreaker)) ->
%%     undefined;
%% parse_circuit_breaker(CircuitBreaker) ->
%%     %% Validate and parse circuit breaker configuration
%%     Enabled = maps:get(<<"enabled">>, CircuitBreaker, false),
%%     if
%%         not Enabled ->
%%             undefined;
%%         true ->
%%             %% Parse configuration fields with defaults
%%             FailureThreshold = maps:get(<<"failure_threshold">>, CircuitBreaker, 5),
%%             SuccessThreshold = maps:get(<<"success_threshold">>, CircuitBreaker, 2),
%%             TimeoutMs = maps:get(<<"timeout_ms">>, CircuitBreaker, 60000),
%%             HalfOpenMaxCalls = maps:get(<<"half_open_max_calls">>, CircuitBreaker, 3),
%%             ErrorRateThreshold = maps:get(<<"error_rate_threshold">>, CircuitBreaker, 0.5),
%%             ErrorRateWindowSeconds = maps:get(<<"error_rate_window_seconds">>, CircuitBreaker, 60),
%%             
%%             %% Validate values
%%             ValidatedConfig = #{
%%                 <<"enabled">> => true,
%%                 <<"failure_threshold">> => max(1, min(100, FailureThreshold)),
%%                 <<"success_threshold">> => max(1, min(10, SuccessThreshold)),
%%                 <<"timeout_ms">> => max(1000, min(300000, TimeoutMs)),
%%                 <<"half_open_max_calls">> => max(1, min(100, HalfOpenMaxCalls)),
%%                 <<"error_rate_threshold">> => max(0.0, min(1.0, ErrorRateThreshold)),
%%                 <<"error_rate_window_seconds">> => max(1, min(3600, ErrorRateWindowSeconds))
%%             },
%%             ValidatedConfig
%%     end.

%% Internal: Parse TTL duration string to seconds
%% Supports: "10m", "5m", "1h", "30s", etc.
parse_ttl_duration(TTLString) when is_binary(TTLString) ->
    TTLStr = binary_to_list(TTLString),
    case parse_ttl_duration_string(TTLStr) of
        {ok, Seconds} ->
            Seconds;
        {error, _} ->
            %% Invalid format, use default
            3600
    end;
parse_ttl_duration(_) ->
    3600.

%% Internal: Parse TTL duration string (e.g., "10m", "5m", "1h")
parse_ttl_duration_string(TTLStr) ->
    %% Match pattern: number + unit (s, m, h)
    case re:run(TTLStr, "^([0-9]+)([smh])$", [{capture, all_but_first, list}]) of
        {match, [NumStr, Unit]} ->
            try
                Num = list_to_integer(NumStr),
                Seconds = case Unit of
                    "s" -> Num;
                    "m" -> Num * 60;
                    "h" -> Num * 3600;
                    _ -> Num
                end,
                {ok, Seconds}
            catch
                _:_ -> {error, invalid_number}
            end;
        _ ->
            {error, invalid_format}
    end.

%% Internal: Parse rate_limit configuration
parse_rate_limit(undefined) ->
    undefined;
parse_rate_limit(RateLimit) when not(is_map(RateLimit)) ->
    undefined;
parse_rate_limit(RateLimit) ->
    Enabled = maps:get(<<"enabled">>, RateLimit, false),
    case Enabled of
        false ->
            undefined;
        true ->
            RequestsPerSecond = maps:get(<<"requests_per_second">>, RateLimit, 100),
            Burst = maps:get(<<"burst">>, RateLimit, 50),
            
            %% Validate values
            ValidatedRPS = max(1, min(1000000, RequestsPerSecond)),
            ValidatedBurst = max(1, min(1000000, Burst)),
            
            #{
                <<"enabled">> => true,
                <<"requests_per_second">> => ValidatedRPS,
                <<"burst">> => ValidatedBurst
            }
    end.

%% Internal: Parse extensions from policy map
%% Supports both "extensions" map and "metadata.extensions" (backward compatibility)
%% Internal: Parse pre-processor extensions
parse_pre_extensions(PreList) when is_list(PreList) ->
    lists:map(fun(PreItem) ->
        case PreItem of
            #{<<"id">> := Id} ->
                Mode = maps:get(<<"mode">>, PreItem, <<"optional">>),
                Config = maps:get(<<"config">>, PreItem, #{}),
                #{id => ensure_binary(Id), mode => ensure_binary(Mode), config => Config};
            _ ->
                #{}
        end
    end, PreList);
parse_pre_extensions(_) ->
    [].

%% Internal: Parse validator extensions
parse_validator_extensions(ValidatorsList) when is_list(ValidatorsList) ->
    lists:map(fun(ValidatorItem) ->
        case ValidatorItem of
            #{<<"id">> := Id} ->
                OnFail = maps:get(<<"on_fail">>, ValidatorItem, <<"block">>),
                #{id => ensure_binary(Id), on_fail => ensure_binary(OnFail)};
            _ ->
                #{}
        end
    end, ValidatorsList);
parse_validator_extensions(_) ->
    [].

%% Internal: Parse post-processor extensions
parse_post_extensions(PostList) when is_list(PostList) ->
    lists:map(fun(PostItem) ->
        case PostItem of
            #{<<"id">> := Id} ->
                Mode = maps:get(<<"mode">>, PostItem, <<"optional">>),
                Config = maps:get(<<"config">>, PostItem, #{}),
                #{id => ensure_binary(Id), mode => ensure_binary(Mode), config => Config};
            _ ->
                #{}
        end
    end, PostList);
parse_post_extensions(_) ->
    [].

%% Internal: Ensure binary value
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(_) -> <<"">>.

%% Internal: Create default policy
create_default_policy() ->
    #policy{
        tenant_id = <<"default_tenant">>,
        policy_id = <<"default">>,
        version = <<"1.0">>,
        defaults = #{
            <<"provider">> => <<"openai">>
        },
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = #{
            <<"provider">> => <<"local_llm">>,
            <<"conditions">> => [<<"all_providers_failed">>, <<"timeout">>]
        },
        sticky = #{
            <<"enabled">> => true,
            <<"ttl_seconds">> => 3600
        },
        metadata = #{
            <<"strategy">> => <<"cp1_static">>,
            <<"description">> => <<"CP1 default policy">>
        }
    }.

%% Internal: Rebuild secondary index from main table
%% Used during initialization and recovery
rebuild_index(Table, IndexTable) ->
    Meta = #{table => ?TABLE, index_table => ?INDEX_TABLE},
    exec_with_telemetry(rebuild_index, Meta, fun() ->
        %% Clear existing index
        ets:delete_all_objects(IndexTable),
        
        %% Rebuild index: iterate through all policies and index by tenant_id
        %% Use ets:select/3 with continuation for large tables to avoid blocking
        %% Use safe_fixtable for long scans to avoid "moving target" effect under load
        ets:safe_fixtable(Table, true),
        try
            Pattern = [{{'$1', '$2'}, '$3'}],
            Guard = [],
            Body = [{{'$1', '$2'}}],  %% Return {tenant_id, policy_id}
            
            RebuildResult = rebuild_index_cont(Table, IndexTable, ets:select(Table, [{Pattern, Guard, Body}], 100)),
            
            %% Audit index consistency: check for duplicates in bag table
            audit_index_consistency(IndexTable),
            
            RebuildResult
        after
            ets:safe_fixtable(Table, false)
        end
    end).
    
rebuild_index_cont(_Table, _IndexTable, '$end_of_table') ->
    ok;
rebuild_index_cont(Table, IndexTable, {Results, Continuation}) ->
    %% Insert index entries with explicit deduplication check
    %% Bag table prevents duplicates, but we check explicitly for consistency
    [begin
        Entry = {TenantId, PolicyId},
        %% Check if entry already exists before inserting
        case ets:match_object(IndexTable, Entry) of
            [] ->
                %% No duplicate, safe to insert
                ets:insert(IndexTable, Entry);
            [_] ->
                %% Entry already exists (shouldn't happen after delete_all_objects, but handle gracefully)
                ok;
            [_ | _] ->
                %% Multiple entries found (inconsistency), clean up and insert one
                ets:match_delete(IndexTable, Entry),
                ets:insert(IndexTable, Entry)
        end
    end || {TenantId, PolicyId} <- Results],
    %% Continue with next batch
    rebuild_index_cont(Table, IndexTable, ets:select(Continuation)).

%% Internal: Audit index consistency
%% Checks for duplicate entries in bag table and logs inconsistencies
%% Called after rebuild_index to verify index integrity
audit_index_consistency(IndexTable) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Use safe_fixtable for long scans to avoid "moving target" effect under load
    ets:safe_fixtable(IndexTable, true),
    try
        %% Get all entries grouped by {TenantId, PolicyId}
        AllEntries = ets:match_object(IndexTable, {'_', '_'}),
        Grouped = lists:foldl(fun({TenantId, PolicyId}, Acc) ->
            Key = {TenantId, PolicyId},
            maps:update_with(Key, fun(V) -> V + 1 end, 1, Acc)
        end, #{}, AllEntries),
        
        %% Find duplicates (entries with count > 1)
        Duplicates = maps:fold(fun(Key, Count, Acc) ->
            case Count > 1 of
                true -> [Key | Acc];
                false -> Acc
            end
        end, [], Grouped),
        
        %% Calculate metrics
        DuplicateCount = length(Duplicates),
        
        %% Log inconsistencies if found (only in production/staging, not in tests)
        TelemetryEnabled = application:get_env(beamline_router, telemetry_enabled, true),
        case Duplicates of
            [] ->
                ok;  %% No duplicates, index is consistent
            _ ->
                %% Emit telemetry counter for monitoring
                case TelemetryEnabled of
                    true ->
                        %% Emit counter per tenant (if available from duplicates)
                        lists:foreach(fun({TenantId, _PolicyId}) ->
                            router_telemetry_helper:execute(
                                [?TELEMETRY_PREFIX, index_inconsistency],
                                #{count => 1},
                                #{
                                    tenant_id => TenantId,
                                    template => <<"policy_index">>
                                }
                            )
                        end, lists:sublist(Duplicates, 10)),  %% Limit to avoid spam
                        
                        %% Emit total counter
                        router_telemetry_helper:execute(
                            [?TELEMETRY_PREFIX, index_inconsistency_total],
                            #{count => DuplicateCount},
                            #{template => <<"policy_index">>}
                        );
                    false ->
                        ok
                end,
                
                %% Log warning (only in production/staging, not in tests)
                case TelemetryEnabled andalso erlang:function_exported(router_logger, warn, 2) of
                    true ->
                        %% Extract trace_id from process dictionary if available
                        TraceId = case erlang:get(trace_id) of
                            undefined -> <<"">>;
                            T when is_binary(T) -> T;
                            T -> iolist_to_binary(io_lib:format("~p", [T]))
                        end,
                        router_logger:warn("Index consistency check found duplicates", #{
                            <<"duplicates">> => DuplicateCount,
                            <<"sample_duplicates">> => lists:sublist(Duplicates, 5),
                            <<"trace_id">> => TraceId
                        });
                    false ->
                        ok
                end,
                
                %% Clean up duplicates: for each duplicate, keep only one entry
                [begin
                    {TenantId, PolicyId} = Key,
                    %% Delete all matching entries
                    ets:match_delete(IndexTable, {TenantId, PolicyId}),
                    %% Insert single entry
                    ets:insert(IndexTable, {TenantId, PolicyId})
                end || Key <- Duplicates]
        end,
        
        %% Emit latency metric (P95) for index recovery
        EndTime = erlang:monotonic_time(microsecond),
        LatencyUs = router_duration:duration_us(StartTime, EndTime),
        case TelemetryEnabled of
            true ->
                router_telemetry_helper:execute(
                    [?TELEMETRY_PREFIX, index_recovery_latency],
                    #{duration_us => LatencyUs},
                    #{template => <<"policy_index">>}
                );
            false ->
                ok
        end
    after
        ets:safe_fixtable(IndexTable, false)
    end.

%% Internal: Upsert policy with validation
%% Updates both main table and secondary index atomically
do_upsert_policy(TenantId, Policy, Table, IndexTable) ->
    %% Use element/2 to avoid badrecord issues with record access
    %% policy_id is at position 3 in the policy tuple
    PolicyId = element(3, Policy),
    
    %% Measure total load time (validation + insert)
    LoadStartTime = erlang:monotonic_time(millisecond),
    
    %% Validate policy using validator
    PolicyMap = policy_to_map(Policy),
    case router_policy_validator:validate(PolicyMap) of
        ok ->
            Key = {TenantId, PolicyId},
            
            %% Check if policy already exists
            Existed = case ets:lookup(Table, Key) of
                [] -> false;
                [_] -> true
            end,
            
            %% Atomic insert/update in main table
            InsertStart = erlang:monotonic_time(microsecond),
            ets:insert(Table, {Key, Policy}),
            InsertLatency = erlang:monotonic_time(microsecond) - InsertStart,
            router_ets_metrics:emit_ets_op(insert, Table, InsertLatency, #{
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            
            %% Update secondary index (only if policy is new)
            case IndexTable of
                undefined ->
                    ok;  %% Index not available (shouldn't happen in normal operation)
                _ ->
                    case Existed of
                        false ->
                            %% New policy: add to index
                            %% Check if index entry already exists (defensive: handle index inconsistencies)
                            %% Use match_object to check for existing entries (bag table may have duplicates)
                            ExistingEntries = ets:match_object(IndexTable, {TenantId, PolicyId}),
                            case ExistingEntries of
                                [] ->
                                    %% No index entry exists, safe to insert
                                    ets:insert(IndexTable, {TenantId, PolicyId}),
                                    %% Verify insertion succeeded (defensive check for race conditions)
                                    case ets:match_object(IndexTable, {TenantId, PolicyId}) of
                                        [] ->
                                            %% Insertion failed (shouldn't happen), retry once
                                            ets:insert(IndexTable, {TenantId, PolicyId});
                                        _ ->
                                            ok
                                    end;
                                [_] ->
                                    %% Index entry already exists (shouldn't happen for new policy, but handle gracefully)
                                    %% This can occur if index was not properly cleaned up after delete
                                    %% Don't insert duplicate - bag table prevents duplicates anyway, but be explicit
                                    ok;
                                [_ | _] ->
                                    %% Multiple entries found (race condition or inconsistency)
                                    %% Clean up duplicates: delete all and insert one
                                    ets:match_delete(IndexTable, {TenantId, PolicyId}),
                                    ets:insert(IndexTable, {TenantId, PolicyId})
                            end;
                        true ->
                            %% Existing policy: index entry should already exist
                            %% Verify index consistency: if index entry is missing, add it
                            %% Also check for duplicates (race conditions in bag table)
                            ExistingEntries = ets:match_object(IndexTable, {TenantId, PolicyId}),
                            case ExistingEntries of
                                [] ->
                                    %% Index entry missing (inconsistency), add it
                                    ets:insert(IndexTable, {TenantId, PolicyId});
                                [_] ->
                                    %% Index entry exists, no action needed
                                    ok;
                                [_ | _] ->
                                    %% Multiple entries found (race condition or inconsistency)
                                    %% Clean up duplicates: delete all and insert one
                                    ets:match_delete(IndexTable, {TenantId, PolicyId}),
                                    ets:insert(IndexTable, {TenantId, PolicyId})
                            end
                    end
            end,
            
            %% Measure total load time (validation + insert + index update)
            LoadEndTime = erlang:monotonic_time(millisecond),
            LoadLatency = LoadEndTime - LoadStartTime,
            
            %% Emit policy store load time metric
            router_metrics:emit_metric(router_policy_store_load_time_ms, #{value => LoadLatency}, #{
                tenant_id => TenantId
            }),
            
            %% Update policy store size and memory metrics
            update_store_metrics(Table, TenantId),
            
            {ok, Policy};
        {error, Details} ->
            LoadEndTime = erlang:monotonic_time(millisecond),
            LoadLatency = LoadEndTime - LoadStartTime,
            
            %% Emit load time metric even on error
            router_metrics:emit_metric(router_policy_store_load_time_ms, #{value => LoadLatency}, #{
                tenant_id => TenantId
            }),
            
            {error, invalid_policy, Details}
    end.

%% Internal: Update policy store size and memory metrics
update_store_metrics(Table, TenantId) ->
    try
        %% Get table size (number of policies)
        TableSize = case ets:info(Table, size) of
            undefined -> 0;
            Size when is_integer(Size) -> Size
        end,
        
        %% Get table memory (bytes)
        TableMemory = case ets:info(Table, memory) of
            undefined -> 0;
            Memory when is_integer(Memory) -> Memory * erlang:system_info(wordsize)  %% Convert words to bytes
        end,
        
        %% Emit size metric
        router_metrics:emit_metric(router_policy_store_size, #{value => TableSize}, #{
            tenant_id => TenantId
        }),
        
        %% Emit memory metric
        router_metrics:emit_metric(router_policy_store_memory_bytes, #{value => TableMemory}, #{
            tenant_id => TenantId
        })
    catch
        _:_ ->
            %% Ignore errors (metrics are optional)
            ok
    end.

%% Internal: Delete policy
%% Removes from both main table and secondary index atomically
do_delete_policy(TenantId, PolicyId, Table, IndexTable) ->
    Key = {TenantId, PolicyId},
    case ets:lookup(Table, Key) of
        [{Key, _Policy}] ->
            %% Delete from main table
            DeleteStart = erlang:monotonic_time(microsecond),
            ets:delete(Table, Key),
            DeleteLatency = erlang:monotonic_time(microsecond) - DeleteStart,
            router_ets_metrics:emit_ets_op(delete, Table, DeleteLatency, #{
                tenant_id => TenantId,
                policy_id => PolicyId
            }),
            
            %% Update policy store size and memory metrics after delete
            update_store_metrics(Table, TenantId),
            
            %% Delete from secondary index
            case IndexTable of
                undefined ->
                    ok;  %% Index not available (shouldn't happen in normal operation)
                _ ->
                    %% Remove index entry (bag table: delete all entries matching {TenantId, PolicyId})
                    %% Use match_delete to remove all entries with key TenantId and value PolicyId
                    ets:match_delete(IndexTable, {TenantId, PolicyId}),
                    ok
            end,
            
            ok;
        [] ->
            {error, not_found}
    end.

%% Internal: List policies for tenant
%% 
%% Performance optimizations:
%% - Uses secondary index table for O(1) tenant lookup instead of linear scan
%% - Uses ets:select/3 with continuation for large result sets to avoid blocking
%% - Results are sorted by policy_id for deterministic ordering
%%
%% Ordering note: ETS set tables do not guarantee order. Results are sorted by policy_id
%% to ensure deterministic ordering for clients.
do_list_policies(TenantId, Table, IndexTable) ->
    ListStart = erlang:monotonic_time(microsecond),
    
    %% Use secondary index to get policy IDs for tenant (O(1) lookup)
    PolicyIds = case IndexTable of
        undefined ->
            %% Fallback to linear scan if index not available
            Pattern = {{TenantId, '_'}, '_'},
            Matches = ets:match_object(Table, Pattern),
            [PolicyId || {{_T, PolicyId}, _Policy} <- Matches];
        _ ->
            %% Fast lookup via index
            [PolicyId || {_TenantId, PolicyId} <- ets:lookup(IndexTable, TenantId)]
    end,
    
    ListLatency = erlang:monotonic_time(microsecond) - ListStart,
    router_ets_metrics:emit_ets_op(match, Table, ListLatency, #{
        tenant_id => TenantId,
        result_count => length(PolicyIds)
    }),
    
    %% Fetch policies using policy IDs (O(n) where n = number of policies for tenant)
    %% Use safe_fixtable for multiple lookups to avoid "moving target" effect
    ets:safe_fixtable(Table, true),
    try
        Policies0 = [begin
            Key = {TenantId, PolicyId},
            case ets:lookup(Table, Key) of
                [{Key, Policy}] -> Policy;
                [] -> undefined  %% Policy was deleted but index not updated (shouldn't happen)
            end
        end || PolicyId <- PolicyIds],
        
        %% Filter out undefined (deleted policies) and sort by policy_id for deterministic ordering
        ValidPolicies = [P || P <- Policies0, P =/= undefined],
        
        %% Sort by policy_id for deterministic ordering (lexicographic binary order)
        %% Use Erlang term ordering (<) for binary comparison
        %% Use element/2 to avoid badrecord issues with record access
        %% policy_id is at position 3 in the policy tuple
        SortedResult = lists:sort(
            fun(P1, P2) -> element(3, P1) < element(3, P2) end,
            ValidPolicies
        ),
        
        %% Performance monitoring: log if result set is large
        case erlang:function_exported(router_logger, is_enabled, 0) of
            true ->
                case router_logger:is_enabled() of
                    true when length(SortedResult) > 100 ->
                        %% Log warning if returning > 100 policies (potential performance issue)
                        router_logger:warn("Large list_policies result set", #{
                            <<"tenant_id">> => TenantId,
                            <<"policy_count">> => length(SortedResult),
                            <<"warning">> => <<"Consider pagination or limiting results">>
                        });
                    _ ->
                        ok
                end;
            false ->
                ok
        end,
        
        {ok, SortedResult}
    after
        ets:safe_fixtable(Table, false)
    end.

%% Internal: Validate policy
%% Internal: Convert policy record to map for validation
%% Use element/2 to avoid badrecord issues with record access
policy_to_map(Policy) ->
    %% Record structure: {policy, tenant_id, policy_id, version, defaults, escalate_on, weights, fallback, fallbacks, sticky, pre, validators, post, metadata}
    %% Elements: 1=policy, 2=tenant_id, 3=policy_id, 4=version, 5=defaults, 6=escalate_on, 7=weights, 8=fallback, 9=fallbacks, 10=sticky, 11=pre, 12=validators, 13=post, 14=metadata
    TenantId = element(2, Policy),
    PolicyId = element(3, Policy),
    Weights = element(7, Policy),
    Sticky = element(10, Policy),
    Fallback = element(8, Policy),
    Metadata = element(14, Policy),
    Map = #{
        <<"tenant_id">> => TenantId,
        <<"policy_id">> => PolicyId,
        <<"weights">> => Weights
    },
    
    %% Add optional fields
    Map1 = case Sticky of
        undefined -> Map;
        _ -> maps:put(<<"sticky">>, sticky_to_map(Sticky), Map)
    end,
    
    Map2 = case Fallback of
        undefined -> Map1;
        _ -> maps:put(<<"fallback">>, fallback_to_map(Fallback), Map1)
    end,
    
    Map3 = case Metadata of
        #{} when map_size(Metadata) =:= 0 -> Map2;
        _ -> maps:put(<<"metadata">>, Metadata, Map2)
    end,
    
    Map3.

%% Internal: Convert sticky to map
sticky_to_map(Sticky) when is_map(Sticky) ->
    Sticky;
sticky_to_map(_) ->
    #{}.

%% Internal: Convert fallback to map
fallback_to_map(Fallback) when is_map(Fallback) ->
    Fallback;
fallback_to_map(_) ->
    #{}.

%% Legacy validation functions removed - validation now handled by router_policy_validator

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

%% @doc Get current index table size (number of entries)
-spec get_index_table_size() -> integer() | undefined.
get_index_table_size() ->
    case ets:info(?INDEX_TABLE, size) of
        undefined -> undefined;
        Size -> Size
    end.

%% @doc Check if table size exceeds configured limit
-spec check_size_limit() -> {ok, integer()} | {error, exceeded, integer(), integer()} | {error, no_limit_configured}.
check_size_limit() ->
    MaxSize = application:get_env(beamline_router, policy_store_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(?TABLE, Limit);
        _ -> {error, invalid_limit}
    end.
