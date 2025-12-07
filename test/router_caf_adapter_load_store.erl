%% @doc Test-only ETS store for router_caf_adapter load threshold tests.
%% Encapsulates all ETS access for metrics tracking (retry, published, exhausted, failure).
-module(router_caf_adapter_load_store).

-export([
    ensure/0,
    reset/0,
    record_retry/1,
    get_retry/0,
    inc_retry_count/0,
    get_retry_count/0,
    record_published/1,
    get_published/0,
    inc_publish_call_count/0,
    get_publish_call_count/0,
    record_exhausted/1,
    get_exhausted/0,
    record_failure/1,
    get_failure/0
]).

-define(RETRY_METRICS_TABLE, router_caf_retry_metrics).
-define(RETRY_COUNT_TABLE, router_caf_retry_count).
-define(PUBLISHED_METRICS_TABLE, router_caf_published_metrics).
-define(PUBLISH_CALL_COUNT_TABLE, router_caf_publish_call_count).
-define(EXHAUSTED_METRICS_TABLE, router_caf_exhausted_metrics).
-define(FAILURE_METRICS_TABLE, router_caf_failure_metrics).

%% @doc Ensure all ETS tables exist.
%% Returns:
%%   ok              - all tables exist or were created
%%   {error, Reason} - creation failed
ensure() ->
    Tables = [
        ?RETRY_METRICS_TABLE,
        ?RETRY_COUNT_TABLE,
        ?PUBLISHED_METRICS_TABLE,
        ?PUBLISH_CALL_COUNT_TABLE,
        ?EXHAUSTED_METRICS_TABLE,
        ?FAILURE_METRICS_TABLE
    ],
    ensure_tables(Tables).

ensure_tables([]) ->
    ok;
ensure_tables([Table | Rest]) ->
    case ensure_table(Table) of
        ok ->
            ensure_tables(Rest);
        Error ->
            Error
    end.

ensure_table(Table) ->
    case catch ets:whereis(Table) of
        undefined ->
            case catch ets:new(Table, [named_table, set, public]) of
                {'EXIT', {badarg, _}} ->
                    %% Table already created in parallel.
                    ok;
                {'EXIT', Reason} ->
                    {error, Reason};
                _Tid ->
                    ok
            end;
        {'EXIT', Reason} ->
            {error, Reason};
        _Tid ->
            ok
    end.

%% @doc Clear all data but keep tables.
reset() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?RETRY_METRICS_TABLE),
            ets:delete_all_objects(?RETRY_COUNT_TABLE),
            ets:delete_all_objects(?PUBLISHED_METRICS_TABLE),
            ets:delete_all_objects(?PUBLISH_CALL_COUNT_TABLE),
            ets:delete_all_objects(?EXHAUSTED_METRICS_TABLE),
            ets:delete_all_objects(?FAILURE_METRICS_TABLE),
            ok;
        Error ->
            Error
    end.

%% @doc Record retry metric.
record_retry(Metadata) ->
    case ensure() of
        ok ->
            true = ets:insert(?RETRY_METRICS_TABLE, {retry, Metadata}),
            ok;
        Error ->
            Error
    end.

%% @doc Get retry metric.
%% Returns:
%%   {ok, Metadata} | not_found | {error, Reason}
get_retry() ->
    case ensure() of
        ok ->
            case ets:lookup(?RETRY_METRICS_TABLE, retry) of
                [{retry, Metadata}] ->
                    {ok, Metadata};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc Increment retry count.
inc_retry_count() ->
    case ensure() of
        ok ->
            Count = case ets:lookup(?RETRY_COUNT_TABLE, retries) of
                [] -> 1;
                [{retries, C}] -> C + 1
            end,
            true = ets:insert(?RETRY_COUNT_TABLE, {retries, Count}),
            {ok, Count};
        Error ->
            Error
    end.

%% @doc Get retry count.
%% Returns:
%%   {ok, Count} | not_found | {error, Reason}
get_retry_count() ->
    case ensure() of
        ok ->
            case ets:lookup(?RETRY_COUNT_TABLE, retries) of
                [{retries, Count}] ->
                    {ok, Count};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc Record published metric.
record_published(Metadata) ->
    case ensure() of
        ok ->
            true = ets:insert(?PUBLISHED_METRICS_TABLE, {published, Metadata}),
            ok;
        Error ->
            Error
    end.

%% @doc Get published metric.
%% Returns:
%%   {ok, Metadata} | not_found | {error, Reason}
get_published() ->
    case ensure() of
        ok ->
            case ets:lookup(?PUBLISHED_METRICS_TABLE, published) of
                [{published, Metadata}] ->
                    {ok, Metadata};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc Increment publish call count.
inc_publish_call_count() ->
    case ensure() of
        ok ->
            Count = case ets:lookup(?PUBLISH_CALL_COUNT_TABLE, calls) of
                [] -> 1;
                [{calls, C}] -> C + 1
            end,
            true = ets:insert(?PUBLISH_CALL_COUNT_TABLE, {calls, Count}),
            {ok, Count};
        Error ->
            Error
    end.

%% @doc Get publish call count.
%% Returns:
%%   {ok, Count} | not_found | {error, Reason}
get_publish_call_count() ->
    case ensure() of
        ok ->
            case ets:lookup(?PUBLISH_CALL_COUNT_TABLE, calls) of
                [{calls, Count}] ->
                    {ok, Count};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc Record exhausted metric.
record_exhausted(Metadata) ->
    case ensure() of
        ok ->
            true = ets:insert(?EXHAUSTED_METRICS_TABLE, {exhausted, Metadata}),
            ok;
        Error ->
            Error
    end.

%% @doc Get exhausted metric.
%% Returns:
%%   {ok, Metadata} | not_found | {error, Reason}
get_exhausted() ->
    case ensure() of
        ok ->
            case ets:lookup(?EXHAUSTED_METRICS_TABLE, exhausted) of
                [{exhausted, Metadata}] ->
                    {ok, Metadata};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc Record failure metric.
record_failure(Metadata) ->
    case ensure() of
        ok ->
            true = ets:insert(?FAILURE_METRICS_TABLE, {failure, Metadata}),
            ok;
        Error ->
            Error
    end.

%% @doc Get failure metric.
%% Returns:
%%   {ok, Metadata} | not_found | {error, Reason}
get_failure() ->
    case ensure() of
        ok ->
            case ets:lookup(?FAILURE_METRICS_TABLE, failure) of
                [{failure, Metadata}] ->
                    {ok, Metadata};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

