%% @doc Extension Registry
%% ETS-based cache for extension metadata
%% CP1: Loads from fixtures
%% CP2-LC: Dual-mode support (database + fixtures)
%% CP2+: Database-only
-module(router_extension_registry).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([lookup/1, lookup_by_type/1, reload/0]).
-export([get_table_size/0, get_table_memory/0, check_size_limit/0]).

-include("beamline_router.hrl").

-define(TABLE, extension_registry).
-define(FIXTURES_DIR, "priv/fixtures/extensions").
-define(TELEMETRY_PREFIX, [router_extension_registry]).
-define(SYNC_INTERVAL_MS, 60000).  % 60 seconds default

%% Extension record is now defined in beamline_router.hrl
%% (moved from here to centralize common definitions)

-record(state, {
    table :: ets:tid(),
    source :: atom(),  % database | fixtures | auto
    db_enabled :: boolean(),
    sync_timer :: reference() | undefined
}).

%% @doc Start extension registry
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize extension registry
init([]) ->
    %% Create ETS table
    Table = ets:new(?TABLE, [
        set,
        named_table,
        public,
        {keypos, #extension.id},
        {read_concurrency, true},
        {write_concurrency, false}
    ]),
    
    %% Get configuration
    ExtensionRegistryConfig = application:get_env(beamline_router, extension_registry, []),
    Source = proplists:get_value(source, ExtensionRegistryConfig, auto),
    DbEnabled = proplists:get_value(db_enabled, ExtensionRegistryConfig, false),
    SyncIntervalSeconds = proplists:get_value(sync_interval_seconds, ExtensionRegistryConfig, 60),
    
    %% Load extensions based on source mode
    _LoadResult = case {Source, DbEnabled} of
        {database, true} ->
            load_from_database(Table);
        {fixtures, _} ->
            load_fixtures(Table);
        {auto, true} ->
            %% Try database first, fallback to fixtures
            case load_from_database(Table) of
                {ok, _} ->
                    ok;
                {error, _} ->
                    router_logger:warn(<<"Database unavailable, falling back to fixtures">>, #{}),
                    load_fixtures(Table)
                end;
        {auto, false} ->
            load_fixtures(Table);
        _ ->
            load_fixtures(Table)
    end,
    
    %% Start periodic sync if database enabled
    SyncTimer = case {Source, DbEnabled} of
        {database, true} ->
            timer:send_interval(SyncIntervalSeconds * 1000, sync);
        {auto, true} ->
            timer:send_interval(SyncIntervalSeconds * 1000, sync);
        _ ->
            undefined
    end,
    
    router_logger:info(<<"Extension Registry initialized">>, #{
        <<"source">> => atom_to_binary(Source, utf8),
        <<"db_enabled">> => DbEnabled,
        <<"extensions_loaded">> => ets:info(Table, size)
    }),
    
    {ok, #state{
        table = Table,
        source = Source,
        db_enabled = DbEnabled,
        sync_timer = SyncTimer
    }}.

%% @doc Lookup extension by ID
lookup(ExtensionId) when is_binary(ExtensionId) ->
    gen_server:call(?MODULE, {lookup, ExtensionId});
lookup(ExtensionId) when is_list(ExtensionId) ->
    lookup(list_to_binary(ExtensionId));
lookup(_ExtensionId) ->
    {error, invalid_id}.

%% @doc Lookup extensions by type
lookup_by_type(Type) when is_binary(Type) ->
    gen_server:call(?MODULE, {lookup_by_type, Type});
lookup_by_type(Type) when is_list(Type) ->
    lookup_by_type(list_to_binary(Type));
lookup_by_type(_) ->
    {error, invalid_type}.

%% @doc Reload extensions from fixtures
reload() ->
    gen_server:call(?MODULE, reload).

%% @doc Handle calls
handle_call({lookup, ExtensionId}, _From, State) ->
    Result = case ets:lookup(State#state.table, ExtensionId) of
        [#extension{} = Ext] ->
            {ok, Ext};
        [] ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call({lookup_by_type, Type}, _From, State) ->
    MatchSpec = [
        {
            #extension{type = Type, _ = '_'},
            [],
            ['$_']
        }
    ],
    Extensions = ets:select(State#state.table, MatchSpec),
    {reply, {ok, Extensions}, State};

handle_call(reload, _From, State) ->
    %% Reload based on source mode
    _LoadResult = case {State#state.source, State#state.db_enabled} of
        {database, true} ->
            load_from_database(State#state.table);
        {fixtures, _} ->
            load_fixtures(State#state.table);
        {auto, true} ->
            case load_from_database(State#state.table) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    router_logger:warn(<<"Database unavailable, falling back to fixtures">>, #{
                        <<"error">> => sanitize_error_for_logging(Reason),
                        <<"event">> => <<"database_fallback_to_fixtures">>
                    }),
                    load_fixtures(State#state.table)
            end;
        _ ->
            load_fixtures(State#state.table)
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(sync, State) ->
    %% Periodic sync from database
    try
        case {State#state.source, State#state.db_enabled} of
            {database, true} ->
                case load_from_database(State#state.table) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        router_logger:warn(<<"Periodic sync from database failed">>, #{
                            <<"error">> => sanitize_error_for_logging(Reason),
                            <<"event">> => <<"sync_failed">>
                        })
                end;
            {auto, true} ->
                case load_from_database(State#state.table) of
                    {ok, _} -> ok;
                    {error, Reason} ->
                        router_logger:warn(<<"Periodic sync from database failed">>, #{
                            <<"error">> => sanitize_error_for_logging(Reason),
                            <<"event">> => <<"sync_failed">>
                        })
                end;
            _ ->
                ok
        end
    catch
        Error:CatchReason ->
            router_logger:error(<<"Periodic sync exception">>, #{
                <<"error">> => Error,
                <<"reason">> => sanitize_error_for_logging(CatchReason),
                <<"event">> => <<"sync_exception">>
            })
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel sync timer if exists
    case State#state.sync_timer of
        undefined ->
            ok;
        Timer ->
            timer:cancel(Timer)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal: Load extensions from fixtures
load_fixtures(Table) ->
    %% Clear existing entries
    ets:delete_all_objects(Table),
    
    %% Load from fixtures directory
    FixturesPath = filename:join([code:priv_dir(beamline_router), "fixtures", "extensions"]),
    case filelib:is_dir(FixturesPath) of
        true ->
            load_fixtures_from_dir(Table, FixturesPath);
        false ->
            %% Create default fixtures if directory doesn't exist
            create_default_fixtures(Table)
    end.

load_fixtures_from_dir(Table, Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(fun(File) ->
                FilePath = filename:join([Dir, File]),
                case filename:extension(File) of
                    ".json" ->
                        load_fixture_file(Table, FilePath);
                    _ ->
                        ok
                end
            end, Files);
        {error, _Reason} ->
            create_default_fixtures(Table)
    end.

load_fixture_file(Table, FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            case jsx:decode(Content, [return_maps]) of
                Extensions when is_list(Extensions) ->
                    lists:foreach(fun(ExtMap) ->
                        case parse_extension(ExtMap) of
                            {ok, Extension} ->
                                ets:insert(Table, Extension);
                            {error, _Reason} ->
                                router_logger:warn(<<"Failed to parse extension">>, #{
                                    <<"file">> => FilePath
                                })
                        end
                    end, Extensions);
                ExtMap when is_map(ExtMap) ->
                    case parse_extension(ExtMap) of
                        {ok, Extension} ->
                            ets:insert(Table, Extension);
                        {error, _Reason} ->
                            router_logger:warn(<<"Failed to parse extension">>, #{
                                <<"file">> => FilePath
                            })
                    end;
                _ ->
                    router_logger:warn(<<"Invalid extension file format">>, #{
                        <<"file">> => FilePath
                    })
            end;
        {error, _Reason} ->
            router_logger:warn(<<"Failed to read extension file">>, #{
                <<"file">> => FilePath
            })
    end.

parse_extension(ExtMap) ->
    try
        Id = maps:get(<<"id">>, ExtMap),
        Type = maps:get(<<"type">>, ExtMap),
        Subject = maps:get(<<"subject">>, ExtMap),
        TimeoutMs = maps:get(<<"timeout_ms">>, ExtMap, 5000),
        Retry = maps:get(<<"retry">>, ExtMap, 0),
        Enabled = maps:get(<<"enabled">>, ExtMap, true),
        Config = maps:get(<<"config">>, ExtMap, #{}),
        Metadata = maps:get(<<"metadata">>, ExtMap, #{}),
        
        Extension = #extension{
            id = ensure_binary(Id),
            type = ensure_binary(Type),
            subject = ensure_binary(Subject),
            timeout_ms = TimeoutMs,
            retry = Retry,
            enabled = Enabled,
            config = Config,
            metadata = Metadata
        },
        {ok, Extension}
    catch
        _:Reason ->
            {error, {parse_error, Reason}}
    end.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(_) -> <<"">>.

%% Internal: Load extensions from database
load_from_database(Table) ->
    try
        case router_extension_registry_db:load_all_extensions() of
            {ok, Extensions} ->
                %% Clear existing entries
                ets:delete_all_objects(Table),
                %% Insert extensions from database
                lists:foreach(fun(Extension) ->
                    ets:insert(Table, Extension)
                end, Extensions),
                router_logger:info(<<"Extensions loaded from database">>, #{
                    <<"count">> => length(Extensions)
                }),
            {ok, Extensions};
            {error, database_not_available} ->
                router_logger:warn(<<"Database not available, skipping load">>, #{}),
                {error, database_not_available};
            {error, Reason} ->
                router_logger:error(<<"Failed to load extensions from database">>, #{
                    <<"error">> => Reason
                }),
                {error, Reason}
        end
    catch
        error:undef ->
            %% Database module not available
            router_logger:warn(<<"Database module not available, skipping load">>, #{}),
            {error, database_not_available};
        _:_ ->
            router_logger:error(<<"Unexpected error loading from database">>, #{}),
            {error, unexpected_error}
    end.

%% Create default fixtures (for testing)
create_default_fixtures(Table) ->
    DefaultExtensions = [
        #{
            <<"id">> => <<"normalize_text">>,
            <<"type">> => <<"pre">>,
            <<"subject">> => <<"beamline.ext.pre.normalize_text.v1">>,
            <<"timeout_ms">> => 100,
            <<"retry">> => 0,
            <<"enabled">> => true
        },
        #{
            <<"id">> => <<"pii_guard">>,
            <<"type">> => <<"validator">>,
            <<"subject">> => <<"beamline.ext.validate.pii_guard.v1">>,
            <<"timeout_ms">> => 200,
            <<"retry">> => 0,
            <<"enabled">> => true
        },
        #{
            <<"id">> => <<"mask_pii">>,
            <<"type">> => <<"post">>,
            <<"subject">> => <<"beamline.ext.post.mask_pii.v1">>,
            <<"timeout_ms">> => 150,
            <<"retry">> => 0,
            <<"enabled">> => true
        }
    ],
    lists:foreach(fun(ExtMap) ->
        case parse_extension(ExtMap) of
            {ok, Extension} ->
                ets:insert(Table, Extension);
            {error, _Reason} ->
                ok
        end
    end, DefaultExtensions).

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
    MaxSize = application:get_env(beamline_router, extension_registry_max_size, undefined),
    case MaxSize of
        undefined -> {error, no_limit_configured};
        Limit when is_integer(Limit), Limit > 0 ->
            router_resource_monitor:check_table_size_limit(?TABLE, Limit);
        _ -> {error, invalid_limit}
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
