%% @doc Secret Management Module
%%
%% Provides secure secret handling, rotation, and validation.
%% Implements secret rotation procedures and secure secret storage.
%%
%% Security:
%% - Never logs secrets
%% - Supports secret rotation
%% - Validates secret formats
%% - Provides secure secret retrieval
%%
%% @see SECURITY_GUIDE.md#secret-management For security best practices
-module(router_secret_manager).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_secret/1, rotate_secret/2, validate_secret/2, list_secret_names/0]).
-export([sanitize_secret_for_logging/1, check_secret_in_code/0]).

-ignore_xref([
    {router_secret_manager, start_link, 0},
    {router_secret_manager, get_secret, 1},
    {router_secret_manager, rotate_secret, 2}
]).

-include("beamline_router.hrl").

%% Secret storage (ETS table for in-memory secrets)
%% In production, this would integrate with a secure secret store (Vault, AWS Secrets Manager, etc.)
-define(SECRETS_TABLE, router_secrets).

-record(state, {
    secrets_table :: ets:tid() | atom(),
    rotation_enabled :: boolean()
}).

%% @doc Start secret manager server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize secret manager
init([]) ->
    %% Create ETS table for secret storage
    SecretsTable = ets:new(?SECRETS_TABLE, [
        set,
        named_table,
        {keypos, 1},
        protected,
        {read_concurrency, true}
    ]),
    
    RotationEnabled = application:get_env(beamline_router, secret_rotation_enabled, false),
    
    router_logger:info(<<"Secret manager started">>, #{
        <<"event">> => <<"secret_manager_started">>,
        <<"rotation_enabled">> => RotationEnabled
    }),
    
    {ok, #state{
        secrets_table = SecretsTable,
        rotation_enabled = RotationEnabled
    }}.

%% @doc Get secret by name
%% Returns: {ok, Secret} | {error, not_found} | {error, Reason}
-spec get_secret(binary() | atom()) -> {ok, binary()} | {error, term()}.
get_secret(Name) ->
    try
        gen_server:call(?MODULE, {get_secret, ensure_binary(Name)}, 5000)
    catch
        exit:{noproc, _} ->
            {error, secret_manager_not_running};
        exit:{timeout, _} ->
            {error, timeout};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc Rotate secret
%% Rotates a secret and updates all references
%% Returns: ok | {error, Reason}
-spec rotate_secret(binary() | atom(), binary()) -> ok | {error, term()}.
rotate_secret(Name, NewSecret) ->
    try
        gen_server:call(?MODULE, {rotate_secret, ensure_binary(Name), NewSecret}, 10000)
    catch
        exit:{noproc, _} ->
            {error, secret_manager_not_running};
        exit:{timeout, _} ->
            {error, timeout};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc Validate secret format
%% Validates that a secret meets security requirements
%% Returns: {ok, valid} | {error, Reason}
-spec validate_secret(binary() | atom(), binary()) -> {ok, valid} | {error, term()}.
validate_secret(Name, Secret) ->
    try
        gen_server:call(?MODULE, {validate_secret, ensure_binary(Name), Secret}, 5000)
    catch
        exit:{noproc, _} ->
            {error, secret_manager_not_running};
        exit:{timeout, _} ->
            {error, timeout};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @doc List all secret names (without values)
%% Returns: [binary()]
-spec list_secret_names() -> [binary()].
list_secret_names() ->
    try
        gen_server:call(?MODULE, list_secret_names, 5000)
    catch
        exit:{noproc, _} ->
            [];
        exit:{timeout, _} ->
            [];
        _:_ ->
            []
    end.

%% @doc Sanitize secret for logging
%% Never logs actual secret values
-spec sanitize_secret_for_logging(binary() | undefined) -> binary().
sanitize_secret_for_logging(undefined) ->
    <<"[REDACTED]">>;
sanitize_secret_for_logging(Secret) when is_binary(Secret) ->
    case byte_size(Secret) of
        0 -> <<"[REDACTED: empty]">>;
        Size when Size =< 8 -> <<"[REDACTED: ", (integer_to_binary(Size))/binary, " bytes]">>;
        _ -> <<"[REDACTED: ", (integer_to_binary(byte_size(Secret)))/binary, " bytes]">>
    end;
sanitize_secret_for_logging(_) ->
    <<"[REDACTED: invalid]">>.

%% @doc Check for hardcoded secrets in code
%% Scans application environment for unsafe default secrets
%% Returns: {ok, []} | {ok, [Warning]} | {error, Reason}
-spec check_secret_in_code() -> {ok, []} | {ok, [binary()]} | {error, term()}.
check_secret_in_code() ->
    Warnings = [],
    
    %% Check admin_api_key
    AdminKey = application:get_env(beamline_router, admin_api_key, undefined),
    Warning1 = case AdminKey of
        <<"dev-admin-key">> ->
            Env = application:get_env(beamline_router, env, dev),
            case Env of
                test -> undefined;  %% OK in test
                _ -> <<"admin_api_key uses unsafe default 'dev-admin-key'">>
            end;
        _ -> undefined
    end,
    
    Warnings1 = case Warning1 of
        undefined -> Warnings;
        _ -> [Warning1 | Warnings]
    end,
    
    %% Check NATS password
    NatsPassword = application:get_env(beamline_router, nats_password, undefined),
    Warning2 = case NatsPassword of
        Password when is_binary(Password), byte_size(Password) > 0 ->
            %% Check for common weak passwords
            case re:run(Password, "(?i)^(password|admin|secret|test|12345|qwerty)$", [{capture, none}]) of
                match -> <<"nats_password appears to be a weak password">>;
                nomatch -> undefined
            end;
        _ -> undefined
    end,
    
    Warnings2 = case Warning2 of
        undefined -> Warnings1;
        _ -> [Warning2 | Warnings1]
    end,
    
    {ok, Warnings2}.

%% ========================================================================
%% GEN_SERVER CALLBACKS
%% ========================================================================

handle_call({get_secret, Name}, _From, State) ->
    try
        case ets:lookup(State#state.secrets_table, Name) of
            [{Name, Secret, _Metadata}] ->
                {reply, {ok, Secret}, State};
            [] ->
                {reply, {error, not_found}, State}
        end
    catch
        _:_ ->
            {reply, {error, table_not_accessible}, State}
    end;

handle_call({rotate_secret, Name, NewSecret}, _From, State) ->
    try
        %% Validate new secret
        case validate_secret_internal(Name, NewSecret) of
            {ok, valid} ->
                %% Store new secret with rotation metadata
                Metadata = #{
                    rotated_at => erlang:system_time(millisecond),
                    rotation_enabled => State#state.rotation_enabled
                },
                ets:insert(State#state.secrets_table, {Name, NewSecret, Metadata}),
                
                router_logger:info(<<"Secret rotated">>, #{
                    <<"event">> => <<"secret_rotated">>,
                    <<"secret_name">> => sanitize_secret_for_logging(Name),
                    <<"secret_size">> => byte_size(NewSecret)
                }),
                
                {reply, ok, State};
            {error, Reason} ->
                {reply, {error, Reason}, State}
        end
    catch
        _:_ ->
            {reply, {error, rotation_failed}, State}
    end;

handle_call({validate_secret, Name, Secret}, _From, State) ->
    Result = validate_secret_internal(Name, Secret),
    {reply, Result, State};

handle_call(list_secret_names, _From, State) ->
    try
        Names = [Name || {Name, _, _} <- ets:tab2list(State#state.secrets_table)],
        {reply, Names, State}
    catch
        _:_ ->
            {reply, [], State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ========================================================================
%% INTERNAL FUNCTIONS
%% ========================================================================

%% @doc Validate secret format internally
-spec validate_secret_internal(binary(), binary()) -> {ok, valid} | {error, term()}.
validate_secret_internal(_Name, Secret) when is_binary(Secret) ->
    %% Check minimum length
    case byte_size(Secret) >= 8 of
        false ->
            {error, secret_too_short};
        true ->
            %% Check for common weak patterns
            case re:run(Secret, "(?i)^(password|admin|secret|test|12345|qwerty|dev-admin-key)$", [{capture, none}]) of
                match ->
                    {error, weak_secret_detected};
                nomatch ->
                    {ok, valid}
            end
    end;
validate_secret_internal(_Name, _Secret) ->
    {error, invalid_secret_type}.

%% @doc Ensure value is binary
-spec ensure_binary(atom() | binary() | list()) -> binary().
ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
ensure_binary(Binary) when is_binary(Binary) ->
    Binary;
ensure_binary(List) when is_list(List) ->
    list_to_binary(List);
ensure_binary(_) ->
    <<"invalid">>.

