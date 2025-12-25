-module(router_security_validator).

-doc "Security Validation Module".
%%
%% Provides comprehensive security validation functions for Router.
%% Implements input validation, format checking, and security pattern detection.
%%
%% Security Features:
%% - Input format validation (tenant_id, policy_id, user_id, etc.)
%% - Length and character set validation
%% - Security pattern detection (SQL injection, XSS, path traversal)
%% - Rate limiting support for validation failures
%%
%% @see SECURITY_GUIDE.md#input-validation For security best practices

-export([validate_tenant_id/1, validate_policy_id/1, validate_user_id/1, validate_role_id/1]).
-export([validate_id_format/2, validate_length/3, detect_security_patterns/1]).
-export([sanitize_input/1, validate_binary_input/2]).

-include("beamline_router.hrl").

%% Returns: {ok, TenantId} | {error, Reason}
-spec validate_tenant_id(binary() | undefined) -> {ok, binary()} | {error, atom()}.
validate_tenant_id(undefined) ->
    {error, tenant_id_required};
validate_tenant_id(<<>>) ->
    {error, tenant_id_empty};
validate_tenant_id(TenantId) when is_binary(TenantId) ->
    %% Check length
    case validate_length(TenantId, 1, 255) of
        {error, _} = Err -> Err;
        ok ->
            %% Check format (alphanumeric, underscore, hyphen, dot)
            case validate_id_format(TenantId, tenant_id) of
                {error, _} = Err -> Err;
                ok ->
                    %% Check for security patterns
                    case detect_security_patterns(TenantId) of
                        {error, _} = Err -> Err;
                        ok -> {ok, TenantId}
                    end
            end
    end;
validate_tenant_id(_) ->
    {error, tenant_id_invalid_type}.

%% Returns: {ok, PolicyId} | {error, Reason}
-spec validate_policy_id(binary() | undefined) -> {ok, binary()} | {error, atom()}.
validate_policy_id(undefined) ->
    {error, policy_id_required};
validate_policy_id(<<>>) ->
    {error, policy_id_empty};
validate_policy_id(PolicyId) when is_binary(PolicyId) ->
    %% Check length
    case validate_length(PolicyId, 1, 255) of
        {error, _} = Err -> Err;
        ok ->
            %% Check format (alphanumeric, underscore, hyphen, dot)
            case validate_id_format(PolicyId, policy_id) of
                {error, _} = Err -> Err;
                ok ->
                    %% Check for security patterns
                    case detect_security_patterns(PolicyId) of
                        {error, _} = Err -> Err;
                        ok -> {ok, PolicyId}
                    end
            end
    end;
validate_policy_id(_) ->
    {error, policy_id_invalid_type}.

%% Returns: {ok, UserId} | {error, Reason}
-spec validate_user_id(binary() | undefined) -> {ok, binary()} | {error, atom()}.
validate_user_id(undefined) ->
    {error, user_id_required};
validate_user_id(<<>>) ->
    {error, user_id_empty};
validate_user_id(UserId) when is_binary(UserId) ->
    %% Check length
    case validate_length(UserId, 1, 64) of
        {error, _} = Err -> Err;
        ok ->
            %% Check format (alphanumeric, underscore, hyphen)
            case validate_id_format(UserId, user_id) of
                {error, _} = Err -> Err;
                ok ->
                    %% Check for security patterns
                    case detect_security_patterns(UserId) of
                        {error, _} = Err -> Err;
                        ok -> {ok, UserId}
                    end
            end
    end;
validate_user_id(_) ->
    {error, user_id_invalid_type}.

%% Returns: {ok, RoleId} | {error, Reason}
-spec validate_role_id(binary() | undefined) -> {ok, binary()} | {error, atom()}.
validate_role_id(undefined) ->
    {error, role_id_required};
validate_role_id(<<>>) ->
    {error, role_id_empty};
validate_role_id(RoleId) when is_binary(RoleId) ->
    %% Check length
    case validate_length(RoleId, 1, 64) of
        {error, _} = Err -> Err;
        ok ->
            %% Check format (alphanumeric, underscore, hyphen)
            case validate_id_format(RoleId, role_id) of
                {error, _} = Err -> Err;
                ok ->
                    %% Check for security patterns
                    case detect_security_patterns(RoleId) of
                        {error, _} = Err -> Err;
                        ok -> {ok, RoleId}
                    end
            end
    end;
validate_role_id(_) ->
    {error, role_id_invalid_type}.

%% Returns: ok | {error, Reason}
-spec validate_id_format(binary(), atom()) -> ok | {error, atom()}.
validate_id_format(Id, tenant_id) ->
    %% Tenant ID: alphanumeric, underscore, hyphen, dot
    case re:run(Id, "^[a-zA-Z0-9_.-]+$", [{capture, none}]) of
        match -> ok;
        _ -> {error, invalid_format}
    end;
validate_id_format(Id, policy_id) ->
    %% Policy ID: alphanumeric, underscore, hyphen, dot
    case re:run(Id, "^[a-zA-Z0-9_.-]+$", [{capture, none}]) of
        match -> ok;
        _ -> {error, invalid_format}
    end;
validate_id_format(Id, user_id) ->
    %% User ID: alphanumeric, underscore, hyphen
    case re:run(Id, "^[a-zA-Z0-9_-]+$", [{capture, none}]) of
        match -> ok;
        _ -> {error, invalid_format}
    end;
validate_id_format(Id, role_id) ->
    %% Role ID: alphanumeric, underscore, hyphen
    case re:run(Id, "^[a-zA-Z0-9_-]+$", [{capture, none}]) of
        match -> ok;
        _ -> {error, invalid_format}
    end;
validate_id_format(_Id, _Type) ->
    {error, unknown_type}.

%% Returns: ok | {error, Reason}
-spec validate_length(binary(), non_neg_integer(), non_neg_integer()) -> ok | {error, atom()}.
validate_length(Input, MinLength, MaxLength) when is_binary(Input) ->
    Size = byte_size(Input),
    case Size >= MinLength andalso Size =< MaxLength of
        true -> ok;
        false -> {error, invalid_length}
    end;
validate_length(_, _, _) ->
    {error, invalid_type}.

%% Detects SQL injection, XSS, path traversal, command injection patterns
%% Returns: ok | {error, security_pattern_detected}
-spec detect_security_patterns(binary()) -> ok | {error, atom()}.
detect_security_patterns(Input) when is_binary(Input) ->
    %% SQL injection patterns
    SQLPatterns = [
        "(?i)(union\\s+select|select\\s+.*\\s+from|insert\\s+into|delete\\s+from|update\\s+.*\\s+set|drop\\s+table)",
        "(?i)(or\\s+1\\s*=\\s*1|or\\s+'1'\\s*=\\s*'1'|or\\s+\"1\"\\s*=\\s*\"1\")",
        "(?i)(;\\s*--|;\\s*/\\*|'\\s*or\\s*'|'\\s*and\\s*')"
    ],
    
    %% XSS patterns
    XSSPatterns = [
        "(?i)(<script|javascript:|onerror=|onload=|onclick=|<iframe|eval\\s*\\()",
        "(?i)(document\\.cookie|document\\.write|window\\.location)"
    ],
    
    %% Path traversal patterns
    PathTraversalPatterns = [
        "\\.\\./|\\.\\.\\\\|%2e%2e%2f|%2e%2e%5c",
        "(?i)(/etc/passwd|/etc/shadow|/proc/|/sys/)"
    ],
    
    %% Command injection patterns
    CommandInjectionPatterns = [
        "(?i)(;\\s*rm\\s+-rf|;\\s*cat\\s+|;\\s*ls\\s+|;\\s*ps\\s+|;\\s*kill\\s+)",
        "(?i)(\\|\\s*sh|\\|\\s*bash|\\|\\s*cmd|`.*`|\\$\\{.*\\})"
    ],
    
    AllPatterns = SQLPatterns ++ XSSPatterns ++ PathTraversalPatterns ++ CommandInjectionPatterns,
    
    case lists:any(fun(Pattern) ->
        case re:run(Input, Pattern, [{capture, none}]) of
            match -> true;
            _ -> false
        end
    end, AllPatterns) of
        true -> {error, security_pattern_detected};
        false -> ok
    end;
detect_security_patterns(_) ->
    {error, invalid_type}.

%% Returns: Sanitized binary
-spec sanitize_input(binary() | undefined) -> binary().
sanitize_input(undefined) ->
    ~"<undefined>";
sanitize_input(Input) when is_binary(Input) ->
    %% Remove or escape potentially dangerous characters
    %% Keep alphanumeric, underscore, hyphen, dot, space
    case re:run(Input, "^[a-zA-Z0-9_.-\\s]+$", [{capture, none}]) of
        match ->
            Input;
        _ ->
            %% Sanitize by removing dangerous characters
            Sanitized = re:replace(Input, "[^a-zA-Z0-9_.-\\s]", "", [global, {return, binary}]),
            Sanitized
    end;
sanitize_input(_) ->
    ~"<invalid>".

%% Returns: {ok, Binary} | {error, Reason}
-spec validate_binary_input(binary() | undefined, atom()) -> {ok, binary()} | {error, atom()}.
validate_binary_input(undefined, _Type) ->
    {error, input_required};
validate_binary_input(<<>>, _Type) ->
    {error, input_empty};
validate_binary_input(Input, Type) when is_binary(Input) ->
    %% Validate based on type
    case Type of
        tenant_id -> validate_tenant_id(Input);
        policy_id -> validate_policy_id(Input);
        user_id -> validate_user_id(Input);
        role_id -> validate_role_id(Input);
        _ -> {error, unknown_type}
    end;
validate_binary_input(_, _) ->
    {error, invalid_type}.

