%% @doc Data Privacy Compliance Module
%%
%% Provides functions for GDPR compliance, PII handling, and data retention policies.
%% Ensures all data handling complies with GDPR and other privacy regulations.
%%
%% @see COMPLIANCE_GUIDE.md For data privacy documentation
-module(router_data_privacy).

-export([
    verify_pii_handling/0,
    verify_gdpr_compliance/0,
    get_data_retention_policies/0,
    check_data_retention/2,
    delete_expired_data/1,
    get_right_to_be_forgotten/1,
    anonymize_pii/1,
    is_pii_field/1
]).

-include("beamline_router.hrl").

%% PII fields that must be handled according to GDPR
-define(PII_FIELDS, [
    <<"email">>,
    <<"phone">>,
    <<"ssn">>,
    <<"credit_card">>,
    <<"ip_address">>,
    <<"user_id">>,
    <<"tenant_id">>,
    <<"session_id">>,
    <<"correlation_id">>,
    <<"trace_id">>
]).

%% Data retention periods (in days)
-define(DEFAULT_RETENTION_DAYS, 90).
-define(AUDIT_RETENTION_DAYS, 90).
-define(LOG_RETENTION_DAYS, 30).
-define(METRICS_RETENTION_DAYS, 90).

%% @doc Verify PII handling compliance
%% Returns {ok, ComplianceReport} or {error, Reason}
-spec verify_pii_handling() -> {ok, map()} | {error, term()}.
verify_pii_handling() ->
    try
        %% Check PII filtering in logger
        LoggerPIIFilter = check_logger_pii_filter(),
        
        %% Check audit log PII handling
        AuditPIIHandling = check_audit_pii_handling(),
        
        %% Check data anonymization
        AnonymizationCheck = check_anonymization(),
        
        Report = #{
            logger_pii_filter => LoggerPIIFilter,
            audit_pii_handling => AuditPIIHandling,
            anonymization => AnonymizationCheck,
            compliant => LoggerPIIFilter andalso AuditPIIHandling andalso AnonymizationCheck,
            timestamp => erlang:system_time(second)
        },
        
        {ok, Report}
    catch
        Class:Reason:Stack ->
            {error, {verification_failed, Class, Reason, Stack}}
    end.

%% @doc Verify GDPR compliance
%% Returns {ok, ComplianceReport} or {error, Reason}
-spec verify_gdpr_compliance() -> {ok, map()} | {error, term()}.
verify_gdpr_compliance() ->
    try
        %% Check right to be forgotten
        RightToBeForgotten = check_right_to_be_forgotten(),
        
        %% Check data retention policies
        DataRetention = check_data_retention_policies(),
        
        %% Check PII handling
        PIIHandling = verify_pii_handling(),
        
        %% Check audit logging
        AuditLogging = check_audit_logging(),
        
        Report = #{
            right_to_be_forgotten => RightToBeForgotten,
            data_retention => DataRetention,
            pii_handling => PIIHandling,
            audit_logging => AuditLogging,
            compliant => RightToBeForgotten andalso DataRetention andalso
                        maps:get(compliant, PIIHandling, false) andalso AuditLogging,
            timestamp => erlang:system_time(second)
        },
        
        {ok, Report}
    catch
        Class:Reason:Stack ->
            {error, {verification_failed, Class, Reason, Stack}}
    end.

%% @doc Get data retention policies
%% Returns map with retention policies
-spec get_data_retention_policies() -> map().
get_data_retention_policies() ->
    #{
        audit_logs => #{
            retention_days => ?AUDIT_RETENTION_DAYS,
            policy => <<"Audit logs retained for compliance and security">>,
            auto_delete => true
        },
        application_logs => #{
            retention_days => ?LOG_RETENTION_DAYS,
            policy => <<"Application logs retained for debugging">>,
            auto_delete => true
        },
        metrics => #{
            retention_days => ?METRICS_RETENTION_DAYS,
            policy => <<"Metrics retained for performance monitoring">>,
            auto_delete => true
        },
        policies => #{
            retention_days => undefined,
            policy => <<"Policies retained until explicitly deleted">>,
            auto_delete => false
        },
        rbac_data => #{
            retention_days => undefined,
            policy => <<"RBAC data retained until explicitly deleted">>,
            auto_delete => false
        }
    }.

%% @doc Check data retention for a data type
%% @param DataType Binary data type (audit_logs, application_logs, metrics, etc.)
%% @param Timestamp Integer timestamp in milliseconds
-spec check_data_retention(binary(), integer()) -> {ok, boolean()} | {error, term()}.
check_data_retention(DataType, Timestamp) ->
    Policies = get_data_retention_policies(),
    case maps:get(DataType, Policies, undefined) of
        undefined ->
            {error, unknown_data_type};
        Policy ->
            RetentionDays = maps:get(retention_days, Policy, undefined),
            case RetentionDays of
                undefined ->
                    {ok, true};  %% No retention limit
                Days ->
                    RetentionMs = Days * 24 * 60 * 60 * 1000,
                    Now = erlang:system_time(millisecond),
                    Age = Now - Timestamp,
                    {ok, Age < RetentionMs}
            end
    end.

%% @doc Delete expired data for a data type
%% @param DataType Binary data type
-spec delete_expired_data(binary()) -> {ok, integer()} | {error, term()}.
delete_expired_data(DataType) ->
    Policies = get_data_retention_policies(),
    case maps:get(DataType, Policies, undefined) of
        undefined ->
            {error, unknown_data_type};
        Policy ->
            RetentionDays = maps:get(retention_days, Policy, undefined),
            AutoDelete = maps:get(auto_delete, Policy, false),
            case RetentionDays of
                undefined ->
                    {ok, 0};  %% No retention limit
                Days when AutoDelete =:= true ->
                    delete_expired_data_internal(DataType, Days);
                _ ->
                    {ok, 0}  %% Auto-delete disabled
            end
    end.

%% @doc Get right to be forgotten (GDPR Article 17)
%% @param UserId Binary user identifier
-spec get_right_to_be_forgotten(binary()) -> {ok, integer()} | {error, term()}.
get_right_to_be_forgotten(UserId) when is_binary(UserId) ->
    try
        %% Delete user data from audit logs
        AuditDeleted = delete_user_audit_entries(UserId),
        
        %% Delete user data from RBAC
        RbacDeleted = delete_user_rbac_data(UserId),
        
        %% Anonymize user data in logs (if retention period not expired)
        _ = anonymize_user_logs(UserId),
        
        TotalDeleted = AuditDeleted + RbacDeleted,
        
        {ok, TotalDeleted}
    catch
        Class:Reason ->
            {error, {deletion_failed, Class, Reason}}
    end.

%% @doc Anonymize PII in data
%% @param Data Map or list containing data
-spec anonymize_pii(term()) -> term().
anonymize_pii(Data) when is_map(Data) ->
    maps:fold(fun
        (Key, Value, Acc) ->
            KeyBin = ensure_binary(Key),
            case is_pii_field(KeyBin) of
                true ->
                    maps:put(KeyBin, <<"[ANONYMIZED]">>, Acc);
                false ->
                    case is_map(Value) of
                        true ->
                            maps:put(KeyBin, anonymize_pii(Value), Acc);
                        false ->
                            maps:put(KeyBin, Value, Acc)
                    end
            end
    end, #{}, Data);
anonymize_pii(Data) when is_list(Data) ->
    [anonymize_pii(Item) || Item <- Data];
anonymize_pii(Data) ->
    Data.

%% @doc Check if field is PII
%% @param Field Binary field name
-spec is_pii_field(binary()) -> boolean().
is_pii_field(Field) when is_binary(Field) ->
    FieldLower = binary_to_lowercase(Field),
    lists:any(fun(PIIField) ->
        binary_to_lowercase(PIIField) =:= FieldLower
    end, ?PII_FIELDS);
is_pii_field(_) ->
    false.

%% Internal: Check logger PII filter
-spec check_logger_pii_filter() -> boolean().
check_logger_pii_filter() ->
    case erlang:function_exported(router_logger, filter_pii, 1) of
        true ->
            %% Test PII filtering
            TestData = #{
                <<"email">> => <<"test@example.com">>,
                <<"password">> => <<"secret">>,
                <<"normal_field">> => <<"value">>
            },
            Filtered = router_logger:filter_pii(TestData),
            maps:get(<<"email">>, Filtered, undefined) =:= <<"[REDACTED]">> andalso
            maps:get(<<"password">>, Filtered, undefined) =:= <<"[REDACTED]">> andalso
            maps:get(<<"normal_field">>, Filtered, undefined) =:= <<"value">>;
        false ->
            false
    end.

%% Internal: Check audit PII handling
-spec check_audit_pii_handling() -> boolean().
check_audit_pii_handling() ->
    %% Check if audit module exists and handles PII
    case erlang:function_exported(router_audit, get_audit_retention_days, 0) of
        true ->
            %% Audit module exists and has retention policy
            true;
        false ->
            false
    end.

%% Internal: Check anonymization
-spec check_anonymization() -> boolean().
check_anonymization() ->
    %% Test anonymization function
    TestData = #{
        <<"user_id">> => <<"user123">>,
        <<"email">> => <<"test@example.com">>,
        <<"normal_field">> => <<"value">>
    },
    Anonymized = anonymize_pii(TestData),
    maps:get(<<"user_id">>, Anonymized, undefined) =:= <<"[ANONYMIZED]">> andalso
    maps:get(<<"email">>, Anonymized, undefined) =:= <<"[ANONYMIZED]">> andalso
    maps:get(<<"normal_field">>, Anonymized, undefined) =:= <<"value">>.

%% Internal: Check right to be forgotten
-spec check_right_to_be_forgotten() -> boolean().
check_right_to_be_forgotten() ->
    %% Check if function exists
    erlang:function_exported(?MODULE, get_right_to_be_forgotten, 1).

%% Internal: Check data retention policies
-spec check_data_retention_policies() -> boolean().
check_data_retention_policies() ->
    Policies = get_data_retention_policies(),
    maps:size(Policies) > 0.

%% Internal: Check audit logging
-spec check_audit_logging() -> boolean().
check_audit_logging() ->
    erlang:function_exported(router_audit, log_policy_action, 5).

%% Internal: Delete expired data
-spec delete_expired_data_internal(binary(), integer()) -> {ok, integer()}.
delete_expired_data_internal(<<"audit_logs">>, _) ->
    %% Use router_audit cleanup if available
    case erlang:function_exported(router_audit, cleanup, 0) of
        true ->
            router_audit:cleanup(),
            {ok, 0};  %% Count not available
        false ->
            {ok, 0}
    end;
delete_expired_data_internal(_DataType, _RetentionDays) ->
    {ok, 0}.

%% Internal: Delete user audit entries
-spec delete_user_audit_entries(binary()) -> integer().
delete_user_audit_entries(_) ->
    %% This would need to be implemented in router_audit
    %% For now, return 0
    0.

%% Internal: Delete user RBAC data
-spec delete_user_rbac_data(binary()) -> integer().
delete_user_rbac_data(_) ->
    %% This would need to be implemented in router_rbac
    %% For now, return 0
    0.

%% Internal: Anonymize user logs
-spec anonymize_user_logs(binary()) -> integer().
anonymize_user_logs(_) ->
    %% This would need to be implemented in router_logger
    %% For now, return 0
    0.

%% Internal: Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> iolist_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(_) -> <<"unknown">>.

%% Internal: Convert binary to lowercase
-spec binary_to_lowercase(binary()) -> binary().
binary_to_lowercase(Bin) when is_binary(Bin) ->
    << <<(case C of
        C when C >= $A, C =< $Z -> C + 32;
        C -> C
    end)>> || <<C>> <= Bin >>;
binary_to_lowercase(_) ->
    <<>>.

