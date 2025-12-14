%% @doc Compliance Tests
%%
%% Tests license compliance and data privacy (GDPR) compliance.
%% Verifies that the router maintains compliance with license requirements and data privacy regulations.
%%
%% @test_category compliance
-module(router_compliance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test functions
-export([
    test_license_compliance/1,
    test_dependency_licenses/1,
    test_license_compatibility/1,
    test_pii_handling/1,
    test_gdpr_compliance/1,
    test_data_retention_policies/1,
    test_data_retention_check/1,
    test_anonymize_pii/1,
    test_is_pii_field/1
]).

all() ->
    [].

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_license_compliance,
            test_dependency_licenses,
            test_license_compatibility,
            test_pii_handling,
            test_gdpr_compliance,
            test_data_retention_policies,
            test_data_retention_check,
            test_anonymize_pii,
            test_is_pii_field
        ]}
    ].

init_per_suite(Config) ->
    ok = router_suite_helpers:start_router_suite(),
    Config.

end_per_suite(_Config) ->
    router_suite_helpers:stop_router_suite(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: License compliance verification
test_license_compliance(_Config) ->
    case router_license_compliance:verify_dependencies() of
        {ok, Report} ->
            ?assert(maps:is_key(compliant, Report)),
            ?assert(maps:is_key(dependencies, Report)),
            ?assert(maps:is_key(total_dependencies, Report)),
            ?assert(maps:is_key(status_counts, Report)),
            
            Compliant = maps:get(compliant, Report),
            ?assert(Compliant),
            
            ok;
        {error, Reason} ->
            ct:fail({license_compliance_verification_failed, Reason})
    end.

%% @doc Test: Get dependency licenses
test_dependency_licenses(_Config) ->
    Dependencies = router_license_compliance:get_dependency_licenses(),
    
    ?assert(maps:is_key(<<"stdlib">>, Dependencies)),
    ?assert(maps:is_key(<<"kernel">>, Dependencies)),
    
    StdlibInfo = maps:get(<<"stdlib">>, Dependencies),
    ?assert(maps:is_key(license, StdlibInfo)),
    ?assert(maps:is_key(version, StdlibInfo)),
    
    ok.

%% @doc Test: License compatibility
test_license_compatibility(_Config) ->
    %% Test allowed licenses
    ?assert(router_license_compliance:is_license_compliant(<<"Apache-2.0">>)),
    ?assert(router_license_compliance:is_license_compliant(<<"MIT">>)),
    ?assert(router_license_compliance:is_license_compliant(<<"BSD-3-Clause">>)),
    ?assert(router_license_compliance:is_license_compliant(<<"Erlang Public License">>)),
    
    %% Test restricted licenses
    ?assertNot(router_license_compliance:is_license_compliant(<<"GPL-2.0">>)),
    ?assertNot(router_license_compliance:is_license_compliant(<<"GPL-3.0">>)),
    
    ok.

%% @doc Test: PII handling verification
test_pii_handling(_Config) ->
    case router_data_privacy:verify_pii_handling() of
        {ok, Report} ->
            ?assert(maps:is_key(compliant, Report)),
            ?assert(maps:is_key(logger_pii_filter, Report)),
            ?assert(maps:is_key(audit_pii_handling, Report)),
            ?assert(maps:is_key(anonymization, Report)),
            
            Compliant = maps:get(compliant, Report),
            ?assert(Compliant),
            
            ok;
        {error, Reason} ->
            ct:fail({pii_handling_verification_failed, Reason})
    end.

%% @doc Test: GDPR compliance verification
test_gdpr_compliance(_Config) ->
    case router_data_privacy:verify_gdpr_compliance() of
        {ok, Report} ->
            ?assert(maps:is_key(compliant, Report)),
            ?assert(maps:is_key(right_to_be_forgotten, Report)),
            ?assert(maps:is_key(data_retention, Report)),
            ?assert(maps:is_key(pii_handling, Report)),
            ?assert(maps:is_key(audit_logging, Report)),
            
            ok;
        {error, Reason} ->
            ct:fail({gdpr_compliance_verification_failed, Reason})
    end.

%% @doc Test: Data retention policies
test_data_retention_policies(_Config) ->
    Policies = router_data_privacy:get_data_retention_policies(),
    
    ?assert(maps:is_key(<<"audit_logs">>, Policies)),
    ?assert(maps:is_key(<<"application_logs">>, Policies)),
    ?assert(maps:is_key(<<"metrics">>, Policies)),
    
    AuditPolicy = maps:get(<<"audit_logs">>, Policies),
    ?assert(maps:is_key(retention_days, AuditPolicy)),
    ?assert(maps:is_key(policy, AuditPolicy)),
    ?assert(maps:is_key(auto_delete, AuditPolicy)),
    
    ok.

%% @doc Test: Data retention check
test_data_retention_check(_Config) ->
    %% Test with recent timestamp (should be retained)
    RecentTimestamp = erlang:system_time(millisecond) - (1 * 24 * 60 * 60 * 1000),  %% 1 day ago
    {ok, ShouldRetain} = router_data_privacy:check_data_retention(<<"audit_logs">>, RecentTimestamp),
    ?assert(ShouldRetain),
    
    %% Test with old timestamp (should be expired)
    OldTimestamp = erlang:system_time(millisecond) - (100 * 24 * 60 * 60 * 1000),  %% 100 days ago
    {ok, ShouldExpire} = router_data_privacy:check_data_retention(<<"audit_logs">>, OldTimestamp),
    ?assertNot(ShouldExpire),
    
    ok.

%% @doc Test: Anonymize PII
test_anonymize_pii(_Config) ->
    TestData = #{
        <<"user_id">> => <<"user123">>,
        <<"email">> => <<"test@example.com">>,
        <<"normal_field">> => <<"value">>,
        <<"nested">> => #{
            <<"phone">> => <<"1234567890">>,
            <<"normal">> => <<"data">>
        }
    },
    
    Anonymized = router_data_privacy:anonymize_pii(TestData),
    
    ?assertEqual(<<"[ANONYMIZED]">>, maps:get(<<"user_id">>, Anonymized)),
    ?assertEqual(<<"[ANONYMIZED]">>, maps:get(<<"email">>, Anonymized)),
    ?assertEqual(<<"value">>, maps:get(<<"normal_field">>, Anonymized)),
    
    Nested = maps:get(<<"nested">>, Anonymized),
    ?assertEqual(<<"[ANONYMIZED]">>, maps:get(<<"phone">>, Nested)),
    ?assertEqual(<<"data">>, maps:get(<<"normal">>, Nested)),
    
    ok.

%% @doc Test: Is PII field
test_is_pii_field(_Config) ->
    %% Test PII fields
    ?assert(router_data_privacy:is_pii_field(<<"email">>)),
    ?assert(router_data_privacy:is_pii_field(<<"phone">>)),
    ?assert(router_data_privacy:is_pii_field(<<"user_id">>)),
    ?assert(router_data_privacy:is_pii_field(<<"tenant_id">>)),
    
    %% Test non-PII fields
    ?assertNot(router_data_privacy:is_pii_field(<<"normal_field">>)),
    ?assertNot(router_data_privacy:is_pii_field(<<"message">>)),
    
    ok.
