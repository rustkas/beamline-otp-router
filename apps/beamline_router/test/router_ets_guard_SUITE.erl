%% @doc Unit tests for router_ets_guard module
%% Tests ETS table invariant verification

-module(router_ets_guard_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_ensure_table_positive/1,
    test_ensure_table_negative/1,
    test_validate_spec/1,
    test_verify_table/1
]}).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_ensure_table_negative/1,
    test_ensure_table_positive/1,
    test_validate_spec/1,
    test_verify_table/1
]).
-export([groups_for_level/1]).

all() ->
    [].

groups_for_level(sanity) -> [];
groups_for_level(fast) -> [{group, unit_tests}];
groups_for_level(full) -> [{group, unit_tests}];
groups_for_level(heavy) -> [{group, unit_tests}].

groups() ->
    [{unit_tests, [sequence], [
        test_ensure_table_positive,
        test_ensure_table_negative,
        test_validate_spec,
        test_verify_table
    ]}].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, false),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    %% Create unique table prefix for this test case to avoid conflicts
    UniqueId = erlang:unique_integer([positive]),
    TablePrefix = list_to_atom("ets_guard_" ++ atom_to_list(TestCase) ++ "_" ++ integer_to_list(UniqueId)),
    [{table_prefix, TablePrefix}, {unique_id, UniqueId} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Cleanup any leftover ETS tables from this test
    _UniqueId = proplists:get_value(unique_id, Config, 0),
    %% Attempt to delete tables created in this test (ets_guard_test_* prefix)
    Tables = ets:all(),
    lists:foreach(fun(Tab) ->
        TabName = case catch ets:info(Tab, name) of
            Name when is_atom(Name) -> Name;
            _ -> undefined
        end,
        case TabName of
            undefined -> ok;
            _ ->
                TabNameStr = atom_to_list(TabName),
                case string:prefix(TabNameStr, "ets_guard_test_") of
                    nomatch -> ok;
                    _ -> catch ets:delete(Tab)
                end
        end
    end, Tables),
    ok.

%% @doc Test positive case: correct specification
test_ensure_table_positive(_Config) ->
    TableName = list_to_atom("ets_guard_test_pos_" ++ integer_to_list(erlang:unique_integer([positive]))),
    
    %% Note: 'compressed' is just a flag atom, not {compressed, false}
    %% Omit it to get compressed=false
    Table = router_ets_helpers:ensure_named_ets_table(TableName, [
        set,
        {keypos, 1},
        protected,
        {read_concurrency, true}
    ]),
    ets:delete_all_objects(Table),
    
    Spec = #{
        type => set,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    
    {ok, Checked} = router_ets_guard:ensure_table(Table, Spec),
    ?assert(is_map(Checked)),
    ?assert(maps:is_key(checked, Checked)),
    ?assert(maps:is_key(latency_us, Checked)),
    
    ets:delete(Table),
    ok.

%% @doc Test negative case: specification violations
test_ensure_table_negative(_Config) ->
    TableName = list_to_atom("ets_guard_test_neg_" ++ integer_to_list(erlang:unique_integer([positive]))),
    
    %% Create table with different spec than expected
    %% Note: 'compressed' is a flag atom, include it to enable compression
    Table = router_ets_helpers:ensure_named_ets_table(TableName, [
        bag,
        {keypos, 2},
        protected,
        compressed  %% Enable compression
    ]),
    ets:delete_all_objects(Table),
    
    Spec = #{
        type => set,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    
    {error, [{table, _}, {reason, Violations}]} = router_ets_guard:ensure_table(Table, Spec),
    ?assert(is_list(Violations)),
    ?assert(length(Violations) > 0),
    
    ets:delete(Table),
    ok.

%% @doc Test spec validation
test_validate_spec(_Config) ->
    ValidSpec = #{
        type => set,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    ok = router_ets_guard:validate_spec(ValidSpec),
    
    InvalidSpec = #{
        type => set,
        invalid_key => value
    },
    %% The actual return format from check_spec is {error, [invalid_key]}
    {error, [invalid_key]} = router_ets_guard:validate_spec(InvalidSpec),
    
    {error, not_a_map} = router_ets_guard:validate_spec(not_a_map),
    
    ok.

%% @doc Test verify_table
test_verify_table(_Config) ->
    TableName = list_to_atom("ets_guard_test_ver_" ++ integer_to_list(erlang:unique_integer([positive]))),
    
    Table = router_ets_helpers:ensure_named_ets_table(TableName, [
        set,
        {keypos, 1},
        protected,
        {read_concurrency, true}
    ]),
    ets:delete_all_objects(Table),
    
    Spec = #{
        type => set,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    
    ok = router_ets_guard:verify_table(Table, Spec),
    
    WrongSpec = #{
        type => bag,
        keypos => 2
    },
    {error, Violations} = router_ets_guard:verify_table(Table, WrongSpec),
    ?assert(is_list(Violations)),
    
    ets:delete(Table),
    ok.
