%% @doc Unit tests for router_ets_guard module
%% Tests ETS table invariant verification

-module(router_ets_guard_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    %% Test functions called via groups
    test_ensure_table_positive/1,
    test_ensure_table_negative/1,
    test_validate_spec/1,
    test_verify_table/1
]}).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_ensure_table_positive,
            test_ensure_table_negative,
            test_validate_spec,
            test_verify_table
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, false),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test positive case: correct specification
test_ensure_table_positive(_Config) ->
    %% Create test table with known spec
    Table = ets:new(test_table_positive, [
        set,
        named_table,
        {keypos, 1},
        protected,
        {read_concurrency, true},
        {compressed, false}
    ]),
    
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
    %% Create table with different spec than expected
    Table = ets:new(test_table_negative, [
        bag,  %% Different type
        named_table,
        {keypos, 2},  %% Different keypos
        protected,
        {read_concurrency, false},  %% Different read_concurrency
        {compressed, true}  %% Different compressed
    ]),
    
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
    %% Valid spec
    ValidSpec = #{
        type => set,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    ok = router_ets_guard:validate_spec(ValidSpec),
    
    %% Invalid spec: invalid key
    InvalidSpec = #{
        type => set,
        invalid_key => value
    },
    {error, {invalid_keys, [invalid_key]}} = router_ets_guard:validate_spec(InvalidSpec),
    
    %% Invalid spec: not a map
    {error, not_a_map} = router_ets_guard:validate_spec(not_a_map),
    
    ok.

%% @doc Test verify_table
test_verify_table(_Config) ->
    %% Create test table
    Table = ets:new(test_verify, [
        set,
        named_table,
        {keypos, 1},
        protected,
        {read_concurrency, true},
        {compressed, false}
    ]),
    
    Spec = #{
        type => set,
        keypos => 1,
        read_concurrency => true,
        write_concurrency => false,
        compressed => false
    },
    
    ok = router_ets_guard:verify_table(Table, Spec),
    
    %% Test with violations
    WrongSpec = #{
        type => bag,
        keypos => 2
    },
    {error, Violations} = router_ets_guard:verify_table(Table, WrongSpec),
    ?assert(is_list(Violations)),
    
    ets:delete(Table),
    ok.
