-module(router_decider_prop_SUITE).

%% INTENTIONAL_EMPTY: placeholder suite, no test cases yet.
%% See docs/tests/README_CT_GROUPS.md.
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() -> [].
groups() -> [].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.
