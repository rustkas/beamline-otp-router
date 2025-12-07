%% @doc Fault tolerance tests for router_policy_store
%% Tests heir mechanism, table transfer, and recovery after crashes
-module(router_policy_store_fault_tolerance_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

