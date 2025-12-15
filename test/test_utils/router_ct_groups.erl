 -module(router_ct_groups).

-export([groups_definitions/2]).

groups_definitions(Module, BaseGroups) ->
    case has_group(BaseGroups, quarantine) of
        true -> BaseGroups;
        false ->
            Tests = collect_tests(Module),
            BaseGroups ++ [{quarantine, [sequence], Tests}]
    end.

% helper no longer reads metadata or environment; suites declare quarantine themselves

collect_tests(Module) ->
    Exports = Module:module_info(exports),
    Tests = [Name || {Name, 1} <- Exports, is_test_fun(Name)],
    Tests.

is_test_fun(Name) ->
    Str = atom_to_list(Name),
    case Str of
        [$t,$e,$s,$t,$_|_] -> true;
        _ -> false
    end.
has_group(Groups, Name) ->
    lists:any(fun({GName, _Props, _Tests}) -> GName =:= Name; (_) -> false end, Groups).
