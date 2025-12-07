%% @doc Admin API Self-Test
%% Tests all admin NATS subjects and validates response formats
-module(router_admin_self_check).

-export([self_check/0, self_check_all/0]).

-include("beamline_router.hrl").

%% @doc Run self-check for all admin endpoints
-spec self_check_all() -> {ok, list()} | {error, term()}.
self_check_all() ->
    Results = [
        self_check_extension_health(),
        self_check_circuit_breaker_states(),
        self_check_dry_run_pipeline(),
        self_check_pipeline_complexity()
    ],
    
    %% Check if all passed
    Failed = [R || R = {_Name, {error, _}} <- Results],
    case Failed of
        [] ->
            {ok, Results};
        _ ->
            {error, {some_checks_failed, Failed}}
    end.

%% @doc Run single self-check (alias for self_check_all/0)
-spec self_check() -> {ok, list()} | {error, term()}.
self_check() ->
    self_check_all().

%% @doc Test get_extension_health endpoint
-spec self_check_extension_health() -> {extension_health, {ok, map()} | {error, term()}}.
self_check_extension_health() ->
    try
        RequestJson = jsx:encode(#{}),
        case router_admin_nats:handle_get_extension_health(RequestJson) of
            {ok, ResponseJson} ->
                Response = jsx:decode(ResponseJson, [return_maps]),
                %% Validate response structure
                case validate_extension_health_response(Response) of
                    ok ->
                        {extension_health, {ok, Response}};
                    {error, Reason} ->
                        {extension_health, {error, {validation_failed, Reason}}}
                end;
            {error, ErrorJson} ->
                Error = jsx:decode(ErrorJson, [return_maps]),
                {extension_health, {error, {endpoint_error, Error}}}
        end
    catch
        ErrorClass:ErrorReason:ErrorStacktrace ->
            {extension_health, {error, {exception, ErrorClass, ErrorReason, ErrorStacktrace}}}
    end.

%% @doc Test get_circuit_breaker_states endpoint
-spec self_check_circuit_breaker_states() -> {circuit_breaker_states, {ok, map()} | {error, term()}}.
self_check_circuit_breaker_states() ->
    try
        RequestJson = jsx:encode(#{}),
        case router_admin_nats:handle_get_circuit_breaker_states(RequestJson) of
            {ok, ResponseJson} ->
                Response = jsx:decode(ResponseJson, [return_maps]),
                %% Validate response structure
                case validate_circuit_breaker_states_response(Response) of
                    ok ->
                        {circuit_breaker_states, {ok, Response}};
                    {error, Reason} ->
                        {circuit_breaker_states, {error, {validation_failed, Reason}}}
                end;
            {error, ErrorJson} ->
                Error = jsx:decode(ErrorJson, [return_maps]),
                {circuit_breaker_states, {error, {endpoint_error, Error}}}
        end
    catch
        ErrorClass:ErrorReason:ErrorStacktrace ->
            {circuit_breaker_states, {error, {exception, ErrorClass, ErrorReason, ErrorStacktrace}}}
    end.

%% @doc Test dry_run_pipeline endpoint
-spec self_check_dry_run_pipeline() -> {dry_run_pipeline, {ok, map()} | {error, term()}}.
self_check_dry_run_pipeline() ->
    try
        %% Create minimal test request
        Request = #{
            <<"tenant_id">> => <<"test_tenant">>,
            <<"policy_id">> => <<"test_policy">>,
            <<"payload">> => #{<<"message">> => <<"test">>}
        },
        RequestJson = jsx:encode(Request),
        case router_admin_nats:handle_dry_run_pipeline(RequestJson) of
            {ok, ResponseJson} ->
                Response = jsx:decode(ResponseJson, [return_maps]),
                %% Validate response structure
                case validate_dry_run_pipeline_response(Response) of
                    ok ->
                        {dry_run_pipeline, {ok, Response}};
                    {error, Reason} ->
                        {dry_run_pipeline, {error, {validation_failed, Reason}}}
                end;
            {error, ErrorJson} ->
                Error = jsx:decode(ErrorJson, [return_maps]),
                {dry_run_pipeline, {error, {endpoint_error, Error}}}
        end
    catch
        ErrorClass:ErrorReason:ErrorStacktrace ->
            {dry_run_pipeline, {error, {exception, ErrorClass, ErrorReason, ErrorStacktrace}}}
    end.

%% @doc Test get_pipeline_complexity endpoint
-spec self_check_pipeline_complexity() -> {pipeline_complexity, {ok, map()} | {error, term()}}.
self_check_pipeline_complexity() ->
    try
        %% Create minimal test request
        Request = #{
            <<"tenant_id">> => <<"test_tenant">>,
            <<"policy_id">> => <<"test_policy">>
        },
        RequestJson = jsx:encode(Request),
        case router_admin_nats:handle_get_pipeline_complexity(RequestJson) of
            {ok, ResponseJson} ->
                Response = jsx:decode(ResponseJson, [return_maps]),
                %% Validate response structure
                case validate_pipeline_complexity_response(Response) of
                    ok ->
                        {pipeline_complexity, {ok, Response}};
                    {error, Reason} ->
                        {pipeline_complexity, {error, {validation_failed, Reason}}}
                end;
            {error, ErrorJson} ->
                Error = jsx:decode(ErrorJson, [return_maps]),
                {pipeline_complexity, {error, {endpoint_error, Error}}}
        end
    catch
        ErrorClass:ErrorReason:ErrorStacktrace ->
            {pipeline_complexity, {error, {exception, ErrorClass, ErrorReason, ErrorStacktrace}}}
    end.

%% Internal: Validate extension health response
-spec validate_extension_health_response(map()) -> ok | {error, term()}.
validate_extension_health_response(Response) ->
    case maps:get(<<"health">>, Response, undefined) of
        undefined ->
            {error, missing_health_field};
        Health when is_map(Health) ->
            %% Validate each health entry
            ValidateEntry = fun(_ExtId, HealthEntry) ->
                RequiredFields = [
                    <<"extension_id">>,
                    <<"status">>,
                    <<"success_rate">>
                ],
                lists:all(fun(Field) ->
                    maps:is_key(Field, HealthEntry)
                end, RequiredFields)
            end,
            case maps:fold(fun(K, V, Acc) ->
                case ValidateEntry(K, V) of
                    true -> Acc;
                    false -> [K | Acc]
                end
            end, [], Health) of
                [] -> ok;
                Missing -> {error, {missing_fields_in_entries, Missing}}
            end;
        _ ->
            {error, health_not_map}
    end.

%% Internal: Validate circuit breaker states response
-spec validate_circuit_breaker_states_response(map()) -> ok | {error, term()}.
validate_circuit_breaker_states_response(Response) ->
    case maps:get(<<"states">>, Response, undefined) of
        undefined ->
            {error, missing_states_field};
        States when is_map(States) ->
            %% Validate each state entry
            ValidateEntry = fun(_ExtId, StateEntry) ->
                RequiredFields = [
                    <<"extension_id">>,
                    <<"state">>
                ],
                lists:all(fun(Field) ->
                    maps:is_key(Field, StateEntry)
                end, RequiredFields)
            end,
            case maps:fold(fun(K, V, Acc) ->
                case ValidateEntry(K, V) of
                    true -> Acc;
                    false -> [K | Acc]
                end
            end, [], States) of
                [] -> ok;
                Missing -> {error, {missing_fields_in_entries, Missing}}
            end;
        _ ->
            {error, states_not_map}
    end.

%% Internal: Validate dry-run pipeline response
-spec validate_dry_run_pipeline_response(map()) -> ok | {error, term()}.
validate_dry_run_pipeline_response(Response) ->
    case maps:get(<<"ok">>, Response, undefined) of
        undefined ->
            {error, missing_ok_field};
        true ->
            %% Success response
            case maps:get(<<"result">>, Response, undefined) of
                undefined ->
                    {error, missing_result_field};
                Result when is_map(Result) ->
                    %% Validate decision field
                    case maps:get(<<"decision">>, Result, undefined) of
                        undefined ->
                            {error, missing_decision_field};
                        Decision when is_map(Decision) ->
                            RequiredFields = [
                                <<"provider_id">>,
                                <<"reason">>
                            ],
                            case lists:all(fun(Field) ->
                                maps:is_key(Field, Decision)
                            end, RequiredFields) of
                                true -> ok;
                                false -> {error, missing_decision_fields}
                            end;
                        _ ->
                            {error, decision_not_map}
                    end;
                _ ->
                    {error, result_not_map}
            end;
        false ->
            %% Error response
            case maps:get(<<"error">>, Response, undefined) of
                undefined ->
                    {error, missing_error_field};
                Error when is_map(Error) ->
                    case maps:is_key(<<"code">>, Error) of
                        true -> ok;
                        false -> {error, missing_error_code}
                    end;
                _ ->
                    {error, error_not_map}
            end;
        _ ->
            {error, invalid_ok_field}
    end.

%% Internal: Validate pipeline complexity response
-spec validate_pipeline_complexity_response(map()) -> ok | {error, term()}.
validate_pipeline_complexity_response(Response) ->
    RequiredFields = [
        <<"complexity_score">>,
        <<"total_extensions">>
    ],
    case lists:all(fun(Field) ->
        maps:is_key(Field, Response)
    end, RequiredFields) of
        true ->
            %% Validate types
            case {maps:get(<<"complexity_score">>, Response),
                  maps:get(<<"total_extensions">>, Response)} of
                {Score, Total} when is_integer(Score), is_integer(Total) ->
                    ok;
                _ ->
                    {error, invalid_field_types}
            end;
        false ->
            {error, missing_required_fields}
    end.

