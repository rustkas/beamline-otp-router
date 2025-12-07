%% @doc Helper module for metric contract validation
%% 
%% Provides standardized validation functions for:
%% - Required and optional labels for each metric
%% - Label format and type validation
%% - Metric contract compliance checking
%% 
%% Based on documentation:
%% - apps/otp/router/docs/PROMETHEUS_ALERTS.md
%% - apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md
-module(router_metrics_contract_helpers).
-export([
    get_metric_contract/1,
    validate_metric_labels/3,
    validate_label_format/2,
    validate_label_type/2,
    assert_metric_contract/3
]).

-include_lib("common_test/include/ct.hrl").

%% @doc Get metric contract (required and optional labels)
%% Returns: {RequiredLabels, OptionalLabels, FormatRules}
-spec get_metric_contract(atom()) -> {list(), list(), map()}.
get_metric_contract(router_jetstream_redelivery_total) ->
    RequiredLabels = [assignment_id, request_id, reason, source],
    OptionalLabels = [msg_id, tenant_id],
    FormatRules = #{
        assignment_id => {binary, undefined},
        request_id => {binary, undefined},
        reason => {binary, undefined},
        source => {binary, undefined},
        msg_id => {binary, optional},
        tenant_id => {binary, optional}
    },
    {RequiredLabels, OptionalLabels, FormatRules};

get_metric_contract(router_jetstream_maxdeliver_exhausted_total) ->
    RequiredLabels = [assignment_id, request_id, reason],
    OptionalLabels = [msg_id, delivery_count, max_deliver, tenant_id],
    FormatRules = #{
        assignment_id => {binary, undefined},
        request_id => {binary, undefined},
        reason => {binary, undefined},
        msg_id => {binary, optional},
        delivery_count => {integer, optional},
        max_deliver => {integer, optional},
        tenant_id => {binary, optional}
    },
    {RequiredLabels, OptionalLabels, FormatRules};

get_metric_contract(router_results_tenant_rejected_total) ->
    RequiredLabels = [assignment_id, request_id, reason, tenant_id],
    OptionalLabels = [msg_id],
    FormatRules = #{
        assignment_id => {binary, undefined},
        request_id => {binary, undefined},
        reason => {binary, undefined},
        tenant_id => {binary, undefined},
        msg_id => {binary, optional}
    },
    {RequiredLabels, OptionalLabels, FormatRules};

get_metric_contract(router_usage_emit_failed_total) ->
    RequiredLabels = [subject],
    OptionalLabels = [error, tenant_id, assignment_id, request_id],
    FormatRules = #{
        subject => {binary, undefined},
        error => {binary, optional},
        tenant_id => {binary, optional},
        assignment_id => {binary, optional},
        request_id => {binary, optional}
    },
    {RequiredLabels, OptionalLabels, FormatRules};

get_metric_contract(router_usage_emit_total) ->
    RequiredLabels = [status],
    OptionalLabels = [provider_id, tenant_id, assignment_id, request_id],
    FormatRules = #{
        status => {binary, undefined},
        provider_id => {binary, optional},
        tenant_id => {binary, optional},
        assignment_id => {binary, optional},
        request_id => {binary, optional}
    },
    {RequiredLabels, OptionalLabels, FormatRules};

get_metric_contract(MetricName) ->
    ct:fail("Unknown metric contract for ~p", [MetricName]).

%% @doc Validate metric labels against contract
%% Returns: {ok, Details} | {fail, Reason}
-spec validate_metric_labels(atom(), map(), map()) -> {ok, map()} | {fail, term()}.
validate_metric_labels(MetricName, Metadata, ExpectedLabels) ->
    {RequiredLabels, OptionalLabels, FormatRules} = get_metric_contract(MetricName),
    
    %% Check all required labels are present
    MissingRequired = [Label || Label <- RequiredLabels,
                                 not maps:is_key(Label, Metadata) andalso
                                 not maps:is_key(atom_to_binary(Label, utf8), Metadata)],
    
    case MissingRequired of
        [] ->
            %% Check for unexpected labels (not in required or optional)
            AllAllowedLabels = RequiredLabels ++ OptionalLabels,
            UnexpectedLabels = [Label || Label <- maps:keys(Metadata),
                                        not lists:member(Label, AllAllowedLabels) andalso
                                        not lists:member(binary_to_atom(Label, utf8), AllAllowedLabels)],
            
            case UnexpectedLabels of
                [] ->
                    %% Validate label formats and types
                    case validate_all_label_formats(Metadata, FormatRules) of
                        {ok, FormatDetails} ->
                            %% Check expected label values if provided
                            case validate_expected_labels(Metadata, ExpectedLabels) of
                                {ok, ExpectedDetails} ->
                                    {ok, #{
                                        required_labels => RequiredLabels,
                                        optional_labels => OptionalLabels,
                                        format_details => FormatDetails,
                                        expected_details => ExpectedDetails
                                    }};
                                {fail, Reason} ->
                                    {fail, Reason}
                            end;
                        {fail, Reason} ->
                            {fail, Reason}
                    end;
                _ ->
                    {fail, {unexpected_labels, UnexpectedLabels}}
            end;
        _ ->
            {fail, {missing_required_labels, MissingRequired}}
    end.

%% @doc Validate all label formats and types
-spec validate_all_label_formats(map(), map()) -> {ok, map()} | {fail, term()}.
validate_all_label_formats(Metadata, FormatRules) ->
    ValidationResults = maps:fold(fun(Key, Value, Acc) ->
        case maps:get(Key, FormatRules, undefined) of
            undefined ->
                %% Check if binary key exists
                case maps:get(atom_to_binary(Key, utf8), FormatRules, undefined) of
                    undefined ->
                        Acc;  %% Unknown label, skip (will be caught by unexpected labels check)
                    {Type, _Required} ->
                        _BinaryKey = atom_to_binary(Key, utf8),
                        case validate_label_type(Value, Type) of
                            ok ->
                                Acc;
                            {fail, Reason} ->
                                [{Key, Reason} | Acc]
                        end
                end;
            {Type, _Required} ->
                case validate_label_type(Value, Type) of
                    ok ->
                        Acc;
                    {fail, Reason} ->
                        [{Key, Reason} | Acc]
                end
        end
    end, [], Metadata),
    
    case ValidationResults of
        [] ->
            {ok, #{validated => maps:size(Metadata)}};
        Errors ->
            {fail, {format_errors, Errors}}
    end.

%% @doc Validate expected label values
-spec validate_expected_labels(map(), map()) -> {ok, map()} | {fail, term()}.
validate_expected_labels(_Metadata, ExpectedLabels) when map_size(ExpectedLabels) =:= 0 ->
    {ok, #{}};
validate_expected_labels(Metadata, ExpectedLabels) ->
    Mismatches = maps:fold(fun(Key, ExpectedValue, Acc) ->
        ActualValue = maps:get(Key, Metadata, maps:get(atom_to_binary(Key, utf8), Metadata, undefined)),
        case ActualValue of
            ExpectedValue ->
                Acc;
            _ ->
                [{Key, ExpectedValue, ActualValue} | Acc]
        end
    end, [], ExpectedLabels),
    
    case Mismatches of
        [] ->
            {ok, #{matched => maps:size(ExpectedLabels)}};
        _ ->
            {fail, {value_mismatches, Mismatches}}
    end.

%% @doc Validate label format (type and format constraints)
-spec validate_label_format(term(), atom()) -> ok | {fail, term()}.
validate_label_format(Value, assignment_id) ->
    validate_binary_format(Value, <<"assignment_id">>);
validate_label_format(Value, request_id) ->
    validate_binary_format(Value, <<"request_id">>);
validate_label_format(Value, msg_id) ->
    validate_binary_format(Value, <<"msg_id">>);
validate_label_format(Value, reason) ->
    validate_binary_format(Value, <<"reason">>);
validate_label_format(Value, source) ->
    validate_binary_format(Value, <<"source">>);
validate_label_format(Value, tenant_id) ->
    validate_binary_format(Value, <<"tenant_id">>);
validate_label_format(Value, delivery_count) ->
    validate_integer_format(Value, <<"delivery_count">>);
validate_label_format(Value, max_deliver) ->
    validate_integer_format(Value, <<"max_deliver">>);
validate_label_format(Value, subject) ->
    validate_binary_format(Value, <<"subject">>);
validate_label_format(Value, error) ->
    validate_binary_format(Value, <<"error">>);
validate_label_format(Value, status) ->
    validate_binary_format(Value, <<"status">>);
validate_label_format(Value, provider_id) ->
    validate_binary_format(Value, <<"provider_id">>);
validate_label_format(_Value, _LabelName) ->
    ok.  %% Unknown label, skip format validation

%% @doc Validate label type
-spec validate_label_type(term(), atom()) -> ok | {fail, term()}.
validate_label_type(Value, binary) when is_binary(Value) ->
    ok;
validate_label_type(Value, binary) ->
    {fail, {invalid_type, binary, Value}};
validate_label_type(Value, integer) when is_integer(Value) andalso Value >= 0 ->
    ok;
validate_label_type(Value, integer) ->
    {fail, {invalid_type, integer, Value}};
validate_label_type(Value, atom) when is_atom(Value) ->
    ok;
validate_label_type(Value, atom) ->
    {fail, {invalid_type, atom, Value}};
validate_label_type(_Value, _Type) ->
    ok.

%% @doc Validate binary format (non-empty binary)
-spec validate_binary_format(term(), binary()) -> ok | {fail, term()}.
validate_binary_format(Value, LabelName) when is_binary(Value) ->
    case byte_size(Value) > 0 of
        true ->
            ok;
        false ->
            {fail, {empty_binary, LabelName}}
    end;
validate_binary_format(Value, LabelName) ->
    {fail, {invalid_binary, LabelName, Value}}.

%% @doc Validate integer format (non-negative integer)
-spec validate_integer_format(term(), binary()) -> ok | {fail, term()}.
validate_integer_format(Value, LabelName) when is_integer(Value) ->
    case Value >= 0 of
        true ->
            ok;
        false ->
            {fail, {negative_integer, LabelName, Value}}
    end;
validate_integer_format(Value, LabelName) ->
    {fail, {invalid_integer, LabelName, Value}}.

%% @doc Assert metric contract compliance (convenience function)
%% Throws ct:fail if contract is violated
-spec assert_metric_contract(atom(), map(), map()) -> ok.
assert_metric_contract(MetricName, Metadata, ExpectedLabels) ->
    case validate_metric_labels(MetricName, Metadata, ExpectedLabels) of
        {ok, _Details} ->
            ok;
        {fail, {missing_required_labels, Missing}} ->
            ct:fail("Metric ~p missing required labels: ~p. Metadata: ~p", 
                   [MetricName, Missing, Metadata]);
        {fail, {unexpected_labels, Unexpected}} ->
            ct:fail("Metric ~p has unexpected labels: ~p. Metadata: ~p", 
                   [MetricName, Unexpected, Metadata]);
        {fail, {format_errors, Errors}} ->
            ct:fail("Metric ~p has format errors: ~p. Metadata: ~p", 
                   [MetricName, Errors, Metadata]);
        {fail, {value_mismatches, Mismatches}} ->
            ct:fail("Metric ~p has value mismatches: ~p. Metadata: ~p", 
                   [MetricName, Mismatches, Metadata]);
        {fail, Reason} ->
            ct:fail("Metric ~p contract validation failed: ~p. Metadata: ~p", 
                   [MetricName, Reason, Metadata])
    end.

