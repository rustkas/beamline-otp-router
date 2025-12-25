%% # Admin Control Plane Status Module
%%
%% Provides admin control plane status and health check functionality.
%% This module is used primarily for testing admin endpoints.

-module(router_admin_cp_status).

-doc "Admin control plane status module for health checks and admin operations".

-export([
    handle_admin_request/2,
    supported_admin_subjects/0
]).

%% List of supported admin subjects
-define(ADMIN_SUBJECTS, [
    <<"beamline.router.admin.get_policy">>,
    <<"beamline.router.admin.put_policy">>,
    <<"beamline.router.admin.delete_policy">>,
    <<"beamline.router.admin.list_policies">>,
    <<"beamline.router.admin.health">>
]).

%% Returns list of supported admin subjects
-spec supported_admin_subjects() -> [binary()].
supported_admin_subjects() ->
    ?ADMIN_SUBJECTS.

%% Handle admin request for a given subject.
%% This is a simplified implementation for testing purposes.
%% In production, this would route to actual admin handlers.
-spec handle_admin_request(binary(), map()) -> {ok, map()} | {error, binary()}.
handle_admin_request(Subject, Request) when is_binary(Subject), is_map(Request) ->
    case lists:member(Subject, ?ADMIN_SUBJECTS) of
        true ->
            %% Return a simple success response for supported subjects
            {ok, #{
                status => <<"ok">>,
                subject => Subject,
                timestamp => erlang:system_time(millisecond)
            }};
        false ->
            %% Return error for unsupported subjects
            {error, <<"Unsupported admin subject">>}
    end;
handle_admin_request(_Subject, _Request) ->
    {error, <<"Invalid request format">>}.

