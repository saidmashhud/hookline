%% Forwards a write request to the current leader node via HTTP.
%% Used when this node is not the leader and receives a POST /v1/events.
%%
%% HL_WRITER_ID — optional single-writer enforcement.
%% When set, only the node whose Erlang node name matches HL_WRITER_ID is
%% allowed to accept event writes directly.  All other nodes must proxy.
%% Use this in embedded mode to guarantee exactly-once write ordering:
%%   HL_WRITER_ID=hookline@node1
-module(hl_leader_proxy).
-export([forward/2, is_writer/0]).

%% Returns true if this node is designated as the sole writer.
%% When HL_WRITER_ID is empty, any node may write (default behaviour).
is_writer() ->
    case hl_config:get_str("HL_WRITER_ID", "") of
        "" ->
            %% No writer restriction — all nodes may write.
            true;
        WriterId ->
            atom_to_list(node()) =:= WriterId
    end.

%% forward/2 — POST Body to the leader's /v1/events endpoint.
%%
%% TenantId : binary()  — value of x-tenant-id header
%% Body     : binary()  — raw request body (JSON)
%%
%% Returns the raw httpc result or {error, Reason}.
forward(TenantId, Body) ->
    case global:whereis_name(hookline_leader) of
        undefined ->
            {error, no_leader};
        Pid ->
            LeaderNode = node(Pid),
            case erpc:call(LeaderNode, hl_config, get_str,
                           ["HL_PUBLIC_URL", ""], 5000) of
                "" ->
                    {error, no_public_url};
                URL ->
                    Target = URL ++ "/v1/events",
                    ServiceToken = hl_config:get_str("HL_SERVICE_TOKEN", ""),
                    AuthHeader = case hl_config:get_str("HL_AUTH_MODE", "api_key") of
                        "service_token" when ServiceToken =/= "" ->
                            {ok, [{"authorization", "Bearer " ++ ServiceToken}]};
                        "service_token" ->
                            {error, missing_service_token};
                        _ ->
                            case hl_config:get_str("HL_API_KEY", "") of
                                "" ->
                                    {error, missing_api_key};
                                ApiKey ->
                                    {ok, [{"authorization", "Bearer " ++ ApiKey}]}
                            end
                    end,
                    case AuthHeader of
                        {error, _} = Err ->
                            Err;
                        {ok, Headers} ->
                            httpc:request(post,
                                {Target,
                                 Headers ++ [{"x-tenant-id", binary_to_list(TenantId)}],
                                 "application/json",
                                 Body},
                                [{timeout, 5000}],
                                [])
                    end
            end
    end.
