%% Audit log: records security-relevant mutations.
-module(gp_core_audit).
-export([log/3]).

%% Log an audit event.
%% Action: binary, e.g. <<"endpoint.created">>, <<"apikey.revoked">>
%% Metadata: map with relevant context
log(TenantId, Action, Metadata) when is_binary(TenantId), is_binary(Action) ->
    catch gp_store_client:append_audit(TenantId, Action, Metadata).
