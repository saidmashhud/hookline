-module(gp_api_tenants).
-export([verify/1]).

%% Multi-tenant mode: look up API key in store
verify(Token) when is_binary(Token) ->
    case gp_store_client:get_event(<<"__apikeys__">>, Token) of
        {ok, #{<<"event">> := #{<<"tenant_id">> := TenantId}}} ->
            {ok, TenantId};
        _ ->
            {error, invalid_token}
    end.
