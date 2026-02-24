-module(hl_api_router).
-export([dispatch/0]).

dispatch() ->
    cowboy_router:compile([
        {'_', routes()}
    ]).

routes() ->
    [
        %% Health
        {"/healthz",                           hl_api_h_health,    #{check => liveness}},
        {"/readyz",                            hl_api_h_health,    #{check => readiness}},
        {"/v1/health/embedded",               hl_api_h_health,    #{check => embedded}},
        %% Metrics
        {"/metrics",                           hl_api_h_metrics,   #{}},
        %% OpenAPI
        {"/openapi.yaml",                      hl_api_h_openapi,   #{}},
        %% Events
        {"/v1/events",                         hl_api_h_events,    #{}},
        {"/v1/events/:id",                     hl_api_h_events,    #{}},
        %% Endpoints
        {"/v1/endpoints",                      hl_api_h_endpoints, #{}},
        {"/v1/endpoints/:id",                  hl_api_h_endpoints, #{}},
        %% Subscriptions
        {"/v1/subscriptions",                  hl_api_h_subscriptions, #{}},
        {"/v1/subscriptions/:id",              hl_api_h_subscriptions, #{}},
        %% Deliveries
        {"/v1/deliveries",                     hl_api_h_deliveries,  #{}},
        {"/v1/deliveries/:id",                 hl_api_h_deliveries,  #{}},
        %% Replay
        {"/v1/replay",                         hl_api_h_replay,    #{}},
        {"/v1/replay/:id",                     hl_api_h_replay,    #{}},
        %% DLQ
        {"/v1/dlq",                            hl_api_h_dlq,       #{}},
        {"/v1/dlq/:id/requeue",                hl_api_h_dlq,       #{action => requeue}},
        %% SSE stream
        {"/v1/stream",                         hl_api_h_stream,    #{}},
        %% API Keys — current tenant self-service
        {"/v1/apikeys",                              hl_api_h_apikeys,         #{}},
        {"/v1/apikeys/:id",                          hl_api_h_apikeys,         #{}},
        %% Tenant management (admin only) — specific routes before wildcards
        {"/v1/tenants/:id/stats",                    hl_api_h_tenants,         #{action => stats}},
        {"/v1/tenants/:tenant_id/api-keys/:key_id",  hl_api_h_tenant_apikeys,  #{}},
        {"/v1/tenants/:tenant_id/api-keys",          hl_api_h_tenant_apikeys,  #{}},
        {"/v1/tenants/:id",                          hl_api_h_tenants,         #{}},
        {"/v1/tenants",                              hl_api_h_tenants,         #{}},
        %% Dev inbox
        {"/v1/dev/inbox",                      hl_api_h_dev_inbox, #{}},
        {"/v1/dev/inbox/ui",                   hl_api_h_dev_inbox, #{action => ui}},
        {"/v1/dev/inbox/messages",             hl_api_h_dev_inbox, #{action => messages}},
        {"/v1/dev/inbox/messages/:id",         hl_api_h_dev_inbox, #{action => messages}},
        {"/v1/dev/inbox/receive/:token",       hl_api_h_dev_inbox, #{action => receive_hook}},
        %% Admin
        {"/v1/admin/stats",                    hl_api_h_admin,     #{}},
        {"/v1/admin/compact",                  hl_api_h_admin,     #{}},
        {"/v1/admin/audit",                    hl_api_h_admin,     #{}},
        {"/v1/admin/rotate-secrets",           hl_api_h_admin,     #{}},
        {"/v1/admin/store/pause-claims",       hl_api_h_admin,     #{}},
        {"/v1/admin/store/resume-claims",      hl_api_h_admin,     #{}},
        %% Management console
        {"/console",                           hl_api_h_console,   #{}},
        {"/console/[...]",                     hl_api_h_console,   #{}}
    ].
