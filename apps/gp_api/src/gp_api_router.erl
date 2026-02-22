-module(gp_api_router).
-export([dispatch/0]).

dispatch() ->
    cowboy_router:compile([
        {'_', routes()}
    ]).

routes() ->
    [
        %% Health
        {"/healthz",                           gp_api_h_health,    #{check => liveness}},
        {"/readyz",                            gp_api_h_health,    #{check => readiness}},
        %% Metrics
        {"/metrics",                           gp_api_h_metrics,   #{}},
        %% OpenAPI
        {"/openapi.yaml",                      gp_api_h_openapi,   #{}},
        %% Events
        {"/v1/events",                         gp_api_h_events,    #{}},
        {"/v1/events/:id",                     gp_api_h_events,    #{}},
        %% Endpoints
        {"/v1/endpoints",                      gp_api_h_endpoints, #{}},
        {"/v1/endpoints/:id",                  gp_api_h_endpoints, #{}},
        %% Subscriptions
        {"/v1/subscriptions",                  gp_api_h_subscriptions, #{}},
        {"/v1/subscriptions/:id",              gp_api_h_subscriptions, #{}},
        %% Deliveries
        {"/v1/deliveries",                     gp_api_h_deliveries,  #{}},
        {"/v1/deliveries/:id",                 gp_api_h_deliveries,  #{}},
        %% Replay
        {"/v1/replay",                         gp_api_h_replay,    #{}},
        {"/v1/replay/:id",                     gp_api_h_replay,    #{}},
        %% DLQ
        {"/v1/dlq",                            gp_api_h_dlq,       #{}},
        {"/v1/dlq/:id",                        gp_api_h_dlq,       #{}},
        %% SSE stream
        {"/v1/stream",                         gp_api_h_stream,    #{}},
        %% API Keys (multi-tenant)
        {"/v1/apikeys",                        gp_api_h_apikeys,   #{}},
        {"/v1/apikeys/:id",                    gp_api_h_apikeys,   #{}},
        %% Dev inbox
        {"/v1/dev/inbox",                      gp_api_h_dev_inbox, #{}},
        {"/v1/dev/inbox/ui",                   gp_api_h_dev_inbox, #{action => ui}},
        {"/v1/dev/inbox/messages",             gp_api_h_dev_inbox, #{action => messages}},
        {"/v1/dev/inbox/messages/:id",         gp_api_h_dev_inbox, #{action => messages}},
        {"/v1/dev/inbox/receive/:token",       gp_api_h_dev_inbox, #{action => receive_hook}},
        %% Admin
        {"/v1/admin/stats",                    gp_api_h_admin,     #{}},
        {"/v1/admin/compact",                  gp_api_h_admin,     #{}},
        {"/v1/admin/audit",                    gp_api_h_admin,     #{}},
        {"/v1/admin/rotate-secrets",           gp_api_h_admin,     #{}},
        {"/v1/admin/store/pause-claims",       gp_api_h_admin,     #{}},
        {"/v1/admin/store/resume-claims",      gp_api_h_admin,     #{}},
        %% Management console
        {"/console",                           gp_api_h_console,   #{}},
        {"/console/[...]",                     gp_api_h_console,   #{}}
    ].
