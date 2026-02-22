%% PostgreSQL store backend stub.
%% Activated when GP_STORE_BACKEND=postgres.
%% Requires GP_POSTGRES_URL environment variable.
%%
%% This module implements the gp_store_behaviour callbacks.
%% In the current release it delegates all operations to the C backend
%% (gp_store_client) as a compatibility shim. Full PostgreSQL support
%% is planned for v2.0-F.
-module(gp_store_pg).
-behaviour(gp_store_behaviour).

-export([
    append_event/3,
    get_event/2,
    list_events/2,
    enqueue_job/1,
    claim_jobs/2,
    ack_job/1,
    nack_job/2,
    put_dlq/1,
    list_dlq/1,
    requeue_dlq/1,
    delete_dlq/1,
    append_attempt/1,
    list_attempts/1,
    put_endpoint/1,
    get_endpoint/1,
    list_endpoints/1,
    delete_endpoint/1,
    put_subscription/1,
    list_subscriptions/1,
    delete_subscription/1,
    compact/1,
    queue_stats/1
]).

%% All callbacks delegate to the C backend for now.
append_event(EventId, TenantId, Payload) ->
    gp_store_client:append_event(EventId, TenantId, Payload).

get_event(TenantId, EventId) ->
    gp_store_client:get_event(TenantId, EventId).

list_events(TenantId, Opts) ->
    gp_store_client:list_events(TenantId, Opts).

enqueue_job(Job) ->
    gp_store_client:enqueue_job(Job).

claim_jobs(MaxCount, LeaseSecs) ->
    gp_store_client:claim_jobs(MaxCount, LeaseSecs).

ack_job(JobId) ->
    gp_store_client:ack_job(JobId).

nack_job(JobId, DelaySecs) ->
    gp_store_client:nack_job(JobId, DelaySecs).

put_dlq(Entry) ->
    gp_store_client:put_dlq(Entry).

list_dlq(TenantId) ->
    gp_store_client:list_dlq(TenantId).

requeue_dlq(JobId) ->
    gp_store_client:requeue_dlq(JobId).

delete_dlq(JobId) ->
    gp_store_client:delete_dlq(JobId).

append_attempt(Attempt) ->
    gp_store_client:append_attempt(Attempt).

list_attempts(Opts) ->
    gp_store_client:list_attempts(Opts).

put_endpoint(Endpoint) ->
    gp_store_client:put_endpoint(Endpoint).

get_endpoint(EndpointId) ->
    gp_store_client:get_endpoint(EndpointId).

list_endpoints(TenantId) ->
    gp_store_client:list_endpoints(TenantId).

delete_endpoint(EndpointId) ->
    gp_store_client:delete_endpoint(EndpointId).

put_subscription(Sub) ->
    gp_store_client:put_subscription(Sub).

list_subscriptions(TenantId) ->
    gp_store_client:list_subscriptions(TenantId).

delete_subscription(SubscriptionId) ->
    gp_store_client:delete_subscription(SubscriptionId).

compact(RetentionSecs) ->
    gp_store_client:compact(RetentionSecs).

queue_stats(TenantId) ->
    gp_store_client:queue_stats(TenantId).
