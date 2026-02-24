%% =============================================================================
%% STUB — PostgreSQL backend is NOT yet implemented.
%%
%% Every callback below delegates to the C backend (hl_store_client).
%% This module exists only to satisfy the hl_store_behaviour contract so that
%% HL_STORE_BACKEND=postgres can be set without compile errors.
%%
%% Full PostgreSQL support (epgsql, migrations, schema) is planned for v2.0-F.
%% Do NOT use this in production with a real PostgreSQL database.
%% =============================================================================
%%
%% Activated when HL_STORE_BACKEND=postgres.
%% Requires HL_POSTGRES_URL environment variable (currently unused).
-module(hl_store_pg).
-behaviour(hl_store_behaviour).

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
    requeue_dlq/2,
    delete_dlq/2,
    append_attempt/1,
    list_attempts/1,
    put_endpoint/1,
    get_endpoint/2,
    list_endpoints/1,
    delete_endpoint/2,
    put_subscription/1,
    list_subscriptions/1,
    delete_subscription/2,
    compact/1,
    queue_stats/1
]).

%% All callbacks delegate to the C backend for now.
append_event(EventId, TenantId, Payload) ->
    hl_store_client:append_event(EventId, TenantId, Payload).

get_event(TenantId, EventId) ->
    hl_store_client:get_event(TenantId, EventId).

list_events(TenantId, Opts) ->
    hl_store_client:list_events(TenantId, Opts).

enqueue_job(Job) ->
    hl_store_client:enqueue_job(Job).

claim_jobs(MaxCount, LeaseSecs) ->
    hl_store_client:claim_jobs(MaxCount, LeaseSecs).

ack_job(JobId) ->
    hl_store_client:ack_job(JobId).

nack_job(JobId, DelaySecs) ->
    hl_store_client:nack_job(JobId, DelaySecs).

put_dlq(Entry) ->
    hl_store_client:put_dlq(Entry).

list_dlq(TenantId) ->
    hl_store_client:list_dlq(TenantId).

requeue_dlq(TenantId, JobId) ->
    hl_store_client:requeue_dlq(TenantId, JobId).

delete_dlq(TenantId, JobId) ->
    hl_store_client:delete_dlq(TenantId, JobId).

append_attempt(Attempt) ->
    hl_store_client:append_attempt(Attempt).

list_attempts(Opts) ->
    hl_store_client:list_attempts(Opts).

put_endpoint(Endpoint) ->
    hl_store_client:put_endpoint(Endpoint).

get_endpoint(TenantId, EndpointId) ->
    hl_store_client:get_endpoint(TenantId, EndpointId).

list_endpoints(TenantId) ->
    hl_store_client:list_endpoints(TenantId).

delete_endpoint(TenantId, EndpointId) ->
    hl_store_client:delete_endpoint(TenantId, EndpointId).

put_subscription(Sub) ->
    hl_store_client:put_subscription(Sub).

list_subscriptions(TenantId) ->
    hl_store_client:list_subscriptions(TenantId).

delete_subscription(TenantId, SubscriptionId) ->
    hl_store_client:delete_subscription(TenantId, SubscriptionId).

compact(RetentionSecs) ->
    hl_store_client:compact(RetentionSecs).

queue_stats(TenantId) ->
    hl_store_client:queue_stats(TenantId).
