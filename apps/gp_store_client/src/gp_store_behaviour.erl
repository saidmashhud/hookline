%% Callback spec for GatePulse store backends.
%% Implemented by gp_store_client (C daemon) and gp_store_pg (PostgreSQL).
-module(gp_store_behaviour).

-callback append_event(EventId :: binary(), TenantId :: binary(),
                       Payload :: binary()) ->
    {ok, map()} | {error, term()}.

-callback get_event(TenantId :: binary(), EventId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback list_events(TenantId :: binary(), Opts :: map()) ->
    {ok, map()} | {error, term()}.

-callback enqueue_job(Job :: map()) ->
    {ok, map()} | {error, term()}.

-callback claim_jobs(MaxCount :: integer(), LeaseSecs :: integer()) ->
    {ok, map()} | {error, term()}.

-callback ack_job(JobId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback nack_job(JobId :: binary(), DelaySecs :: integer()) ->
    {ok, map()} | {error, term()}.

-callback put_dlq(Entry :: map()) ->
    {ok, map()} | {error, term()}.

-callback list_dlq(TenantId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback requeue_dlq(JobId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback delete_dlq(JobId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback append_attempt(Attempt :: map()) ->
    {ok, map()} | {error, term()}.

-callback list_attempts(Opts :: map()) ->
    {ok, map()} | {error, term()}.

-callback put_endpoint(Endpoint :: map()) ->
    {ok, map()} | {error, term()}.

-callback get_endpoint(EndpointId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback list_endpoints(TenantId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback delete_endpoint(EndpointId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback put_subscription(Sub :: map()) ->
    {ok, map()} | {error, term()}.

-callback list_subscriptions(TenantId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback delete_subscription(SubscriptionId :: binary()) ->
    {ok, map()} | {error, term()}.

-callback compact(RetentionSecs :: integer()) ->
    {ok, map()} | {error, term()}.

-callback queue_stats(TenantId :: binary()) ->
    {ok, map()} | {error, term()}.
