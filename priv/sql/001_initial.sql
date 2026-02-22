-- GatePulse PostgreSQL schema
-- Migration: 001_initial

CREATE TABLE IF NOT EXISTS gp_events (
    id            TEXT        PRIMARY KEY,
    tenant_id     TEXT        NOT NULL,
    topic         TEXT        NOT NULL,
    payload       JSONB       NOT NULL,
    occurred_at   BIGINT      NOT NULL,
    created_at    BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_events_tenant_created
    ON gp_events (tenant_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_gp_events_tenant_topic
    ON gp_events (tenant_id, topic);

CREATE TABLE IF NOT EXISTS gp_endpoints (
    id            TEXT        PRIMARY KEY,
    tenant_id     TEXT        NOT NULL,
    url           TEXT        NOT NULL,
    name          TEXT,
    secret        TEXT,
    secret_iv     TEXT,
    secret_ver    INTEGER     DEFAULT 1,
    created_at    BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT,
    deleted_at    BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_endpoints_tenant
    ON gp_endpoints (tenant_id) WHERE deleted_at IS NULL;

CREATE TABLE IF NOT EXISTS gp_subscriptions (
    id            TEXT        PRIMARY KEY,
    tenant_id     TEXT        NOT NULL,
    endpoint_id   TEXT        NOT NULL REFERENCES gp_endpoints(id),
    topic_pattern TEXT        NOT NULL DEFAULT '#',
    filter        JSONB,
    transform     JSONB,
    created_at    BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT,
    deleted_at    BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_subscriptions_tenant
    ON gp_subscriptions (tenant_id) WHERE deleted_at IS NULL;

CREATE TABLE IF NOT EXISTS gp_jobs (
    id            TEXT        PRIMARY KEY,
    event_id      TEXT        NOT NULL REFERENCES gp_events(id),
    endpoint_id   TEXT        NOT NULL REFERENCES gp_endpoints(id),
    tenant_id     TEXT        NOT NULL,
    status        TEXT        NOT NULL DEFAULT 'pending',
    attempts      INTEGER     NOT NULL DEFAULT 0,
    max_attempts  INTEGER     NOT NULL DEFAULT 5,
    next_attempt  BIGINT,
    lease_until   BIGINT,
    claimed_by    TEXT,
    created_at    BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_jobs_pending
    ON gp_jobs (next_attempt) WHERE status = 'pending';

CREATE TABLE IF NOT EXISTS gp_attempts (
    id            BIGSERIAL   PRIMARY KEY,
    job_id        TEXT        NOT NULL REFERENCES gp_jobs(id),
    event_id      TEXT        NOT NULL,
    endpoint_id   TEXT        NOT NULL,
    tenant_id     TEXT        NOT NULL,
    attempt       INTEGER     NOT NULL,
    status        TEXT        NOT NULL,
    http_status   INTEGER,
    latency_ms    INTEGER,
    response_body TEXT,
    error         TEXT,
    attempted_at  BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_attempts_job
    ON gp_attempts (job_id);

CREATE TABLE IF NOT EXISTS gp_dlq (
    id            TEXT        PRIMARY KEY,
    job_id        TEXT,
    event_id      TEXT,
    endpoint_id   TEXT,
    tenant_id     TEXT        NOT NULL,
    reason        TEXT,
    payload       JSONB,
    created_at    BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_dlq_tenant
    ON gp_dlq (tenant_id, created_at DESC);

CREATE TABLE IF NOT EXISTS gp_api_keys (
    id            TEXT        PRIMARY KEY,
    tenant_id     TEXT        NOT NULL,
    token         TEXT        NOT NULL UNIQUE,
    name          TEXT,
    scopes        TEXT[]      NOT NULL DEFAULT ARRAY['*'],
    created_at    BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT,
    revoked_at    BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_api_keys_token
    ON gp_api_keys (token) WHERE revoked_at IS NULL;

CREATE TABLE IF NOT EXISTS gp_audit (
    id            BIGSERIAL   PRIMARY KEY,
    tenant_id     TEXT        NOT NULL,
    action        TEXT        NOT NULL,
    metadata      JSONB,
    ts            BIGINT      NOT NULL DEFAULT (EXTRACT(EPOCH FROM NOW()) * 1000)::BIGINT
);

CREATE INDEX IF NOT EXISTS idx_gp_audit_tenant_ts
    ON gp_audit (tenant_id, ts DESC);
